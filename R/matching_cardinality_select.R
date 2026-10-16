# ==============================================================================
# Moment-constrained cardinality matching as a unit-selection search
# ==============================================================================
# A linear moment constraint on a matched sample is a constraint on which units
# are matched, never on how they are paired: with u, w and b the coefficients
# of .moment_coefficients(), a row's value on a matched set is
#
#     sum_i u_i s_i - sum_j w_j t_j - b * sum_i s_i,
#
# s and t the indicators of the matched left and right units. So is everything
# else the balance network's objective ranks above distance. The number of
# pairs is sum_i s_i; exact balance on a partition holds the matched counts of
# every cell equal; the imbalance at a refined level is half the sum over its
# cells of |a_c - b_c|, a_c and b_c the matched counts. Only the distance
# depends on the pairing, and for a fixed selection the cheapest pairing is an
# assignment problem.
#
# The search therefore runs over selections, one 0/1 column per unit, and
# settles the objective one tier at a time, in the order the network's tier
# weights rank them (.lex_tier_weights() makes each tier outweigh everything
# below it, so the order is exact):
#
#   1. the most pairs: maximize sum_i s_i;
#   2. per refined level, coarsest first, the least imbalance at that size;
#   3. the least total distance at that size and balance.
#
# Each tier is an integer programme over a few rows - the moment rows, one
# equation per exactly balanced cell, the optimum of every tier above it - and
# is solved by branch and bound on its linear relaxation
# (src/select/select_bb.h), its bound read exactly from the duals by weak
# duality.
#
# Two things are not rows of the programme until a selection needs them.
#
# Matchability. A selection with equal counts is a matched sample only if a
# perfect matching joins its units over the admissible pairs. When every pair
# is admissible that is automatic; when calipers or a distance cut forbid some,
# it is Hall's condition, and a selection violating it shows a deficient set X
# of its left units, whose admissible partners N(X) number fewer than X among
# the selected right units. Every matched sample satisfies
#
#     sum_{i in X} s_i <= sum_{j in N(X)} t_j,
#
# N(X) taken over every right unit, since each matched unit of X needs a
# distinct matched partner in N(X). The search adds that row and goes on.
#
# Distance. For a selection S the cheapest pairing is read off the balance
# network with every unit arc pinned to S, solved exactly. Its potentials give
# per-unit values U and V with d_ij - U_i - V_j >= 0 on every admissible pair,
# exactly (src/flow/flow_exact_read.h), and equality on the pairs it matched.
# By assignment duality every selection S' with a perfect matching then costs
#
#     dist(S') >= sum_{i in S'} U_i + sum_{j in S'} V_j,
#
# with equality at S. That is a Benders cut on a column theta standing for the
# distance, and the third tier minimizes theta under the cuts the selections it
# visits generate.
#
# Every number the search reports is exact. The bounds are weak-duality bounds
# evaluated in exact arithmetic or with a rigorous rounding margin; a selection
# is accepted only after its rows are recomputed exactly; and the matched set
# returned is read from the pinned network solve, audited and certified like
# any other.
# ==============================================================================

# The columns of a tier: s for the left units, t for the right ones, then the
# tier's own: an imbalance column per cell of every refined level, and theta.
.select_columns <- function(index) {
  hier <- index$hier
  soft <- if (hier$exact < hier$n_levels) seq.int(hier$exact + 1L, hier$n_levels)
          else integer(0)
  n_cells <- vapply(soft, function(h) hier$n_cats[[h + 1L]], numeric(1))
  ends <- cumsum(n_cells)
  e_offset <- index$n_left + index$n_right
  e_index <- lapply(seq_along(soft), function(k) {
    e_offset + seq.int(ends[[k]] - n_cells[[k]] + 1L, length.out = n_cells[[k]])
  })
  n_e <- if (length(ends)) ends[[length(ends)]] else 0L
  list(n_left = index$n_left, n_right = index$n_right,
       s = seq_len(index$n_left),
       t = index$n_left + seq_len(index$n_right),
       soft = soft, e = e_index,
       theta = e_offset + n_e + 1L,
       n = e_offset + n_e + 1L)
}

# One row: a coefficient matrix with one row per column, whose rows sum
# exactly to the coefficients, and the row's bounds. `parts` maps column
# indices to coefficient vectors or matrices of components.
.select_row <- function(cols, parts, lo, hi, hard = TRUE) {
  width <- max(1L, vapply(parts, function(p) NCOL(p$coef), integer(1)))
  coef <- matrix(0, cols$n, width)
  for (p in parts) {
    v <- as.matrix(p$coef)
    coef[p$at, seq_len(ncol(v))] <- v
  }
  list(coef = coef, lo = as.numeric(lo), hi = as.numeric(hi), hard = hard)
}

# Matched counts equal in every cell of the deepest exactly balanced level, or
# overall when no level is.
.select_balance_rows <- function(index, cols) {
  hier <- index$hier
  if (hier$exact >= 1L) {
    code_l <- .balance_codes_at(hier, index$code_left, hier$exact)
    code_r <- .balance_codes_at(hier, index$code_right, hier$exact)
    cells <- sort(unique(c(code_l, code_r)))
  } else {
    code_l <- rep(1L, index$n_left)
    code_r <- rep(1L, index$n_right)
    cells <- 1L
  }
  lapply(cells, function(cell) {
    .select_row(cols, list(list(at = cols$s, coef = as.numeric(code_l == cell)),
                           list(at = cols$t, coef = -as.numeric(code_r == cell))),
                0, 0)
  })
}

# One row per one-sided moment bound. The left entry u_i - b crosses as its
# two terms, which the solver adds exactly.
.select_moment_rows <- function(coefs, cols) {
  lapply(.as_moment_coefficient_list(coefs), function(cf) {
    .select_row(cols, list(list(at = cols$s, coef = cbind(cf$u, -cf$b)),
                           list(at = cols$t, coef = -cf$w)),
                -Inf, 0)
  })
}

# sum_i s_i fixed at a tier's optimum.
.select_count_row <- function(cols, k) {
  .select_row(cols, list(list(at = cols$s, coef = rep(1, cols$n_left))), k, k)
}

# e_c >= |a_c - b_c| for every cell of refined level `k` of cols$soft, as two
# rows per cell. They bind e from below only, so they are not hard rows: a
# selection's imbalance is computed, not checked.
.select_imbalance_rows <- function(index, cols, k) {
  hier <- index$hier
  h <- cols$soft[[k]]
  code_l <- .balance_codes_at(hier, index$code_left, h)
  code_r <- .balance_codes_at(hier, index$code_right, h)
  e <- cols$e[[k]]
  out <- vector("list", 0L)
  for (cell in seq_along(e)) {
    a <- as.numeric(code_l == cell)
    b <- as.numeric(code_r == cell)
    for (sgn in c(1, -1)) {
      out[[length(out) + 1L]] <- .select_row(
        cols, list(list(at = cols$s, coef = -sgn * a),
                   list(at = cols$t, coef = sgn * b),
                   list(at = e[[cell]], coef = 1)),
        0, Inf, hard = FALSE)
    }
  }
  out
}

# sum of e over level `k` at most `limit`: a refined level's optimum, held
# while the tiers below it are searched.
.select_imbalance_cap <- function(cols, k, limit) {
  .select_row(cols, list(list(at = cols$e[[k]], coef = rep(1, length(cols$e[[k]])))),
              -Inf, limit, hard = FALSE)
}

# Twice a selection's imbalance at refined level `k`.
.select_imbalance <- function(index, cols, k, sel) {
  hier <- index$hier
  h <- cols$soft[[k]]
  n_h <- hier$n_cats[[h + 1L]]
  a <- tabulate(.balance_codes_at(hier, index$code_left, h)[sel$left], nbins = n_h)
  b <- tabulate(.balance_codes_at(hier, index$code_right, h)[sel$right], nbins = n_h)
  abs(a - b)
}

.select_selection <- function(x, cols) {
  list(left = which(x[cols$s] > 0.5), right = which(x[cols$t] > 0.5))
}

# A point of the tier's columns holding a selection, its imbalance columns at
# their least values and theta at `theta`.
.select_point <- function(index, cols, sel, theta = 0) {
  x <- numeric(cols$n)
  x[cols$s[sel$left]] <- 1
  x[cols$t[sel$right]] <- 1
  for (k in seq_along(cols$soft)) {
    x[cols$e[[k]]] <- .select_imbalance(index, cols, k, sel)
  }
  x[[cols$theta]] <- theta
  x
}

# The arc bounds that restrict the network to a selection: a selected unit's
# arc keeps its capacity of one and every other unit's is closed. The arcs are
# not forced to one, so a solve over generated pairs that cannot yet join two
# selected units is a smaller matching rather than an infeasible network, and
# pricing then finds the pairs it lacks; the network ranks cardinality first,
# so the solve matches every selected unit whenever the selection can be
# matched at all.
.select_pin <- function(index, sel) {
  ranges <- index$ranges
  s <- numeric(index$n_left)
  s[sel$left] <- 1
  t <- numeric(index$n_right)
  t[sel$right] <- 1
  arcs <- c(ranges$unit_left, ranges$unit_right)
  .cardinality_add_edit(.cardinality_no_edits(), arcs, numeric(length(arcs)), c(s, t))
}

# Whether every pair is admissible, in which case any selection with equal
# counts has a perfect matching and Hall's condition never needs checking.
.select_complete <- function(cost) {
  if (is_lazy_cost_spec(cost)) {
    return(!is.finite(cost$max_distance) && !length(cost$calipers))
  }
  all(.is_valid_cost(cost))
}

# The Hall row a selection violates, or NULL when its units have a perfect
# matching over the admissible pairs.
.select_hall_row <- function(cost, session, cols, sel) {
  if (!length(sel$left)) {
    return(NULL)
  }
  if (is_lazy_cost_spec(cost)) {
    sub <- lazy_cost_spec_rows(cost, sel$left, sel$right)
    witness <- lap_hall_witness_lazy(sub$left_mat, sub$right_mat, cost$distance,
                                     lazy_cost_spec_inv_cov(cost),
                                     cost$max_distance,
                                     lazy_cost_spec_calipers(sub))
  } else {
    witness <- lap_hall_witness_dense(cost[sel$left, sel$right, drop = FALSE])
  }
  if (isTRUE(witness$row_perfect)) {
    return(NULL)
  }
  deficient <- sel$left[witness$rows]
  if (is_lazy_cost_spec(cost)) {
    n_right <- cols$n_right
    reach <- vapply(deficient, function(i) {
      list(which(!is.na(lap_pricing_cost(session, rep(i, n_right),
                                         seq_len(n_right)))))
    }, list(1L))
    partners <- sort(unique(unlist(reach)))
  } else {
    partners <- which(colSums(.is_valid_cost(cost[deficient, , drop = FALSE])) > 0)
  }
  .select_row(cols, list(list(at = cols$s[deficient], coef = rep(1, length(deficient))),
                         list(at = cols$t[partners], coef = -rep(1, length(partners)))),
              -Inf, 0)
}

# The pinned network solve of a selection, cached by selection: the cheapest
# pairing of its units, read exactly, with the distance it costs and the cut
# its potentials give.
.select_pairing <- function(ctx, sel) {
  key <- paste(c(sel$left, 0L, sel$right), collapse = ",")
  hit <- ctx$cache[[key]]
  if (!is.null(hit)) {
    return(hit)
  }
  fl <- .cardinality_flow(ctx$problem, ctx$index, edits = .select_pin(ctx$index, sel),
                          cost = ctx$cost, warm = ctx$warm)
  ctx$n_solves <- ctx$n_solves + 1L
  out <- list(flow = fl, feasible = FALSE)
  if (!.cardinality_no_flow(fl$status) && !identical(fl$status, "interrupted")) {
    ctx$warm <- list(flow = fl$flow, potential = fl$potential)
    index <- fl$index
    pair <- index$ranges$pair
    out$feasible <- isTRUE(fl$audit_ok) && fl$integral &&
      identical(fl$status, "optimal") && fl$read$n_pairs == length(sel$left)
    out$distance <- lap_exact_dot(fl$problem$arcs$cost[pair], fl$flow[pair])
    out$exact_duals <- .select_lift(ctx, sel, fl$exact_duals)
  }
  ctx$cache[[key]] <- out
  out
}

# A pinned solve's pair potentials raised on the units outside the selection
# as far as validity allows (src/select/select_lift.h), which is what makes
# the cut charge a selection for the units it swaps in.
.select_lift <- function(ctx, sel, duals) {
  if (is.null(duals)) {
    return(NULL)
  }
  in_left <- seq_len(ctx$index$n_left) %in% sel$left
  in_right <- seq_len(ctx$index$n_right) %in% sel$right
  if (is.null(ctx$session)) {
    lap_select_lift_dense(ctx$cost, ctx$index$cost_shift, in_left, in_right,
                          duals$u, duals$v)
  } else {
    lap_pricing_lift(ctx$session, ctx$index$cost_shift, in_left, in_right,
                     duals$u, duals$v)
  }
}

# The distance cut of a pinned solve: theta - sum U s - sum V t >= 0, with U
# and V the solve's exact pair potentials. NULL when the solve's potentials
# were not recovered exactly, since only exact ones make the cut valid.
.select_distance_cut <- function(cols, pairing) {
  duals <- pairing$exact_duals
  if (is.null(duals)) {
    return(NULL)
  }
  .select_row(cols, list(list(at = cols$s, coef = -duals$u),
                         list(at = cols$t, coef = -duals$v),
                         list(at = cols$theta, coef = 1)),
              0, Inf, hard = FALSE)
}

# Integer supplies proportional to a fractional selection, M units to a whole
# one, rounded to nearest and evened out so that the two sides carry the same
# total.
.select_scaled_supplies <- function(s, t, M) {
  a <- round(M * s)
  b <- round(M * t)
  gap <- sum(a) - sum(b)
  if (gap != 0) {
    # Each unit of gap is taken off the larger side or added to the smaller,
    # wherever rounding moved a supply furthest in the direction of the gap.
    dir <- sign(gap)
    can_a <- if (dir > 0) a > 0 else a < M
    can_b <- if (dir > 0) b < M else b > 0
    resid <- c(dir * (a - M * s)[can_a], dir * (M * t - b)[can_b])
    side <- c(rep(1L, sum(can_a)), rep(2L, sum(can_b)))
    at <- c(which(can_a), which(can_b))
    pick <- order(-resid, side, at)[seq_len(min(abs(gap), length(at)))]
    for (k in pick) {
      if (side[[k]] == 1L) a[at[[k]]] <- a[at[[k]]] - dir
      else b[at[[k]]] <- b[at[[k]]] + dir
    }
  }
  list(a = a, b = b)
}

# The transportation network a fractional cut is read from, built once: a
# unit arc from the source to every left unit and from every right unit to the
# sink, then one arc per admissible pair, at its cost less the network's
# shift. A cost matrix contributes every admissible pair; a specification
# contributes the pairs the balance network has generated, and each solve
# extends them by pricing. Pairs are appended, so the arcs already there keep
# their indices and the last solve's flow stays a starting point.
.select_transport_net <- function(ctx) {
  if (!is.null(ctx$transport)) {
    return(ctx$transport)
  }
  index <- ctx$index
  n_left <- index$n_left
  n_right <- index$n_right
  if (is.null(ctx$session)) {
    cell <- which(.is_valid_cost(ctx$cost), arr.ind = TRUE)
    left <- as.integer(cell[, 1L])
    right <- as.integer(cell[, 2L])
    cost <- as.numeric(ctx$cost[cell]) - index$cost_shift
  } else {
    built <- .cardinality_current_index(ctx$index)
    left <- built$pair_left
    right <- built$pair_right
    cost <- built$pair_cost
  }
  node_left <- 2L + seq_len(n_left)
  node_right <- 2L + n_left + seq_len(n_right)
  ctx$transport <- list(
    n_nodes = 2L + n_left + n_right, node_left = node_left, node_right = node_right,
    n_units = n_left + n_right, left = left, right = right,
    tail = c(rep(1L, n_left), node_right, node_left[left]),
    head = c(node_left, rep(2L, n_right), node_right[right]),
    cost = c(numeric(n_left + n_right), cost),
    warm = NULL)
  ctx$transport
}

# A distance cut at a fractional selection: the duals of the transportation
# problem that ships `s` from the left units to `t` on the right, solved
# exactly at a scale of M units per whole unit. Any duals feasible on every
# admissible pair give a valid cut whatever the supplies were, so the scaling
# costs the cut nothing but a little of its tightness at (s, t). Returns NULL
# when the solve gives no exactly recovered duals.
.select_transport_cut <- function(ctx, s, t, M = 16) {
  sup <- .select_scaled_supplies(s, t, M)
  total <- sum(sup$a)
  if (total <= 0) {
    return(NULL)
  }
  index <- ctx$index
  net <- .select_transport_net(ctx)
  supply <- numeric(net$n_nodes)
  supply[[1L]] <- total
  supply[[2L]] <- -total
  repeat {
    n_pairs <- length(net$left)
    lower <- c(sup$a, sup$b, numeric(n_pairs))
    upper <- c(sup$a, sup$b, rep(total, n_pairs))
    warm <- net$warm
    warm_flow <- numeric(0)
    warm_potential <- numeric(0)
    if (!is.null(warm)) {
      f <- c(warm$flow, numeric(length(lower) - length(warm$flow)))
      warm_flow <- pmin(pmax(f, lower), upper)
      warm_potential <- warm$potential
    }
    solved <- lap_flow_solve(net$n_nodes, supply, net$tail, net$head, lower, upper,
                             net$cost, tol = 1e-12, relax_eps = 1e-18,
                             max_augmentations = 0, return_potentials = TRUE,
                             warm_flow = warm_flow, warm_potential = warm_potential,
                             time_limit = Inf)
    if (!identical(solved$status, "optimal")) {
      return(NULL)
    }
    step <- lap_flow_exact_read(
      net$n_nodes, supply, net$tail, net$head, lower, upper, net$cost,
      as.numeric(solved$flow), as.numeric(solved$potential),
      net$n_units + seq_len(n_pairs), net$left, net$right, net$node_left,
      net$node_right)
    if (!isTRUE(step$recovered)) {
      return(NULL)
    }
    net$warm <- list(flow = step$flow, potential = step$potential)
    ctx$transport <- net
    if (is.null(ctx$session)) {
      break
    }
    priced <- lap_pricing_price_exact(ctx$session, step$pair_u, step$pair_v,
                                      index$cost_shift, index$gen$keep_per_row)
    if (!length(priced$i)) {
      break
    }
    added <- lap_pricing_cost(ctx$session, priced$i, priced$j) - index$cost_shift
    net$left <- c(net$left, as.integer(priced$i))
    net$right <- c(net$right, as.integer(priced$j))
    net$tail <- c(net$tail, net$node_left[priced$i])
    net$head <- c(net$head, net$node_right[priced$j])
    net$cost <- c(net$cost, added)
    ctx$transport <- net
  }
  sel <- list(left = which(sup$a > 0), right = which(sup$b > 0))
  lifted <- .select_lift(ctx, sel, list(u = step$pair_u, v = step$pair_v))
  .select_distance_cut(ctx$cols, list(exact_duals = lifted))
}

# The value a cut row takes at a point, in doubles.
.select_row_value <- function(row, x) {
  sum(rowSums(row$coef) * x)
}

# The deepest node at which the distance stage cuts at fractional points.
.SELECT_SEPARATION_DEPTH <- 2L

# One tier: the rows, the objective, the oracle and a starting incumbent,
# searched under what remains of the budget. `read` turns the tier's objective
# value and bound into the stage's own units for the record the report keeps.
.select_tier <- function(ctx, stage, read, cost_vec, rows, oracle, start,
                         integral, separator = NULL) {
  started <- proc.time()[["elapsed"]]
  cols <- ctx$cols
  lower <- rep(0, cols$n)
  upper <- c(rep(1, cols$n_left + cols$n_right),
             rep(Inf, cols$n - cols$n_left - cols$n_right))
  upper[cols$s[ctx$dead_left]] <- 0
  upper[cols$t[ctx$dead_right]] <- 0
  integer <- c(rep(TRUE, cols$n_left + cols$n_right),
               rep(FALSE, cols$n - cols$n_left - cols$n_right))
  remaining_nodes <- ctx$node_limit - ctx$n_nodes
  res <- lap_select_solve(
    cost_vec, lower, upper, integer, c(ctx$rows, rows),
    if (cost_vec[[cols$theta]] != 0) cols$theta else 0L,
    integral, integral, start, oracle,
    max(0, remaining_nodes), .cardinality_remaining(ctx$deadline),
    ctx$stop, separator)
  ctx$n_nodes <- ctx$n_nodes + as.integer(res$n_nodes)
  ctx$stages[[length(ctx$stages) + 1L]] <- list(
    stage = stage,
    closed = identical(res$stopped_on, "optimality") && isTRUE(res$bound_certified),
    stopped_on = res$stopped_on,
    n_nodes = as.integer(res$n_nodes),
    seconds = proc.time()[["elapsed"]] - started,
    value = read(sum(res$value)),
    bound = read(res$bound))
  res
}

# The stages of a search as the report shows them, one row each.
.select_stage_table <- function(stages) {
  tibble::tibble(
    stage = vapply(stages, `[[`, character(1), "stage"),
    closed = vapply(stages, `[[`, logical(1), "closed"),
    stopped_on = vapply(stages, `[[`, character(1), "stopped_on"),
    n_nodes = vapply(stages, `[[`, integer(1), "n_nodes"),
    seconds = vapply(stages, `[[`, numeric(1), "seconds"),
    value = vapply(stages, `[[`, numeric(1), "value"),
    bound = vapply(stages, `[[`, numeric(1), "bound"))
}

# Lexicographic search over selections. Returns a `cardinality_run`.
.cardinality_select <- function(built, coefs, cost, node_limit, time_limit,
                                should_stop = NULL) {
  problem <- built$problem
  index <- built$index
  coefs <- .as_moment_coefficient_list(coefs)
  node_limit <- .flow_count(node_limit, "node_limit", allow_inf = TRUE)
  if (!is.numeric(time_limit) || length(time_limit) != 1L ||
      is.na(time_limit) || time_limit < 0) {
    stop("`time_limit` must be a single non-negative number of seconds, or ",
         "Inf.", call. = FALSE)
  }
  if (!is.null(should_stop) && !is.function(should_stop)) {
    stop("`should_stop` must be a function of the search state.", call. = FALSE)
  }
  started <- proc.time()[["elapsed"]]

  # The largest matched sample with no moment row, solved exactly. It bounds
  # every tier, and when it already satisfies every moment row it is the
  # answer: the optimum of a relaxation that is feasible for the problem.
  free <- .cardinality_flow(problem, index, cost = cost)
  index <- .cardinality_current_index(index)
  free_ok <- !.cardinality_no_flow(free$status) &&
    !identical(free$status, "interrupted")
  if (free_ok && (!length(coefs) ||
                  all(.cardinality_rows_exact(index, coefs, free$read) <= 0L))) {
    return(.select_run(index, free, coefs, best_possible = free$read$n_pairs,
                       settled = TRUE, stopped_on = "optimality", n_nodes = 0L,
                       stages = list()))
  }

  ctx <- new.env(parent = emptyenv())
  ctx$problem <- problem
  ctx$index <- index
  ctx$cost <- cost
  ctx$cache <- new.env(parent = emptyenv())
  ctx$warm <- if (free_ok) list(flow = free$flow, potential = free$potential)
  ctx$n_solves <- 1L
  ctx$n_nodes <- 0L
  ctx$stages <- list()
  ctx$node_limit <- node_limit
  ctx$deadline <- if (is.finite(time_limit)) started + time_limit else Inf
  # The predicate is asked before every node. A condition of class
  # "interrupt" raised from it stops the search the way the predicate
  # returning TRUE does, as an interrupt arriving between nodes would.
  ctx$stop <- if (!is.null(should_stop)) function(state) {
    tryCatch(isTRUE(should_stop(state)), interrupt = function(e) TRUE)
  }
  ctx$cols <- cols <- .select_columns(index)
  session <- if (is.null(cost)) index$gen$session else NULL
  ctx$session <- session
  complete <- .select_complete(if (is.null(cost)) index$gen$spec else cost)
  lazy_cost <- if (is.null(cost)) index$gen$spec else cost

  # Units with no admissible partner at all are never matched.
  if (!is.null(cost) && !is_lazy_cost_spec(cost)) {
    valid <- .is_valid_cost(cost)
    ctx$dead_left <- which(rowSums(valid) == 0)
    ctx$dead_right <- which(colSums(valid) == 0)
  } else {
    ctx$dead_left <- integer(0)
    ctx$dead_right <- integer(0)
  }

  ctx$rows <- c(.select_balance_rows(index, cols), .select_moment_rows(coefs, cols))
  for (k in seq_along(cols$soft)) {
    ctx$rows <- c(ctx$rows, .select_imbalance_rows(index, cols, k))
  }

  # Hall rows a tier finds hold for every tier after it.
  hall <- function(sel) {
    if (complete) {
      return(NULL)
    }
    .select_hall_row(lazy_cost, session, cols, sel)
  }
  # The empty selection satisfies every row.
  empty <- list(left = integer(0), right = integer(0))

  # Tier 1: the most pairs.
  c1 <- numeric(cols$n)
  c1[cols$s] <- -1
  oracle1 <- if (complete) NULL else function(x) {
    row <- hall(.select_selection(x, cols))
    list(feasible = is.null(row), value = NULL,
         rows = if (is.null(row)) NULL else list(row))
  }
  t1 <- .select_tier(ctx, "size", function(v) -v, c1, list(), oracle1,
                     list(x = .select_point(index, cols, empty), value = 0), TRUE)
  best <- .select_selection(t1$x, cols)
  # The largest matching bounds every tier, and a search that opened no node
  # proved nothing tighter.
  best_possible <- if (free_ok) free$read$n_pairs else min(index$n_left, index$n_right)
  if (is.finite(t1$bound)) {
    best_possible <- min(best_possible, floor(-t1$bound))
  }
  settled <- identical(t1$stopped_on, "optimality") && isTRUE(t1$bound_certified)
  stopped_on <- t1$stopped_on
  k_star <- length(best$left)
  ctx$rows <- c(ctx$rows, list(.select_count_row(cols, k_star)))

  # Tier 2: the least imbalance per refined level, coarsest first.
  for (k in seq_along(cols$soft)) {
    if (!settled) break
    c2 <- numeric(cols$n)
    c2[cols$e[[k]]] <- 1
    oracle2 <- function(x) {
      sel <- .select_selection(x, cols)
      row <- hall(sel)
      if (!is.null(row)) {
        return(list(feasible = FALSE, value = NULL, rows = list(row)))
      }
      list(feasible = TRUE, value = sum(.select_imbalance(index, cols, k, sel)),
           rows = NULL)
    }
    start <- list(x = .select_point(index, cols, best),
                  value = sum(.select_imbalance(index, cols, k, best)))
    level <- .cardinality_level_label(index$hier, cols$soft[[k]])
    t2 <- .select_tier(ctx, paste0("imbalance (", level, ")"),
                       function(v) v / 2, c2, list(), oracle2, start, TRUE)
    best <- .select_selection(t2$x, cols)
    settled <- identical(t2$stopped_on, "optimality") && isTRUE(t2$bound_certified)
    stopped_on <- t2$stopped_on
    ctx$rows <- c(ctx$rows, list(.select_imbalance_cap(
      cols, k, sum(.select_imbalance(index, cols, k, best)))))
  }

  # Tier 3: the least distance.
  pairing <- .select_pairing(ctx, best)
  if (settled && pairing$feasible) {
    c3 <- numeric(cols$n)
    c3[[cols$theta]] <- 1
    first_cut <- .select_distance_cut(cols, pairing)
    oracle3 <- function(x) {
      sel <- .select_selection(x, cols)
      row <- hall(sel)
      if (!is.null(row)) {
        return(list(feasible = FALSE, value = NULL, rows = list(row)))
      }
      p <- .select_pairing(ctx, sel)
      if (!p$feasible) {
        return(list(feasible = FALSE, value = NULL, rows = NULL))
      }
      value <- sum(p$distance)
      theta <- x[[cols$theta]]
      cut <- if (theta < value - 1e-9 * max(1, abs(value))) {
        .select_distance_cut(cols, p)
      }
      list(feasible = TRUE, value = p$distance,
           rows = if (is.null(cut)) NULL else list(cut))
    }
    start <- list(x = .select_point(index, cols, best,
                                    theta = .exact_value(pairing$distance, "up")),
                  value = pairing$distance)
    # At a fractional point the cut comes from a transportation solve; it is
    # added when the point's theta sits below it. A transportation solve costs
    # far more than a node, so it is spent where the bound of the whole tree is
    # made, at the nodes nearest the root; deeper nodes rely on the cuts their
    # integral points give.
    separator3 <- function(x, depth) {
      if (depth > .SELECT_SEPARATION_DEPTH) {
        return(NULL)
      }
      s <- x[cols$s]
      t <- x[cols$t]
      cut <- .select_transport_cut(ctx, s, t)
      if (is.null(cut)) {
        return(NULL)
      }
      at <- .select_row_value(cut, x)
      if (at <= -1e-9 * max(1, abs(x[[cols$theta]]))) list(cut) else NULL
    }
    shift <- k_star * index$cost_shift
    t3 <- .select_tier(ctx, "distance", function(v) v + shift, c3,
                       if (is.null(first_cut)) list() else list(first_cut),
                       oracle3, start, FALSE, separator = separator3)
    best <- .select_selection(t3$x, cols)
    settled <- identical(t3$stopped_on, "optimality") && isTRUE(t3$bound_certified) &&
      !is.null(first_cut)
    stopped_on <- t3$stopped_on
    pairing <- .select_pairing(ctx, best)
  }

  # Every selection the search accepts was matched in full by its own solve,
  # so this holds by construction; the empty selection, which satisfies every
  # row, stands in should it ever not.
  if (!pairing$feasible) {
    pairing <- .select_pairing(ctx, empty)
    settled <- FALSE
  }
  .select_run(ctx$index, pairing$flow, coefs, best_possible = best_possible,
              settled = settled, stopped_on = stopped_on,
              n_nodes = ctx$n_nodes, n_solves = ctx$n_solves,
              stages = ctx$stages)
}

# The run object .cardinality_report() reads, from the solve the matched set
# was read off.
.select_run <- function(index, solution, coefs, best_possible, settled,
                        stopped_on, n_nodes, n_solves = 1L, stages = list()) {
  n_pairs <- solution$read$n_pairs
  best_possible <- max(as.integer(best_possible), n_pairs)
  certified <- settled && isTRUE(solution$certified) && isTRUE(solution$audit_ok) &&
    identical(solution$status, "optimal") && best_possible <= n_pairs
  objective <- solution$objective
  # A lower bound on the network objective: the objective itself when the
  # search settled, and otherwise the cardinality term alone, since every tier
  # below it is non-negative.
  bound <- if (certified) objective else
    index$tiers$penalty * (index$total_budget - best_possible)
  structure(list(index = index,
                 solution = solution,
                 objective = objective,
                 bound = bound,
                 root_bound = bound,
                 best_possible = best_possible,
                 bound_certified = settled,
                 certified = certified,
                 stopped_on = .validate_cardinality_stop(
                   if (stopped_on %in% .CARDINALITY_STOPS) stopped_on else "optimality"),
                 status = if (certified) "optimal" else "iteration_limit",
                 n_nodes = as.integer(n_nodes),
                 n_solves = as.integer(n_solves),
                 stages = .select_stage_table(stages),
                 engine = if (length(coefs)) "branch_bound" else "flow",
                 coefs = coefs),
            class = "cardinality_run")
}
