# ==============================================================================
# Balance-constrained cardinality matching
# ==============================================================================
# The largest matched sample meeting a balance requirement is the optimum of the
# flow problem built in matching_balance_flow.R whenever every part of that
# requirement is categorical. This file is the driver for the case where it is
# not: a linear moment constraint on the matched sample has no place in a flow
# network, so it is the only thing relaxed, and everything else stays inside the
# network where it is solved exactly and certified.
#
# Write x for the arc flows of the balance network, so that
#
#     c(x) = P * (K - pairs) + sum_h eps_h * imbalance_h + sum_ij d_ij x_ij
#
# is the lexicographic objective the network encodes, and A x <= 0 for the
# moment rows of matching_moments.R, one row per one-sided bound. The problem
# solved here is
#
#     Z* = min { c(x) : x a flow of the network, A x <= 0 }.
#
# Relaxation. For multipliers lambda >= 0 the Lagrangian
#
#     L(lambda) = min { c(x) + lambda' A x : x a flow of the network }
#
# is a flow problem again: a^r_ij = u^r_i - w^r_j - b^r decomposes additively,
# so lambda touches nothing but the cost of the pair arcs, and the network's
# topology, bounds and every other arc cost are the ones already validated.
# For any x feasible for the original problem, A x <= 0 and lambda >= 0 give
# lambda' A x <= 0, hence L(lambda) <= c(x) and therefore L(lambda) <= Z*. The
# right-hand side of the moment rows is zero, so nothing is subtracted from that
# bound. It holds for every lambda >= 0, so the best bound a node knows is the
# largest L it has seen, and the multipliers are moved toward it by projected
# subgradient ascent, the subgradient of L at lambda being the vector of
# constraint values at the relaxed optimum.
#
# Cardinality from a bound. The objective separates as Z = P * q + r with
# q = S + T the units of slack and transfer, an integer, and
#
#     r = sum_h eps_h * imbalance_h + sum_ij d_ij x_ij,
#     0 <= r <= r_max = P - 1,
#
# the upper bound being what .lex_tier_weights() builds P to exceed. A lower
# bound Z_LB on Z* therefore bounds q from below by ceiling((Z_LB - r_max) / P),
# and since pairs = K - q,
#
#     best_possible = K - max(0, ceiling((Z_LB - r_max) / P))
#
# is an integer-valued upper bound on the cardinality of any feasible matched
# sample. The gap it forms against an incumbent is exact rather than estimated:
# one unit of cardinality is worth P, and P is larger than everything the rest
# of the objective can accumulate.
#
# Branching. A node is the same network with a few arc bounds fixed. Branching
# on the inclusion of left unit i sets the upper bound of its unit arc to 0 in
# one child and the lower bound to 1 in the other; branching on a pair sets the
# bounds of that pair arc the same way. Both children are single arc-bound edits
# on one network and their union covers the parent, so the tree is exhaustive.
# Each branch fixes an arc that was free, and a node whose pair arcs are all
# fixed has its matched set determined, so the tree is finite: unit branching
# hands over to pair branching once every left unit is decided.
#
# Root incumbent. A bound prunes nothing until something feasible is known, and
# the empty matched set leaves every bound below it, so the root cuts a matched
# set out of the relaxed optimum at zero multipliers before the first node is
# opened: pairs come off it until every moment row holds. It counts as an
# incumbent only once those rows are recomputed on the pairs themselves, which
# is what makes c(incumbent) an upper bound on Z* and lets the search prune
# against it.
#
# Certification. A node's bound is accepted only when verify_flow() certifies
# the solve it came from. An uncertified solve neither prunes nor contributes to
# the global bound, and it marks the whole run so that no gap of zero is ever
# reported as certified optimality. The incumbent is a matched set read back
# from a flow whose moment constraints are all recomputed and satisfied, so it
# is feasible for every stated constraint at every point in the search,
# including every early stop.
# ==============================================================================

# The reasons the node loop stops, in the order they are checked. Anything
# outside this set is a name nobody defined, and is refused where it is written
# rather than where it is read.
.CARDINALITY_STOPS <- c("optimality", "bound", "node_limit", "time_limit",
                        "interrupt")

.validate_cardinality_stop <- function(stopped_on) {
  if (!is.character(stopped_on) || length(stopped_on) != 1L ||
      is.na(stopped_on) || !stopped_on %in% .CARDINALITY_STOPS) {
    stop("Unknown stopping reason \"", stopped_on, "\"; expected one of ",
         paste(.CARDINALITY_STOPS, collapse = ", "), ".", call. = FALSE)
  }
  stopped_on
}

# A node problem is the network and the index that reads it. Accepts either the
# pair .balance_flow_problem() returns or the two objects separately.
.cardinality_as_node <- function(problem, index = NULL) {
  if (is.null(index)) {
    if (is.list(problem) && !is.null(problem$problem) &&
        !is.null(problem$index)) {
      return(list(problem = .as_flow_problem(problem$problem),
                  index = problem$index))
    }
    stop("`index` is required unless `problem` carries one, as the pair ",
         "`.balance_flow_problem()` returns does.", call. = FALSE)
  }
  list(problem = .as_flow_problem(problem), index = index)
}

# The empty set of branching decisions.
.cardinality_no_edits <- function() {
  list(arc = integer(0), lower = numeric(0), upper = numeric(0))
}

# One more arc pinned. An arc is edited once and never revisited, so a node's
# edits never disagree with each other.
.cardinality_add_edit <- function(edits, arc, lower, upper) {
  list(arc = c(edits$arc, as.integer(arc)),
       lower = c(edits$lower, as.numeric(lower)),
       upper = c(edits$upper, as.numeric(upper)))
}

# The network with a node's arc bounds in place. Costs are untouched here; the
# multipliers are applied separately so that the same node can be solved at
# several lambda without rebuilding its bounds.
.cardinality_edit_problem <- function(problem, edits) {
  if (is.null(edits) || !length(edits$arc)) {
    return(problem)
  }
  arcs <- problem$arcs
  arcs$lower[edits$arc] <- edits$lower
  arcs$upper[edits$arc] <- edits$upper
  .flow_problem(problem$n_nodes, problem$supply, arcs)
}

# Pair-arc costs under the multipliers. The additive decomposition means one
# vectorized pass per moment row over the admissible pairs, and no object of
# size n by m is built.
.cardinality_repriced_cost <- function(index, base_cost, coefs, lambda) {
  coefs <- .as_moment_coefficient_list(coefs)
  if (!length(coefs) || is.null(lambda) || all(lambda == 0)) {
    return(base_cost)
  }
  pair <- index$ranges$pair
  if (!length(pair)) {
    return(base_cost)
  }
  base_cost[pair] <- .moment_reprice(
    list(i = index$pair_left, j = index$pair_right, cost = index$pair_cost),
    coefs, lambda)
  base_cost
}

# A shortfall against the supply means no feasible flow exists at all. The
# solver splits that outcome by whether it managed to place any flow before
# running out of augmenting paths, so both halves of the split say the same
# thing about the node: it holds no matched set.
.cardinality_no_flow <- function(status) {
  status %in% c("infeasible", "partial")
}

# Every check the audit makes, as one answer.
.cardinality_audit_ok <- function(audit) {
  isTRUE(audit$feasible) &&
    abs(audit$identity_gap) <= 1e-6 * max(1, abs(audit$arc_cost)) &&
    abs(audit$pairs_identity_gap) <= 1e-9 &&
    audit$n_self_crossing == 0
}

#' One solve of a balance network
#'
#' Applies a node's arc bounds and multipliers, solves the network, certifies
#' the flow against the problem it was solved on, audits it against the
#' objective identity the design encodes, and reads the matched set back.
#'
#' @param problem The network, or the pair `.balance_flow_problem()` returns.
#' @param index The network's index, unless `problem` carries one.
#' @param coefs Moment coefficients, one per one-sided row.
#' @param lambda Multipliers, one per row.
#' @param edits The node's arc-bound decisions.
#' @param cost Optional distance matrix for the audit.
#' @param tol Numeric tolerance for the certificate.
#'
#' @return A list with the solve status, the flow and potentials, the
#'   certificate, the audit, the matched set, the true objective `objective`
#'   and the relaxed objective `relaxed` the multipliers price.
#' @keywords internal
.cardinality_flow <- function(problem, index = NULL, coefs = NULL,
                              lambda = NULL, edits = NULL, cost = NULL,
                              tol = 1e-9) {
  node <- .cardinality_as_node(problem, index)
  index <- node$index
  base <- .cardinality_edit_problem(node$problem, edits)

  arc_cost <- .cardinality_repriced_cost(index, base$arcs$cost, coefs, lambda)
  solve_problem <- base
  if (!identical(arc_cost, base$arcs$cost)) {
    solve_problem$arcs$cost <- arc_cost
  }

  solved <- .flow_solve(solve_problem)
  flow <- as.numeric(solved$flow)
  certificate <- verify_flow(solved, tol = tol)

  read <- .balance_flow_read(index, flow)
  audit <- .balance_flow_audit(base, index, flow, cost = cost,
                               tiers = index$tiers)

  list(status = solved$status,
       flow = flow,
       potential = as.numeric(solved$potential),
       integral = max(abs(flow - round(flow)), 0) <= 1e-6,
       certificate = certificate,
       certified = isTRUE(certificate$certified_optimal),
       audit = audit,
       audit_ok = .cardinality_audit_ok(audit),
       read = read,
       objective = sum(base$arcs$cost * flow),
       relaxed = sum(arc_cost * flow),
       problem = base)
}

# The value of every moment row on a matched set. Zero or below is satisfied.
.cardinality_violations <- function(coefs, read) {
  coefs <- .as_moment_coefficient_list(coefs)
  if (!length(coefs)) {
    return(numeric(0))
  }
  vapply(coefs, function(cf) .moment_violation(cf, read$left, read$right),
         numeric(1))
}

#' Lagrangian bound for one node
#'
#' Solves the node's network at a sequence of multipliers, each step one
#' `.flow_solve()` and one `verify_flow()`, and returns the largest certified
#' relaxed optimum it reached. Multipliers move along the projected subgradient
#' with step `t_0 / (1 + k)`, warm-started from whatever `lambda` is handed in,
#' which is the parent's best set during a search.
#'
#' @param problem The node's network, or the pair `.balance_flow_problem()`
#'   returns.
#' @param coefs Moment coefficients, one per one-sided row.
#' @param lambda Starting multipliers, one per row.
#' @param steps How many multiplier updates to take.
#' @param index The network's index, unless `problem` carries one.
#' @param edits The node's arc-bound decisions.
#' @param incumbent The best objective known, which sets the step scale.
#' @param step0 An explicit `t_0`, overriding that scale.
#' @param tol Numeric tolerance for certification and for the row values.
#' @param cost Optional distance matrix for the audit.
#'
#' @return A list with `bound`, the multipliers that attained it, the relaxed
#'   solve at those multipliers, any moment-feasible solutions the ascent
#'   passed through, and `certified`, whether the bound it reports came out of
#'   a certified solve.
#' @keywords internal
.cardinality_lagrangian <- function(problem, coefs = NULL, lambda = NULL,
                                    steps = 20L, index = NULL, edits = NULL,
                                    incumbent = Inf, step0 = NULL,
                                    tol = 1e-9, cost = NULL) {
  node <- .cardinality_as_node(problem, index)
  coefs <- .as_moment_coefficient_list(coefs)
  n_rows <- length(coefs)

  lambda <- if (is.null(lambda)) {
    numeric(n_rows)
  } else {
    pmax(0, as.numeric(lambda))
  }
  if (length(lambda) != n_rows) {
    stop("`lambda` must hold one multiplier per moment row; got ",
         length(lambda), " for ", n_rows, " rows.", call. = FALSE)
  }
  steps <- max(1L, as.integer(steps))
  if (!n_rows) {
    steps <- 1L
  }

  bound <- -Inf
  best_lambda <- lambda
  best_solve <- NULL
  solutions <- list()
  certified <- FALSE
  status <- "optimal"
  n_solves <- 0L
  t0 <- step0

  for (k in seq_len(steps)) {
    fl <- .cardinality_flow(node$problem, node$index, coefs = coefs,
                            lambda = lambda, edits = edits, cost = cost,
                            tol = tol)
    n_solves <- n_solves + 1L

    if (.cardinality_no_flow(fl$status)) {
      # The node's forced arcs admit no flow meeting the budget, so it holds no
      # matched set.
      return(list(bound = Inf, lambda = lambda, relaxed = fl,
                  solutions = list(), certified = TRUE, status = "infeasible",
                  n_solves = n_solves))
    }

    # Only a certified solve states a bound. An uncertified one still shows a
    # matched set to branch on, and still moves the multipliers, but nothing it
    # reports is allowed to prune.
    usable <- fl$certified && fl$audit_ok && fl$integral &&
      identical(fl$status, "optimal")
    if (usable) {
      if (fl$relaxed > bound) {
        bound <- fl$relaxed
        best_lambda <- lambda
        best_solve <- fl
      }
      certified <- TRUE
    } else {
      status <- fl$status
    }
    if (is.null(best_solve)) {
      best_solve <- fl
    }

    g <- .cardinality_violations(coefs, fl$read)
    if (!length(g) || all(g <= tol)) {
      # A relaxed optimum that satisfies every row is feasible for the original
      # problem, and when the multipliers price none of the satisfied rows it
      # also attains the bound, which settles the node.
      solutions[[length(solutions) + 1L]] <- fl
      if (!length(g) || sum(lambda * g) >= -tol) {
        break
      }
    }

    gnorm2 <- sum(g * g)
    if (gnorm2 <= 0) {
      break
    }
    if (is.null(t0)) {
      reach <- if (is.finite(incumbent) && incumbent > fl$relaxed) {
        incumbent - fl$relaxed
      } else {
        max(abs(fl$relaxed) * 1e-3, 1)
      }
      t0 <- reach / gnorm2
    }
    lambda <- pmax(0, lambda + (t0 / k) * g)
  }

  list(bound = bound, lambda = best_lambda, relaxed = best_solve,
       solutions = solutions, certified = certified, status = status,
       n_solves = n_solves)
}

# The cardinality no feasible matched set can exceed, given a lower bound on the
# objective. One unit of cardinality is worth P and the rest of the objective
# reaches at most P - 1, so the count this returns is exact rather than rounded.
.cardinality_best_possible <- function(index, bound) {
  budget <- index$total_budget
  penalty <- index$tiers$penalty
  if (is.na(bound) || bound == -Inf) {
    return(as.integer(budget))
  }
  if (bound == Inf) {
    return(0L)
  }
  r_max <- penalty - 1
  q <- max(0, ceiling((bound - r_max) / penalty - 1e-9))
  as.integer(max(0, budget - q))
}

# A matched set every moment row accepts, cut out of one that violates some of
# them. Removing a pair moves row r by exactly that pair's coefficient a^r_ij,
# so which pair to drop is arithmetic on the pairs rather than another solve:
# each step drops the one whose contribution to the violated rows, weighted by
# how far each is violated, is largest. Summed over the pairs still held those
# scores come to sum_r g_r^2 over the violated rows, which is positive, so a
# pair that reduces them exists at every step and the loop ends within one pass
# over the pairs.
#
# A pair is a candidate for removal only when its two units sit in the same cell
# of the deepest exactly balanced level. The levels are nested, so that keeps
# the matched counts equal at every level balance is enforced exactly on, and
# what the loop leaves behind is a matched set the network can carry. With no
# exact level every pair is a candidate.
#
# Returns NULL when the rows still ask for a removal no candidate supplies, and
# when nothing is left to keep.
.cardinality_feasible_subset <- function(index, read, coefs, tol = 1e-9) {
  coefs <- .as_moment_coefficient_list(coefs)
  left <- read$left
  right <- read$right
  if (!length(coefs) || !length(left)) {
    return(NULL)
  }

  hier <- index$hier
  candidate <- if (hier$exact >= 1L) {
    .balance_codes_at(hier, index$code_left, hier$exact)[left] ==
      .balance_codes_at(hier, index$code_right, hier$exact)[right]
  } else {
    rep(TRUE, length(left))
  }

  contrib <- do.call(cbind, lapply(coefs, function(cf) {
    cf$u[left] - cf$w[right] - cf$b
  }))
  value <- colSums(contrib)
  held <- rep(TRUE, length(left))

  while (any(value > tol)) {
    hot <- value > tol
    score <- as.numeric(contrib[, hot, drop = FALSE] %*% value[hot])
    score[!(held & candidate)] <- -Inf
    k <- which.max(score)
    if (!length(k) || !is.finite(score[[k]]) || score[[k]] <= 0) {
      return(NULL)
    }
    held[[k]] <- FALSE
    value <- value - contrib[k, ]
  }

  if (!any(held)) {
    return(NULL)
  }
  list(left = left[held], right = right[held])
}

# A matched set as the arc bounds that hold it: every pair arc it uses forced to
# one and every other barred. A solve under those bounds reads that set back and
# no other, so its objective, its certificate and its audit come off the same
# path every incumbent in the search comes off.
.cardinality_pin_pairs <- function(index, left, right) {
  pair <- index$ranges$pair
  used <- match(left + (right - 1L) * index$n_left, index$pair_key)
  bound <- numeric(length(pair))
  bound[used] <- 1
  .cardinality_add_edit(.cardinality_no_edits(), pair, bound, bound)
}

# The left unit, or the pair, whose contribution to a relaxed row is largest. A
# row counts as relaxed when the current matched set violates it or when the
# multipliers price it, which are the two ways a row keeps the node's bound
# below its optimum.
#
# The decision has to exist whatever the relaxed optimum looks like, or a node
# would be dropped with its subtree unexamined, so the rule falls back to any
# undecided left unit and then to any undecided pair. Every branch decides an
# arc that was free, and a node with every pair arc decided has its matched set
# determined, which is where the recursion ends.
.cardinality_branch_pick <- function(index, read, coefs, g, lambda,
                                     fixed_units, fixed_pairs, branch) {
  left <- read$left
  right <- read$right
  relaxed <- which(g > 0 | lambda > 0)
  score <- rep(0, length(left))
  if (length(relaxed) && length(left)) {
    score <- rep(-Inf, length(left))
    for (r in relaxed) {
      cf <- coefs[[r]]
      score <- pmax(score, abs(cf$u[left] - cf$w[right] - cf$b))
    }
  }

  if (identical(branch, "unit")) {
    free <- which(!(left %in% fixed_units))
    if (length(free)) {
      k <- free[[which.max(score[free])]]
      return(list(kind = "unit", unit = left[[k]],
                  arc = index$ranges$unit_left[[left[[k]]]]))
    }
    rest <- setdiff(seq_len(index$n_left), fixed_units)
    if (length(rest)) {
      return(list(kind = "unit", unit = rest[[1L]],
                  arc = index$ranges$unit_left[[rest[[1L]]]]))
    }
  }

  # `pair_key` locates a pair within its own arc class, and the network's arc
  # ids for that class are what an edit has to name.
  arcs <- index$ranges$pair[match(left + (right - 1L) * index$n_left,
                                  index$pair_key)]
  free <- which(!(arcs %in% fixed_pairs))
  if (length(free)) {
    k <- free[[which.max(score[free])]]
    return(list(kind = "pair", arc = arcs[[k]]))
  }
  rest <- setdiff(index$ranges$pair, fixed_pairs)
  if (length(rest)) {
    return(list(kind = "pair", arc = rest[[1L]]))
  }
  NULL
}

# The two children a branching decision produces: the arc barred, and the arc
# forced. Their union is the parent.
.cardinality_children <- function(node, pick) {
  out <- node
  out$edits <- .cardinality_add_edit(node$edits, pick$arc, 0, 0)
  keep <- node
  keep$edits <- .cardinality_add_edit(node$edits, pick$arc, 1, 1)
  if (identical(pick$kind, "unit")) {
    out$fixed_units <- c(node$fixed_units, pick$unit)
    keep$fixed_units <- out$fixed_units
  } else {
    out$fixed_pairs <- c(node$fixed_pairs, pick$arc)
    keep$fixed_pairs <- out$fixed_pairs
  }
  out$depth <- node$depth + 1L
  keep$depth <- node$depth + 1L
  list(out, keep)
}

#' Branch and bound over the moment constraints
#'
#' Searches the tree of inclusion decisions, bounding every node by its
#' Lagrangian relaxation and keeping an incumbent that satisfies every stated
#' constraint. Returns at any interruption with that incumbent and a global
#' bound that is valid for the whole problem, never with an unproven claim of
#' optimality.
#'
#' A single `.flow_solve()` is not interruptible, so `time_limit` is checked
#' between nodes and is only as responsive as one solve of the network.
#'
#' @param problem The network, or the pair `.balance_flow_problem()` returns.
#' @param index The network's index, unless `problem` carries one.
#' @param coefs Moment coefficients, one per one-sided row.
#' @param dual_steps Multiplier updates per node.
#' @param branch Whether to branch on left-unit inclusion or on pairs.
#' @param node_limit,time_limit Search budget, in nodes and in seconds.
#' @param should_stop Optional predicate of the search state; `TRUE` stops the
#'   search the way an interrupt would.
#' @param cost Optional distance matrix for the audit.
#' @param tol Numeric tolerance for certification, pruning and the row values.
#'
#' @return A list of class `cardinality_run`.
#' @keywords internal
.cardinality_branch_bound <- function(problem, index = NULL, coefs = NULL,
                                      dual_steps = 20L,
                                      branch = c("unit", "pair"),
                                      node_limit = 500L, time_limit = Inf,
                                      should_stop = NULL, cost = NULL,
                                      tol = 1e-9) {
  node0 <- .cardinality_as_node(problem, index)
  index <- node0$index
  coefs <- .as_moment_coefficient_list(coefs)
  branch <- match.arg(branch)
  node_limit <- .flow_count(node_limit, "node_limit", allow_inf = TRUE)
  if (!is.numeric(time_limit) || length(time_limit) != 1L ||
      is.na(time_limit) || time_limit < 0) {
    stop("`time_limit` must be a single non-negative number of seconds, or ",
         "Inf.", call. = FALSE)
  }
  if (!is.null(should_stop) && !is.function(should_stop)) {
    stop("`should_stop` must be a function of the search state.", call. = FALSE)
  }

  # The empty matched set satisfies every moment row, since each row's value is
  # a sum over pairs, so an incumbent that is feasible for every stated
  # constraint exists before the first node is opened. It is taken from the node
  # of the tree that excludes every left unit, so it arrives certified and
  # audited like any other, rather than as a hand-built flow.
  bare <- .cardinality_no_edits()
  for (a in index$ranges$unit_left) {
    bare <- .cardinality_add_edit(bare, a, 0, 0)
  }
  empty <- .cardinality_flow(node0$problem, index, edits = bare, cost = cost,
                             tol = tol)

  # A bound prunes nothing until something feasible is known, and the empty set
  # leaves every bound below it, so the root looks for a matched set worth
  # pruning against before the first node is opened. The relaxed optimum at zero
  # multipliers is the largest matched set there is; pairs come off it until
  # every moment row holds, and what is left enters the incumbent once the rows
  # are recomputed on those pairs and the solve behind them is audited. When the
  # rows admit nothing there, the empty set stands.
  incumbent <- empty
  root_solves <- 0L
  if (length(coefs)) {
    free <- .cardinality_flow(node0$problem, index, cost = cost, tol = tol)
    root_solves <- root_solves + 1L
    if (!.cardinality_no_flow(free$status)) {
      subset <- .cardinality_feasible_subset(index, free$read, coefs, tol)
      if (!is.null(subset)) {
        seed <- .cardinality_flow(node0$problem, index, cost = cost, tol = tol,
                                  edits = .cardinality_pin_pairs(
                                    index, subset$left, subset$right))
        root_solves <- root_solves + 1L
        rows_hold <- all(.cardinality_violations(coefs, seed$read) <= tol)
        if (rows_hold && seed$audit_ok && seed$integral &&
            identical(seed$status, "optimal") &&
            seed$objective < incumbent$objective) {
          incumbent <- seed
        }
      }
    }
  }

  env <- new.env(parent = emptyenv())
  env$incumbent <- incumbent
  env$best <- incumbent$objective
  env$n_nodes <- 0L
  env$n_solves <- root_solves
  env$bound_certified <- TRUE
  env$stopped_on <- "optimality"
  env$active <- NULL
  env$prune_slack <- 0
  env$frontier <- list(list(edits = .cardinality_no_edits(),
                            lambda = numeric(length(coefs)),
                            bound = -Inf, depth = 0L,
                            fixed_units = integer(0),
                            fixed_pairs = integer(0)))
  env$root_bound <- -Inf
  started <- proc.time()[["elapsed"]]

  frontier_bound <- function() {
    open <- vapply(env$frontier, function(nd) nd$bound, numeric(1))
    if (!is.null(env$active)) {
      open <- c(open, env$active$bound)
    }
    if (!length(open)) Inf else min(open)
  }
  # A node pruned on its bound holds nothing below the incumbent to within the
  # tolerance the comparison used, so that tolerance is taken off the bound
  # rather than assumed away. It is relative and far below one unit of
  # cardinality, which is worth P.
  global_bound <- function() {
    min(env$best, frontier_bound()) - env$prune_slack
  }

  tryCatch({
    repeat {
      if (!length(env$frontier)) {
        env$stopped_on <- "optimality"
        break
      }
      if (env$n_nodes >= node_limit) {
        env$stopped_on <- "node_limit"
        break
      }
      if (proc.time()[["elapsed"]] - started > time_limit) {
        env$stopped_on <- "time_limit"
        break
      }
      if (!is.null(should_stop) &&
          isTRUE(should_stop(list(n_nodes = env$n_nodes,
                                  elapsed = proc.time()[["elapsed"]] - started,
                                  incumbent = env$best,
                                  bound = global_bound())))) {
        env$stopped_on <- "interrupt"
        break
      }
      # The global form of the pruning test: when no open node can reach below
      # the incumbent, the incumbent is optimal and the nodes still listed hold
      # nothing worth opening.
      if (frontier_bound() >= env$best - tol * max(1, abs(env$best))) {
        env$prune_slack <- max(env$prune_slack, tol * max(1, abs(env$best)))
        env$stopped_on <- "bound"
        break
      }

      # Best-first on the bound, since that is what closes a tree, and deepest
      # first among the nodes that share one. Children inherit their parent's
      # bound, so ties are the common case, and taking the deeper of two equal
      # nodes walks down to a matched set instead of across a level.
      open_bound <- vapply(env$frontier, function(nd) nd$bound, numeric(1))
      open_depth <- vapply(env$frontier, function(nd) nd$depth, numeric(1))
      pos <- order(open_bound, -open_depth)[[1L]]
      env$active <- env$frontier[[pos]]
      env$frontier <- env$frontier[-pos]
      env$n_nodes <- env$n_nodes + 1L

      dual <- .cardinality_lagrangian(node0$problem, coefs = coefs,
                                      lambda = env$active$lambda,
                                      steps = dual_steps, index = index,
                                      edits = env$active$edits,
                                      incumbent = env$best, tol = tol,
                                      cost = cost)
      env$n_solves <- env$n_solves + dual$n_solves
      if (!dual$certified) {
        env$bound_certified <- FALSE
      }

      for (sol in dual$solutions) {
        if (!sol$audit_ok) next
        ties <- sol$objective <= env$best + tol * max(1, abs(env$best)) &&
          isTRUE(sol$certified) && !isTRUE(env$incumbent$certified)
        if (sol$objective < env$best || ties) {
          env$best <- min(env$best, sol$objective)
          env$incumbent <- sol
        }
      }

      # An uncertified bound proves nothing, so the node keeps the bound it
      # inherited rather than a number nothing checked.
      node_bound <- if (dual$certified && is.finite(dual$bound)) {
        max(env$active$bound, dual$bound)
      } else if (identical(dual$status, "infeasible")) {
        Inf
      } else {
        env$active$bound
      }
      if (env$n_nodes == 1L) {
        env$root_bound <- node_bound
      }

      prune_tol <- tol * max(1, abs(env$best))
      if (node_bound >= env$best - prune_tol) {
        env$prune_slack <- max(env$prune_slack, prune_tol)
        env$active <- NULL
        next
      }

      # A moment-feasible relaxed optimum that attains the node's bound has
      # already entered the incumbent, so the prune above is what closes a node
      # that is solved. Anything reaching here has a bound below the incumbent
      # and a subtree still worth opening.
      relaxed <- dual$relaxed
      g <- .cardinality_violations(coefs, relaxed$read)
      pick <- .cardinality_branch_pick(index, relaxed$read, coefs, g,
                                       dual$lambda, env$active$fixed_units,
                                       env$active$fixed_pairs, branch)
      if (is.null(pick)) {
        # Every pair arc is decided, so the node's matched set is determined and
        # is either the incumbent already or infeasible for a moment row.
        env$active <- NULL
        next
      }
      kids <- .cardinality_children(env$active, pick)
      env$frontier <- c(env$frontier, lapply(kids, function(kid) {
        kid$bound <- node_bound
        kid$lambda <- dual$lambda
        kid
      }))
      env$active <- NULL
    }
  }, interrupt = function(e) {
    env$stopped_on <- "interrupt"
  })

  bound <- global_bound()
  best_possible <- .cardinality_best_possible(index, bound)
  n_pairs <- env$incumbent$read$n_pairs
  settled <- env$stopped_on %in% c("optimality", "bound")
  certified <- settled && env$bound_certified &&
    env$incumbent$certified && env$incumbent$audit_ok &&
    identical(env$incumbent$status, "optimal") &&
    best_possible <= n_pairs

  structure(list(index = index,
                 solution = env$incumbent,
                 objective = env$best,
                 bound = bound,
                 root_bound = env$root_bound,
                 best_possible = max(best_possible, n_pairs),
                 bound_certified = env$bound_certified,
                 certified = certified,
                 stopped_on = .validate_cardinality_stop(env$stopped_on),
                 status = if (certified) "optimal" else "iteration_limit",
                 n_nodes = env$n_nodes,
                 n_solves = env$n_solves,
                 engine = if (length(coefs)) "branch_bound" else "flow",
                 coefs = coefs),
            class = "cardinality_run")
}

# The achieved value of one moment row on a matched set, and the room left under
# its bound. The statistic is the one the row was stated in: a standardized
# difference carries the pooled spread the row fixed once, a mean difference
# carries none.
.cardinality_constraint_row <- function(spec, coefs, read, tol = 1e-9) {
  k <- length(read$left)
  scale <- if (identical(spec$stat, "std_diff")) spec$denominator else 1
  signed <- if (k) {
    (sum(coefs$u[read$left]) - sum(coefs$w[read$right])) / k
  } else {
    NA_real_
  }
  stated <- k > 0L && is.finite(scale) && scale != 0 && !isTRUE(spec$trivial)
  achieved <- if (stated) signed / (scale * spec$direction) else NA_real_
  slack <- if (is.na(achieved)) NA_real_ else (spec$bound - signed) / scale
  violation <- if (!k) 0 else k * (signed - spec$bound)
  list(kind = spec$stat,
       target = .moment_var_label(spec$var, spec$transform),
       bound = spec$limit,
       achieved = achieved,
       slack = slack,
       satisfied = isTRUE(violation <= tol))
}

# Matched counts per category at one level of the hierarchy.
.cardinality_level_balance <- function(index, read, h) {
  hier <- index$hier
  n_h <- hier$n_cats[[h + 1L]]
  a <- tabulate(.balance_codes_at(hier, index$code_left, h)[read$left],
                nbins = n_h)
  b <- tabulate(.balance_codes_at(hier, index$code_right, h)[read$right],
                nbins = n_h)
  list(a = a, b = b, labels = .balance_label(hier$labels[[h]]))
}

.cardinality_level_label <- function(hier, h) {
  cols <- hier$columns[[h]]
  if (!length(cols)) "(all units)" else paste(cols, collapse = " : ")
}

#' Report of a cardinality match
#'
#' Turns a completed search into the object callers read: the matched sample's
#' size, the largest size the bound admits, the gap between them, and the state
#' of every constraint the match was asked to meet.
#'
#' @param run A `cardinality_run` from [.cardinality_branch_bound()].
#' @param specs The moment rows the run was given, from `.moment_specs()`.
#' @param tol Numeric tolerance for reading a constraint as satisfied.
#'
#' @return An object of class `cardinality_report`, a list with elements:
#' \itemize{
#'   \item `n_matched`, `n_left_matched` - matched pairs, and the left units
#'         they use.
#'   \item `best_possible` - the largest matched sample the bound admits.
#'   \item `gap`, `gap_fraction` - `best_possible - n_matched`, in matched
#'         units and as a share of `best_possible`.
#'   \item `certified` - `TRUE` only when the search settled, every solve its
#'         bound rests on was certified, the incumbent's own solve was
#'         certified and audited, and `gap` is zero.
#'   \item `objective`, `bound` - the incumbent's value of the network
#'         objective and the global lower bound on it. `best_possible` is read
#'         from `bound`.
#'   \item `stopped_on`, `n_nodes`, `engine`, `status`.
#'   \item `constraints` - one row per stated constraint, with what it asked
#'         for and what the matched sample achieved.
#'   \item `balance` - matched counts and imbalance per category at every
#'         level.
#'   \item `tiers`, `precision_headroom`, `shift` - the weights that order the
#'         objective, how many times its range fits inside the range a double
#'         orders exactly, and the constant taken off every distance.
#'   \item `total_distance`, `pairs` - the matched set itself.
#' }
#' @keywords internal
.cardinality_report <- function(run, specs = NULL, tol = 1e-9) {
  if (!inherits(run, "cardinality_run")) {
    stop("`run` must come from `.cardinality_branch_bound()`.", call. = FALSE)
  }
  index <- run$index
  hier <- index$hier
  read <- run$solution$read
  specs <- if (is.null(specs)) list() else specs
  coefs <- run$coefs

  n_pairs <- read$n_pairs
  best_possible <- max(run$best_possible, n_pairs)
  gap <- best_possible - n_pairs
  gap_fraction <- if (best_possible > 0) gap / best_possible else 0

  rows <- lapply(seq_along(specs), function(r) {
    .cardinality_constraint_row(specs[[r]], coefs[[r]], read, tol)
  })
  constraints <- tibble::tibble(
    kind = vapply(rows, `[[`, character(1), "kind"),
    target = vapply(rows, `[[`, character(1), "target"),
    bound = vapply(rows, function(x) as.numeric(x$bound), numeric(1)),
    achieved = vapply(rows, function(x) as.numeric(x$achieved), numeric(1)),
    slack = vapply(rows, function(x) as.numeric(x$slack), numeric(1)),
    satisfied = vapply(rows, `[[`, logical(1), "satisfied"))

  exact_levels <- seq_len(hier$exact)
  if (length(exact_levels)) {
    imb <- vapply(exact_levels, function(h) {
      lv <- .cardinality_level_balance(index, read, h)
      sum(pmax(lv$a - lv$b, 0))
    }, numeric(1))
    constraints <- rbind(constraints, tibble::tibble(
      kind = rep("exact_balance", length(exact_levels)),
      target = vapply(exact_levels, function(h)
        .cardinality_level_label(hier, h), character(1)),
      bound = rep(0, length(exact_levels)),
      achieved = imb,
      slack = -imb,
      satisfied = imb == 0))
  }

  balance <- do.call(rbind, lapply(seq_len(hier$n_levels), function(h) {
    lv <- .cardinality_level_balance(index, read, h)
    tibble::tibble(level = rep(h, length(lv$a)),
                   category = lv$labels,
                   n_left = as.integer(lv$a),
                   n_right = as.integer(lv$b),
                   imbalance = as.integer(lv$a - lv$b))
  }))
  if (is.null(balance)) {
    balance <- tibble::tibble(level = integer(0), category = character(0),
                              n_left = integer(0), n_right = integer(0),
                              imbalance = integer(0))
  }

  tiers <- index$tiers
  reach <- tiers$base_magnitude + sum(tiers$counts * tiers$weights)
  headroom <- if (reach > 0) PAD_PRECISION_LIMIT / reach else Inf

  pairs <- tibble::tibble(
    left = as.integer(read$left),
    right = as.integer(read$right),
    distance = index$pair_cost[match(read$left + (read$right - 1L) *
                                       index$n_left, index$pair_key)] +
      index$cost_shift)

  structure(list(n_matched = as.integer(n_pairs),
                 n_left_matched = length(unique(read$left)),
                 best_possible = as.integer(best_possible),
                 gap = as.integer(gap),
                 gap_fraction = gap_fraction,
                 certified = isTRUE(run$certified),
                 objective = run$objective,
                 bound = run$bound,
                 stopped_on = run$stopped_on,
                 n_nodes = as.integer(run$n_nodes),
                 engine = run$engine,
                 constraints = constraints,
                 balance = balance,
                 tiers = tiers,
                 precision_headroom = headroom,
                 shift = index$cost_shift,
                 status = run$status,
                 total_distance = read$total_distance + n_pairs *
                   index$cost_shift,
                 pairs = pairs),
            class = "cardinality_report")
}

#' @param x A `cardinality_report`.
#' @param ... Ignored.
#' @return Invisibly returns `x`.
#' @export
#' @method print cardinality_report
#' @rdname dot-cardinality_report
print.cardinality_report <- function(x, ...) {
  line <- function(label, value) cat(sprintf("%-21s%s\n", label, value))
  line("Matched units:", format(x$n_matched))
  if (isTRUE(x$certified)) {
    line("Best possible:", format(x$best_possible))
    line("Optimality gap:", format(x$gap))
    cat("Certified optimal\n")
  } else {
    line("Global upper bound:", format(x$best_possible))
    line("Cardinality gap:",
         sprintf("%s %s (%.3f%%)", format(x$gap),
                 if (isTRUE(x$gap == 1)) "unit" else "units",
                 100 * x$gap_fraction))
  }
  invisible(x)
}

#' Balance-constrained cardinality match
#'
#' Builds the balance network for a distance matrix and a nested partition,
#' searches it under whatever moment constraints are stated, and reports the
#' matched sample with the bound that goes with it.
#'
#' @param left,right Data frames of units, one row each.
#' @param cost Numeric distance matrix, `nrow(left)` by `nrow(right)`. A cell
#'   that is `Inf` or `NA` is a pair the match may not use.
#' @param refined The nested groupings balance is required on, coarsest first.
#' @param exact How many of the coarsest levels are enforced exactly.
#' @param moments,max_std_diff,vars Moment constraints, as `.moment_specs()`
#'   takes them.
#' @param dual_steps Multiplier updates per node.
#' @param branch Whether to branch on left-unit inclusion or on pairs.
#' @param node_limit,time_limit Search budget.
#' @param should_stop Optional predicate of the search state; `TRUE` stops the
#'   search the way an interrupt would.
#' @param tol Numeric tolerance for certification and for the constraint values.
#'
#' @return A `cardinality_report`.
#' @keywords internal
.cardinality_solve <- function(left, right, cost, refined = NULL, exact = 1L,
                               moments = NULL, max_std_diff = NULL, vars = NULL,
                               dual_steps = 20L, branch = c("unit", "pair"),
                               node_limit = 500L, time_limit = Inf,
                               should_stop = NULL, tol = 1e-9) {
  hier <- .refined_hierarchy(left, right, refined, exact = exact)
  built <- .balance_flow_problem(cost, hier)
  specs <- .moment_specs(moments = moments, max_std_diff = max_std_diff,
                         vars = vars, left = left, right = right)
  coefs <- lapply(specs, .moment_coefficients, left = left, right = right)

  run <- .cardinality_branch_bound(built$problem, built$index, coefs = coefs,
                                   dual_steps = dual_steps,
                                   branch = branch, node_limit = node_limit,
                                   time_limit = time_limit,
                                   should_stop = should_stop, cost = cost,
                                   tol = tol)
  .cardinality_report(run, specs = specs, tol = tol)
}
