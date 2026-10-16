# ==============================================================================
# Balance-constrained cardinality matching: the network solves and the report
# ==============================================================================
# The largest matched sample meeting a balance requirement is the optimum of the
# flow problem built in matching_balance_flow.R whenever every part of that
# requirement is categorical. A linear moment constraint is not, and the search
# that adds them works over unit selections (matching_cardinality_select.R).
# Both read their matched sets off solves of the balance network, and this file
# is what such a solve goes through: the arc bounds a solve is given, the exact
# reading of its flow and potentials, the audit, and the report a finished
# search becomes.
#
# Write x for the arc flows of the balance network, so that
#
#     c(x) = P * (K - pairs) + sum_h eps_h * imbalance_h + sum_ij d_ij x_ij
#
# is the lexicographic objective the network encodes. Every solve is read
# exactly: the solver's flow is repaired to the exact optimum of the costs as
# stored, its potentials are recovered from the residual graph, and the dual
# objective at those potentials is evaluated in exact arithmetic
# (src/flow/flow_exact_read.h). That dual objective is a lower bound on the
# network optimum by weak duality whatever the solver returned, and equals the
# optimum when the potentials were recovered, which is what lets a solve be
# certified with no tolerance anywhere.
# ==============================================================================

# Generation. A search over a specification rather than a matrix solves every
# network over the pairs it has generated so far. The network always carries
# its full budget, since a category that cannot fill it with pairs pays slack,
# so a solve is always a flow and the only question about the pairs it omits is
# price: a pair arc left out sits at its lower bound of zero, so the solve is
# optimal for the network holding every pair once no omitted pair prices below
# zero against its potentials. The potentials are the ones the solve is read
# at, held exactly, and a pair prices in when its reduced cost is exactly
# negative, so once none does the omitted pairs add exactly nothing to the
# bound. Pairs that price in are appended to the network and it is solved
# again, warm; the arcs already there keep their indices, so every bound placed
# on one stands.

# The reasons a search stops, in the order they are checked. Anything
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

# More arcs given bounds. An arc is edited once, so a solve's edits never
# disagree with each other.
.cardinality_add_edit <- function(edits, arc, lower, upper) {
  list(arc = c(edits$arc, as.integer(arc)),
       lower = c(edits$lower, as.numeric(lower)),
       upper = c(edits$upper, as.numeric(upper)))
}

# The network with a solve's arc bounds in place. Costs are untouched.
.cardinality_edit_problem <- function(problem, edits) {
  if (is.null(edits) || !length(edits$arc)) {
    return(problem)
  }
  arcs <- problem$arcs
  arcs$lower[edits$arc] <- edits$lower
  arcs$upper[edits$arc] <- edits$upper
  .flow_problem(problem$n_nodes, problem$supply, arcs)
}

# A previous solve's flow, made usable as a starting point for this one. The
# network is the same one throughout a search, so a flow stays aligned to it;
# what moves is an arc bound, and the arcs a selection restricts are exactly
# the ones the previous flow can sit outside of. Clamping is what the starting
# point is for: the solver repairs the conservation that breaks, and a flow
# outside an arc bound is not a starting point it can repair from.
.cardinality_warm_flow <- function(flow, problem) {
  if (is.null(flow) || !length(flow)) {
    return(NULL)
  }
  arcs <- problem$arcs
  if (length(flow) > nrow(arcs)) {
    return(NULL)
  }
  # Pairs generated since the flow was solved sit on the end of the arc list,
  # and a pair nobody placed flow on starts at its lower bound.
  if (length(flow) < nrow(arcs)) {
    flow <- c(flow, arcs$lower[seq.int(length(flow) + 1L, nrow(arcs))])
  }
  pmin(pmax(flow, arcs$lower), arcs$upper)
}

# The index a search should read now. A generating search appends pairs as it
# goes, and the index handed to it at the start does not hold them.
.cardinality_current_index <- function(index) {
  if (is.null(index$gen)) index else index$gen$built$index
}

# A solve's node potentials read in distance terms. A pair arc costs d - shift
# and reduces by pi(left_i) - pi(right_j), which is d - u_i - v_j for the u and
# v returned here: the reduced cost of a pair is its distance less the two.
.cardinality_pair_duals <- function(index, potential) {
  layout <- index$layout
  pi_left <- potential[layout$node_left(seq_len(index$n_left))]
  pi_right <- potential[layout$node_right(seq_len(index$n_right))]
  list(u = index$cost_shift - pi_left,
       v = pi_right)
}

# The double on the side of an exact value, an expansion, the caller names.
.exact_value <- function(x, direction = c("down", "up")) {
  direction <- match.arg(direction)
  if (length(x) == 1L && is.infinite(x)) {
    return(x)
  }
  lap_exact_round(x, direction)
}

# The moment rows as the exact routines read them: one column of u and of w per
# row, and one b per row.
.moment_matrices <- function(coefs, n_left, n_right) {
  coefs <- .as_moment_coefficient_list(coefs)
  if (!length(coefs)) {
    return(list(u = matrix(0, n_left, 0L), w = matrix(0, n_right, 0L),
                b = numeric(0)))
  }
  list(u = matrix(as.numeric(unlist(lapply(coefs, `[[`, "u"))), n_left),
       w = matrix(as.numeric(unlist(lapply(coefs, `[[`, "w"))), n_right),
       b = vapply(coefs, function(cf) as.numeric(cf$b), numeric(1)))
}

# The sign of every moment row on a matched set, decided exactly: zero or below
# is satisfied.
.cardinality_rows_exact <- function(index, coefs, read) {
  coefs <- .as_moment_coefficient_list(coefs)
  if (!length(coefs)) {
    return(integer(0))
  }
  mats <- .moment_matrices(coefs, index$n_left, index$n_right)
  lap_exact_moment_rows(mats$u, mats$w, mats$b, as.integer(read$left),
                        as.integer(read$right))
}

# One solve of the network read exactly: the bound D(pi) it proves, whether
# its potentials were recovered exactly, and the pair potentials the omitted
# pairs are priced against. See src/flow/flow_exact_read.h.
.cardinality_exact_read <- function(problem, index, solved) {
  arcs <- problem$arcs
  layout <- index$layout
  lap_flow_exact_read(
    problem$n_nodes, problem$supply, arcs$tail, arcs$head, arcs$lower,
    arcs$upper, arcs$cost, as.numeric(solved$flow),
    as.numeric(solved$potential), as.integer(index$ranges$pair),
    as.integer(index$pair_left), as.integer(index$pair_right),
    as.integer(layout$node_left(seq_len(index$n_left))),
    as.integer(layout$node_right(seq_len(index$n_right))))
}

# Seconds left of a budget stated as an absolute elapsed time, which is what a
# sequence of solves shares. Inf is no budget.
.cardinality_remaining <- function(deadline) {
  if (!is.finite(deadline)) {
    return(Inf)
  }
  max(0, deadline - proc.time()[["elapsed"]])
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
#' Applies a solve's arc bounds, solves the network, reads the solve exactly,
#' audits the flow against the objective identity the design encodes, and
#' reads the matched set back.
#'
#' The bound returned is D(pi) at the solve's potentials, which weak duality
#' makes a lower bound on the network optimum whether or not the solve was
#' optimal, and which is the optimum when it was. A generating search prices
#' the pairs it omits against the same potentials, at zero and exactly, and
#' adds any that price below until none do, so the bound holds over every
#' admissible pair.
#'
#' @param problem The network, or the pair `.balance_flow_problem()` returns.
#' @param index The network's index, unless `problem` carries one.
#' @param edits The solve's arc-bound decisions.
#' @param cost Optional distance matrix for the audit.
#' @param warm Optional `list(flow, potential)` from an earlier solve of the
#'   same network, used as the solver's starting point.
#' @param time_limit Seconds this one solve may run.
#'
#' @return A list with the solve status, the flow and potentials, the pair
#'   potentials in distance terms (`pair_duals`, see
#'   `.cardinality_pair_duals()`) and exactly (`exact_duals`, present when they
#'   were recovered), `bound` and `bound_exact`, the bound the solve proves,
#'   rounded down and exact, `certified`, whether the flow is exactly optimal
#'   over every admissible pair, the audit, the matched set, and the objective
#'   as `objective_exact` and as `objective`, rounded up. A solve that ran out
#'   of time comes back with status `"interrupted"`, its flow and potentials,
#'   and nothing else: it proved neither an optimum nor the absence of one.
#' @keywords internal
.cardinality_flow <- function(problem, index = NULL, edits = NULL, cost = NULL,
                              warm = NULL, time_limit = Inf) {
  node <- .cardinality_as_node(problem, index)
  gen <- node$index$gen
  if (!is.null(gen)) {
    node <- gen$built
  }
  started <- proc.time()[["elapsed"]]
  step <- NULL

  repeat {
    index <- node$index
    base <- .cardinality_edit_problem(node$problem, edits)

    remaining <- if (is.finite(time_limit)) {
      max(0, time_limit - (proc.time()[["elapsed"]] - started))
    } else {
      Inf
    }
    solved <- .flow_solve(base,
                          warm_flow = .cardinality_warm_flow(warm$flow, base),
                          warm_potential = warm$potential,
                          time_limit = remaining)
    if (identical(solved$status, "interrupted")) {
      return(list(status = "interrupted",
                  flow = as.numeric(solved$flow),
                  potential = as.numeric(solved$potential),
                  integral = FALSE,
                  bound = -Inf,
                  bound_exact = -Inf,
                  certified = FALSE,
                  audit = NULL,
                  audit_ok = FALSE,
                  read = NULL,
                  objective = NA_real_,
                  objective_exact = Inf,
                  problem = base,
                  index = index))
    }
    if (.cardinality_no_flow(solved$status)) {
      step <- NULL
      break
    }
    step <- .cardinality_exact_read(base, index, solved)
    if (is.null(gen)) {
      break
    }
    priced <- lap_pricing_price_exact(gen$session, step$pair_u, step$pair_v,
                                      index$cost_shift, gen$keep_per_row)
    if (!length(priced$i)) {
      break
    }
    gen$built <- .balance_add_pairs(gen$built, priced$i, priced$j,
                                    lap_pricing_cost(gen$session, priced$i,
                                                     priced$j))
    node <- gen$built
    warm <- list(flow = step$flow, potential = step$potential)
  }
  # The read's flow is the solver's with any negative cycle the exact costs
  # showed cancelled, and its potentials are the ones the bound was read at,
  # rounded, so both replace the solver's from here on.
  flow <- if (is.null(step)) as.numeric(solved$flow) else step$flow
  potential <- if (is.null(step)) as.numeric(solved$potential) else step$potential
  read <- .balance_flow_read(index, flow)
  audit <- .balance_flow_audit(base, index, flow, cost = cost,
                               tiers = index$tiers)
  objective_exact <- lap_exact_dot(base$arcs$cost, flow)

  list(status = solved$status,
       flow = flow,
       potential = potential,
       pair_duals = .cardinality_pair_duals(index, potential),
       exact_duals = if (!is.null(step) && isTRUE(step$recovered))
         list(u = step$pair_u, v = step$pair_v),
       integral = all(flow == round(flow)),
       bound = if (is.null(step)) -Inf else step$bound,
       bound_exact = if (is.null(step)) -Inf else step$bound_exact,
       certified = !is.null(step) && isTRUE(step$recovered),
       audit = audit,
       audit_ok = .cardinality_audit_ok(audit),
       read = read,
       objective = .exact_value(objective_exact, "up"),
       objective_exact = objective_exact,
       problem = base,
       index = index)
}

# The achieved value of one moment row on a matched set, and the room left under
# its bound. The statistic is the one the row was stated in: a standardized
# difference carries the pooled spread the row fixed once, a mean difference
# carries none. `row_sign` is the exact sign of the row's value on the set,
# which is what the search held it to.
.cardinality_constraint_row <- function(spec, coefs, read, row_sign) {
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
  list(kind = spec$stat,
       target = .moment_var_label(spec$var, spec$transform),
       bound = spec$limit,
       achieved = achieved,
       slack = slack,
       satisfied = row_sign <= 0L)
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
#' @param run A `cardinality_run` from `.cardinality_select()`.
#' @param specs The moment rows the run was given, from `.moment_specs()`.
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
#'   \item `stages` - one row per stage of a search over unit selections, in
#'         the order the objective ranks them (`"size"`, an `"imbalance"` row
#'         per refined level, `"distance"`): whether it closed, what stopped
#'         it, its nodes and seconds, and its value and bound in its own units
#'         (matched units, units of imbalance, total distance). No rows when
#'         one network solve answered the call.
#'   \item `constraints` - one row per stated constraint, with what it asked
#'         for and what the matched sample achieved.
#'   \item `balance` - matched counts and imbalance per category at every
#'         level.
#'   \item `tiers`, `precision_headroom`, `shift` - the weights that order the
#'         objective, how many times its range fits inside the range a double
#'         orders exactly, and the constant taken off every distance.
#'   \item `total_distance`, `pairs` - the matched set itself.
#'   \item `potentials` - the pair potentials of the solve the matched set
#'         came from, as `.cardinality_pair_duals()` reads them, or `NULL`
#'         when that solve was not certified.
#' }
#' @keywords internal
.cardinality_report <- function(run, specs = NULL) {
  if (!inherits(run, "cardinality_run")) {
    stop("`run` must come from `.cardinality_select()`.", call. = FALSE)
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

  row_sign <- .cardinality_rows_exact(index, coefs, read)
  rows <- lapply(seq_along(specs), function(r) {
    .cardinality_constraint_row(specs[[r]], coefs[[r]], read, row_sign[[r]])
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
                 stages = run$stages,
                 engine = run$engine,
                 constraints = constraints,
                 balance = balance,
                 tiers = tiers,
                 precision_headroom = headroom,
                 shift = index$cost_shift,
                 status = run$status,
                 total_distance = read$total_distance + n_pairs *
                   index$cost_shift,
                 pairs = pairs,
                 potentials = if (isTRUE(run$solution$certified))
                   run$solution$pair_duals else NULL),
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

#' @rdname dot-cardinality_report
#' @param object A cardinality report, including a pruning-heuristic report.
#' @return `summary()` returns a `summary.cardinality_report` containing the
#'   original report; printing it displays the size bound, constraints,
#'   category counts, and optimization stages where available.
#' @export
summary.cardinality_report <- function(object, ...) {
  structure(list(report = object), class = "summary.cardinality_report")
}

#' @export
print.summary.cardinality_report <- function(x, ...) {
  print(x$report)
  if (!is.null(x$report$total_distance)) {
    cat("Total distance:", x$report$total_distance, "\n")
  }
  if (!is.null(x$report$max_std_diff)) {
    cat("Standardized-difference target:", x$report$max_std_diff, "\n")
  }
  for (field in c("constraints", "balance", "stages")) {
    report <- x$report[[field]]
    if (is.data.frame(report) && nrow(report)) {
      cat("\n", switch(field, constraints = "Balance constraints",
                      balance = "Selected category counts",
                      stages = "Optimization stages"), ":\n", sep = "")
      print(as.data.frame(report), row.names = FALSE)
    }
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
#' @param node_limit,time_limit Search budget.
#' @param should_stop Optional predicate of the search state; `TRUE` stops the
#'   search the way an interrupt would.
#'
#' @return A `cardinality_report`.
#' @keywords internal
.cardinality_solve <- function(left, right, cost, refined = NULL, exact = 1L,
                               moments = NULL, max_std_diff = NULL, vars = NULL,
                               node_limit = 500L, time_limit = Inf,
                               should_stop = NULL) {
  hier <- .refined_hierarchy(left, right, refined, exact = exact)
  gen <- NULL
  if (is_lazy_cost_spec(cost)) {
    gen <- .cardinality_generator(cost)
    built <- .balance_flow_problem(gen$pool, hier)
    built$index$gen <- gen
    gen$built <- built
    cost <- NULL
  } else {
    built <- .balance_flow_problem(cost, hier)
  }
  specs <- .moment_specs(moments = moments, max_std_diff = max_std_diff,
                         vars = vars, left = left, right = right)
  coefs <- lapply(specs, .moment_coefficients, left = left, right = right)

  run <- .cardinality_select(built, coefs, cost, node_limit = node_limit,
                             time_limit = time_limit, should_stop = should_stop)
  report <- .cardinality_report(run, specs = specs)
  if (!is.null(gen) && is.function(gen$spec$distance)) {
    lazy_pair_distances(gen$spec, report$pairs$left, report$pairs$right)
  }
  if (!is.null(gen)) {
    report$search <- list(
      seed_width      = as.integer(gen$seed_width),
      candidate_edges = as.numeric(length(gen$built$index$pair_left)),
      possible_edges  = as.numeric(gen$pool$n_left) * gen$pool$n_right,
      edges_evaluated = as.numeric(lap_pricing_evaluated(gen$session))
    )
  }
  report
}

# The pricing session a search over a specification generates pairs from, and
# the pool its network starts with: each left unit's cheapest admissible right
# units, at the seed width the implicit loop uses, together with the smallest
# and largest admissible distance over every pair, read in one pass, which the
# tier weights have to be built against.
.cardinality_generator <- function(spec) {
  session <- lap_pricing_session(spec$left_mat, spec$right_mat, spec$distance,
                                 lazy_cost_spec_inv_cov(spec), spec$max_distance,
                                 lazy_cost_spec_calipers(spec))
  range <- lap_pricing_range(session)
  seed_width <- lap_implicit_seed_width(spec$n_right)
  seed <- lap_pricing_seed(session, seed_width)
  distance <- lap_pricing_cost(session, seed$i, seed$j)
  some <- range$n_admissible > 0
  gen <- new.env(parent = emptyenv())
  gen$spec <- spec
  gen$session <- session
  gen$seed_width <- seed_width
  gen$keep_per_row <- .implicit_defaults()$keep_per_row
  gen$pool <- structure(list(n_left = spec$n_left, n_right = spec$n_right,
                             left = as.integer(seed$i), right = as.integer(seed$j),
                             distance = as.numeric(distance),
                             min = if (some) range$min else NA_real_,
                             max = if (some) range$max else NA_real_),
                        class = "balance_pair_pool")
  gen
}
