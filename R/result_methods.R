# ==============================================================================
# The couplr_result class family: shared methods and accessors
# ==============================================================================
# match_couples() and cardinality_match() return a "matching_result",
# full_match() a "full_matching_result", cem_match() a "cem_result" and
# subclass_match() a "subclass_result". All four inherit from "couplr_result",
# which carries the methods they share: print() for a short overview,
# summary() for the full report, glance() for one row of headline numbers,
# tidy() and as.data.frame() for the matched units, and the accessors
# certificate(), potentials() and unmatched_units(). A subclass adds a method only
# where its behaviour differs (balance_diagnostics(), match_data(), plot()).
# ==============================================================================

#' Matching Results
#'
#' @description
#' Every matching function returns an object inheriting from class
#' `"couplr_result"`:
#'
#' | Function | Class | Matched units |
#' |---|---|---|
#' | [match_couples()], [cardinality_match()] | `"matching_result"` | pairs |
#' | [full_match()] | `"full_matching_result"` | groups |
#' | [cem_match()] | `"cem_result"` | strata |
#' | [subclass_match()] | `"subclass_result"` | subclasses |
#'
#' `print()` gives a short overview and `summary()` the full report: the
#' distribution of pair distances, the certificate conditions, the column
#' generation rounds and the balance constraints of a cardinality matching,
#' where present. `glance()` returns the headline numbers as a one-row tibble
#' and `as.data.frame()` (or `tidy()`) the matched units, one row per pair or
#' per unit. [certificate()], [potentials()] and [unmatched_units()] read the
#' certificate, the dual potentials and the unmatched units. Further methods
#' take any of these results: [balance_diagnostics()], [join_matched()],
#' [match_data()], [as_matchit()] and `cobalt::bal.tab()`.
#'
#' @param x,object A matching result.
#' @param row.names,optional Ignored; present for consistency with the generic.
#' @param ... Ignored.
#'
#' @return `print()` returns `x` invisibly. `summary()` returns an object of
#'   class `"summary.couplr_result"`. `glance()` returns a one-row tibble with
#'   columns `design`, `n_left`, `n_right`, `n_matched_left`,
#'   `n_matched_right`, `n_pairs`, `n_groups`, `total_distance`, `status`,
#'   `certified` and `max_suboptimality`, plus `best_possible`, `gap`,
#'   `n_nodes`, `stopped_on`, `size_certified` (whether the largest size the
#'   constraints admit was proven, which `certified` also requires of the
#'   balance and distance stages) and `size_seconds` (how long proving it took,
#'   `NA` when one network solve answered the call) for a cardinality matching
#'   and `n_rounds`, `candidate_edges` and
#'   `possible_edges` for column generation. `as.data.frame()` and `tidy()`
#'   return the matched units.
#'
#' @examples
#' set.seed(1)
#' left <- data.frame(id = 1:5, x = rnorm(5))
#' right <- data.frame(id = 6:15, x = rnorm(10))
#' m <- match_couples(left, right, vars = "x", certify = TRUE)
#' m
#' glance(m)
#' as.data.frame(m)
#' certificate(m)
#' summary(m)
#'
#' @name couplr_result
NULL

#' Certificate, Potentials and Unmatched Units of a Result
#'
#' Accessors for the parts of a matching or assignment result that later
#' analyses read.
#'
#' * `certificate()` returns the checked optimality certificate, an
#'   `assignment_certificate` or `flow_certificate` (see
#'   [verify_assignment()]), or `NULL` when none was computed. A matching
#'   carries one when it was solved with `certify = TRUE` or in implicit mode.
#'   Cardinality matching returns a `cardinality_certificate` recording its
#'   completed search and checked bounds. Its summary exposes the size,
#'   balance, and distance stages and the constraint report.
#' * `potentials()` returns the dual potentials as a list with elements
#'   `left` and `right` (named by unit id) for a matching, or `u` and `v` for
#'   an assignment of a cost matrix; `NULL` when the solver returned none.
#' * `unmatched_units()` returns the ids of the unmatched units as a list with
#'   elements `left` and `right`, or the unmatched row indices of an
#'   assignment.
#'
#' @param x,object A result of [match_couples()], [full_match()],
#'   [cardinality_match()], [cem_match()], [subclass_match()],
#'   [match_path()], [assignment()] or [assignment_duals()]; for `glance()`,
#'   a certificate from [verify_assignment()] or [verify_flow()].
#' @param ... Ignored.
#'
#' @return See Details.
#'
#' @examples
#' set.seed(1)
#' cost <- matrix(runif(16), 4)
#' res <- assignment_duals(cost)
#' potentials(res)
#' certificate(assignment(cost, certify = TRUE))
#'
#' @name result_accessors
NULL

#' @rdname result_accessors
#' @export
certificate <- function(x, ...) UseMethod("certificate")

#' @rdname result_accessors
#' @export
potentials <- function(x, ...) UseMethod("potentials")

#' @rdname result_accessors
#' @export
unmatched_units <- function(x, ...) UseMethod("unmatched_units")

#' @export
certificate.default <- function(x, ...) {
  stop("certificate() has no method for an object of class ",
       paste(class(x), collapse = "/"), ".", call. = FALSE)
}

#' @export
potentials.default <- function(x, ...) {
  stop("potentials() has no method for an object of class ",
       paste(class(x), collapse = "/"), ".", call. = FALSE)
}

#' @export
unmatched_units.default <- function(x, ...) {
  stop("unmatched_units() has no method for an object of class ",
       paste(class(x), collapse = "/"), ".", call. = FALSE)
}

#' @export
certificate.couplr_result <- function(x, ...) x[["certificate"]]

#' @rdname result_accessors
#' @export
print.cardinality_certificate <- function(x, ...) {
  cat("Search-Based Cardinality Certificate\n")
  cat("Certified optimal:", x$certified_optimal, "\n")
  cat("Matched pairs:", x$n_matched, "\n")
  cat("Size bound:", x$best_possible, "; gap:", x$gap, "\n")
  invisible(x)
}

#' @rdname result_accessors
#' @export
summary.cardinality_certificate <- function(object, ...) {
  structure(unclass(object), class = "summary.cardinality_certificate")
}

#' @export
print.summary.cardinality_certificate <- function(x, ...) {
  print.cardinality_certificate(x)
  cat("\nSearch report:\n")
  print(summary(x$report))
  invisible(x)
}

#' @rdname result_accessors
#' @export
glance.cardinality_certificate <- function(x, ...) {
  tibble::tibble(certified = isTRUE(x$certified_optimal),
                 n_matched = x$n_matched, best_possible = x$best_possible,
                 gap = x$gap, arithmetic = x$arithmetic)
}

#' @export
potentials.couplr_result <- function(x, ...) x[["potentials"]]

#' @export
unmatched_units.couplr_result <- function(x, ...) {
  if (!is.null(x[["unmatched"]])) {
    return(x[["unmatched"]])
  }
  units <- .result_units(x)
  if (is.null(units) || is.null(units$side)) {
    return(NULL)
  }
  out <- units[units$weight == 0, , drop = FALSE]
  list(left = out$id[out$side == "left"], right = out$id[out$side == "right"])
}

#' @export
certificate.lap_solve_result <- function(x, ...) .assignment_metadata(x)$certificate

#' @export
potentials.lap_solve_result <- function(x, ...) {
  metadata <- .assignment_metadata(x)
  if (is.null(metadata$u) || is.null(metadata$v)) NULL else
    list(u = metadata$u, v = metadata$v)
}

#' @export
unmatched_units.lap_solve_result <- function(x, ...) .assignment_metadata(x)$unmatched

#' @export
certificate.couplr_path <- function(x, ...) x[["certificate"]]

# ------------------------------------------------------------------------------
# Overview
# ------------------------------------------------------------------------------

# Design label printed in the header and returned by glance().
.design_label <- function(x) {
  if (inherits(x, "full_matching_result")) return("full")
  if (inherits(x, "cem_result")) return("coarsened exact")
  if (inherits(x, "subclass_result")) return("subclassification")
  info <- x$info
  if (!is.null(x$cardinality)) return("cardinality")
  if (identical(info$method, "greedy")) return("greedy one-to-one")
  if (isTRUE(info$replace)) return("one-to-one with replacement")
  if (!is.null(info$ratio) && info$ratio > 1L) {
    return(paste0(info$ratio, "-to-one"))
  }
  "one-to-one"
}

# The matched units of a result, one row per pair or per unit.
.result_units <- function(x) {
  if (inherits(x, "full_matching_result")) return(x$groups)
  if (inherits(x, "cem_result") || inherits(x, "subclass_result")) {
    return(x$matched)
  }
  x$pairs
}

# Unit counts on each side. A pair table counts matched units by distinct id; a
# unit table by weight.
.unit_counts <- function(x) {
  info <- x$info
  units <- .result_units(x)
  if (inherits(x, "matching_result")) {
    n_ml <- length(unique(units$left_id))
    n_mr <- length(unique(units$right_id))
    n_ul <- length(x$unmatched$left)
    n_ur <- length(x$unmatched$right)
    n_left <- info$n_left %||% if (!is.null(x$unmatched)) n_ml + n_ul else NA_integer_
    n_right <- info$n_right %||% if (!is.null(x$unmatched)) n_mr + n_ur else NA_integer_
  } else {
    on_side <- function(side) units$side == side
    n_left <- info$n_left %||% sum(on_side("left"))
    n_right <- info$n_right %||% sum(on_side("right"))
    n_ml <- info$n_matched_left %||% sum(on_side("left") & units$weight > 0)
    n_mr <- info$n_matched_right %||% sum(on_side("right") & units$weight > 0)
  }
  list(n_left = as.integer(n_left), n_right = as.integer(n_right),
       n_matched_left = as.integer(n_ml), n_matched_right = as.integer(n_mr))
}

#' @export
generics::glance

#' @export
generics::tidy

#' @rdname couplr_result
#' @importFrom generics glance
#' @export
glance.couplr_result <- function(x, ...) {
  counts <- .unit_counts(x)
  cert <- x$certificate
  card <- x$cardinality
  certified <- if (!is.null(card)) {
    isTRUE(card$certified)
  } else if (!is.null(cert)) {
    isTRUE(cert$certified_optimal)
  } else {
    NA
  }
  out <- list(
    design = .design_label(x),
    n_left = counts$n_left,
    n_right = counts$n_right,
    n_matched_left = counts$n_matched_left,
    n_matched_right = counts$n_matched_right,
    n_pairs = if (inherits(x, "matching_result")) nrow(x$pairs) else NA_integer_,
    n_groups = as.integer(x$info$n_groups %||% x$info$n_matched_strata %||%
                            x$info$n_subclasses %||% NA_integer_),
    total_distance = x$info$total_distance %||% NA_real_,
    status = x$status %||% NA_character_,
    certified = certified,
    max_suboptimality = if (is.null(cert$max_suboptimality)) NA_real_ else
      as.numeric(cert$max_suboptimality)
  )
  if (!is.null(x$info$strategy)) out$strategy <- x$info$strategy
  if (!is.null(x$info$n_blocks)) out$n_blocks <- as.integer(x$info$n_blocks)
  if (!is.null(card)) {
    out$best_possible <- as.integer(card$best_possible)
    out$gap <- as.integer(card$gap)
    out$n_nodes <- as.integer(card$n_nodes)
    out$stopped_on <- as.character(card$stopped_on)
    stages <- card$stages
    size <- if (is.null(stages)) NULL else stages[stages$stage == "size", , drop = FALSE]
    found <- !is.null(size) && nrow(size) > 0L
    out$size_certified <- if (found) {
      isTRUE(size$closed[[1L]]) && isTRUE(card$gap == 0L)
    } else {
      isTRUE(card$certified)
    }
    out$size_seconds <- if (found) size$seconds[[1L]] else NA_real_
  }
  if (!is.null(x$search)) {
    out$n_rounds <- as.integer(x$search$n_rounds)
    out$candidate_edges <- as.numeric(x$search$candidate_edges)
    out$possible_edges <- as.numeric(x$search$possible_edges)
  }
  tibble::as_tibble(out)
}

#' @rdname couplr_result
#' @importFrom generics tidy
#' @export
tidy.couplr_result <- function(x, ...) tibble::as_tibble(.result_units(x))

#' @rdname couplr_result
#' @export
as.data.frame.couplr_result <- function(x, row.names = NULL, optional = FALSE,
                                        ...) {
  as.data.frame(.result_units(x))
}

.fmt_count <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)

# The overview lines shared by print() and summary().
.overview_lines <- function(g) {
  lines <- character()
  add <- function(label, value) {
    lines[[length(lines) + 1L]] <<- sprintf("  %-19s %s", paste0(label, ":"), value)
  }
  add("Left units", sprintf("%s matched of %s", .fmt_count(g$n_matched_left),
                            .fmt_count(g$n_left)))
  add("Right units", sprintf("%s matched of %s", .fmt_count(g$n_matched_right),
                             .fmt_count(g$n_right)))
  has <- function(col) col %in% names(g)
  if (has("strategy")) add("Greedy strategy", g$strategy)
  if (has("n_blocks")) add("Blocks", .fmt_count(g$n_blocks))
  if (!is.na(g$n_pairs)) add("Pairs", .fmt_count(g$n_pairs))
  if (!is.na(g$n_groups)) add("Groups", .fmt_count(g$n_groups))
  if (!is.na(g$total_distance)) add("Total distance", format(g$total_distance, digits = 6))
  if (has("best_possible")) {
    add("Size bound", sprintf("%s (gap %s)", .fmt_count(g$best_possible), g$gap))
  }
  if (!is.na(g$certified)) {
    add("Certified optimal", if (g$certified) "yes" else "no")
  }
  if (has("n_rounds")) {
    add("Pairs generated", sprintf("%s of %s in %d rounds",
                                   .fmt_count(g$candidate_edges),
                                   .fmt_count(g$possible_edges), g$n_rounds))
  }
  lines
}

.header_line <- function(g) {
  status <- if (is.na(g$status)) "" else paste0(", ", g$status)
  sprintf("couplr result: %s matching%s", g$design, status)
}

#' @rdname couplr_result
#' @export
print.couplr_result <- function(x, ...) {
  g <- glance(x)
  cat(.header_line(g), "\n", sep = "")
  cat(.overview_lines(g), sep = "\n")
  invisible(x)
}

#' @rdname couplr_result
#' @export
summary.couplr_result <- function(object, ...) {
  units <- .result_units(object)
  distance <- units[["distance"]]
  out <- list(
    glance = glance(object),
    distances = if (length(distance)) {
      stats::quantile(distance, c(0, 0.25, 0.5, 0.75, 0.9, 1),
                      na.rm = TRUE)
    },
    group_sizes = if (inherits(object, "full_matching_result") &&
                      nrow(units) > 0) {
      table(table(units$group_id))
    },
    strata = object$strata_summary %||% object$subclass_summary,
    certificate = object$certificate,
    cardinality = object$cardinality,
    rounds = object$search$rounds,
    unmatched = unmatched_units(object)
  )
  class(out) <- "summary.couplr_result"
  out
}

#' @export
print.summary.couplr_result <- function(x, ...) {
  g <- x$glance
  cat(.header_line(g), "\n", sep = "")
  cat(.overview_lines(g), sep = "\n")
  if (!is.null(x$distances)) {
    cat("\nPair distances (quantiles):\n")
    q <- x$distances
    names(q) <- c("min", "25%", "median", "75%", "90%", "max")
    print(signif(q, 4))
  }
  if (!is.null(x$group_sizes)) {
    cat("\nGroups by number of units:\n")
    print(x$group_sizes)
  }
  if (!is.null(x$strata)) {
    cat("\nStrata:\n")
    print(x$strata, n = 10)
  }
  if (!is.null(x$cardinality)) {
    cat("\n")
    print(summary(x$cardinality))
  } else if (!is.null(x$certificate)) {
    cat("\n")
    print(summary(x$certificate))
  }
  if (!is.null(x$rounds)) {
    cat("\nColumn generation rounds:\n")
    print(x$rounds[, intersect(c("round", "pairs_added", "candidate_edges",
                                 "min_reduced_cost", "elapsed_s"),
                               names(x$rounds))], n = 10)
  }
  if (!is.null(x$unmatched) && (length(x$unmatched$left) || length(x$unmatched$right))) {
    show <- function(ids) {
      if (length(ids) > 10) paste(c(utils::head(ids, 10), "..."), collapse = " ") else
        paste(ids, collapse = " ")
    }
    cat("\nUnmatched left: ", length(x$unmatched$left),
        if (length(x$unmatched$left)) paste0(" (", show(x$unmatched$left), ")"),
        "\nUnmatched right: ", length(x$unmatched$right), "\n", sep = "")
  }
  invisible(x)
}

#' @rdname result_accessors
#' @details `glance()` on a certificate returns one row with `certified`,
#'   `arithmetic`, `primal_objective`, `dual_objective`, `duality_gap` and,
#'   for an assignment certificate, `max_suboptimality`.
#' @export
glance.assignment_certificate <- function(x, ...) {
  tibble::tibble(
    certified = isTRUE(x$certified_optimal),
    arithmetic = x$arithmetic,
    primal_objective = as.numeric(x$primal_objective),
    dual_objective = as.numeric(x$dual_objective),
    duality_gap = as.numeric(x$duality_gap),
    max_suboptimality = as.numeric(x$max_suboptimality)
  )
}

#' @rdname result_accessors
#' @export
glance.flow_certificate <- function(x, ...) {
  tibble::tibble(
    certified = isTRUE(x$certified_optimal),
    arithmetic = x$arithmetic,
    primal_objective = as.numeric(x$primal_objective),
    dual_objective = as.numeric(x$dual_objective),
    duality_gap = as.numeric(x$duality_gap)
  )
}

#' @rdname compute_distances
#' @export
as.matrix.distance_object <- function(x, ...) {
  if (is_lazy_cost_spec(x$cost_matrix)) {
    stop("This distance object stores no matrix (memory_mode = \"lazy\").",
         call. = FALSE)
  }
  x$cost_matrix
}
