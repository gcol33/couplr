utils::globalVariables(c(".data", "solution_id", "total_cost",
                        "lap_certify_dense", "lap_certify_lazy",
                        "lap_scan_reduced_costs",
                        "lap_hall_witness_dense", "lap_hall_witness_lazy",
                        "lap_flow_solve", "lap_flow_certify",
                        "lap_flow_compile_full_match",
                        "lap_flow_compile_couples",
                        "lap_implicit_dense", "lap_implicit_lazy",
                        "lap_design_implicit", "lap_replace_lazy"))

#' Validate and prepare cost data
#'
#' Internal helper that ensures a numeric, non-empty cost matrix.
#'
#' @param x Cost matrix or data frame
#' @param forbidden Value representing forbidden assignments (use NA or Inf)
#'
#' @return Numeric cost matrix
#' @keywords internal
validate_cost_data <- function(x, forbidden = NA) {
  if (is.data.frame(x)) {
    stop("Data frame input requires source, target, and cost columns. Use assign(df, source, target, cost).")
  }

  cost_matrix <- as.matrix(x)

  # FIX: Check for empty matrix BEFORE checking type
  # This ensures we get "empty" error instead of "must be numeric" error
  # when user passes matrix(nrow = 0, ncol = 0) which defaults to logical type
  if (nrow(cost_matrix) == 0 || ncol(cost_matrix) == 0) {
    stop("Cost matrix must have at least one row and one column.")
  }

  if (!is.numeric(cost_matrix)) {
    stop("Cost matrix must be numeric.")
  }
  
  if (any(is.nan(cost_matrix))) {
    stop("NaN values are not allowed. Use NA or Inf for forbidden assignments.")
  }

  cost_matrix
}

#' @rdname assignment
#' @param x Object to test
#' @export
is_lap_solve_result <- function(x) {
  inherits(x, "lap_solve_result")
}

#' @rdname lap_solve_batch
#' @param x Object to test
#' @export
is_lap_solve_batch_result <- function(x) {
  inherits(x, "lap_solve_batch_result")
}

#' @rdname lap_solve_kbest
#' @param x Object to test
#' @export
is_lap_solve_kbest_result <- function(x) {
  inherits(x, "lap_solve_kbest_result")
}

#' @rdname assignment
#' @param x An assignment result object
#' @export
get_total_cost <- function(x) UseMethod("get_total_cost")

#' @export
get_total_cost.lap_solve_result <- function(x) {
  tc <- .assignment_metadata(x)$total_cost
  if (is.null(tc)) stop("total_cost attribute not found.")
  tc
}

#' @export
get_total_cost.default <- function(x) {
  stop("Object is not a valid assignment result.")
}

.assignment_unique_column <- function(x, name) {
  value <- x[[name]]
  if (is.null(value)) stop("Object is not a valid assignment result.")
  unique(value)
}

#' @export
get_total_cost.lap_solve_batch_result <- function(x) .assignment_unique_column(x, "total_cost")

#' @export
get_total_cost.lap_solve_kbest_result <- function(x) .assignment_unique_column(x, "total_cost")

#' @rdname assignment
#' @param x An assignment result object
#' @export
get_method_used <- function(x) UseMethod("get_method_used")

#' @export
get_method_used.lap_solve_result <- function(x) {
  mu <- .assignment_metadata(x)$method_used
  if (is.null(mu)) stop("method_used attribute not found.")
  mu
}

#' @export
get_method_used.default <- function(x) {
  stop("Object is not a valid assignment result.")
}

#' @export
get_method_used.lap_solve_batch_result <- function(x) .assignment_unique_column(x, "method_used")

#' Convert an Assignment Result to a Binary Matrix
#'
#' Turns a list or tidy assignment result into a 0/1 assignment matrix.
#' Original dimensions and identifiers are retained where recorded.
#'
#' @param x An assignment result object of class `lap_solve_result`
#' @param n_sources Number of source nodes, optional
#' @param n_targets Number of target nodes, optional
#'
#' @return Integer matrix with 0 and 1 entries
#' @export
as_assignment_matrix <- function(x, n_sources = NULL, n_targets = NULL) {
  if (!is_lap_solve_result(x)) {
    stop("x must be a lap_solve_result object.")
  }

  pairs <- .assignment_pairs(x)
  dimensions <- attr(x, "dimensions", exact = TRUE)
  extent <- function(values) if (length(values)) max(values) else 0L
  ns <- n_sources %||% dimensions[[1L]] %||% extent(pairs$source)
  nt <- n_targets %||% dimensions[[2L]] %||% extent(pairs$target)
  valid_dimension <- function(n) is.numeric(n) && length(n) == 1L &&
    is.finite(n) && n >= 0 && n == floor(n)
  if (!valid_dimension(ns) || !valid_dimension(nt)) {
    stop("n_sources and n_targets must be non-negative whole numbers.")
  }
  if (ns < extent(pairs$source) || nt < extent(pairs$target)) {
    stop("n_sources and n_targets must cover every assigned index.")
  }
  mat <- matrix(0L, nrow = ns, ncol = nt)
  if (nrow(pairs)) mat[cbind(pairs$source, pairs$target)] <- 1L
  labels <- function(levels, n) {
    if (length(levels) == n &&
        !identical(as.character(levels), as.character(seq_len(n)))) as.character(levels)
  }
  dn <- list(labels(attr(x, "source_levels", exact = TRUE), ns),
             labels(attr(x, "target_levels", exact = TRUE), nt))
  if (any(lengths(dn))) dimnames(mat) <- dn
  mat
}

.ASSIGNMENT_METADATA <- c("total_cost", "method_used", "status", "certificate",
                          "u", "v", "unmatched", "search", "dispatch",
                          "cardinality", "n_matched")

.assignment_metadata <- function(x) {
  read <- if (is.data.frame(x)) {
    function(name) attr(x, name, exact = TRUE)
  } else {
    function(name) x[[name]]
  }
  stats::setNames(lapply(.ASSIGNMENT_METADATA, read), .ASSIGNMENT_METADATA)
}

.tidy_assignment <- function(pairs, result, source_levels, target_levels) {
  metadata <- .assignment_metadata(result)
  for (name in names(metadata)) attr(pairs, name) <- metadata[[name]]
  attr(pairs, "source_levels") <- source_levels
  attr(pairs, "target_levels") <- target_levels
  attr(pairs, "dimensions") <- c(length(source_levels), length(target_levels))
  class(pairs) <- c("lap_solve_result", class(pairs))
  pairs
}

.assignment_pairs <- function(x) {
  if (!is.data.frame(x)) {
    rows <- which(x$match > 0L)
    return(data.frame(source = rows, target = x$match[rows]))
  }
  if (!all(c("source", "target") %in% names(x))) {
    stop("Result must contain 'source' and 'target' columns.")
  }
  source <- x$source
  target <- x$target
  source_levels <- attr(x, "source_levels", exact = TRUE)
  target_levels <- attr(x, "target_levels", exact = TRUE)
  if (!is.null(source_levels)) source <- match(source, source_levels)
  if (!is.null(target_levels)) target <- match(target, target_levels)
  if (anyNA(source) || anyNA(target) || !is.numeric(source) || !is.numeric(target)) {
    stop("Assignment indices must be numeric or recorded in the result's identifier levels.")
  }
  valid <- function(index) all(is.finite(index) & index >= 1L & index == floor(index))
  if (!valid(source) || !valid(target)) stop("Assignment indices must be positive whole numbers.")
  data.frame(source = as.integer(source), target = as.integer(target))
}

#' Null-coalescing operator
#' @noRd
#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Apply the `forbidden` sentinel to a cost matrix
#'
#' Every front door that documents a `forbidden` argument masks with this one,
#' so `lap_solve()`, `lap_solve_batch()` and `lap_solve_kbest()` read the same
#' sentinel the same way. NA and Inf cells are forbidden to the solvers
#' already, so `forbidden = NA` is the identity.
#'
#' @return The cost matrix with sentinel cells replaced by `Inf`.
#' @keywords internal
mask_forbidden <- function(cost_matrix, forbidden = NA) {
  if (length(forbidden) != 1L || is.na(forbidden)) {
    return(cost_matrix)
  }
  cost_matrix[cost_matrix == forbidden] <- Inf
  cost_matrix
}

#' Build a cost matrix from source / target / cost columns
#'
#' The long-format door of `lap_solve()` and `lap_solve_kbest()`. Cells no row
#' names are `forbidden`, which is what makes an absent pair an absent edge
#' rather than a zero-cost one.
#'
#' @return List with the matrix and the source / target level vectors its row
#'   and column indices stand for.
#' @keywords internal
long_to_cost_matrix <- function(source_vals, target_vals, cost_vals,
                                forbidden = NA) {
  unique_sources <- sort(unique(source_vals))
  unique_targets <- sort(unique(target_vals))

  row_idx <- match(source_vals, unique_sources)
  col_idx <- match(target_vals, unique_targets)

  cost_matrix <- matrix(forbidden,
                        nrow = length(unique_sources),
                        ncol = length(unique_targets))
  cost_matrix[cbind(row_idx, col_idx)] <- cost_vals

  list(cost_matrix = cost_matrix,
       sources = unique_sources,
       targets = unique_targets)
}
