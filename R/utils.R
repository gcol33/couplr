utils::globalVariables(c(".data", "solution_id", "total_cost"))

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

  if (!is.numeric(cost_matrix)) {
    stop("Cost matrix must be numeric.")
  }
  if (any(is.nan(cost_matrix))) {
    stop("NaN values are not allowed. Use NA or Inf for forbidden assignments.")
  }
  if (nrow(cost_matrix) == 0 || ncol(cost_matrix) == 0) {
    stop("Cost matrix must have at least one row and one column.")
  }

  cost_matrix
}

#' Check if object is an assignment result
#'
#' @param x Object to test
#' @return Logical indicating if x is an assignment result
#' @export
is_lap_solve_result <- function(x) {
  inherits(x, "lap_solve_result")
}

#' Check if object is a batch assignment result
#'
#' @param x Object to test
#' @return Logical indicating if x is a batch assignment result
#' @export
is_lap_solve_batch_result <- function(x) {
  inherits(x, "lap_solve_batch_result")
}

#' Check if object is a k-best assignment result
#'
#' @param x Object to test
#' @return Logical indicating if x is a k-best assignment result
#' @export
is_lap_solve_kbest_result <- function(x) {
  inherits(x, "lap_solve_kbest_result")
}

#' Extract total cost from assignment result
#'
#' @param x An assignment result object
#' @return Numeric total cost
#' @export
get_total_cost <- function(x) {
  if (is_lap_solve_result(x)) {
    tc <- attr(x, "total_cost", exact = TRUE)
    if (is.null(tc)) stop("total_cost attribute not found.")
    return(tc)
  }

  if (is_lap_solve_batch_result(x) || is_lap_solve_kbest_result(x)) {
    if ("total_cost" %in% names(x)) {
      vals <- unique(x$total_cost)
      return(vals)
    }
  }

  stop("Object is not a valid assignment result.")
}

#' Extract method used from assignment result
#'
#' @param x An assignment result object
#' @return Character string indicating method used
#' @export
get_method_used <- function(x) {
  if (is_lap_solve_result(x)) {
    mu <- attr(x, "method_used", exact = TRUE)
    if (is.null(mu)) stop("method_used attribute not found.")
    return(mu)
  }

  if (is_lap_solve_batch_result(x)) {
    if ("method_used" %in% names(x)) {
      vals <- unique(x$method_used)
      return(vals)
    }
  }

  stop("Object is not a valid assignment result.")
}

#' Convert assignment result to a binary matrix
#'
#' Turns a tidy assignment result back into a 0/1 assignment matrix.
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

  if (!nrow(x)) {
    ns <- if (is.null(n_sources)) 0L else as.integer(n_sources)
    nt <- if (is.null(n_targets)) 0L else as.integer(n_targets)
    return(matrix(0L, nrow = ns, ncol = nt))
  }

  if (!all(c("source", "target") %in% names(x))) {
    stop("Result must contain 'source' and 'target' columns.")
  }

  ns <- n_sources %||% max(x$source)
  nt <- n_targets %||% max(x$target)

  ns <- as.integer(ns)
  nt <- as.integer(nt)

  if (ns < 0L || nt < 0L) stop("n_sources and n_targets must be non-negative.")

  mat <- matrix(0L, nrow = ns, ncol = nt)
  idx <- cbind(as.integer(x$source), as.integer(x$target))
  mat[idx] <- 1L
  mat
}

#' Null-coalescing operator
#' @noRd
#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x
