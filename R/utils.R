#' Validate and prepare cost data
#'
#' Internal function to validate and prepare cost data for assignment algorithms
#'
#' @param x Cost matrix or data frame
#' @param forbidden Value representing forbidden assignments
#'
#' @return Validated cost matrix
#' @keywords internal
validate_cost_data <- function(x, forbidden = NA) {
  if (is.data.frame(x)) {
    stop("Data frame input requires source, target, and cost columns. Use assign(df, source, target, cost)")
  }
  
  cost_matrix <- as.matrix(x)
  
  if (!is.numeric(cost_matrix)) {
    stop("Cost matrix must be numeric")
  }
  
  if (any(is.nan(cost_matrix))) {
    stop("NaN values not allowed in cost matrix. Use NA or Inf for forbidden assignments.")
  }
  
  if (nrow(cost_matrix) == 0 || ncol(cost_matrix) == 0) {
    stop("Cost matrix must have at least one row and one column")
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
    return(attr(x, "total_cost"))
  } else if (is_lap_solve_batch_result(x) || is_lap_solve_kbest_result(x)) {
    if ("total_cost" %in% names(x)) {
      return(unique(x$total_cost))
    }
  }
  
  stop("Object is not a valid assignment result")
}

#' Extract method used from assignment result
#'
#' @param x An assignment result object
#' @return Character string indicating method used
#' @export
get_method_used <- function(x) {
  if (is_lap_solve_result(x)) {
    return(attr(x, "method_used"))
  } else if (is_lap_solve_batch_result(x)) {
    if ("method_used" %in% names(x)) {
      return(unique(x$method_used))
    }
  }
  
  stop("Object is not a valid assignment result")
}

#' Convert assignment result to matrix format
#'
#' Convert a tidy assignment result back to a binary assignment matrix
#'
#' @param x An assignment result object
#' @param n_sources Number of source nodes (optional, inferred if not provided)
#' @param n_targets Number of target nodes (optional, inferred if not provided)
#'
#' @return Binary assignment matrix (0/1)
#' @export
as_assignment_matrix <- function(x, n_sources = NULL, n_targets = NULL) {
  if (!is_lap_solve_result(x)) {
    stop("x must be a lap_solve_result object")
  }
  
  if (nrow(x) == 0) {
    n_sources <- n_sources %||% 0
    n_targets <- n_targets %||% 0
    return(matrix(0L, nrow = n_sources, ncol = n_targets))
  }
  
  n_sources <- n_sources %||% max(x$source)
  n_targets <- n_targets %||% max(x$target)
  
  mat <- matrix(0L, nrow = n_sources, ncol = n_targets)
  mat[cbind(x$source, x$target)] <- 1L
  
  mat
}

#' Null-coalescing operator
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
