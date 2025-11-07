#' Solve linear assignment problems
#'
#' Provides a tidy interface for solving the linear assignment problem using
#' Hungarian or Jonker-Volgenant algorithms. Supports rectangular matrices,
#' NA/Inf masking, and data frame inputs.
#'
#' @param x Cost matrix, data frame, or tibble. If a data frame/tibble,
#'   must include columns specified by `source`, `target`, and `cost`.
#' @param source Column name for source/row indices (if `x` is a data frame)
#' @param target Column name for target/column indices (if `x` is a data frame)
#' @param cost Column name for costs (if `x` is a data frame)
#' @param maximize Logical; if TRUE, maximizes total cost instead of minimizing (default: FALSE)
#' @param method Algorithm to use. One of:
#'   - "auto" (default): automatically selects best algorithm
#'   - "jv": Jonker-Volgenant algorithm (general purpose, fast)
#'   - "hungarian": Classic Hungarian algorithm
#'   - "auction": Auction algorithm (good for large dense problems)
#'   - "sap": Sparse assignment (good for sparse/rectangular problems)
#'   - "hk01": Hopcroft-Karp for binary/uniform costs
#' @param forbidden Value to mark forbidden assignments (default: NA). Can also use Inf.
#'
#' @return A tibble with columns:
#'   - `source`: row/source indices
#'   - `target`: column/target indices  
#'   - `cost`: cost of each assignment
#'   - `total_cost`: total cost (attribute)
#'
#' @examples
#' # Matrix input
#' cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), nrow = 3)
#' lap_solve(cost)
#'
#' # Data frame input
#' library(dplyr)
#' df <- tibble(
#'   source = rep(1:3, each = 3),
#'   target = rep(1:3, times = 3),
#'   cost = c(4, 2, 5, 3, 3, 6, 7, 5, 4)
#' )
#' lap_solve(df, source, target, cost)
#'
#' # With NA masking (forbidden assignments)
#' cost[1, 3] <- NA
#' lap_solve(cost)
#'
#' # Grouped data frames
#' df <- tibble(
#'   sim = rep(1:2, each = 9),
#'   source = rep(1:3, times = 6),
#'   target = rep(1:3, each = 3, times = 2),
#'   cost = runif(18, 1, 10)
#' )
#' df |> group_by(sim) |> lap_solve(source, target, cost)
#'
#' @export
lap_solve <- function(x, source = NULL, target = NULL, cost = NULL,
                   maximize = FALSE, method = "auto", forbidden = NA) {
  
  # Check if this is a grouped data frame
  is_grouped <- inherits(x, "grouped_df")
  
  if (is_grouped) {
    # Handle grouped data frames
    return(lap_solve_grouped(x, {{ source }}, {{ target }}, {{ cost }},
                         maximize = maximize, method = method, forbidden = forbidden))
  }
  
  # Handle data frame input
  if (is.data.frame(x)) {
    source_col <- rlang::enquo(source)
    target_col <- rlang::enquo(target)
    cost_col <- rlang::enquo(cost)
    
    if (rlang::quo_is_null(source_col) || rlang::quo_is_null(target_col) || 
        rlang::quo_is_null(cost_col)) {
      stop("For data frame input, must specify `source`, `target`, and `cost` columns")
    }
    
    return(lap_solve_df(x, source_col, target_col, cost_col, 
                    maximize = maximize, method = method, forbidden = forbidden))
  }
  
  # Handle matrix input
  cost_matrix <- as.matrix(x)
  
  # Call the underlying assignment function
  result <- assignment(cost_matrix, maximize = maximize, method = method)
  
  # Convert to tidy tibble format
  matched_indices <- which(result$match > 0)
  
  if (length(matched_indices) == 0) {
    out <- tibble::tibble(
      source = integer(0),
      target = integer(0),
      cost = numeric(0)
    )
  } else {
    out <- tibble::tibble(
      source = matched_indices,
      target = result$match[matched_indices],
      cost = cost_matrix[cbind(matched_indices, result$match[matched_indices])]
    )
  }
  
  # Add total_cost as attribute
  attr(out, "total_cost") <- result$total_cost
  attr(out, "method_used") <- result$method_used
  class(out) <- c("lap_solve_result", class(out))
  
  out
}

#' @keywords internal
lap_solve_df <- function(df, source_col, target_col, cost_col, 
                     maximize = FALSE, method = "auto", forbidden = NA) {
  
  # Extract columns with error handling
  source_vals <- tryCatch(
    rlang::eval_tidy(source_col, df),
    error = function(e) stop("For data frame input, must specify `source`, `target`, and `cost` columns", call. = FALSE)
  )
  target_vals <- tryCatch(
    rlang::eval_tidy(target_col, df),
    error = function(e) stop("For data frame input, must specify `source`, `target`, and `cost` columns", call. = FALSE)
  )
  cost_vals <- tryCatch(
    rlang::eval_tidy(cost_col, df),
    error = function(e) stop("For data frame input, must specify `source`, `target`, and `cost` columns", call. = FALSE)
  )
  
  # Get unique indices
  unique_sources <- sort(unique(source_vals))
  unique_targets <- sort(unique(target_vals))
  
  # Create mapping to 1-based indices
  source_map <- stats::setNames(seq_along(unique_sources), unique_sources)
  target_map <- stats::setNames(seq_along(unique_targets), unique_targets)
  
  # Build cost matrix
  n_sources <- length(unique_sources)
  n_targets <- length(unique_targets)
  cost_matrix <- matrix(forbidden, nrow = n_sources, ncol = n_targets)
  
  for (i in seq_len(nrow(df))) {
    row_idx <- source_map[as.character(source_vals[i])]
    col_idx <- target_map[as.character(target_vals[i])]
    cost_matrix[row_idx, col_idx] <- cost_vals[i]
  }
  
  # Solve
  result <- assignment(cost_matrix, maximize = maximize, method = method)
  
  # Convert back to original indices
  matched_indices <- which(result$match > 0)
  
  if (length(matched_indices) == 0) {
    out <- tibble::tibble(
      source = unique_sources[integer(0)],
      target = unique_targets[integer(0)],
      cost = numeric(0)
    )
  } else {
    out <- tibble::tibble(
      source = unique_sources[matched_indices],
      target = unique_targets[result$match[matched_indices]],
      cost = cost_matrix[cbind(matched_indices, result$match[matched_indices])]
    )
  }
  
  attr(out, "total_cost") <- result$total_cost
  attr(out, "method_used") <- result$method_used
  class(out) <- c("lap_solve_result", class(out))
  
  out
}

#' @keywords internal
lap_solve_grouped <- function(df, source_col, target_col, cost_col,
                          maximize = FALSE, method = "auto", forbidden = NA) {
  
  source_col <- rlang::enquo(source_col)
  target_col <- rlang::enquo(target_col)
  cost_col <- rlang::enquo(cost_col)
  
  # Get group variables
  groups <- dplyr::group_vars(df)
  
  # Split by groups and solve each
  df |>
    dplyr::group_split() |>
    purrr::map_dfr(function(group_df) {
      # Extract group values
      group_vals <- group_df[1, groups, drop = FALSE]
      
      # Solve for this group
      result <- lap_solve_df(group_df, source_col, target_col, cost_col,
                         maximize = maximize, method = method, forbidden = forbidden)
      
      # Add group columns back
      dplyr::bind_cols(group_vals, result)
    })
}

#' Print method for assignment results
#'
#' Nicely prints a `lap_solve_result` object, including the assignments,
#' total cost, and method used.
#'
#' @param x A `lap_solve_result` object.
#' @param ... Additional arguments passed to `print()`. Currently ignored.
#'
#' @export
#' @method print lap_solve_result
print.lap_solve_result <- function(x, ...) {
  cat("Assignment Result\n")
  cat("=================\n\n")

  total_cost <- attr(x, "total_cost")
  method_used <- attr(x, "method_used")

  # Print the tibble
  print(tibble::as_tibble(x), ...)

  cat("\nTotal cost:", total_cost, "\n")
  if (!is.null(method_used)) {
    cat("Method:", method_used, "\n")
  }

  invisible(x)
}

