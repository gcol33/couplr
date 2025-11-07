#' Find k-best optimal assignments
#'
#' Returns the top k optimal (or near-optimal) assignments using Murty's algorithm.
#' Useful for exploring alternative optimal solutions or finding robust assignments.
#'
#' @param x Cost matrix, data frame, or tibble. If a data frame/tibble,
#'   must include columns specified by `source`, `target`, and `cost`.
#' @param k Number of best solutions to return (default: 3)
#' @param source Column name for source/row indices (if `x` is a data frame)
#' @param target Column name for target/column indices (if `x` is a data frame)
#' @param cost Column name for costs (if `x` is a data frame)
#' @param maximize Logical; if TRUE, finds k-best maximizing assignments (default: FALSE)
#' @param method Algorithm for each sub-problem (default: "murty"). Future versions
#'   may support additional methods.
#' @param single_method Algorithm used for solving each node in the search tree
#'   (default: "jv")
#' @param forbidden Value to mark forbidden assignments (default: NA)
#'
#' @return A tibble with columns:
#'   - `rank`: ranking of solutions (1 = best, 2 = second best, etc.)
#'   - `solution_id`: unique identifier for each solution
#'   - `source`: source indices
#'   - `target`: target indices
#'   - `cost`: cost of each edge in the assignment
#'   - `total_cost`: total cost of the complete solution
#'
#' @examples
#' # Matrix input - find 5 best solutions
#' cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), nrow = 3)
#' lap_solve_kbest(cost, k = 5)
#'
#' # Data frame input
#' library(dplyr)
#' df <- tibble(
#'   source = rep(1:3, each = 3),
#'   target = rep(1:3, times = 3),
#'   cost = c(4, 2, 5, 3, 3, 6, 7, 5, 4)
#' )
#' lap_solve_kbest(df, k = 3, source, target, cost)
#'
#' # With maximization
#' lap_solve_kbest(cost, k = 3, maximize = TRUE)
#'
#' @export
lap_solve_kbest <- function(x, k = 3, source = NULL, target = NULL, cost = NULL,
                        maximize = FALSE, method = "murty", 
                        single_method = "jv", forbidden = NA) {
  
  # Handle data frame input
  if (is.data.frame(x)) {
    source_col <- rlang::enquo(source)
    target_col <- rlang::enquo(target)
    cost_col <- rlang::enquo(cost)
    
    if (rlang::quo_is_null(source_col) || rlang::quo_is_null(target_col) || 
        rlang::quo_is_null(cost_col)) {
      stop("For data frame input, must specify `source`, `target`, and `cost` columns")
    }
    
    return(lap_solve_kbest_df(x, k = k, source_col, target_col, cost_col,
                          maximize = maximize, method = method,
                          single_method = single_method, forbidden = forbidden))
  }
  
  # Handle matrix input
  cost_matrix <- as.matrix(x)
  
  # Call the underlying k-best function
  result <- kbest_assignment(cost_matrix, k = k, maximize = maximize,
                            method = method, single_method = single_method)
  
  # Convert to modern tidy format
  if (nrow(result) == 0) {
    out <- tibble::tibble(
      rank = integer(0),
      solution_id = integer(0),
      source = integer(0),
      target = integer(0),
      cost = numeric(0),
      total_cost = numeric(0)
    )
  } else {
    out <- tibble::tibble(
      rank = result$rank,
      solution_id = result$match_id,
      source = result$row,
      target = result$col,
      cost = result$cost_edge,
      total_cost = result$cost_total
    )
  }
  
  class(out) <- c("lap_solve_kbest_result", class(out))
  out
}

#' @keywords internal
lap_solve_kbest_df <- function(df, k, source_col, target_col, cost_col,
                           maximize = FALSE, method = "murty",
                           single_method = "jv", forbidden = NA) {
  
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
  source_map <- setNames(seq_along(unique_sources), unique_sources)
  target_map <- setNames(seq_along(unique_targets), unique_targets)
  
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
  result <- kbest_assignment(cost_matrix, k = k, maximize = maximize,
                            method = method, single_method = single_method)
  
  # Convert back to original indices
  if (nrow(result) == 0) {
    out <- tibble::tibble(
      rank = integer(0),
      solution_id = integer(0),
      source = unique_sources[integer(0)],
      target = unique_targets[integer(0)],
      cost = numeric(0),
      total_cost = numeric(0)
    )
  } else {
    out <- tibble::tibble(
      rank = result$rank,
      solution_id = result$match_id,
      source = unique_sources[result$row],
      target = unique_targets[result$col],
      cost = result$cost_edge,
      total_cost = result$cost_total
    )
  }
  
  class(out) <- c("lap_solve_kbest_result", class(out))
  out
}

#' Print method for k-best assignment results
#' @export
print.lap_solve_kbest_result <- function(x, ...) {
  cat("K-Best Assignment Results\n")
  cat("=========================\n\n")
  
  if (nrow(x) == 0) {
    cat("No solutions found\n")
    return(invisible(x))
  }
  
  n_solutions <- length(unique(x$solution_id))
  cat("Number of solutions:", n_solutions, "\n")
  
  # Get summary of total costs
  solution_costs <- x |>
    dplyr::group_by(solution_id, rank) |>
    dplyr::summarise(total_cost = dplyr::first(total_cost), .groups = "drop") |>
    dplyr::arrange(rank)
  
  cat("\nSolution costs:\n")
  for (i in seq_len(min(5, nrow(solution_costs)))) {
    cat(sprintf("  Rank %d: %.4f\n", 
                solution_costs$rank[i], 
                solution_costs$total_cost[i]))
  }
  
  if (nrow(solution_costs) > 5) {
    cat(sprintf("  ... and %d more solutions\n", nrow(solution_costs) - 5))
  }
  
  cat("\nAssignments:\n")
  print(tibble::as_tibble(x), ...)
  
  invisible(x)
}

#' Get summary of k-best results
#'
#' Extract summary information from k-best assignment results
#'
#' @param x An object of class `lap_solve_kbest_result`
#' @param ... Additional arguments (not used)
#'
#' @return A tibble with one row per solution containing:
#'   - `rank`: solution rank
#'   - `solution_id`: solution identifier
#'   - `total_cost`: total cost of solution
#'   - `n_assignments`: number of assignments in solution
#'
#' @export
summary.lap_solve_kbest_result <- function(x, ...) {
  if (nrow(x) == 0) {
    return(tibble::tibble(
      rank = integer(0),
      solution_id = integer(0),
      total_cost = numeric(0),
      n_assignments = integer(0)
    ))
  }
  
  x |>
    dplyr::group_by(rank, solution_id, total_cost) |>
    dplyr::summarise(n_assignments = dplyr::n(), .groups = "drop") |>
    dplyr::arrange(rank)
}
