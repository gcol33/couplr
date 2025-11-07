#' Solve multiple assignment problems efficiently
#'
#' Solve many independent assignment problems at once. Supports lists of matrices,
#' 3D arrays, or grouped data frames. Optional parallel execution via `n_threads`.
#'
#' @param x One of: List of cost matrices, 3D array, or grouped data frame
#' @param source Column name for source indices (if `x` is a grouped data frame)
#' @param target Column name for target indices (if `x` is a grouped data frame)
#' @param cost Column name for costs (if `x` is a grouped data frame)
#' @param maximize Logical; if TRUE, maximizes total cost (default: FALSE)
#' @param method Algorithm to use (default: "auto"). See `lap_solve` for options.
#' @param n_threads Number of threads for parallel execution (default: 1).
#'   Set to NULL to use all available cores.
#' @param forbidden Value to mark forbidden assignments (default: NA)
#'
#' @return A tibble with columns:
#'   - `problem_id`: identifier for each problem
#'   - `source`: source indices for assignments
#'   - `target`: target indices for assignments
#'   - `cost`: cost of each assignment
#'   - `total_cost`: total cost for each problem
#'   - `method_used`: algorithm used for each problem
#'
#' @examples
#' # List of matrices
#' costs <- list(
#'   matrix(c(1, 2, 3, 4), 2, 2),
#'   matrix(c(5, 6, 7, 8), 2, 2)
#' )
#' lap_solve_batch(costs)
#'
#' # 3D array
#' arr <- array(runif(2 * 2 * 10), dim = c(2, 2, 10))
#' lap_solve_batch(arr)
#'
#' # Grouped data frame
#' library(dplyr)
#' df <- tibble(
#'   sim = rep(1:5, each = 9),
#'   source = rep(1:3, times = 15),
#'   target = rep(1:3, each = 3, times = 5),
#'   cost = runif(45, 1, 10)
#' )
#' df |> group_by(sim) |> lap_solve_batch(source, target, cost)
#'
#' # Parallel execution (requires n_threads > 1)
#' lap_solve_batch(costs, n_threads = 2)
#'
#' @export
lap_solve_batch <- function(x, source = NULL, target = NULL, cost = NULL,
                        maximize = FALSE, method = "auto", 
                        n_threads = 1, forbidden = NA) {
  
  # Handle grouped data frames
  if (inherits(x, "grouped_df")) {
    return(lap_solve_batch_grouped(x, {{ source }}, {{ target }}, {{ cost }},
                               maximize = maximize, method = method,
                               n_threads = n_threads, forbidden = forbidden))
  }
  
  # Handle data frames (convert to grouped by row)
  if (is.data.frame(x) && !is.null(source)) {
    stop("For data frame input, use group_by() first to define problems")
  }
  
  # Convert to list of matrices
  matrices <- NULL
  if (is.array(x) && length(dim(x)) == 3) {
    # 3D array
    n_problems <- dim(x)[3]
    matrices <- lapply(seq_len(n_problems), function(i) x[, , i])
  } else if (is.list(x)) {
    # List of matrices
    matrices <- lapply(x, as.matrix)
  } else {
    stop("`x` must be a list of matrices, 3D array, or grouped data frame")
  }
  
  n_problems <- length(matrices)
  
  # Check for empty input
  if (n_problems == 0) {
    stop("Input must contain at least one problem to solve")
  }
  
  # Determine number of threads
  if (is.null(n_threads)) {
    n_threads <- parallel::detectCores()
  }
  n_threads <- max(1, as.integer(n_threads))
  
  # Solve problems
  if (n_threads == 1 || n_problems < 4) {
    # Sequential execution
    results <- lapply(seq_len(n_problems), function(i) {
      result <- assignment(matrices[[i]], maximize = maximize, method = method)
      
      matched_indices <- which(result$match > 0)
      
      list(
        problem_id = i,
        source = matched_indices,
        target = result$match[matched_indices],
        cost = matrices[[i]][cbind(matched_indices, result$match[matched_indices])],
        total_cost = result$total_cost,
        method_used = result$method_used
      )
    })
  } else {
    # Parallel execution
    if (!requireNamespace("parallel", quietly = TRUE)) {
      warning("Package 'parallel' not available, using sequential execution")
      n_threads <- 1
    }
    
    cl <- parallel::makeCluster(n_threads)
    on.exit(parallel::stopCluster(cl))
    
    # Export necessary functions
    parallel::clusterExport(cl, c("assignment", "matrices", "maximize", "method"),
                          envir = environment())
    
    results <- parallel::parLapply(cl, seq_len(n_problems), function(i) {
      result <- assignment(matrices[[i]], maximize = maximize, method = method)
      
      matched_indices <- which(result$match > 0)
      
      list(
        problem_id = i,
        source = matched_indices,
        target = result$match[matched_indices],
        cost = matrices[[i]][cbind(matched_indices, result$match[matched_indices])],
        total_cost = result$total_cost,
        method_used = result$method_used
      )
    })
  }
  
  # Convert to tidy tibble
  out <- purrr::map_dfr(results, function(r) {
    if (length(r$source) == 0) {
      tibble::tibble(
        problem_id = r$problem_id,
        source = integer(0),
        target = integer(0),
        cost = numeric(0),
        total_cost = r$total_cost,
        method_used = r$method_used
      )
    } else {
      tibble::tibble(
        problem_id = r$problem_id,
        source = r$source,
        target = r$target,
        cost = r$cost,
        total_cost = r$total_cost,
        method_used = r$method_used
      )
    }
  })
  
  class(out) <- c("lap_solve_batch_result", class(out))
  out
}

#' @keywords internal
lap_solve_batch_grouped <- function(df, source_col, target_col, cost_col,
                                maximize = FALSE, method = "auto",
                                n_threads = 1, forbidden = NA) {
  
  source_col <- rlang::enquo(source_col)
  target_col <- rlang::enquo(target_col)
  cost_col <- rlang::enquo(cost_col)
  
  if (rlang::quo_is_null(source_col) || rlang::quo_is_null(target_col) || 
      rlang::quo_is_null(cost_col)) {
    stop("For grouped data frame input, must specify `source`, `target`, and `cost` columns")
  }
  
  # Get group variables
  groups <- dplyr::group_vars(df)
  group_data <- dplyr::group_keys(df)
  
  # Split by groups
  group_splits <- dplyr::group_split(df)
  n_problems <- length(group_splits)
  
  # Determine number of threads
  if (is.null(n_threads)) {
    n_threads <- parallel::detectCores()
  }
  n_threads <- max(1, as.integer(n_threads))
  
  # Solve each group
  if (n_threads == 1 || n_problems < 4) {
    # Sequential execution
    results <- lapply(seq_len(n_problems), function(i) {
      group_df <- group_splits[[i]]
      
      # Get group keys
      group_vals <- group_data[i, , drop = FALSE]
      
      # Solve
      result <- lap_solve_df(group_df, source_col, target_col, cost_col,
                         maximize = maximize, method = method, forbidden = forbidden)
      
      # Add group columns
      dplyr::bind_cols(group_vals, result, 
                      tibble::tibble(
                        total_cost = attr(result, "total_cost"),
                        method_used = attr(result, "method_used")
                      ))
    })
  } else {
    # Parallel execution
    cl <- parallel::makeCluster(n_threads)
    on.exit(parallel::stopCluster(cl))
    
    # Export
    parallel::clusterExport(cl, 
                          c("group_splits", "group_data", "lap_solve_df",
                            "source_col", "target_col", "cost_col",
                            "maximize", "method", "forbidden"),
                          envir = environment())
    
    results <- parallel::parLapply(cl, seq_len(n_problems), function(i) {
      group_df <- group_splits[[i]]
      group_vals <- group_data[i, , drop = FALSE]
      
      result <- lap_solve_df(group_df, source_col, target_col, cost_col,
                         maximize = maximize, method = method, forbidden = forbidden)
      
      dplyr::bind_cols(group_vals, result,
                      tibble::tibble(
                        total_cost = attr(result, "total_cost"),
                        method_used = attr(result, "method_used")
                      ))
    })
  }
  
  # Combine results
  out <- dplyr::bind_rows(results)
  class(out) <- c("lap_solve_batch_result", class(out))
  out
}

#' Print method for batch assignment results
#' @export
print.lap_solve_batch_result <- function(x, ...) {
  cat("Batch Assignment Results\n")
  cat("========================\n\n")
  
  n_problems <- length(unique(x$problem_id))
  cat("Number of problems solved:", n_problems, "\n")
  
  if ("total_cost" %in% names(x)) {
    total_costs <- unique(x[c("problem_id", "total_cost")])
    cat("Total cost range:", 
        sprintf("[%.2f, %.2f]", min(total_costs$total_cost), max(total_costs$total_cost)),
        "\n")
  }
  
  cat("\n")
  print(tibble::as_tibble(x), ...)
  
  invisible(x)
}
