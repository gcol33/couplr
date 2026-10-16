#' Number of worker processes to use when `n_threads` is NULL
#'
#' `parallel::detectCores()` reports the physical core count and ignores the
#' two-core limit that `R CMD check` advertises through `_R_CHECK_LIMIT_CORES_`,
#' so it is honoured here.
#'
#' @return Integer number of workers, at least 1.
#' @noRd
.batch_threads <- function() {
  if (nzchar(Sys.getenv("_R_CHECK_LIMIT_CORES_"))) return(2L)
  n <- parallel::detectCores()
  if (is.na(n)) 1L else as.integer(n)
}

#' Solve Many Assignment Problems
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
#'   `summary()` returns a `summary.lap_solve_batch_result` whose `problems`
#'   table has one row per input problem, with grouping keys, assignment
#'   counts, total costs, and solver names. Each problem contributes once.
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
    # 3D array. Reshape each slice explicitly so problems with a singleton
    # row or column dimension keep their orientation (x[, , i] would drop it).
    d <- dim(x)
    n_problems <- d[3]
    matrices <- lapply(seq_len(n_problems), function(i) {
      matrix(x[, , i], nrow = d[1], ncol = d[2])
    })
  } else if (is.list(x)) {
    # List of matrices
    matrices <- lapply(x, as.matrix)
  } else {
    stop("`x` must be a list of matrices, 3D array, or grouped data frame")
  }
  
  # The sentinel is applied at the door, so a list or a 3D array masks the
  # same way lap_solve() does on a single matrix.
  matrices <- lapply(matrices, mask_forbidden, forbidden = forbidden)
  
  n_problems <- length(matrices)
  
  # Check for empty input
  if (n_problems == 0) {
    stop("Input must contain at least one problem to solve")
  }
  
  # Determine number of threads
  if (is.null(n_threads)) {
    n_threads <- .batch_threads()
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
  
  .new_batch_result(out, "problem_id")
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
    n_threads <- .batch_threads()
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
  .new_batch_result(out, groups)
}

.new_batch_result <- function(out, keys) {
  attr(out, "problem_keys") <- keys
  class(out) <- c("lap_solve_batch_result", setdiff(class(out), "lap_solve_result"))
  out
}

.batch_problem_table <- function(x) {
  keys <- attr(x, "problem_keys", exact = TRUE) %||% "problem_id"
  metadata_columns <- c(keys, "total_cost", "method_used")
  if (!all(metadata_columns %in% names(x))) return(NULL)
  data <- tibble::as_tibble(x)
  metadata <- unique(data[, metadata_columns, drop = FALSE])
  if (anyDuplicated(metadata[, keys, drop = FALSE])) {
    stop("A batch problem has inconsistent total cost or solver metadata.", call. = FALSE)
  }
  counts <- dplyr::summarise(
    dplyr::group_by(data, dplyr::across(dplyr::all_of(keys))),
    n_assignments = dplyr::n(), .groups = "drop")
  out <- dplyr::left_join(metadata, counts, by = keys)
  out[, c(keys, "n_assignments", "total_cost", "method_used"), drop = FALSE]
}

#' @rdname lap_solve_batch
#' @param x A `lap_solve_batch_result` object.
#' @param ... Additional arguments passed to `print()`. Currently ignored.
#'
#' @export
#' @method print lap_solve_batch_result
print.lap_solve_batch_result <- function(x, ...) {
  .print_batch_overview(.batch_problem_table(x))
  invisible(x)
}

.print_batch_overview <- function(problems) {
  cat("Batch Assignment Results\n")
  cat("========================\n\n")

  if (!is.null(problems)) {
    cat("Number of problems solved:", nrow(problems), "\n")
    cat("Assignments:", sum(problems$n_assignments), "\n")
  }
  if (!is.null(problems) && nrow(problems)) {
    rng <- range(problems$total_cost, na.rm = TRUE)
    cat("Total cost range:", sprintf("[%.2f, %.2f]", rng[1], rng[2]), "\n")
  }

  cat("\n")
}

#' @rdname lap_solve_batch
#' @param object A `lap_solve_batch_result`.
#' @export
summary.lap_solve_batch_result <- function(object, ...) {
  problems <- .batch_problem_table(object)
  if (is.null(problems)) {
    stop("Batch results need their problem keys, total_cost and method_used columns.",
         call. = FALSE)
  }
  structure(list(problems = problems), class = "summary.lap_solve_batch_result")
}

#' @export
print.summary.lap_solve_batch_result <- function(x, ...) {
  .print_batch_overview(x$problems)
  cat("Problems:\n")
  print(x$problems, n = Inf, width = Inf)
  invisible(x)
}
