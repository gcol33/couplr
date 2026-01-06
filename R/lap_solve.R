# ==============================================================================
# Core LAP Solver (Low-Level Interface)
# ==============================================================================
# This section contains assignment(), the low-level matrix-based LAP solver.
# For most users, prefer the tidy interface lap_solve() below.
# ==============================================================================
#' Linear assignment solver
#'
#' Solve the linear assignment problem (minimum- or maximum-cost matching)
#' using several algorithms. Forbidden edges can be marked as `NA` or `Inf`.
#'
#' @param cost Numeric matrix; rows = tasks, columns = agents. `NA` or `Inf`
#'   entries are treated as forbidden assignments.
#' @param maximize Logical; if `TRUE`, maximizes the total cost instead of minimizing.
#' @param method Character string indicating the algorithm to use.
#'   One of `"auto"`, `"jv"`, `"hungarian"`, `"auction"`, `"auction_gs"`,
#'   `"sap"`, `"ssp"`, `"csflow"`, `"hk01"`, `"lapmod"`, `"csa"`, or `"bruteforce"`.
#'   `"ssp"` is accepted as an alias for `"sap"`.
#'   `"lapmod"` is a sparse variant of JV, faster for large matrices with >50% NA/Inf.
#'   `"csa"` is Goldberg-Kennedy cost-scaling, often fastest for medium-large problems.
#' @param auction_eps Optional numeric epsilon for the Auction/Auction-GS methods.
#'   If `NULL`, an internal default (e.g., `1e-9`) is used.
#' @param eps Deprecated. Use `auction_eps`. If provided and `auction_eps` is `NULL`,
#'   its value is used for `auction_eps`.
#'
#' @return An object of class `lap_solve_result`, a list with elements:
#' \itemize{
#'   \item `match` — integer vector of length `min(nrow(cost), ncol(cost))`
#'         giving the assigned column for each row (0 if unassigned).
#'   \item `total_cost` — numeric scalar, the objective value.
#'   \item `status` — character scalar, e.g. `"optimal"`.
#'   \item `method_used` — character scalar, the algorithm actually used.
#' }
#'
#' @details
#' `method = "auto"` selects an algorithm based on problem size/shape and data
#' characteristics:
#' \itemize{
#'   \item Very small (n≤8): `"bruteforce"` — exact enumeration
#'   \item Binary/constant costs: `"hk01"` — specialized for 0/1 costs
#'   \item Large sparse (n>100, >50\% NA/Inf): `"lapmod"` — sparse JV variant
#'   \item Sparse or very rectangular: `"sap"` — handles sparsity well
#'   \item Small-medium (8<n≤50): `"hungarian"` — provides exact dual solutions
#'   \item Medium (50<n≤75): `"jv"` — fast general-purpose solver
#'   \item Large (n>75): `"auction_scaled"` — fastest for large dense problems
#' }
#' Benchmarks show auction_scaled and JV are 100-1500x faster than Hungarian at n=500.
#'
#' @examples
#' cost <- matrix(c(4,2,5, 3,3,6, 7,5,4), nrow = 3, byrow = TRUE)
#' res  <- assignment(cost)
#' res$match; res$total_cost
#'
#' @export
assignment <- function(cost, maximize = FALSE,
                       method = c("auto","jv","hungarian","auction","auction_gs","auction_scaled",
                                  "sap","ssp","csflow","hk01","bruteforce",
                                  "ssap_bucket","cycle_cancel","gabow_tarjan","lapmod","csa",
                                  "ramshaw_tarjan","push_relabel"),
                       auction_eps = NULL, eps = NULL
                       # , auction_schedule = c("alpha7","pow2","halves"),  # optional (see below)
                       # , auction_final_eps = NULL                          # optional (see below)
                       ) {
  method <- match.arg(method)

  # Back-compat: eps → auction_eps
  if (!is.null(eps) && is.null(auction_eps)) auction_eps <- eps
  if (method == "ssp") method <- "sap"

  cost <- as.matrix(cost)
  if (any(is.nan(cost))) stop("NaN not allowed in `cost`")

  n <- nrow(cost); m <- ncol(cost)

  # Validate non-empty matrix
  if (n == 0 || m == 0) {
    stop("Cost matrix must have at least one row and one column.")
  }

  if (method == "auto") {
    # Check for special cost structures first
    hk01_candidate <- function(M) {
      x <- as.numeric(M[is.finite(M)]); if (!length(x)) return(FALSE)
      ux <- sort(unique(round(x, 12)))
      if (length(ux) == 1L) return(TRUE)
      if (length(ux) == 2L && all(ux %in% c(0,1))) return(TRUE)
      FALSE
    }

    # Strategy based on comprehensive benchmarks:
    # - n≤8: bruteforce (exact enumeration for very small problems)
    # - 8<n≤50: hungarian (exact dual solutions for small-medium)
    # - 50<n≤75: jv (fast general-purpose solver)
    # - n>75: auction_scaled (fastest for large problems)
    # Special cases override size-based selection:
    # - Binary/constant costs: hk01 (specialized algorithm)
    # - Sparse/rectangular: sap (handles sparsity well)

    if (n <= 8 && m <= 8) {
      method <- "bruteforce"
    } else if (hk01_candidate(cost)) {
      method <- "hk01"
    } else {
      # Count NA and Inf as sparse entries
      na_rate <- mean(is.na(cost) | is.infinite(cost))
      # Sparse or very rectangular problems
      if (na_rate > 0.5) {
        # Large sparse: use LAPMOD (sparse JV variant)
        if (n > 100) {
          method <- "lapmod"
        } else {
          method <- "sap"
        }
      } else if (m >= 3 * n) {
        # Very rectangular: SAP handles this well
        method <- "sap"
      } else if (n <= 50) {
        # Small-medium: Hungarian provides exact dual solutions
        method <- "hungarian"
      } else if (n <= 75) {
        # Medium: JV is fast and reliable
        method <- "jv"
      } else {
        # Large: auction_scaled is fastest (benchmarks show it beats JV)
        method <- "auction_scaled"
      }
    }
  }

  # auto-transpose if rows > cols
  transposed <- FALSE
  work <- cost
  if (n > m) {
    work <- t(cost); transposed <- TRUE
    tmp <- n; n <- m; m <- tmp
  }

  res_raw <- switch(
    method,
    "bruteforce"    = lap_solve_bruteforce(work, maximize),
    "jv"            = lap_solve_jv(work, maximize),
    "hungarian"     = lap_solve_hungarian(work, maximize),
    "auction"       = lap_solve_auction(work, maximize, auction_eps),
    "auction_gs"    = lap_solve_auction_gs(work, maximize, auction_eps),
    "auction_scaled"= lap_solve_auction_scaled(work, maximize),
    "sap"           = lap_solve_ssp(work, maximize),
    "csflow"        = lap_solve_csflow(work, maximize),
    "hk01"          = lap_solve_hk01(work, maximize),
    "ssap_bucket"   = lap_solve_ssap_bucket(work, maximize),
    "cycle_cancel"  = lap_solve_cycle_cancel(work, maximize),
    "gabow_tarjan"  = lap_solve_gabow_tarjan(work, maximize),
    "lapmod"        = lap_solve_lapmod(work, maximize),
    "csa"           = lap_solve_csa(work, maximize),
    "ramshaw_tarjan"= lap_solve_ramshaw_tarjan(work, maximize),
    "push_relabel"  = lap_solve_push_relabel(work, maximize),
    stop("Unknown or unimplemented method: ", method)
  )

  match_out <- as.integer(res_raw$match)
  if (transposed) {
    n0 <- ncol(work); m0 <- nrow(work)
    inv <- integer(n0); inv[] <- 0L
    for (i in seq_len(m0)) {
      j <- match_out[i]
      if (j > 0L) inv[j] <- i
    }
    match_out <- inv
  }

  out <- list(
    match = match_out,
    total_cost = as.numeric(res_raw$total_cost),
    status = "optimal",
    method_used = method
  )
  class(out) <- "lap_solve_result"
  out
}

# ==============================================================================
# Tidy LAP Interface (User-Facing)
# ==============================================================================
# This section contains lap_solve() and related tidy wrappers.
# ==============================================================================
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

  # Check if this is a tibble (from lap_solve) or a plain list (from assignment)
  if (inherits(x, "tbl_df") || inherits(x, "data.frame")) {
    # It's already a tibble/data.frame, just print it
    print(tibble::as_tibble(x), ...)
  } else {
    # It's a plain list from assignment(), format nicely
    cat("Assignments (1-based indices):\n")
    matched <- which(x$match > 0)
    if (length(matched) > 0) {
      for (i in matched[1:min(10, length(matched))]) {
        cat(sprintf("  Row %d -> Column %d\n", i, x$match[i]))
      }
      if (length(matched) > 10) {
        cat(sprintf("  ... and %d more\n", length(matched) - 10))
      }
    } else {
      cat("  (no assignments)\n")
    }
    total_cost <- x$total_cost
    method_used <- x$method_used
  }

  cat("\nTotal cost:", total_cost, "\n")
  if (!is.null(method_used)) {
    cat("Method:", method_used, "\n")
  }

  invisible(x)
}

# ============================================================================
# Specialized 1-D Line-Metric LAP Solver
# ============================================================================

#' Solve 1-D Line Assignment Problem
#'
#' Solves the linear assignment problem when both sources and targets are 
#' ordered points on a line. Uses efficient O(n*m) dynamic programming
#' for rectangular problems and O(n) sorting for square problems.
#'
#' This is a specialized solver that exploits the structure of 1-dimensional
#' assignment problems where costs depend only on the distance between points
#' on a line. It is much faster than general LAP solvers for this special case.
#'
#' @param x Numeric vector of source positions (will be sorted internally)
#' @param y Numeric vector of target positions (will be sorted internally)
#' @param cost Cost function for distance. Either:
#'   - "L1" (default): absolute distance (Manhattan distance)
#'   - "L2": squared distance (squared Euclidean distance)
#'   Can also use aliases: "abs", "manhattan" for L1; "sq", "squared", "quadratic" for L2
#' @param maximize Logical; if TRUE, maximizes total cost instead of minimizing (default: FALSE)
#'
#' @details
#' The algorithm works as follows:
#' 
#' **Square case (n == m):**
#' Both vectors are sorted and matched in order: \code{x[1] -> y[1]}, \code{x[2] -> y[2]}, etc.
#' This is optimal for any metric cost function on a line.
#'
#' **Rectangular case (n < m):**
#' Uses dynamic programming to find the optimal assignment that matches all n sources
#' to a subset of the m targets, minimizing total distance. The DP recurrence is:
#'
#' \code{dp[i][j] = min(dp[i][j-1], dp[i-1][j-1] + cost(x[i], y[j]))}
#' 
#' This finds the minimum cost to match the first i sources to the first j targets.
#' 
#' **Complexity:**
#' - Time: O(n*m) for rectangular, O(n log n) for square
#' - Space: O(n*m) for DP table
#'
#' @return A list with components:
#'   - `match`: Integer vector of length n with 1-based column indices
#'   - `total_cost`: Total cost of the assignment
#'
#' @examples
#' # Square case: equal number of sources and targets
#' x <- c(1.5, 3.2, 5.1)
#' y <- c(2.0, 3.0, 5.5)
#' result <- lap_solve_line_metric(x, y, cost = "L1")
#' print(result)
#' 
#' # Rectangular case: more targets than sources
#' x <- c(1.0, 3.0, 5.0)
#' y <- c(0.5, 2.0, 3.5, 4.5, 6.0)
#' result <- lap_solve_line_metric(x, y, cost = "L2")
#' print(result)
#' 
#' # With unsorted inputs (will be sorted internally)
#' x <- c(5.0, 1.0, 3.0)
#' y <- c(4.5, 0.5, 6.0, 2.0, 3.5)
#' result <- lap_solve_line_metric(x, y, cost = "L1")
#' print(result)
#'
#' @export
lap_solve_line_metric <- function(x, y, cost = "L1", maximize = FALSE) {
  # Validate inputs
  if (!is.numeric(x) || length(x) == 0) {
    stop("x must be a non-empty numeric vector")
  }
  if (!is.numeric(y) || length(y) == 0) {
    stop("y must be a non-empty numeric vector")
  }
  if (length(x) > length(y)) {
    stop("Number of sources (length of x) must be <= number of targets (length of y)")
  }
  
  # Check for NaN/Inf values
  if (any(!is.finite(x))) {
    stop("x must contain only finite values (no NA, NaN, or Inf)")
  }
  if (any(!is.finite(y))) {
    stop("y must contain only finite values (no NA, NaN, or Inf)")
  }
  
  # Validate cost parameter
  cost_str <- as.character(cost)[1]
  valid_costs <- c("L1", "l1", "abs", "manhattan", "L2", "l2", "sq", "squared", "quadratic")
  if (!(cost_str %in% valid_costs)) {
    stop("cost must be one of: 'L1', 'L2', 'abs', 'manhattan', 'sq', 'squared', 'quadratic'")
  }
  
  # Call C++ implementation
  result <- lap_solve_line_metric_cpp(
    x = as.numeric(x),
    y = as.numeric(y),
    cost = cost_str,
    maximize = as.logical(maximize)
  )
  
  class(result) <- c("lap_line_metric_result", "lap_result", class(result))
  return(result)
}

#' @export
print.lap_line_metric_result <- function(x, ...) {
  cat("1-D Line Assignment Result\n")
  cat("===========================\n\n")
  
  n <- length(x$match)
  cat("Assignments (1-based):\n")
  for (i in seq_len(min(n, 10))) {
    cat(sprintf("  Source %d -> Target %d\n", i, x$match[i]))
  }
  if (n > 10) {
    cat(sprintf("  ... (%d more assignments)\n", n - 10))
  }
  
  cat("\nTotal cost:", x$total_cost, "\n")
  invisible(x)
}

# ==============================================================================
# Bottleneck Assignment Problem (BAP) Solver
# ==============================================================================

#' Solve the Bottleneck Assignment Problem
#'
#' Finds an assignment that minimizes (or maximizes) the maximum edge cost
#' in a perfect matching. Unlike standard LAP which minimizes the sum of costs,
#' BAP minimizes the maximum (bottleneck) cost.
#'
#' @param cost Numeric matrix; rows = tasks, columns = agents. `NA` or `Inf`
#'   entries are treated as forbidden assignments.
#' @param maximize Logical; if `TRUE`, maximizes the minimum edge cost instead
#'   of minimizing the maximum (maximin objective). Default is `FALSE` (minimax).
#'
#' @return A list with class `"bottleneck_result"` containing:
#'   \itemize{
#'     \item `match` - integer vector of length `nrow(cost)` giving the
#'           assigned column for each row (1-based indexing)
#'     \item `bottleneck` - numeric scalar, the bottleneck (max/min edge) value
#'     \item `status` - character scalar, e.g. `"optimal"`
#'   }
#'
#' @details
#' The Bottleneck Assignment Problem (BAP) is a variant of the Linear Assignment
#' Problem where instead of minimizing the sum of assignment costs, we minimize
#' the maximum cost among all assignments (minimax objective).
#'
#' **Algorithm:**
#' Uses binary search on the sorted unique costs combined with Hopcroft-Karp
#' bipartite matching to find the minimum threshold that allows a perfect matching.
#'
#' **Complexity:** O(E * sqrt(V) * log(unique costs)) where E = edges, V = vertices.
#'
#' **Applications:**
#' \itemize{
#'   \item Task scheduling with deadline constraints (minimize latest completion)
#'   \item Resource allocation (minimize maximum load/distance)
#'   \item Network routing (minimize maximum link utilization)
#'   \item Fair division problems (minimize maximum disparity)
#' }
#'
#' @examples
#' # Simple example: minimize max cost
#' cost <- matrix(c(1, 5, 3,
#'                  2, 4, 6,
#'                  7, 1, 2), nrow = 3, byrow = TRUE)
#' result <- bottleneck_assignment(cost)
#' result$bottleneck  # Maximum edge cost in optimal assignment
#'
#' # Maximize minimum (fair allocation)
#' profits <- matrix(c(10, 5, 8,
#'                     6, 12, 4,
#'                     3, 7, 11), nrow = 3, byrow = TRUE)
#' result <- bottleneck_assignment(profits, maximize = TRUE)
#' result$bottleneck  # Minimum profit among all assignments
#'
#' # With forbidden assignments
#' cost <- matrix(c(1, NA, 3,
#'                  2, 4, Inf,
#'                  5, 1, 2), nrow = 3, byrow = TRUE)
#' result <- bottleneck_assignment(cost)
#'
#' @seealso [assignment()] for standard LAP (sum objective), [lap_solve()] for
#'   tidy LAP interface
#'
#' @export
bottleneck_assignment <- function(cost, maximize = FALSE) {
  cost <- as.matrix(cost)

  if (any(is.nan(cost))) {
    stop("NaN not allowed in `cost`")
  }

  n <- nrow(cost)
  m <- ncol(cost)

  if (n == 0 || m == 0) {
    stop("Cost matrix must have at least one row and one column.")
  }

  if (n > m) {
    stop("Bottleneck assignment requires nrow <= ncol. ",
         "Got ", n, " rows and ", m, " columns.")
  }

  # Call C++ implementation
  res_raw <- lap_solve_bottleneck(cost, maximize)

  out <- list(
    match = as.integer(res_raw$match),
    bottleneck = as.numeric(res_raw$total_cost),
    status = "optimal"
  )
  class(out) <- "bottleneck_result"
  out
}

#' @export
print.bottleneck_result <- function(x, ...) {
  cat("Bottleneck Assignment Result\n")
  cat("============================\n\n")

  n <- length(x$match)
  cat("Assignments (1-based indices):\n")
  for (i in seq_len(min(n, 10))) {
    cat(sprintf("  Row %d -> Column %d\n", i, x$match[i]))
  }
  if (n > 10) {
    cat(sprintf("  ... (%d more assignments)\n", n - 10))
  }

  cat("\nBottleneck value:", x$bottleneck, "\n")
  cat("Status:", x$status, "\n")

  invisible(x)
}

# ==============================================================================
# Note on Specialized Algorithms
# ==============================================================================
# For specialized algorithms like ssap_bucket, cycle_cancel, and gabow_tarjan,
# use assignment(cost, method = "ssap_bucket"), assignment(cost, method = "cycle_cancel"),
# or assignment(cost, method = "gabow_tarjan") respectively.
#
# These are accessed via the method parameter in assignment() rather than
# separate wrapper functions to keep the API clean.

# ==============================================================================
# Sinkhorn-Knopp (Entropy-Regularized Optimal Transport)
# ==============================================================================

#' Sinkhorn-Knopp optimal transport solver
#'
#' Compute an entropy-regularized optimal transport plan using the Sinkhorn-Knopp
#' algorithm. Unlike other LAP solvers that return a hard 1-to-1 assignment,
#' this returns a soft assignment (doubly stochastic matrix).
#'
#' @param cost Numeric matrix of transport costs. `NA` or `Inf` entries are
#'   treated as very high cost (effectively forbidden).
#' @param lambda Regularization parameter (default 10). Higher values produce
#'   sharper (more deterministic) transport plans; lower values produce smoother
#'   distributions. Typical range: 1-100.
#' @param tol Convergence tolerance (default 1e-9).
#' @param max_iter Maximum iterations (default 1000).
#' @param r_weights Optional numeric vector of row marginals (source distribution).
#'   Default is uniform. Will be normalized to sum to 1.
#' @param c_weights Optional numeric vector of column marginals (target distribution).
#'   Default is uniform. Will be normalized to sum to 1.
#'
#' @return A list with elements:
#' \itemize{
#'   \item `transport_plan` — numeric matrix, the optimal transport plan P.
#'         Row sums approximate r_weights, column sums approximate c_weights.
#'   \item `cost` — the transport cost <C, P> (without entropy term).
#'   \item `u`, `v` — scaling vectors (P = diag(u) * K * diag(v) where K = exp(-lambda*C)).
#'   \item `converged` — logical, whether the algorithm converged.
#'   \item `iterations` — number of iterations used.
#'   \item `lambda` — the regularization parameter used.
#' }
#'
#' @details
#' The Sinkhorn-Knopp algorithm solves the entropy-regularized optimal transport
#' problem:
#'
#' \deqn{P^* = \arg\min_P \langle C, P \rangle - \frac{1}{\lambda} H(P)}
#'
#' subject to row sums = r_weights and column sums = c_weights.
#'
#' The entropy term H(P) encourages spread in the transport plan. As lambda -> Inf,

#' the solution approaches the standard (unregularized) optimal transport.
#'
#' **Key differences from standard LAP solvers:**
#' - Returns a soft assignment (probabilities) not a hard 1-to-1 matching
#' - Supports unequal marginals (weighted distributions)
#' - Differentiable, making it useful in ML pipelines
#' - Very fast: O(n²) per iteration with typically O(1/ε²) iterations
#'
#' Use [sinkhorn_to_assignment()] to round the soft assignment to a hard matching.
#'
#' @examples
#' cost <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, byrow = TRUE)
#'
#' # Soft assignment with default parameters
#' result <- sinkhorn(cost)
#' print(round(result$transport_plan, 3))
#'
#' # Sharper assignment (higher lambda)
#' result_sharp <- sinkhorn(cost, lambda = 50)
#' print(round(result_sharp$transport_plan, 3))
#'
#' # With custom marginals (more mass from row 1)
#' result_weighted <- sinkhorn(cost, r_weights = c(0.5, 0.25, 0.25))
#' print(round(result_weighted$transport_plan, 3))
#'
#' # Round to hard assignment
#' hard_match <- sinkhorn_to_assignment(result)
#' print(hard_match)
#'
#' @seealso [assignment()] for hard 1-to-1 matching, [sinkhorn_to_assignment()]
#'   to round soft assignments.
#'
#' @references
#' Cuturi, M. (2013). Sinkhorn Distances: Lightspeed Computation of Optimal
#' Transport. *Advances in Neural Information Processing Systems*, 26.
#'
#' @export
sinkhorn <- function(cost, lambda = 10, tol = 1e-9, max_iter = 1000,
                     r_weights = NULL, c_weights = NULL) {
  if (!is.matrix(cost)) {
    cost <- as.matrix(cost)
  }
  if (!is.numeric(cost)) {
    stop("cost must be a numeric matrix")
  }
  if (lambda <= 0) {
    stop("lambda must be positive")
  }

  lap_solve_sinkhorn(cost, lambda, tol, max_iter, r_weights, c_weights)
}

#' Round Sinkhorn transport plan to hard assignment
#'
#' Convert a soft transport plan from [sinkhorn()] to a hard 1-to-1 assignment
#' using greedy rounding.
#'
#' @param result Either a result from [sinkhorn()] or a transport plan matrix.
#'
#' @return Integer vector of column assignments (1-based), same format as
#'   [assignment()].
#'
#' @details
#' Greedy rounding iteratively assigns each row to its most probable column,
#' ensuring no column is assigned twice. This may not give the globally optimal
#' hard assignment; for that, use the transport plan as a cost matrix with
#' [assignment()].
#'
#' @examples
#' cost <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, byrow = TRUE)
#' result <- sinkhorn(cost, lambda = 20)
#' hard_match <- sinkhorn_to_assignment(result)
#' print(hard_match)
#'
#' @seealso [sinkhorn()]
#' @export
sinkhorn_to_assignment <- function(result) {
  if (is.list(result) && "transport_plan" %in% names(result)) {
    P <- result$transport_plan
  } else if (is.matrix(result)) {
    P <- result
  } else {
    stop("result must be a sinkhorn() result or a transport plan matrix")
  }

  sinkhorn_round(P)
}

# ==============================================================================
# Assignment with Dual Variables
# ==============================================================================

#' Solve assignment problem and return dual variables
#'
#' Solves the linear assignment problem and returns dual potentials (u, v)
#' in addition to the optimal matching. The dual variables provide an
#' optimality certificate and enable sensitivity analysis.
#'
#' @param cost Numeric matrix; rows = tasks, columns = agents. `NA` or `Inf`
#'   entries are treated as forbidden assignments.
#' @param maximize Logical; if `TRUE`, maximizes the total cost instead of minimizing.
#'
#' @return A list with class `"assignment_duals_result"` containing:
#'   \itemize{
#'     \item `match` - integer vector of column assignments (1-based)
#'     \item `total_cost` - optimal objective value
#'     \item `u` - numeric vector of row dual variables (length n)
#'     \item `v` - numeric vector of column dual variables (length m)
#'     \item `status` - character, e.g. "optimal"
#'   }
#'
#' @details
#' The dual variables satisfy the complementary slackness conditions:
#' \itemize{
#'   \item For minimization: `u[i] + v[j] <= cost[i,j]` for all (i,j)
#'   \item For any assigned pair (i,j): `u[i] + v[j] = cost[i,j]`
#' }
#'
#' This implies that `sum(u) + sum(v) = total_cost` (strong duality).
#'
#' **Applications of dual variables:**
#' \itemize{
#'   \item **Optimality verification**: Check that duals satisfy constraints

#'   \item **Sensitivity analysis**: Reduced cost `c[i,j] - u[i] - v[j]` shows
#'         how much an edge cost must decrease before it enters the solution
#'   \item **Pricing in column generation**: Use duals to price new columns
#'   \item **Warm starting**: Reuse duals when costs change slightly
#' }
#'
#' @examples
#' cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), nrow = 3, byrow = TRUE)
#' result <- assignment_duals(cost)
#'
#' # Check optimality: u + v should equal cost for assigned pairs
#' for (i in 1:3) {
#'   j <- result$match[i]
#'   cat(sprintf("Row %d -> Col %d: u + v = %.2f, cost = %.2f\n",
#'               i, j, result$u[i] + result$v[j], cost[i, j]))
#' }
#'
#' # Verify strong duality
#' cat("sum(u) + sum(v) =", sum(result$u) + sum(result$v), "\n")
#' cat("total_cost =", result$total_cost, "\n")
#'
#' # Reduced costs (how much must cost decrease to enter solution)
#' reduced <- outer(result$u, result$v, "+")
#' reduced_cost <- cost - reduced
#' print(round(reduced_cost, 2))
#'
#' @seealso [assignment()] for standard assignment without duals
#' @export
assignment_duals <- function(cost, maximize = FALSE) {
  cost <- as.matrix(cost)
  if (any(is.nan(cost))) stop("NaN not allowed in `cost`")

  n <- nrow(cost)
  m <- ncol(cost)

  if (n == 0 || m == 0) {
    stop("Cost matrix must have at least one row and one column.")
  }

  # Auto-transpose if rows > cols
  transposed <- FALSE
  work <- cost
  if (n > m) {
    work <- t(cost)
    transposed <- TRUE
    tmp <- n; n <- m; m <- tmp
  }

  # Call JV with duals

  res_raw <- lap_solve_jv_duals(work, maximize)

  match_out <- as.integer(res_raw$match)
  u_out <- as.numeric(res_raw$u)
  v_out <- as.numeric(res_raw$v)

  if (transposed) {
    # Map back: work was m0 x n0, match_work is length m0
    n0 <- ncol(work)
    m0 <- nrow(work)
    inv <- integer(n0)
    inv[] <- 0L
    for (i in seq_len(m0)) {
      j <- match_out[i]
      if (j > 0L) inv[j] <- i
    }
    match_out <- inv
    # Swap u and v for transposed case
    tmp_u <- u_out
    u_out <- v_out
    v_out <- tmp_u
  }

  out <- list(
    match = match_out,
    total_cost = as.numeric(res_raw$total_cost),
    u = u_out,
    v = v_out,
    status = "optimal"
  )
  class(out) <- "assignment_duals_result"
  out
}

#' @export
print.assignment_duals_result <- function(x, ...) {
  cat("Assignment Result with Duals\n")
  cat("============================\n\n")

  n <- length(x$match)
  cat("Assignments (1-based indices):\n")
  for (i in seq_len(min(n, 10))) {
    if (x$match[i] > 0) {
      cat(sprintf("  Row %d -> Column %d\n", i, x$match[i]))
    }
  }
  if (n > 10) {
    cat(sprintf("  ... (%d more assignments)\n", n - 10))
  }

  cat("\nTotal cost:", x$total_cost, "\n")
  cat("Status:", x$status, "\n")

  cat("\nDual variables:\n")
  cat("  u (row):", head(x$u, 5))
  if (length(x$u) > 5) cat(" ...")
  cat("\n")
  cat("  v (col):", head(x$v, 5))
  if (length(x$v) > 5) cat(" ...")
  cat("\n")

  invisible(x)
}
