# ==============================================================================
# Matching Core - match_couples() and greedy_couples()
# ==============================================================================

#' Optimal matching using linear assignment
#'
#' Performs optimal one-to-one matching between two datasets using linear
#' assignment problem (LAP) solvers. Supports blocking, distance constraints,
#' and various distance metrics.
#'
#' This function finds the matching that minimizes total distance among all
#' feasible matchings, subject to constraints. Use [greedy_couples()] for
#' faster approximate matching on large datasets.
#'
#' @param left Data frame of "left" units (e.g., treated, cases)
#' @param right Data frame of "right" units (e.g., control, controls)
#' @param vars Variable names to use for distance computation
#' @param distance Distance metric: "euclidean", "manhattan", "mahalanobis",
#'   or a custom function
#' @param weights Optional named vector of variable weights
#' @param scale Scaling method: FALSE (none), "standardize", "range", or "robust"
#' @param auto_scale If TRUE, automatically check variable health and select
#'   scaling method (default: FALSE)
#' @param max_distance Maximum allowed distance (pairs exceeding this are forbidden)
#' @param calipers Named list of per-variable maximum absolute differences
#' @param block_id Column name containing block IDs (for stratified matching)
#' @param ignore_blocks If TRUE, ignore block_id even if present
#' @param require_full_matching If TRUE, error if any units remain unmatched
#' @param method LAP solver: "auto", "hungarian", "jv", "gabow_tarjan", etc.
#' @param return_unmatched Include unmatched units in output
#' @param return_diagnostics Include detailed diagnostics in output
#' @param parallel Enable parallel processing for blocked matching.
#'   Requires 'future' and 'future.apply' packages. Can be:
#'   - `FALSE`: Sequential processing (default)
#'   - `TRUE`: Auto-configure parallel backend
#'   - Character: Specify future plan (e.g., "multisession", "multicore")
#' @param check_costs If TRUE, check distance distribution for potential problems
#'   and provide helpful warnings before matching (default: TRUE)
#'
#' @return A list with class "matching_result" containing:
#'   - `pairs`: Tibble of matched pairs with distances
#'   - `unmatched`: List of unmatched left and right IDs
#'   - `info`: Matching diagnostics and metadata
#'
#' @examples
#' # Basic matching
#' left <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 5), y = c(2, 4, 6, 8, 10))
#' right <- data.frame(id = 6:10, x = c(1.1, 2.2, 3.1, 4.2, 5.1), y = c(2.1, 4.1, 6.2, 8.1, 10.1))
#' result <- match_couples(left, right, vars = c("x", "y"))
#' print(result$pairs)
#'
#' # With constraints
#' result <- match_couples(left, right, vars = c("x", "y"),
#'                         max_distance = 1,
#'                         calipers = list(x = 0.5))
#'
#' # With blocking
#' left$region <- c("A", "A", "B", "B", "B")
#' right$region <- c("A", "A", "B", "B", "B")
#' blocks <- matchmaker(left, right, block_type = "group", block_by = "region")
#' result <- match_couples(blocks$left, blocks$right, vars = c("x", "y"))
#'
#' @export
match_couples <- function(left, right = NULL,
                          vars = NULL,
                          distance = "euclidean",
                          weights = NULL,
                          scale = FALSE,
                          auto_scale = FALSE,
                          max_distance = Inf,
                          calipers = NULL,
                          block_id = NULL,
                          ignore_blocks = FALSE,
                          require_full_matching = FALSE,
                          method = "auto",
                          return_unmatched = TRUE,
                          return_diagnostics = FALSE,
                          parallel = FALSE,
                          check_costs = TRUE) {

  # Check if left is a distance_object
  if (is_distance_object(left)) {
    return(match_couples_from_distance(
      left,
      max_distance = max_distance,
      calipers = calipers,
      ignore_blocks = ignore_blocks,
      require_full_matching = require_full_matching,
      method = method,
      return_unmatched = return_unmatched,
      return_diagnostics = return_diagnostics,
      check_costs = check_costs
    ))
  }

  # Standard path: left and right are datasets
  if (is.null(right)) {
    couplr_stop("When left is a dataset, right must be provided\n",
                "  ", couplr_emoji("search"),
                "Need two datasets to make couples!")
  }

  if (is.null(vars)) {
    couplr_stop("When left is a dataset, vars must be specified\n",
                "  ", couplr_emoji("info"),
                "Use vars = c('var1', 'var2', ...) to specify matching ",
                "variables")
  }

  # Apply automatic preprocessing if requested
  if (auto_scale) {
    preproc <- preprocess_matching_vars(
      left, right, vars,
      auto_scale = TRUE,
      scale_method = if (identical(scale, FALSE)) "auto" else scale,
      check_health = TRUE,
      remove_problematic = TRUE,
      verbose = TRUE
    )

    # Update vars and scale based on preprocessing
    vars <- preproc$vars
    if (preproc$scaling_method != "none") {
      scale <- preproc$scaling_method
    }
  }

  # Validate inputs
  validate_matching_inputs(left, right, vars)
  weights <- validate_weights(weights, vars)
  calipers <- validate_calipers(calipers, vars)

  # Extract IDs
  left_ids <- extract_ids(left, "left")
  right_ids <- extract_ids(right, "right")

  # Store original row indices
  left$..row_idx <- seq_len(nrow(left))
  right$..row_idx <- seq_len(nrow(right))

  # Detect blocking
  block_info <- detect_blocking(left, right, block_id, ignore_blocks)

  if (block_info$use_blocking) {
    # Setup parallel processing if requested
    parallel_state <- setup_parallel(parallel)
    on.exit(restore_parallel(parallel_state), add = TRUE)

    # Blocked matching
    result <- match_couples_blocked(
      left, right, left_ids, right_ids,
      block_col = block_info$block_col,
      vars = vars, distance = distance, weights = weights, scale = scale,
      max_distance = max_distance, calipers = calipers,
      method = method,
      parallel = parallel_state$setup
    )
  } else {
    # Single matching
    result <- match_couples_single(
      left, right, left_ids, right_ids,
      vars = vars, distance = distance, weights = weights, scale = scale,
      max_distance = max_distance, calipers = calipers,
      method = method,
      check_costs = check_costs
    )
  }

  # Clean up temporary column
  left$..row_idx <- NULL
  right$..row_idx <- NULL

  # Check for full matching if required
  if (require_full_matching) {
    check_full_matching(result)
  }

  # Add metadata
  result$info$method <- "lap"
  result$info$distance_metric <- distance
  result$info$scaled <- !identical(scale, FALSE)
  result$info$n_left <- nrow(left)
  result$info$n_right <- nrow(right)

  if (!return_unmatched) {
    result$unmatched <- NULL
  }

  if (!return_diagnostics) {
    result$info <- result$info[c("method", "n_matched", "total_distance")]
  }

  structure(result, class = c("matching_result", "couplr_result"))
}

#' Match from Precomputed Distance Object
#'
#' Internal function to handle matching when a distance_object is provided
#'
#' @keywords internal
match_couples_from_distance <- function(dist_obj,
                                        max_distance = Inf,
                                        calipers = NULL,
                                        ignore_blocks = FALSE,
                                        require_full_matching = FALSE,
                                        method = "auto",
                                        return_unmatched = TRUE,
                                        return_diagnostics = FALSE,
                                        check_costs = TRUE) {

  # Extract from distance object
  cost_matrix <- dist_obj$cost_matrix
  left <- dist_obj$original_left
  right <- dist_obj$original_right
  left_ids <- dist_obj$left_ids
  right_ids <- dist_obj$right_ids

  # Apply additional constraints if specified
  if (!is.infinite(max_distance) || !is.null(calipers)) {
    cost_matrix <- apply_all_constraints(
      cost_matrix,
      left, right,
      dist_obj$metadata$vars,
      max_distance,
      calipers
    )
  }

  # Check cost distribution if requested
  if (check_costs) {
    check_cost_distribution(cost_matrix, warn = TRUE)
  }

  # Check for valid pairs
  if (!has_valid_pairs(cost_matrix)) {
    err_no_valid_pairs("No valid pairs after applying constraints")

    return(structure(
      list(
        pairs = tibble::tibble(
          left_id = character(0),
          right_id = character(0),
          distance = numeric(0)
        ),
        unmatched = list(
          left = left_ids,
          right = right_ids
        ),
        info = list(
          method = "from_distance_object",
          solver = NA_character_,
          n_matched = 0,
          total_distance = 0,
          n_left = length(left_ids),
          n_right = length(right_ids)
        )
      ),
      class = c("matching_result", "couplr_result")
    ))
  }

  # Solve LAP
  lap_result <- assignment(cost_matrix, maximize = FALSE, method = method)

  # Extract matches
  matched_rows <- which(lap_result$match > 0)

  if (length(matched_rows) == 0) {
    pairs <- tibble::tibble(
      left_id = character(0),
      right_id = character(0),
      distance = numeric(0)
    )
  } else {
    # Get actual distances
    distances <- numeric(length(matched_rows))
    for (i in seq_along(matched_rows)) {
      row <- matched_rows[i]
      col <- lap_result$match[row]
      distances[i] <- cost_matrix[row, col]
    }

    # Filter out BIG_COST matches
    valid <- distances < BIG_COST
    matched_rows <- matched_rows[valid]
    distances <- distances[valid]

    pairs <- tibble::tibble(
      left_id = left_ids[matched_rows],
      right_id = right_ids[lap_result$match[matched_rows]],
      distance = distances
    )
  }

  # Unmatched units
  matched_left <- matched_rows
  matched_right <- lap_result$match[matched_rows]
  unmatched_left <- setdiff(seq_along(left_ids), matched_left)
  unmatched_right <- setdiff(seq_along(right_ids), matched_right)

  result <- list(
    pairs = pairs,
    unmatched = list(
      left = left_ids[unmatched_left],
      right = right_ids[unmatched_right]
    ),
    info = list(
      method = "from_distance_object",
      solver = lap_result$solver,
      n_matched = nrow(pairs),
      total_distance = sum(pairs$distance),
      distance_metric = dist_obj$metadata$distance,
      scaled = !identical(dist_obj$metadata$scale, FALSE),
      n_left = length(left_ids),
      n_right = length(right_ids)
    )
  )

  # Check for full matching if required
  if (require_full_matching) {
    check_full_matching(result)
  }

  if (!return_unmatched) {
    result$unmatched <- NULL
  }

  if (!return_diagnostics) {
    result$info <- result$info[c("method", "n_matched", "total_distance")]
  }

  structure(result, class = c("matching_result", "couplr_result"))
}

#' Match without blocking (single problem)
#'
#' @keywords internal
match_couples_single <- function(left, right, left_ids, right_ids,
                                 vars, distance, weights, scale,
                                 max_distance, calipers, method,
                                 check_costs = TRUE) {

  # Build cost matrix
  cost_matrix <- build_cost_matrix(left, right, vars, distance, weights, scale)

  # Apply constraints
  cost_matrix <- apply_all_constraints(cost_matrix, left, right, vars,
                                       max_distance, calipers)

  # Check cost distribution if requested
  if (check_costs) {
    check_cost_distribution(cost_matrix, warn = TRUE)
  }

  # Check for valid pairs
  if (!has_valid_pairs(cost_matrix)) {
    err_no_valid_pairs("No valid pairs after applying constraints")

    return(list(
      pairs = tibble::tibble(
        left_id = character(0),
        right_id = character(0),
        distance = numeric(0)
      ),
      unmatched = list(
        left = left_ids,
        right = right_ids
      ),
      info = list(
        solver = NA_character_,
        n_matched = 0,
        total_distance = 0
      )
    ))
  }

  # Solve LAP
  lap_result <- assignment(cost_matrix, maximize = FALSE, method = method)

  # Extract matches
  matched_rows <- which(lap_result$match > 0)

  if (length(matched_rows) == 0) {
    pairs <- tibble::tibble(
      left_id = character(0),
      right_id = character(0),
      distance = numeric(0)
    )
  } else {
    # Get actual distances (not BIG_COST)
    distances <- numeric(length(matched_rows))
    for (i in seq_along(matched_rows)) {
      row <- matched_rows[i]
      col <- lap_result$match[row]
      distances[i] <- cost_matrix[row, col]
    }

    # Filter out BIG_COST matches (shouldn't happen, but safety check)
    valid <- distances < BIG_COST
    matched_rows <- matched_rows[valid]
    distances <- distances[valid]

    pairs <- tibble::tibble(
      left_id = left_ids[matched_rows],
      right_id = right_ids[lap_result$match[matched_rows]],
      distance = distances
    )

    # Add variable differences if requested
    for (v in vars) {
      left_vals <- left[[v]][matched_rows]
      right_vals <- right[[v]][lap_result$match[matched_rows]]
      pairs[[paste0(".", v, "_diff")]] <- left_vals - right_vals
    }
  }

  # Unmatched units
  matched_left <- matched_rows
  matched_right <- lap_result$match[matched_rows]

  unmatched_left <- setdiff(seq_len(nrow(left)), matched_left)
  unmatched_right <- setdiff(seq_len(nrow(right)), matched_right)

  list(
    pairs = pairs,
    unmatched = list(
      left = left_ids[unmatched_left],
      right = right_ids[unmatched_right]
    ),
    info = list(
      solver = lap_result$method_used,
      n_matched = nrow(pairs),
      total_distance = sum(pairs$distance, na.rm = TRUE)
    )
  )
}

#' Match with blocking (multiple problems)
#'
#' @keywords internal
match_couples_blocked <- function(left, right, left_ids, right_ids,
                                  block_col, vars, distance, weights, scale,
                                  max_distance, calipers, method,
                                  parallel = FALSE) {

  blocks <- unique(c(left[[block_col]], right[[block_col]]))

  # Use parallel processing if requested and available
  if (parallel && length(blocks) > 1) {
    result <- match_blocks_parallel(
      blocks, left, right, left_ids, right_ids,
      block_col, vars, distance, weights, scale,
      max_distance, calipers, method,
      parallel = TRUE
    )

    # Reorder columns to put block_id first
    if (nrow(result$pairs) > 0) {
      result$pairs <- dplyr::select(result$pairs, "block_id", dplyr::everything())
    }

    # Add additional summary statistics
    if (nrow(result$block_summary) > 0) {
      result$block_summary <- dplyr::mutate(
        result$block_summary,
        n_pairs = .data$n_matched,
        total_distance = .data$n_matched * .data$mean_distance,
        n_unmatched_left = .data$n_left - .data$n_matched,
        n_unmatched_right = .data$n_right - .data$n_matched
      )
    }

    return(list(
      pairs = result$pairs,
      unmatched = result$unmatched,
      info = list(
        n_matched = nrow(result$pairs),
        total_distance = sum(result$pairs$distance, na.rm = TRUE),
        blocked = TRUE,
        n_blocks = length(blocks),
        block_summary = result$block_summary
      )
    ))
  }

  # Sequential processing (original code)
  all_pairs <- list()
  all_unmatched_left <- character(0)
  all_unmatched_right <- character(0)
  block_summaries <- list()

  for (block in blocks) {
    left_block <- left[left[[block_col]] == block, ]
    right_block <- right[right[[block_col]] == block, ]

    if (nrow(left_block) == 0 || nrow(right_block) == 0) {
      # Skip blocks with no units on one side
      if (nrow(left_block) > 0) {
        block_left_ids <- left_ids[left[[block_col]] == block]
        all_unmatched_left <- c(all_unmatched_left, block_left_ids)
      }
      if (nrow(right_block) > 0) {
        block_right_ids <- right_ids[right[[block_col]] == block]
        all_unmatched_right <- c(all_unmatched_right, block_right_ids)
      }
      next
    }

    # Get IDs for this block
    block_left_ids <- left_ids[left[[block_col]] == block]
    block_right_ids <- right_ids[right[[block_col]] == block]

    # Match within block
    block_result <- match_couples_single(
      left_block, right_block, block_left_ids, block_right_ids,
      vars, distance, weights, scale,
      max_distance, calipers, method
    )

    # Add block_id column
    if (nrow(block_result$pairs) > 0) {
      block_result$pairs$block_id <- block
      all_pairs[[length(all_pairs) + 1]] <- block_result$pairs
    }

    # Accumulate unmatched
    all_unmatched_left <- c(all_unmatched_left, block_result$unmatched$left)
    all_unmatched_right <- c(all_unmatched_right, block_result$unmatched$right)

    # Block summary
    block_summaries[[length(block_summaries) + 1]] <- tibble::tibble(
      block_id = block,
      n_pairs = nrow(block_result$pairs),
      total_distance = sum(block_result$pairs$distance, na.rm = TRUE),
      mean_distance = mean(block_result$pairs$distance, na.rm = TRUE),
      n_unmatched_left = length(block_result$unmatched$left),
      n_unmatched_right = length(block_result$unmatched$right)
    )
  }

  # Combine results
  if (length(all_pairs) > 0) {
    pairs <- dplyr::bind_rows(all_pairs)
    # Reorder columns to put block_id first
    pairs <- dplyr::select(pairs, "block_id", dplyr::everything())
  } else {
    pairs <- tibble::tibble(
      block_id = character(0),
      left_id = character(0),
      right_id = character(0),
      distance = numeric(0)
    )
  }

  block_summary_df <- if (length(block_summaries) > 0) {
    dplyr::bind_rows(block_summaries)
  } else {
    tibble::tibble(
      block_id = character(0),
      n_pairs = integer(0),
      total_distance = numeric(0),
      mean_distance = numeric(0),
      n_unmatched_left = integer(0),
      n_unmatched_right = integer(0)
    )
  }

  list(
    pairs = pairs,
    unmatched = list(
      left = all_unmatched_left,
      right = all_unmatched_right
    ),
    info = list(
      solver = method,
      n_blocks = length(blocks),
      n_matched = nrow(pairs),
      total_distance = sum(pairs$distance, na.rm = TRUE),
      block_summary = block_summary_df
    )
  )
}

#' Detect and validate blocking
#'
#' @keywords internal
detect_blocking <- function(left, right, block_id, ignore_blocks) {
  if (ignore_blocks) {
    return(list(use_blocking = FALSE, block_col = NULL))
  }

  # Explicit block_id specified
  if (!is.null(block_id)) {
    if (!(block_id %in% names(left))) {
      stop(sprintf("block_id column '%s' not found in left", block_id), call. = FALSE)
    }
    if (!(block_id %in% names(right))) {
      stop(sprintf("block_id column '%s' not found in right", block_id), call. = FALSE)
    }
    return(list(use_blocking = TRUE, block_col = block_id))
  }

  # Auto-detect block column
  left_block_col <- get_block_id_column(left)
  right_block_col <- get_block_id_column(right)

  if (!is.null(left_block_col) && !is.null(right_block_col)) {
    if (left_block_col == right_block_col) {
      return(list(use_blocking = TRUE, block_col = left_block_col))
    }
  }

  list(use_blocking = FALSE, block_col = NULL)
}

#' Check if full matching was achieved
#'
#' @keywords internal
check_full_matching <- function(result) {
  n_unmatched <- length(result$unmatched$left) + length(result$unmatched$right)

  if (n_unmatched > 0) {
    stop(
      sprintf("Full matching required but %d units remain unmatched:\n", n_unmatched),
      sprintf("  - %d left unmatched\n", length(result$unmatched$left)),
      sprintf("  - %d right unmatched\n", length(result$unmatched$right)),
      "Consider relaxing constraints (max_distance, calipers) or set require_full_matching = FALSE",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

# ==============================================================================
# Greedy Matching (Approximate)
# ==============================================================================

#' Fast approximate matching using greedy algorithm
#'
#' Performs fast one-to-one matching using greedy strategies. Does not guarantee
#' optimal total distance but is much faster than [match_couples()] for large
#' datasets. Supports blocking, distance constraints, and various distance metrics.
#'
#' @inheritParams match_couples
#' @param strategy Greedy strategy:
#'   - "row_best": For each row, find best available column (default)
#'   - "sorted": Sort all pairs by distance, greedily assign
#'   - "pq": Use priority queue (good for very large problems)
#'
#' @return A list with class "matching_result" (same structure as match_couples)
#'
#' @details
#' Greedy strategies do not guarantee optimal total distance but are much faster:
#' - "row_best": O(n*m) time, simple and often produces good results
#' - "sorted": O(n*m*log(n*m)) time, better quality but slower
#' - "pq": O(n*m*log(n*m)) time, memory-efficient for large problems
#'
#' Use greedy_couples when:
#' - Dataset is very large (> 10,000 x 10,000)
#' - Approximate solution is acceptable
#' - Speed is more important than optimality
#'
#' @examples
#' # Basic greedy matching
#' left <- data.frame(id = 1:100, x = rnorm(100))
#' right <- data.frame(id = 101:200, x = rnorm(100))
#' result <- greedy_couples(left, right, vars = "x")
#'
#' # Compare to optimal
#' result_opt <- match_couples(left, right, vars = "x")
#' result_greedy <- greedy_couples(left, right, vars = "x")
#' result_greedy$info$total_distance / result_opt$info$total_distance  # Quality ratio
#'
#' @export
greedy_couples <- function(left, right = NULL,
                           vars = NULL,
                           distance = "euclidean",
                           weights = NULL,
                           scale = FALSE,
                           auto_scale = FALSE,
                           max_distance = Inf,
                           calipers = NULL,
                           block_id = NULL,
                           ignore_blocks = FALSE,
                           require_full_matching = FALSE,
                           strategy = c("row_best", "sorted", "pq"),
                           return_unmatched = TRUE,
                           return_diagnostics = FALSE,
                           parallel = FALSE,
                           check_costs = TRUE) {

  strategy <- match.arg(strategy)

  # Check if left is a distance_object
  if (is_distance_object(left)) {
    return(greedy_couples_from_distance(
      left,
      max_distance = max_distance,
      calipers = calipers,
      check_costs = check_costs,
      ignore_blocks = ignore_blocks,
      require_full_matching = require_full_matching,
      strategy = strategy,
      return_unmatched = return_unmatched,
      return_diagnostics = return_diagnostics
    ))
  }

  # Standard path: left and right are datasets
  if (is.null(right)) {
    stop("When left is a dataset, right must be provided")
  }

  if (is.null(vars)) {
    stop("When left is a dataset, vars must be specified")
  }

  # Apply automatic preprocessing if requested
  if (auto_scale) {
    preproc <- preprocess_matching_vars(
      left, right, vars,
      auto_scale = TRUE,
      scale_method = if (identical(scale, FALSE)) "auto" else scale,
      check_health = TRUE,
      remove_problematic = TRUE,
      verbose = TRUE
    )

    # Update vars and scale based on preprocessing
    vars <- preproc$vars
    if (preproc$scaling_method != "none") {
      scale <- preproc$scaling_method
    }
  }

  # Validate inputs
  validate_matching_inputs(left, right, vars)
  weights <- validate_weights(weights, vars)
  calipers <- validate_calipers(calipers, vars)

  # Extract IDs
  left_ids <- extract_ids(left, "left")
  right_ids <- extract_ids(right, "right")

  # Store original row indices
  left$..row_idx <- seq_len(nrow(left))
  right$..row_idx <- seq_len(nrow(right))

  # Detect blocking
  block_info <- detect_blocking(left, right, block_id, ignore_blocks)

  if (block_info$use_blocking) {
    # Setup parallel processing if requested
    parallel_state <- setup_parallel(parallel)
    on.exit(restore_parallel(parallel_state), add = TRUE)

    # Blocked matching
    result <- greedy_couples_blocked(
      left, right, left_ids, right_ids,
      block_col = block_info$block_col,
      vars = vars, distance = distance, weights = weights, scale = scale,
      max_distance = max_distance, calipers = calipers,
      strategy = strategy,
      parallel = parallel_state$setup
    )
  } else {
    # Single matching
    result <- greedy_couples_single(
      left, right, left_ids, right_ids,
      vars = vars, distance = distance, weights = weights, scale = scale,
      max_distance = max_distance, calipers = calipers,
      strategy = strategy
    )
  }

  # Clean up temporary column
  left$..row_idx <- NULL
  right$..row_idx <- NULL

  # Check for full matching if required
  if (require_full_matching) {
    check_full_matching(result)
  }

  # Add metadata
  result$info$method <- "greedy"
  result$info$strategy <- strategy
  result$info$distance_metric <- distance
  result$info$scaled <- !identical(scale, FALSE)
  result$info$n_left <- nrow(left)
  result$info$n_right <- nrow(right)

  if (!return_unmatched) {
    result$unmatched <- NULL
  }

  if (!return_diagnostics) {
    # Keep n_blocks if present (for blocked matching)
    fields_to_keep <- c("method", "strategy", "n_matched", "total_distance")
    if (!is.null(result$info$n_blocks)) {
      fields_to_keep <- c(fields_to_keep, "n_blocks")
    }
    result$info <- result$info[fields_to_keep]
  }

  structure(result, class = c("matching_result", "couplr_result"))
}

#' Greedy Matching from Precomputed Distance Object
#'
#' Internal function to handle greedy matching when a distance_object is provided
#'
#' @keywords internal
greedy_couples_from_distance <- function(dist_obj,
                                        max_distance = Inf,
                                        calipers = NULL,
                                        ignore_blocks = FALSE,
                                        require_full_matching = FALSE,
                                        strategy = "row_best",
                                        return_unmatched = TRUE,
                                        return_diagnostics = FALSE) {

  # Extract from distance object
  cost_matrix <- dist_obj$cost_matrix
  left <- dist_obj$original_left
  right <- dist_obj$original_right
  left_ids <- dist_obj$left_ids
  right_ids <- dist_obj$right_ids

  # Apply additional constraints if specified
  if (!is.infinite(max_distance) || !is.null(calipers)) {
    cost_matrix <- apply_all_constraints(
      cost_matrix,
      left, right,
      dist_obj$metadata$vars,
      max_distance,
      calipers
    )
  }

  # Check for valid pairs
  if (!has_valid_pairs(cost_matrix)) {
    warning("No valid pairs found after applying constraints", call. = FALSE)

    return(structure(
      list(
        pairs = tibble::tibble(
          left_id = character(0),
          right_id = character(0),
          distance = numeric(0)
        ),
        unmatched = list(
          left = left_ids,
          right = right_ids
        ),
        info = list(
          method = "greedy",
          strategy = strategy,
          n_matched = 0,
          total_distance = 0,
          n_left = length(left_ids),
          n_right = length(right_ids)
        )
      ),
      class = c("matching_result", "couplr_result")
    ))
  }

  # Call greedy matching implementation
  greedy_result <- greedy_matching(cost_matrix, maximize = FALSE, strategy = strategy)

  # Extract matches
  matched_rows <- which(greedy_result$match > 0)

  if (length(matched_rows) == 0) {
    pairs <- tibble::tibble(
      left_id = character(0),
      right_id = character(0),
      distance = numeric(0)
    )
  } else {
    # Get actual distances
    distances <- numeric(length(matched_rows))
    for (i in seq_along(matched_rows)) {
      row <- matched_rows[i]
      col <- greedy_result$match[row]
      distances[i] <- cost_matrix[row, col]
    }

    # Filter out BIG_COST matches
    valid <- distances < BIG_COST
    matched_rows <- matched_rows[valid]
    distances <- distances[valid]

    pairs <- tibble::tibble(
      left_id = left_ids[matched_rows],
      right_id = right_ids[greedy_result$match[matched_rows]],
      distance = distances
    )
  }

  # Unmatched units
  matched_left <- matched_rows
  matched_right <- greedy_result$match[matched_rows]
  unmatched_left <- setdiff(seq_along(left_ids), matched_left)
  unmatched_right <- setdiff(seq_along(right_ids), matched_right)

  result <- list(
    pairs = pairs,
    unmatched = list(
      left = left_ids[unmatched_left],
      right = right_ids[unmatched_right]
    ),
    info = list(
      method = "greedy",
      strategy = strategy,
      n_matched = nrow(pairs),
      total_distance = sum(pairs$distance),
      distance_metric = dist_obj$metadata$distance,
      scaled = !identical(dist_obj$metadata$scale, FALSE),
      n_left = length(left_ids),
      n_right = length(right_ids)
    )
  )

  # Check for full matching if required
  if (require_full_matching) {
    check_full_matching(result)
  }

  if (!return_unmatched) {
    result$unmatched <- NULL
  }

  if (!return_diagnostics) {
    result$info <- result$info[c("method", "strategy", "n_matched", "total_distance")]
  }

  structure(result, class = c("matching_result", "couplr_result"))
}

#' Greedy matching without blocking
#'
#' @keywords internal
greedy_couples_single <- function(left, right, left_ids, right_ids,
                                  vars, distance, weights, scale,
                                  max_distance, calipers, strategy) {

  # Build cost matrix
  cost_matrix <- build_cost_matrix(left, right, vars, distance, weights, scale)

  # Apply constraints
  cost_matrix <- apply_all_constraints(cost_matrix, left, right, vars,
                                       max_distance, calipers)

  # Check for valid pairs
  if (!has_valid_pairs(cost_matrix)) {
    warning("No valid pairs found after applying constraints", call. = FALSE)

    return(list(
      pairs = tibble::tibble(
        left_id = character(0),
        right_id = character(0),
        distance = numeric(0)
      ),
      unmatched = list(
        left = left_ids,
        right = right_ids
      ),
      info = list(
        n_matched = 0,
        total_distance = 0
      )
    ))
  }

  # Solve using greedy algorithm
  greedy_result <- greedy_matching(cost_matrix, maximize = FALSE, strategy = strategy)

  # Extract matches
  matched_rows <- which(greedy_result$match > 0)

  if (length(matched_rows) == 0) {
    pairs <- tibble::tibble(
      left_id = character(0),
      right_id = character(0),
      distance = numeric(0)
    )
  } else {
    # Get actual distances
    distances <- numeric(length(matched_rows))
    for (i in seq_along(matched_rows)) {
      row <- matched_rows[i]
      col <- greedy_result$match[row]
      distances[i] <- cost_matrix[row, col]
    }

    # Filter out BIG_COST matches
    valid <- distances < BIG_COST
    matched_rows <- matched_rows[valid]
    distances <- distances[valid]

    pairs <- tibble::tibble(
      left_id = left_ids[matched_rows],
      right_id = right_ids[greedy_result$match[matched_rows]],
      distance = distances
    )

    # Add variable differences
    for (v in vars) {
      left_vals <- left[[v]][matched_rows]
      right_vals <- right[[v]][greedy_result$match[matched_rows]]
      pairs[[paste0(".", v, "_diff")]] <- left_vals - right_vals
    }
  }

  # Unmatched units
  matched_left <- matched_rows
  matched_right <- greedy_result$match[matched_rows]

  unmatched_left <- setdiff(seq_len(nrow(left)), matched_left)
  unmatched_right <- setdiff(seq_len(nrow(right)), matched_right)

  list(
    pairs = pairs,
    unmatched = list(
      left = left_ids[unmatched_left],
      right = right_ids[unmatched_right]
    ),
    info = list(
      n_matched = nrow(pairs),
      total_distance = sum(pairs$distance, na.rm = TRUE)
    )
  )
}

#' Greedy matching with blocking
#'
#' @keywords internal
greedy_couples_blocked <- function(left, right, left_ids, right_ids,
                                   block_col, vars, distance, weights, scale,
                                   max_distance, calipers, strategy,
                                   parallel = FALSE) {

  blocks <- unique(c(left[[block_col]], right[[block_col]]))

  # Use parallel processing if requested and available
  if (parallel && length(blocks) > 1) {
    result <- greedy_blocks_parallel(
      blocks, left, right, left_ids, right_ids,
      block_col, vars, distance, weights, scale,
      max_distance, calipers, strategy,
      parallel = TRUE
    )

    # Reorder columns to put block_id first
    if (nrow(result$pairs) > 0) {
      result$pairs <- dplyr::select(result$pairs, "block_id", dplyr::everything())
    }

    # Add additional summary statistics
    if (nrow(result$block_summary) > 0) {
      result$block_summary <- dplyr::mutate(
        result$block_summary,
        n_pairs = .data$n_matched,
        total_distance = .data$n_matched * .data$mean_distance,
        n_unmatched_left = .data$n_left - .data$n_matched,
        n_unmatched_right = .data$n_right - .data$n_matched
      )
    }

    return(list(
      pairs = result$pairs,
      unmatched = result$unmatched,
      info = list(
        n_matched = nrow(result$pairs),
        total_distance = sum(result$pairs$distance, na.rm = TRUE),
        blocked = TRUE,
        n_blocks = length(blocks),
        block_summary = result$block_summary
      )
    ))
  }

  # Sequential processing (original code)
  all_pairs <- list()
  all_unmatched_left <- character(0)
  all_unmatched_right <- character(0)
  block_summaries <- list()

  for (block in blocks) {
    left_block <- left[left[[block_col]] == block, ]
    right_block <- right[right[[block_col]] == block, ]

    if (nrow(left_block) == 0 || nrow(right_block) == 0) {
      # Skip blocks with no units on one side
      if (nrow(left_block) > 0) {
        block_left_ids <- left_ids[left[[block_col]] == block]
        all_unmatched_left <- c(all_unmatched_left, block_left_ids)
      }
      if (nrow(right_block) > 0) {
        block_right_ids <- right_ids[right[[block_col]] == block]
        all_unmatched_right <- c(all_unmatched_right, block_right_ids)
      }
      next
    }

    # Get IDs for this block
    block_left_ids <- left_ids[left[[block_col]] == block]
    block_right_ids <- right_ids[right[[block_col]] == block]

    # Match within block
    block_result <- greedy_couples_single(
      left_block, right_block, block_left_ids, block_right_ids,
      vars, distance, weights, scale,
      max_distance, calipers, strategy
    )

    # Add block_id column
    if (nrow(block_result$pairs) > 0) {
      block_result$pairs$block_id <- block
      all_pairs[[length(all_pairs) + 1]] <- block_result$pairs
    }

    # Accumulate unmatched
    all_unmatched_left <- c(all_unmatched_left, block_result$unmatched$left)
    all_unmatched_right <- c(all_unmatched_right, block_result$unmatched$right)

    # Block summary
    block_summaries[[length(block_summaries) + 1]] <- tibble::tibble(
      block_id = block,
      n_pairs = nrow(block_result$pairs),
      total_distance = sum(block_result$pairs$distance, na.rm = TRUE),
      mean_distance = mean(block_result$pairs$distance, na.rm = TRUE),
      n_unmatched_left = length(block_result$unmatched$left),
      n_unmatched_right = length(block_result$unmatched$right)
    )
  }

  # Combine results
  if (length(all_pairs) > 0) {
    pairs <- dplyr::bind_rows(all_pairs)
    pairs <- dplyr::select(pairs, "block_id", dplyr::everything())
  } else {
    pairs <- tibble::tibble(
      block_id = character(0),
      left_id = character(0),
      right_id = character(0),
      distance = numeric(0)
    )
  }

  block_summary_df <- if (length(block_summaries) > 0) {
    dplyr::bind_rows(block_summaries)
  } else {
    tibble::tibble(
      block_id = character(0),
      n_pairs = integer(0),
      total_distance = numeric(0),
      mean_distance = numeric(0),
      n_unmatched_left = integer(0),
      n_unmatched_right = integer(0)
    )
  }

  list(
    pairs = pairs,
    unmatched = list(
      left = all_unmatched_left,
      right = all_unmatched_right
    ),
    info = list(
      n_blocks = length(blocks),
      n_matched = nrow(pairs),
      total_distance = sum(pairs$distance, na.rm = TRUE),
      block_summary = block_summary_df
    )
  )
}

# ==============================================================================
# Print Methods
# ==============================================================================

#' Print method for matching results
#'
#' @param x A matching_result object
#' @param ... Additional arguments (ignored)
#'
#' @export
#' @method print matching_result
print.matching_result <- function(x, ...) {
  cat("Matching Result\n")
  cat("===============\n\n")

  cat("Method:", x$info$method, "\n")
  if (!is.null(x$info$strategy)) {
    cat("Strategy:", x$info$strategy, "\n")
  }
  cat("Pairs matched:", x$info$n_matched, "\n")

  if (!is.null(x$info$n_blocks) && x$info$n_blocks > 1) {
    cat("Blocks:", x$info$n_blocks, "\n")
  }

  if (!is.null(x$unmatched)) {
    cat("Unmatched (left):", length(x$unmatched$left), "\n")
    cat("Unmatched (right):", length(x$unmatched$right), "\n")
  }

  cat("Total distance:", sprintf("%.4f", x$info$total_distance), "\n")

  if (nrow(x$pairs) > 0) {
    cat("\nMatched pairs:\n")
    print(x$pairs, n = 10)
  }

  invisible(x)
}
