# ==============================================================================
# Matching Core - match_couples() and greedy_couples()
# ==============================================================================

# ==============================================================================
# Shared Internal Implementations
# ==============================================================================

#' Shared single matching implementation
#'
#' Core logic for both optimal (LAP) and greedy matching without blocking.
#' Called by match_couples_single() and greedy_couples_single().
#'
#' @param solver_fn Solver function (assignment or greedy_matching)
#' @param solver_params Named list of extra args passed to solver_fn
#' @param check_costs If TRUE, run check_cost_distribution before solving
#' @param strict_no_pairs If TRUE, call err_no_valid_pairs (stops); else warn
#' @return List with pairs tibble, unmatched list, and info list.
#' @keywords internal
.couples_single <- function(left, right, left_ids, right_ids,
                            vars, distance, weights, scale,
                            max_distance, calipers,
                            solver_fn, solver_params = list(),
                            check_costs = FALSE,
                            strict_no_pairs = FALSE,
                            replace = FALSE,
                            ratio = 1L) {

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
    if (strict_no_pairs) {
      err_no_valid_pairs("No valid pairs after applying constraints")
    } else {
      warning("No valid pairs found after applying constraints", call. = FALSE)
    }

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

  # --- Replacement matching ---
  if (replace) {
    return(.couples_replace(
      cost_matrix, left, right, left_ids, right_ids, vars, ratio
    ))
  }

  # --- k:1 matching (ratio > 1, without replacement) ---
  if (ratio > 1L) {
    return(.couples_ratio(
      cost_matrix, left, right, left_ids, right_ids, vars, ratio,
      solver_fn, solver_params
    ))
  }

  # --- Standard 1:1 matching ---
  # Solve
  solver_result <- do.call(solver_fn, c(list(cost_matrix, maximize = FALSE),
                                        solver_params))

  # Extract matches
  matched_rows <- which(solver_result$match > 0)

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
      col <- solver_result$match[row]
      distances[i] <- cost_matrix[row, col]
    }

    # Filter out BIG_COST matches
    valid <- distances < BIG_COST
    matched_rows <- matched_rows[valid]
    distances <- distances[valid]

    pairs <- tibble::tibble(
      left_id = left_ids[matched_rows],
      right_id = right_ids[solver_result$match[matched_rows]],
      distance = distances
    )

    # Add variable differences
    for (v in vars) {
      left_vals <- left[[v]][matched_rows]
      right_vals <- right[[v]][solver_result$match[matched_rows]]
      pairs[[paste0(".", v, "_diff")]] <- left_vals - right_vals
    }
  }

  # Unmatched units
  matched_left <- matched_rows
  matched_right <- solver_result$match[matched_rows]

  unmatched_left <- setdiff(seq_len(nrow(left)), matched_left)
  unmatched_right <- setdiff(seq_len(nrow(right)), matched_right)

  list(
    pairs = pairs,
    unmatched = list(
      left = left_ids[unmatched_left],
      right = right_ids[unmatched_right]
    ),
    info = list(
      solver = solver_result$method_used,
      n_matched = nrow(pairs),
      total_distance = sum(pairs$distance, na.rm = TRUE)
    )
  )
}

#' Replacement matching: each left picks its best right independently
#'
#' @return List with pairs tibble, unmatched list, and info list.
#' @keywords internal
.couples_replace <- function(cost_matrix, left, right,
                             left_ids, right_ids, vars, ratio = 1L) {
  n_left <- nrow(cost_matrix)
  n_right <- ncol(cost_matrix)
  all_pairs <- list()

  for (i in seq_len(n_left)) {
    row_costs <- cost_matrix[i, ]
    # Find the k best right units
    k <- min(ratio, n_right)
    ordered_cols <- order(row_costs)[seq_len(k)]
    ordered_dists <- row_costs[ordered_cols]

    # Keep only valid (< BIG_COST)
    valid <- ordered_dists < BIG_COST
    if (any(valid)) {
      cols <- ordered_cols[valid]
      dists <- ordered_dists[valid]

      pair_df <- tibble::tibble(
        left_id = rep(left_ids[i], length(cols)),
        right_id = right_ids[cols],
        distance = dists
      )

      # Add variable differences
      for (v in vars) {
        pair_df[[paste0(".", v, "_diff")]] <- left[[v]][i] - right[[v]][cols]
      }

      all_pairs[[length(all_pairs) + 1]] <- pair_df
    }
  }

  if (length(all_pairs) > 0) {
    pairs <- dplyr::bind_rows(all_pairs)
  } else {
    pairs <- tibble::tibble(
      left_id = character(0), right_id = character(0), distance = numeric(0)
    )
  }

  # Unmatched: left units with no valid match
  matched_left_ids <- unique(pairs$left_id)
  matched_right_ids <- unique(pairs$right_id)

  list(
    pairs = pairs,
    unmatched = list(
      left = setdiff(left_ids, matched_left_ids),
      right = setdiff(right_ids, matched_right_ids)
    ),
    info = list(
      n_matched = nrow(pairs),
      total_distance = sum(pairs$distance, na.rm = TRUE),
      replace = TRUE,
      ratio = ratio
    )
  )
}

#' k:1 matching via cost matrix expansion
#'
#' Replicates left-side rows k times so each left unit can match up to k
#' different right units. Solves as standard LAP, then maps expanded rows
#' back to original left indices.
#'
#' @return List with pairs tibble, unmatched list, and info list.
#' @keywords internal
.couples_ratio <- function(cost_matrix, left, right,
                           left_ids, right_ids, vars, ratio,
                           solver_fn, solver_params) {
  n_left <- nrow(cost_matrix)
  n_right <- ncol(cost_matrix)

  # Expand: replicate each left row `ratio` times
  row_map <- rep(seq_len(n_left), each = ratio)
  expanded_cost <- cost_matrix[row_map, , drop = FALSE]

  # Solve the expanded problem
  solver_result <- do.call(solver_fn, c(list(expanded_cost, maximize = FALSE),
                                        solver_params))

  # Extract matches and map back to original left indices
  matched_exp_rows <- which(solver_result$match > 0)

  if (length(matched_exp_rows) == 0) {
    pairs <- tibble::tibble(
      left_id = character(0), right_id = character(0), distance = numeric(0)
    )
    original_rows <- integer(0)
    matched_cols <- integer(0)
  } else {
    original_rows <- row_map[matched_exp_rows]
    matched_cols <- solver_result$match[matched_exp_rows]

    # Get distances from original cost matrix
    distances <- vapply(seq_along(matched_exp_rows), function(i) {
      cost_matrix[original_rows[i], matched_cols[i]]
    }, numeric(1))

    # Filter out BIG_COST
    valid <- distances < BIG_COST
    original_rows <- original_rows[valid]
    matched_cols <- matched_cols[valid]
    distances <- distances[valid]

    pairs <- tibble::tibble(
      left_id = left_ids[original_rows],
      right_id = right_ids[matched_cols],
      distance = distances
    )

    for (v in vars) {
      pairs[[paste0(".", v, "_diff")]] <- left[[v]][original_rows] -
        right[[v]][matched_cols]
    }
  }

  # Unmatched
  unmatched_left <- setdiff(seq_len(n_left), unique(original_rows))
  unmatched_right <- setdiff(seq_len(n_right), unique(matched_cols))

  list(
    pairs = pairs,
    unmatched = list(
      left = left_ids[unmatched_left],
      right = right_ids[unmatched_right]
    ),
    info = list(
      solver = solver_result$method_used,
      n_matched = nrow(pairs),
      total_distance = sum(pairs$distance, na.rm = TRUE),
      ratio = ratio
    )
  )
}

#' Shared matching from precomputed distance object
#'
#' Core logic for both optimal (LAP) and greedy matching from distance objects.
#' Called by match_couples_from_distance() and greedy_couples_from_distance().
#'
#' @param solver_fn Solver function (assignment or greedy_matching)
#' @param solver_params Named list of extra args passed to solver_fn
#' @param check_costs If TRUE, run check_cost_distribution before solving
#' @param strict_no_pairs If TRUE, call err_no_valid_pairs (stops); else warn
#' @param method_label String for info$method (e.g., "from_distance_object")
#' @param extra_info Named list of extra fields to add to info
#' @return A matching_result object with pairs, info, and optional diagnostics.
#' @keywords internal
.couples_from_distance <- function(dist_obj,
                                   max_distance = Inf,
                                   calipers = NULL,
                                   ignore_blocks = FALSE,
                                   require_full_matching = FALSE,
                                   return_unmatched = TRUE,
                                   return_diagnostics = FALSE,
                                   solver_fn, solver_params = list(),
                                   check_costs = FALSE,
                                   strict_no_pairs = FALSE,
                                   method_label = "from_distance_object",
                                   extra_info = list(),
                                   diagnostics_fields = c("method", "n_matched",
                                                          "total_distance")) {

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
    if (strict_no_pairs) {
      err_no_valid_pairs("No valid pairs after applying constraints")
    } else {
      warning("No valid pairs found after applying constraints", call. = FALSE)
    }

    info <- c(
      list(
        method = method_label,
        n_matched = 0,
        total_distance = 0,
        n_left = length(left_ids),
        n_right = length(right_ids)
      ),
      extra_info
    )

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
        info = info
      ),
      class = c("matching_result", "couplr_result")
    ))
  }

  # Solve
  solver_result <- do.call(solver_fn, c(list(cost_matrix, maximize = FALSE),
                                        solver_params))

  # Extract matches
  matched_rows <- which(solver_result$match > 0)

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
      col <- solver_result$match[row]
      distances[i] <- cost_matrix[row, col]
    }

    # Filter out BIG_COST matches
    valid <- distances < BIG_COST
    matched_rows <- matched_rows[valid]
    distances <- distances[valid]

    pairs <- tibble::tibble(
      left_id = left_ids[matched_rows],
      right_id = right_ids[solver_result$match[matched_rows]],
      distance = distances
    )
  }

  # Unmatched units
  matched_left <- matched_rows
  matched_right <- solver_result$match[matched_rows]
  unmatched_left <- setdiff(seq_along(left_ids), matched_left)
  unmatched_right <- setdiff(seq_along(right_ids), matched_right)

  info <- c(
    list(
      method = method_label,
      solver = solver_result$method_used,
      n_matched = nrow(pairs),
      total_distance = sum(pairs$distance),
      distance_metric = dist_obj$metadata$distance,
      scaled = !identical(dist_obj$metadata$scale, FALSE),
      n_left = length(left_ids),
      n_right = length(right_ids)
    ),
    extra_info
  )

  result <- list(
    pairs = pairs,
    unmatched = list(
      left = left_ids[unmatched_left],
      right = right_ids[unmatched_right]
    ),
    info = info
  )

  # Check for full matching if required
  if (require_full_matching) {
    check_full_matching(result)
  }

  if (!return_unmatched) {
    result$unmatched <- NULL
  }

  if (!return_diagnostics) {
    result$info <- result$info[diagnostics_fields]
  }

  structure(result, class = c("matching_result", "couplr_result"))
}

#' Shared blocked matching implementation
#'
#' Core logic for both optimal (LAP) and greedy blocked matching.
#' Called by match_couples_blocked() and greedy_couples_blocked().
#'
#' @param solver_fn Solver function (assignment or greedy_matching)
#' @param solver_params Named list of extra args passed to solver_fn
#' @param check_costs If TRUE, passed through to .couples_single
#' @param strict_no_pairs If TRUE, passed through to .couples_single
#' @return List with pairs tibble, unmatched list, and info list.
#' @keywords internal
.couples_blocked <- function(left, right, left_ids, right_ids,
                             block_col, vars, distance, weights, scale,
                             max_distance, calipers,
                             solver_fn, solver_params = list(),
                             check_costs = FALSE,
                             strict_no_pairs = FALSE,
                             parallel = FALSE,
                             replace = FALSE,
                             ratio = 1L) {

  blocks <- unique(c(left[[block_col]], right[[block_col]]))

  # Use parallel processing if requested and available
  if (parallel && length(blocks) > 1) {
    result <- .blocks_parallel(
      blocks, left, right, left_ids, right_ids,
      block_col, vars, distance, weights, scale,
      max_distance, calipers,
      solver_fn = solver_fn, solver_params = solver_params,
      check_costs = check_costs, strict_no_pairs = strict_no_pairs,
      parallel = TRUE,
      replace = replace, ratio = ratio
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

  # Sequential processing
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
    block_result <- .couples_single(
      left_block, right_block, block_left_ids, block_right_ids,
      vars, distance, weights, scale,
      max_distance, calipers,
      solver_fn = solver_fn, solver_params = solver_params,
      check_costs = check_costs, strict_no_pairs = strict_no_pairs,
      replace = replace, ratio = ratio
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
      solver = solver_params$method,
      n_blocks = length(blocks),
      n_matched = nrow(pairs),
      total_distance = sum(pairs$distance, na.rm = TRUE),
      block_summary = block_summary_df
    )
  )
}

# ==============================================================================
# Exported Matching Functions
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
#' @param replace If TRUE, allow matching with replacement (same right unit
#'   can be matched to multiple left units). Default: FALSE.
#' @param ratio Integer, number of right units to match per left unit.
#'   Default: 1 (one-to-one matching). For k:1 matching, set ratio = k.
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
                          replace = FALSE,
                          ratio = 1L,
                          check_costs = TRUE) {

  # Validate replace and ratio
  if (!is.logical(replace) || length(replace) != 1) {
    stop("replace must be TRUE or FALSE", call. = FALSE)
  }
  ratio <- as.integer(ratio)
  if (length(ratio) != 1 || is.na(ratio) || ratio < 1L) {
    stop("ratio must be a positive integer", call. = FALSE)
  }

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
      parallel = parallel_state$setup,
      replace = replace, ratio = ratio
    )
  } else {
    # Single matching
    result <- match_couples_single(
      left, right, left_ids, right_ids,
      vars = vars, distance = distance, weights = weights, scale = scale,
      max_distance = max_distance, calipers = calipers,
      method = method,
      check_costs = check_costs,
      replace = replace, ratio = ratio
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
  if (replace) result$info$replace <- TRUE
  if (ratio > 1L) result$info$ratio <- ratio

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
#' @return A matching_result object with pairs, info, and optional diagnostics.
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
  .couples_from_distance(
    dist_obj,
    max_distance = max_distance,
    calipers = calipers,
    ignore_blocks = ignore_blocks,
    require_full_matching = require_full_matching,
    return_unmatched = return_unmatched,
    return_diagnostics = return_diagnostics,
    solver_fn = assignment,
    solver_params = list(method = method),
    check_costs = check_costs,
    strict_no_pairs = TRUE,
    method_label = "from_distance_object",
    diagnostics_fields = c("method", "n_matched", "total_distance")
  )
}

#' Match without blocking (single problem)
#'
#' @return List with pairs tibble and matching info.
#' @keywords internal
match_couples_single <- function(left, right, left_ids, right_ids,
                                 vars, distance, weights, scale,
                                 max_distance, calipers, method,
                                 check_costs = TRUE,
                                 replace = FALSE, ratio = 1L) {
  .couples_single(
    left, right, left_ids, right_ids,
    vars, distance, weights, scale,
    max_distance, calipers,
    solver_fn = assignment,
    solver_params = list(method = method),
    check_costs = check_costs,
    strict_no_pairs = TRUE,
    replace = replace, ratio = ratio
  )
}

#' Match with blocking (multiple problems)
#'
#' @return List with pairs tibble and matching info.
#' @keywords internal
match_couples_blocked <- function(left, right, left_ids, right_ids,
                                  block_col, vars, distance, weights, scale,
                                  max_distance, calipers, method,
                                  parallel = FALSE,
                                  replace = FALSE, ratio = 1L) {
  .couples_blocked(
    left, right, left_ids, right_ids,
    block_col, vars, distance, weights, scale,
    max_distance, calipers,
    solver_fn = assignment,
    solver_params = list(method = method),
    check_costs = FALSE,
    strict_no_pairs = TRUE,
    parallel = parallel,
    replace = replace, ratio = ratio
  )
}

#' Detect and validate blocking
#'
#' @return List with use_blocking (logical) and block_col (character or NULL).
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
#' @return No return value; throws error if unmatched units exist.
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
                           replace = FALSE,
                           ratio = 1L,
                           check_costs = TRUE) {

  strategy <- match.arg(strategy)

  # Validate replace and ratio
  if (!is.logical(replace) || length(replace) != 1) {
    stop("replace must be TRUE or FALSE", call. = FALSE)
  }
  ratio <- as.integer(ratio)
  if (length(ratio) != 1 || is.na(ratio) || ratio < 1L) {
    stop("ratio must be a positive integer", call. = FALSE)
  }

  # Check if left is a distance_object
  if (is_distance_object(left)) {
    return(greedy_couples_from_distance(
      left,
      max_distance = max_distance,
      calipers = calipers,
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
      parallel = parallel_state$setup,
      replace = replace, ratio = ratio
    )
  } else {
    # Single matching
    result <- greedy_couples_single(
      left, right, left_ids, right_ids,
      vars = vars, distance = distance, weights = weights, scale = scale,
      max_distance = max_distance, calipers = calipers,
      strategy = strategy,
      replace = replace, ratio = ratio
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
  if (replace) result$info$replace <- TRUE
  if (ratio > 1L) result$info$ratio <- ratio

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
#' @return A matching_result object with pairs, info, and optional diagnostics.
#' @keywords internal
greedy_couples_from_distance <- function(dist_obj,
                                        max_distance = Inf,
                                        calipers = NULL,
                                        ignore_blocks = FALSE,
                                        require_full_matching = FALSE,
                                        strategy = "row_best",
                                        return_unmatched = TRUE,
                                        return_diagnostics = FALSE) {
  .couples_from_distance(
    dist_obj,
    max_distance = max_distance,
    calipers = calipers,
    ignore_blocks = ignore_blocks,
    require_full_matching = require_full_matching,
    return_unmatched = return_unmatched,
    return_diagnostics = return_diagnostics,
    solver_fn = greedy_matching,
    solver_params = list(strategy = strategy),
    check_costs = FALSE,
    strict_no_pairs = FALSE,
    method_label = "greedy",
    extra_info = list(strategy = strategy),
    diagnostics_fields = c("method", "strategy", "n_matched", "total_distance")
  )
}

#' Greedy matching without blocking
#'
#' @return List with pairs tibble and matching info.
#' @keywords internal
greedy_couples_single <- function(left, right, left_ids, right_ids,
                                  vars, distance, weights, scale,
                                  max_distance, calipers, strategy,
                                  replace = FALSE, ratio = 1L) {
  .couples_single(
    left, right, left_ids, right_ids,
    vars, distance, weights, scale,
    max_distance, calipers,
    solver_fn = greedy_matching,
    solver_params = list(strategy = strategy),
    check_costs = FALSE,
    strict_no_pairs = FALSE,
    replace = replace, ratio = ratio
  )
}

#' Greedy matching with blocking
#'
#' @return List with pairs tibble and matching info.
#' @keywords internal
greedy_couples_blocked <- function(left, right, left_ids, right_ids,
                                   block_col, vars, distance, weights, scale,
                                   max_distance, calipers, strategy,
                                   parallel = FALSE,
                                   replace = FALSE, ratio = 1L) {
  .couples_blocked(
    left, right, left_ids, right_ids,
    block_col, vars, distance, weights, scale,
    max_distance, calipers,
    solver_fn = greedy_matching,
    solver_params = list(strategy = strategy),
    check_costs = FALSE,
    strict_no_pairs = FALSE,
    parallel = parallel,
    replace = replace, ratio = ratio
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
#' @return Invisibly returns the input object `x`.
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

#' Summary method for matching results
#'
#' @param object A matching_result object
#' @param ... Additional arguments (ignored)
#'
#' @return A list containing summary statistics (invisibly)
#' @export
#' @method summary matching_result
summary.matching_result <- function(object, ...) {
  n_matched <- object$info$n_matched
  total_dist <- object$info$total_distance
  mean_dist <- if (n_matched > 0) total_dist / n_matched else NA_real_
  distances <- object$pairs$distance

  # Match rate: proportion of the smaller side that was matched
  n_left <- object$info$n_left %||% NA_integer_
  n_right <- object$info$n_right %||% NA_integer_
  match_rate <- if (!is.na(n_left) && !is.na(n_right) && min(n_left, n_right) > 0) {
    n_matched / min(n_left, n_right)
  } else {
    NA_real_
  }

  # Distance percentiles
  distance_percentiles <- if (length(distances) > 0) {
    stats::quantile(distances, c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95),
                    na.rm = TRUE)
  } else {
    NULL
  }

  # Build summary list
  out <- list(
    method = object$info$method,
    strategy = object$info$strategy,
    n_matched = n_matched,
    n_blocks = object$info$n_blocks %||% 1L,
    total_distance = total_dist,
    mean_distance = mean_dist,
    match_rate = match_rate,
    distance_stats = if (length(distances) > 0) {
      list(
        min = min(distances, na.rm = TRUE),
        q1 = stats::quantile(distances, 0.25, na.rm = TRUE),
        median = stats::median(distances, na.rm = TRUE),
        q3 = stats::quantile(distances, 0.75, na.rm = TRUE),
        max = max(distances, na.rm = TRUE),
        sd = stats::sd(distances, na.rm = TRUE)
      )
    } else NULL,
    distance_percentiles = distance_percentiles,
    n_unmatched_left = if (!is.null(object$unmatched)) length(object$unmatched$left) else NA_integer_,
    n_unmatched_right = if (!is.null(object$unmatched)) length(object$unmatched$right) else NA_integer_,
    replace = object$info$replace %||% FALSE,
    ratio = object$info$ratio %||% 1L
  )

  class(out) <- "summary.matching_result"
  out
}

#' @export
print.summary.matching_result <- function(x, ...) {
  cat("Matching Result Summary\n")
  cat("=======================\n\n")

  cat("Method:", x$method)
  if (!is.null(x$strategy)) cat(" (", x$strategy, ")", sep = "")
  cat("\n")

  cat("Pairs matched:", x$n_matched, "\n")
  if (x$n_blocks > 1) cat("Blocks:", x$n_blocks, "\n")

  if (!is.na(x$match_rate)) {
    cat("Match rate:", sprintf("%.1f%%", x$match_rate * 100), "\n")
  }

  if (isTRUE(x$replace)) cat("Replacement: yes\n")
  if (!is.null(x$ratio) && x$ratio > 1) cat("Ratio:", x$ratio, ":1\n")

  if (!is.na(x$n_unmatched_left)) {
    cat("Unmatched: ", x$n_unmatched_left, " left, ",
        x$n_unmatched_right, " right\n", sep = "")
  }

  cat("\nDistance Statistics:\n")
  cat("  Total:", sprintf("%.4f", x$total_distance), "\n")
  cat("  Mean:", sprintf("%.4f", x$mean_distance), "\n")

  if (!is.null(x$distance_stats)) {
    ds <- x$distance_stats
    cat("  Min:", sprintf("%.4f", ds$min), "\n")
    cat("  Q1:", sprintf("%.4f", ds$q1), "\n")
    cat("  Median:", sprintf("%.4f", ds$median), "\n")
    cat("  Q3:", sprintf("%.4f", ds$q3), "\n")
    cat("  Max:", sprintf("%.4f", ds$max), "\n")
    cat("  SD:", sprintf("%.4f", ds$sd), "\n")
  }

  if (!is.null(x$distance_percentiles)) {
    cat("\nDistance Percentiles:\n")
    pct_names <- names(x$distance_percentiles)
    for (i in seq_along(x$distance_percentiles)) {
      cat(sprintf("  %s: %.4f\n", pct_names[i], x$distance_percentiles[i]))
    }
  }

  invisible(x)
}

#' Plot method for matching results
#'
#' Produces a histogram of pairwise distances from a matching result.
#'
#' @param x A matching_result object
#' @param type Type of plot: "histogram" (default), "density", or "ecdf"
#' @param ... Additional arguments passed to plotting functions
#'
#' @return The matching_result object (invisibly)
#' @export
#' @method plot matching_result
plot.matching_result <- function(x, type = c("histogram", "density", "ecdf"), ...) {
  type <- match.arg(type)
  distances <- x$pairs$distance

  if (length(distances) == 0) {
    message("No matched pairs to plot")
    return(invisible(x))
  }

  main_title <- paste0("Matching Distances (n=", length(distances), ")")


  switch(type,
    histogram = {
      graphics::hist(distances,
                     main = main_title,
                     xlab = "Distance",
                     col = "steelblue",
                     border = "white",
                     ...)
    },
    density = {
      d <- stats::density(distances)
      graphics::plot(d,
                     main = main_title,
                     xlab = "Distance",
                     ...)
      graphics::polygon(d, col = "steelblue", border = "steelblue")
    },
    ecdf = {
      graphics::plot(stats::ecdf(distances),
                     main = main_title,
                     xlab = "Distance",
                     ylab = "Cumulative Proportion",
                     ...)
    }
  )

  invisible(x)
}
