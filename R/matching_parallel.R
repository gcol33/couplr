# ==============================================================================
# Parallel Matching Helpers
# ==============================================================================

#' Check if parallel processing is available
#'
#' @return Logical indicating if future package is available
#' @keywords internal
can_parallelize <- function() {
  requireNamespace("future", quietly = TRUE) &&
    requireNamespace("future.apply", quietly = TRUE)
}

#' Setup parallel processing with future
#'
#' @param parallel Logical or plan specification
#' @param n_workers Number of workers (NULL for auto-detect)
#' @return List with original plan and whether we set up parallelization
#' @keywords internal
setup_parallel <- function(parallel = FALSE, n_workers = NULL) {
  # Return early if parallel is FALSE
  if (isFALSE(parallel)) {
    return(list(
      setup = FALSE,
      original_plan = NULL
    ))
  }

  # Check if future packages are available
  if (!can_parallelize()) {
    if (isTRUE(parallel)) {
      warning(
        "Parallel processing requested but 'future' and/or 'future.apply' ",
        "packages are not installed. Install with:\n",
        "  install.packages(c('future', 'future.apply'))\n",
        "Falling back to sequential processing.",
        call. = FALSE
      )
    }
    return(list(
      setup = FALSE,
      original_plan = NULL
    ))
  }

  # Store original plan
  original_plan <- future::plan()

  # Setup parallel plan
  if (isTRUE(parallel)) {
    # Auto-select plan based on platform
    if (is.null(n_workers)) {
      n_workers <- future::availableCores() - 1  # Leave one core free
      n_workers <- max(1, n_workers)  # At least 1 worker
    }

    # Use multisession (works on all platforms)
    future::plan(future::multisession, workers = n_workers)

    message(
      "Parallel processing enabled with ", n_workers, " workers\n",
      "Tip: Set plan explicitly with future::plan() for more control"
    )
  } else if (is.character(parallel)) {
    # Allow user to specify plan by name
    tryCatch({
      future::plan(parallel)
    }, error = function(e) {
      warning(
        "Could not set parallel plan '", parallel, "': ", e$message, "\n",
        "Falling back to sequential processing.",
        call. = FALSE
      )
      return(list(setup = FALSE, original_plan = NULL))
    })
  }

  list(
    setup = TRUE,
    original_plan = original_plan
  )
}

#' Restore original parallel plan
#'
#' @param parallel_state State from setup_parallel()
#' @keywords internal
restore_parallel <- function(parallel_state) {
  if (parallel_state$setup && !is.null(parallel_state$original_plan)) {
    future::plan(parallel_state$original_plan)
  }
}

#' Parallel lapply using future
#'
#' @param X Vector to iterate over
#' @param FUN Function to apply
#' @param ... Additional arguments to FUN
#' @param parallel Whether parallel processing is enabled
#' @return List of results
#' @keywords internal
parallel_lapply <- function(X, FUN, ..., parallel = FALSE) {
  if (parallel && can_parallelize()) {
    # Use future.apply for parallel execution
    future.apply::future_lapply(X, FUN, ..., future.seed = TRUE)
  } else {
    # Fall back to regular lapply
    lapply(X, FUN, ...)
  }
}

#' Match blocks in parallel
#'
#' @param blocks Vector of block IDs
#' @param left Left dataset with block_col
#' @param right Right dataset with block_col
#' @param left_ids IDs from left
#' @param right_ids IDs from right
#' @param block_col Name of blocking column
#' @param vars Variables for matching
#' @param distance Distance metric
#' @param weights Variable weights
#' @param scale Scaling method
#' @param max_distance Maximum distance
#' @param calipers Caliper constraints
#' @param method LAP method
#' @param parallel Whether to use parallel processing
#' @return List with combined results from all blocks
#' @keywords internal
match_blocks_parallel <- function(blocks, left, right, left_ids, right_ids,
                                  block_col, vars, distance, weights, scale,
                                  max_distance, calipers, method,
                                  parallel = FALSE) {

  # Function to match a single block
  match_one_block <- function(block) {
    left_block <- left[left[[block_col]] == block, ]
    right_block <- right[right[[block_col]] == block, ]

    # Handle empty blocks
    if (nrow(left_block) == 0 || nrow(right_block) == 0) {
      block_left_ids <- if (nrow(left_block) > 0) {
        left_ids[left[[block_col]] == block]
      } else {
        character(0)
      }

      block_right_ids <- if (nrow(right_block) > 0) {
        right_ids[right[[block_col]] == block]
      } else {
        character(0)
      }

      return(list(
        pairs = tibble::tibble(
          left_id = character(0),
          right_id = character(0),
          distance = numeric(0),
          block_id = character(0)
        ),
        unmatched_left = block_left_ids,
        unmatched_right = block_right_ids,
        summary = tibble::tibble(
          block_id = as.character(block),
          n_left = nrow(left_block),
          n_right = nrow(right_block),
          n_matched = 0L,
          mean_distance = NA_real_
        )
      ))
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

    # Add block_id column to pairs
    if (nrow(block_result$pairs) > 0) {
      block_result$pairs$block_id <- as.character(block)
    }

    # Create summary
    summary <- tibble::tibble(
      block_id = as.character(block),
      n_left = nrow(left_block),
      n_right = nrow(right_block),
      n_matched = nrow(block_result$pairs),
      mean_distance = if (nrow(block_result$pairs) > 0) {
        mean(block_result$pairs$distance)
      } else {
        NA_real_
      }
    )

    list(
      pairs = block_result$pairs,
      unmatched_left = block_result$unmatched$left,
      unmatched_right = block_result$unmatched$right,
      summary = summary
    )
  }

  # Process blocks (in parallel if enabled)
  block_results <- parallel_lapply(
    blocks,
    match_one_block,
    parallel = parallel
  )

  # Combine results
  all_pairs <- do.call(rbind, lapply(block_results, function(x) x$pairs))
  all_unmatched_left <- unlist(lapply(block_results, function(x) x$unmatched_left))
  all_unmatched_right <- unlist(lapply(block_results, function(x) x$unmatched_right))
  all_summaries <- do.call(rbind, lapply(block_results, function(x) x$summary))

  # Handle empty results
  if (is.null(all_pairs) || nrow(all_pairs) == 0) {
    all_pairs <- tibble::tibble(
      left_id = character(0),
      right_id = character(0),
      distance = numeric(0),
      block_id = character(0)
    )
  }

  list(
    pairs = all_pairs,
    unmatched = list(
      left = as.character(all_unmatched_left),
      right = as.character(all_unmatched_right)
    ),
    block_summary = all_summaries
  )
}

#' Greedy match blocks in parallel
#'
#' @param blocks Vector of block IDs
#' @param left Left dataset with block_col
#' @param right Right dataset with block_col
#' @param left_ids IDs from left
#' @param right_ids IDs from right
#' @param block_col Name of blocking column
#' @param vars Variables for matching
#' @param distance Distance metric
#' @param weights Variable weights
#' @param scale Scaling method
#' @param max_distance Maximum distance
#' @param calipers Caliper constraints
#' @param strategy Greedy strategy
#' @param parallel Whether to use parallel processing
#' @return List with combined results from all blocks
#' @keywords internal
greedy_blocks_parallel <- function(blocks, left, right, left_ids, right_ids,
                                   block_col, vars, distance, weights, scale,
                                   max_distance, calipers, strategy,
                                   parallel = FALSE) {

  # Function to greedy match a single block
  greedy_one_block <- function(block) {
    left_block <- left[left[[block_col]] == block, ]
    right_block <- right[right[[block_col]] == block, ]

    # Handle empty blocks
    if (nrow(left_block) == 0 || nrow(right_block) == 0) {
      block_left_ids <- if (nrow(left_block) > 0) {
        left_ids[left[[block_col]] == block]
      } else {
        character(0)
      }

      block_right_ids <- if (nrow(right_block) > 0) {
        right_ids[right[[block_col]] == block]
      } else {
        character(0)
      }

      return(list(
        pairs = tibble::tibble(
          left_id = character(0),
          right_id = character(0),
          distance = numeric(0),
          block_id = character(0)
        ),
        unmatched_left = block_left_ids,
        unmatched_right = block_right_ids,
        summary = tibble::tibble(
          block_id = as.character(block),
          n_left = nrow(left_block),
          n_right = nrow(right_block),
          n_matched = 0L,
          mean_distance = NA_real_
        )
      ))
    }

    # Get IDs for this block
    block_left_ids <- left_ids[left[[block_col]] == block]
    block_right_ids <- right_ids[right[[block_col]] == block]

    # Greedy match within block
    block_result <- greedy_couples_single(
      left_block, right_block, block_left_ids, block_right_ids,
      vars, distance, weights, scale,
      max_distance, calipers, strategy
    )

    # Add block_id column to pairs
    if (nrow(block_result$pairs) > 0) {
      block_result$pairs$block_id <- as.character(block)
    }

    # Create summary
    summary <- tibble::tibble(
      block_id = as.character(block),
      n_left = nrow(left_block),
      n_right = nrow(right_block),
      n_matched = nrow(block_result$pairs),
      mean_distance = if (nrow(block_result$pairs) > 0) {
        mean(block_result$pairs$distance)
      } else {
        NA_real_
      }
    )

    list(
      pairs = block_result$pairs,
      unmatched_left = block_result$unmatched$left,
      unmatched_right = block_result$unmatched$right,
      summary = summary
    )
  }

  # Process blocks (in parallel if enabled)
  block_results <- parallel_lapply(
    blocks,
    greedy_one_block,
    parallel = parallel
  )

  # Combine results
  all_pairs <- do.call(rbind, lapply(block_results, function(x) x$pairs))
  all_unmatched_left <- unlist(lapply(block_results, function(x) x$unmatched_left))
  all_unmatched_right <- unlist(lapply(block_results, function(x) x$unmatched_right))
  all_summaries <- do.call(rbind, lapply(block_results, function(x) x$summary))

  # Handle empty results
  if (is.null(all_pairs) || nrow(all_pairs) == 0) {
    all_pairs <- tibble::tibble(
      left_id = character(0),
      right_id = character(0),
      distance = numeric(0),
      block_id = character(0)
    )
  }

  list(
    pairs = all_pairs,
    unmatched = list(
      left = as.character(all_unmatched_left),
      right = as.character(all_unmatched_right)
    ),
    block_summary = all_summaries
  )
}