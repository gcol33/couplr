# ==============================================================================
# Cardinality Matching
# ==============================================================================

#' Cardinality Matching
#'
#' Maximizes the number of matched pairs subject to balance constraints.
#' Uses iterative pruning: starts with a full match, then removes pairs
#' that contribute most to imbalance until all variables satisfy the
#' standardized difference threshold.
#'
#' @param left Data frame of "left" units
#' @param right Data frame of "right" units
#' @param vars Character vector of matching variable names
#' @param max_std_diff Maximum allowed absolute standardized difference
#'   (default: 0.1, corresponding to "excellent" balance)
#' @param distance Distance metric (default: "euclidean")
#' @param weights Optional named vector of variable weights
#' @param scale Scaling method (default: FALSE)
#' @param auto_scale If TRUE, automatically select scaling (default: FALSE)
#' @param method LAP solver method (default: "auto")
#' @param max_iter Maximum pruning iterations (default: 100)
#' @param batch_fraction Fraction of worst pairs to remove per iteration
#'   (default: 0.1). Larger values speed up convergence but may over-prune.
#'
#' @return A matching_result object with additional cardinality info:
#'   `result$info$pruning_iterations`, `result$info$pairs_removed`,
#'   `result$info$final_balance`.
#'
#' @details
#' Cardinality matching (Zubizarreta 2014) finds the largest matched sample
#' that satisfies pre-specified balance constraints. This implementation uses
#' an iterative pruning heuristic:
#'
#' 1. Run full optimal matching via [match_couples()]
#' 2. Compute balance diagnostics
#' 3. While any |std_diff| exceeds `max_std_diff`:
#'    - Identify the variable with worst balance
#'    - Remove the batch of pairs contributing most to that imbalance
#'    - Recompute balance
#'
#' @examples
#' set.seed(42)
#' left <- data.frame(id = 1:20, x = rnorm(20, 0, 1), y = rnorm(20, 0, 1))
#' right <- data.frame(id = 21:50, x = rnorm(30, 0.5, 1), y = rnorm(30, 0.3, 1))
#' result <- cardinality_match(left, right, vars = c("x", "y"), max_std_diff = 0.2)
#' print(result)
#'
#' @export
cardinality_match <- function(left, right, vars,
                              max_std_diff = 0.1,
                              distance = "euclidean",
                              weights = NULL,
                              scale = FALSE,
                              auto_scale = FALSE,
                              method = "auto",
                              max_iter = 100L,
                              batch_fraction = 0.1) {

  # Validate
  if (!is.numeric(max_std_diff) || length(max_std_diff) != 1 ||
      max_std_diff <= 0) {
    stop("max_std_diff must be a positive number", call. = FALSE)
  }

  # Step 1: Full initial matching
  result <- match_couples(
    left, right, vars = vars,
    distance = distance, weights = weights,
    scale = scale, auto_scale = auto_scale,
    method = method,
    return_unmatched = TRUE,
    return_diagnostics = TRUE
  )

  if (result$info$n_matched == 0) {
    result$info$pruning_iterations <- 0L
    result$info$pairs_removed <- 0L
    return(result)
  }

  # Step 2: Iterative pruning
  pairs <- result$pairs
  original_n <- nrow(pairs)
  iter <- 0L

  # Detect ID columns
  left_id_col <- if ("id" %in% names(left)) "id" else names(left)[1]
  right_id_col <- if ("id" %in% names(right)) "id" else names(right)[1]

  for (it in seq_len(max_iter)) {
    if (nrow(pairs) <= 1) break

    # Compute balance for current pairs
    bal <- .compute_pair_balance(pairs, left, right, vars,
                                left_id_col, right_id_col)

    # Check if balance is achieved
    if (all(abs(bal$std_diffs) <= max_std_diff)) break

    iter <- it

    # Find worst variable
    worst_idx <- which.max(abs(bal$std_diffs))
    worst_var <- vars[worst_idx]

    # For each pair, compute contribution to imbalance on worst variable
    pair_diffs <- .pair_var_diffs(pairs, left, right, worst_var,
                                 left_id_col, right_id_col)
    abs_diffs <- abs(pair_diffs)

    # Remove worst batch
    n_remove <- max(1L, ceiling(nrow(pairs) * batch_fraction))
    n_remove <- min(n_remove, nrow(pairs) - 1L)  # Keep at least 1 pair
    remove_idx <- order(abs_diffs, decreasing = TRUE)[seq_len(n_remove)]
    pairs <- pairs[-remove_idx, , drop = FALSE]
  }

  # Step 3: Build result
  # Update unmatched lists
  matched_left <- pairs$left_id
  matched_right <- pairs$right_id
  all_left_ids <- if (left_id_col %in% names(left)) {
    as.character(left[[left_id_col]])
  } else {
    as.character(seq_len(nrow(left)))
  }
  all_right_ids <- if (right_id_col %in% names(right)) {
    as.character(right[[right_id_col]])
  } else {
    as.character(seq_len(nrow(right)))
  }

  result$pairs <- pairs
  result$unmatched <- list(
    left = setdiff(all_left_ids, as.character(matched_left)),
    right = setdiff(all_right_ids, as.character(matched_right))
  )
  result$info$n_matched <- nrow(pairs)
  result$info$total_distance <- sum(pairs$distance, na.rm = TRUE)
  result$info$pruning_iterations <- iter
  result$info$pairs_removed <- original_n - nrow(pairs)
  result$info$max_std_diff_target <- max_std_diff

  result
}

#' Compute standardized differences for current pairs
#' @keywords internal
.compute_pair_balance <- function(pairs, left, right, vars,
                                  left_id_col, right_id_col) {
  # Merge matched left values
  left_matched <- merge(
    data.frame(left_id = pairs$left_id, stringsAsFactors = FALSE),
    left, by.x = "left_id", by.y = left_id_col, all.x = TRUE
  )

  right_matched <- merge(
    data.frame(right_id = pairs$right_id, stringsAsFactors = FALSE),
    right, by.x = "right_id", by.y = right_id_col, all.x = TRUE
  )

  std_diffs <- vapply(vars, function(v) {
    standardized_difference(left_matched[[v]], right_matched[[v]])
  }, numeric(1))

  list(std_diffs = std_diffs)
}

#' Compute per-pair differences on a single variable
#' @keywords internal
.pair_var_diffs <- function(pairs, left, right, var,
                            left_id_col, right_id_col) {
  # Check if diff column already exists
  diff_col <- paste0(".", var, "_diff")
  if (diff_col %in% names(pairs)) {
    return(pairs[[diff_col]])
  }

  # Otherwise compute from original data
  left_vals <- merge(
    data.frame(left_id = pairs$left_id, stringsAsFactors = FALSE),
    left[, c(left_id_col, var), drop = FALSE],
    by.x = "left_id", by.y = left_id_col, all.x = TRUE
  )[[var]]

  right_vals <- merge(
    data.frame(right_id = pairs$right_id, stringsAsFactors = FALSE),
    right[, c(right_id_col, var), drop = FALSE],
    by.x = "right_id", by.y = right_id_col, all.x = TRUE
  )[[var]]

  left_vals - right_vals
}
