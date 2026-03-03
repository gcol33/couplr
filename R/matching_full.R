# ==============================================================================
# Full Matching - Variable-ratio group matching
# ==============================================================================

#' Full Matching
#'
#' Assigns every unit (left and right) to a matched group with variable ratios
#' (1:k or k:1). Unlike 1:1 matching, full matching does not discard units,
#' producing matched groups where each group contains at least one left and one
#' right unit.
#'
#' @param left Data frame of left (treated) units
#' @param right Data frame of right (control) units
#' @param vars Character vector of variable names to match on
#' @param distance Distance metric: \code{"euclidean"} (default),
#'   \code{"mahalanobis"}, \code{"manhattan"}, or a custom function
#' @param min_controls Minimum number of right units per group (default: 1)
#' @param max_controls Maximum number of right units per group (default: Inf)
#' @param caliper Maximum allowable distance for a match. Units with no
#'   eligible partner within the caliper are left unmatched.
#' @param caliper_sd If not NULL, caliper is expressed in standard deviations
#'   of the pooled distance distribution rather than absolute units.
#' @param weights Named numeric vector of variable weights
#' @param scale Scaling method: \code{FALSE} (default), \code{"robust"},
#'   \code{"standardize"}, or \code{"range"}
#' @param auto_scale If TRUE, automatically preprocess and scale variables
#' @param sigma Optional covariance matrix for Mahalanobis distance
#' @param left_id Name of ID column in left (default: \code{"id"})
#' @param right_id Name of ID column in right (default: \code{"id"})
#'
#' @return An S3 object of class \code{c("full_matching_result", "couplr_result")}
#'   containing:
#' \describe{
#'   \item{groups}{Tibble with columns \code{group_id}, \code{id}, \code{side}
#'     (\code{"left"}/\code{"right"}), and \code{weight}}
#'   \item{info}{List with \code{n_groups}, \code{n_left}, \code{n_right},
#'     \code{n_unmatched_left}, \code{n_unmatched_right}, \code{method},
#'     \code{vars}}
#'   \item{unmatched}{List of unmatched left and right IDs (if caliper excludes
#'     units)}
#' }
#'
#' @details
#' Full matching creates matched groups of variable size. The algorithm:
#' \enumerate{
#'   \item Computes the distance matrix between all left and right units
#'   \item Applies caliper constraints (if specified)
#'   \item Assigns each left unit to its nearest eligible right unit (forming
#'     initial 1:1 pairs)
#'   \item Assigns remaining unmatched right units to their nearest
#'     already-matched left unit, respecting \code{max_controls}
#'   \item Computes weights inversely proportional to group size
#' }
#'
#' Weights are computed so that within each group, the total weight of right
#' units equals the total weight of left units (which is 1). For a group with
#' 1 left and k right units, each right unit receives weight 1/k.
#'
#' For optimal full matching (minimizing total distance across all groups),
#' consider the \pkg{optmatch} package. This implementation uses a greedy
#' approach that is fast and produces good (but not necessarily optimal) results.
#'
#' @examples
#' set.seed(42)
#' left <- data.frame(id = 1:5, age = c(25, 35, 45, 55, 65))
#' right <- data.frame(id = 6:20, age = runif(15, 20, 70))
#' result <- full_match(left, right, vars = "age")
#' print(result)
#'
#' @export
full_match <- function(left, right, vars,
                       distance = "euclidean",
                       min_controls = 1,
                       max_controls = Inf,
                       caliper = NULL,
                       caliper_sd = NULL,
                       weights = NULL,
                       scale = FALSE,
                       auto_scale = FALSE,
                       sigma = NULL,
                       left_id = "id",
                       right_id = "id") {

  # --- Input validation ---
  if (!is.data.frame(left) || !is.data.frame(right)) {
    stop("left and right must be data frames", call. = FALSE)
  }
  if (is.null(vars) || length(vars) == 0) {
    stop("vars must be specified", call. = FALSE)
  }
  if (!left_id %in% names(left)) {
    stop(sprintf("left_id column '%s' not found in left data", left_id),
         call. = FALSE)
  }
  if (!right_id %in% names(right)) {
    stop(sprintf("right_id column '%s' not found in right data", right_id),
         call. = FALSE)
  }
  min_controls <- as.integer(min_controls)
  if (is.na(min_controls) || min_controls < 1) {
    stop("min_controls must be a positive integer", call. = FALSE)
  }
  if (!is.infinite(max_controls)) {
    max_controls <- as.integer(max_controls)
    if (is.na(max_controls) || max_controls < min_controls) {
      stop("max_controls must be >= min_controls", call. = FALSE)
    }
  }

  # --- Preprocessing ---
  if (auto_scale) {
    preproc <- preprocess_matching_vars(
      left, right, vars,
      auto_scale = TRUE,
      scale_method = if (identical(scale, FALSE)) "auto" else scale,
      check_health = TRUE, remove_problematic = TRUE, verbose = TRUE
    )
    vars <- preproc$vars
    if (preproc$scaling_method != "none") {
      scale <- preproc$scaling_method
    }
  }

  validate_matching_inputs(left, right, vars)
  weights <- validate_weights(weights, vars)

  # Extract IDs
  l_ids <- as.character(left[[left_id]])
  r_ids <- as.character(right[[right_id]])

  # --- Distance matrix ---
  cost_matrix <- build_cost_matrix(left, right, vars, distance, weights, scale,
                                   sigma = sigma)

  # --- Caliper ---
  caliper_val <- NULL
  if (!is.null(caliper_sd)) {
    finite_dists <- cost_matrix[is.finite(cost_matrix)]
    if (length(finite_dists) > 1) {
      caliper_val <- caliper_sd * stats::sd(finite_dists)
    }
  } else if (!is.null(caliper)) {
    caliper_val <- caliper
  }

  # Apply caliper: set distances beyond caliper to Inf
  if (!is.null(caliper_val)) {
    cost_matrix[cost_matrix > caliper_val] <- Inf
  }

  n_left <- nrow(cost_matrix)
  n_right <- ncol(cost_matrix)

  # --- Greedy group formation ---
  # Step 1: Each left unit picks its nearest eligible right unit
  left_to_right <- integer(n_left)  # which right unit each left is paired with
  right_assigned <- logical(n_right)
  group_of_left <- integer(n_left)  # group id for each left unit

  # Sort left units by their minimum distance (hardest-to-match first)
  min_dists <- apply(cost_matrix, 1, function(row) {
    finite <- row[is.finite(row)]
    if (length(finite) == 0) Inf else min(finite)
  })
  left_order <- order(min_dists)

  group_id <- 0L
  # groups_list: list of lists, each with left_idx, right_idxs
  groups_list <- vector("list", n_left)
  unmatched_left_idx <- integer(0)

  for (i in left_order) {
    row <- cost_matrix[i, ]
    # Find nearest unassigned right unit
    available <- which(is.finite(row) & !right_assigned)
    if (length(available) == 0) {
      # No eligible right unit within caliper
      unmatched_left_idx <- c(unmatched_left_idx, i)
      next
    }
    best_j <- available[which.min(row[available])]
    group_id <- group_id + 1L
    left_to_right[i] <- best_j
    right_assigned[best_j] <- TRUE
    group_of_left[i] <- group_id
    groups_list[[group_id]] <- list(left_idx = i, right_idxs = best_j)
  }

  # Step 2: Assign remaining right units to nearest already-matched left unit
  # (respecting max_controls)
  remaining_right <- which(!right_assigned)
  unmatched_right_idx <- integer(0)

  if (length(remaining_right) > 0) {
    # For each unmatched right unit, find nearest matched left unit
    matched_left_idxs <- which(group_of_left > 0)

    for (j in remaining_right) {
      col <- cost_matrix[matched_left_idxs, j]
      # Find eligible left units (finite distance and group not at max_controls)
      eligible <- rep(TRUE, length(matched_left_idxs))
      if (!is.infinite(max_controls)) {
        for (k in seq_along(matched_left_idxs)) {
          li <- matched_left_idxs[k]
          gi <- group_of_left[li]
          if (length(groups_list[[gi]]$right_idxs) >= max_controls) {
            eligible[k] <- FALSE
          }
        }
      }
      eligible <- eligible & is.finite(col)

      if (!any(eligible)) {
        unmatched_right_idx <- c(unmatched_right_idx, j)
        next
      }

      best_k <- which(eligible)[which.min(col[eligible])]
      best_left <- matched_left_idxs[best_k]
      gi <- group_of_left[best_left]
      groups_list[[gi]]$right_idxs <- c(groups_list[[gi]]$right_idxs, j)
    }
  }

  # Step 3: Check min_controls constraint
  # Mark groups that don't meet minimum (set to NULL after collecting)
  if (min_controls > 1) {
    keep <- rep(TRUE, length(groups_list))
    for (g in seq_len(group_id)) {
      if (is.null(groups_list[[g]])) { keep[g] <- FALSE; next }
      if (length(groups_list[[g]]$right_idxs) < min_controls) {
        unmatched_left_idx <- c(unmatched_left_idx, groups_list[[g]]$left_idx)
        unmatched_right_idx <- c(unmatched_right_idx, groups_list[[g]]$right_idxs)
        keep[g] <- FALSE
      }
    }
    groups_list <- groups_list[keep]
  }

  # --- Build output tibble ---
  groups_rows <- list()
  final_group_id <- 0L

  for (g in seq_along(groups_list)) {
    grp <- groups_list[[g]]
    if (is.null(grp)) next
    final_group_id <- final_group_id + 1L
    n_right_in_group <- length(grp$right_idxs)
    right_weight <- 1 / n_right_in_group

    # Left unit
    groups_rows[[length(groups_rows) + 1]] <- tibble::tibble(
      group_id = final_group_id,
      id = l_ids[grp$left_idx],
      side = "left",
      weight = 1.0
    )
    # Right units
    groups_rows[[length(groups_rows) + 1]] <- tibble::tibble(
      group_id = rep(final_group_id, n_right_in_group),
      id = r_ids[grp$right_idxs],
      side = rep("right", n_right_in_group),
      weight = rep(right_weight, n_right_in_group)
    )
  }

  if (length(groups_rows) > 0) {
    groups_tbl <- dplyr::bind_rows(groups_rows)
  } else {
    groups_tbl <- tibble::tibble(
      group_id = integer(0), id = character(0),
      side = character(0), weight = numeric(0)
    )
  }

  # Unmatched IDs
  unmatched <- list(
    left = l_ids[unmatched_left_idx],
    right = r_ids[unmatched_right_idx]
  )

  info <- list(
    n_groups = final_group_id,
    n_left = n_left,
    n_right = n_right,
    n_matched_left = n_left - length(unmatched_left_idx),
    n_matched_right = n_right - length(unmatched_right_idx),
    n_unmatched_left = length(unmatched_left_idx),
    n_unmatched_right = length(unmatched_right_idx),
    method = "full",
    distance_metric = if (is.function(distance)) "custom" else distance,
    vars = vars
  )

  result <- list(
    groups = groups_tbl,
    info = info,
    unmatched = unmatched
  )

  structure(result, class = c("full_matching_result", "couplr_result"))
}


#' Print Method for Full Matching Results
#'
#' @param x A full_matching_result object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the input object \code{x}.
#' @export
print.full_matching_result <- function(x, ...) {
  cat("\nFull Matching Result\n")
  cat("====================\n\n")
  cat(sprintf("  Groups formed: %d\n", x$info$n_groups))
  cat(sprintf("  Left units:  %d matched, %d unmatched (of %d)\n",
              x$info$n_matched_left, x$info$n_unmatched_left, x$info$n_left))
  cat(sprintf("  Right units: %d matched, %d unmatched (of %d)\n",
              x$info$n_matched_right, x$info$n_unmatched_right, x$info$n_right))

  if (x$info$n_groups > 0) {
    # Group size distribution
    grp_sizes <- table(x$groups$group_id[x$groups$side == "right"])
    cat(sprintf("\n  Right units per group: min=%d, median=%.0f, max=%d\n",
                min(grp_sizes), stats::median(as.numeric(grp_sizes)),
                max(grp_sizes)))
  }
  cat("\n")
  invisible(x)
}
