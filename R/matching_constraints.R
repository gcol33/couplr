# ==============================================================================
# Matching Constraints - Apply calipers and max_distance
# ==============================================================================

#' Large value for forbidden pairs
#'
#' A numeric constant used to mark forbidden pairs in cost matrices.
#' @format Numeric value (half of .Machine$double.xmax).
#' @keywords internal
BIG_COST <- .Machine$double.xmax / 2

#' Apply maximum distance constraint
#'
#' @return Modified cost matrix with forbidden pairs marked.
#' @keywords internal
apply_max_distance <- function(cost_matrix, max_distance = Inf) {
  if (is.null(max_distance)) {
    return(cost_matrix)
  }

  if (!is.numeric(max_distance) || length(max_distance) != 1) {
    stop("max_distance must be a single numeric value", call. = FALSE)
  }

  if (is.infinite(max_distance)) {
    return(cost_matrix)
  }

  if (max_distance <= 0) {
    stop("max_distance must be positive", call. = FALSE)
  }

  if (is_lazy_cost_spec(cost_matrix)) {
    # O(1): just record the threshold. Cheaper than the dense O(n*m) masking
    # below, since no matrix ever gets materialized in the first place.
    cost_matrix$max_distance <- max_distance
    return(cost_matrix)
  }

  # Mark pairs exceeding max_distance as forbidden (Inf, not BIG_COST: the
  # LAP solvers detect non-finite cells as forbidden and short-circuit on
  # infeasibility; a huge finite "BIG_COST" looks like a regular edge to JV
  # and can stall the solver on sparse problems).
  cost_matrix[cost_matrix > max_distance] <- Inf

  cost_matrix
}

#' Apply caliper constraints
#'
#' Calipers impose per-variable maximum absolute differences.
#'
#' @return Modified cost matrix with forbidden pairs marked.
#' @keywords internal
apply_calipers <- function(cost_matrix, left, right, calipers, vars) {
  if (is.null(calipers)) {
    return(cost_matrix)
  }

  if (is_lazy_cost_spec(cost_matrix)) {
    # O(length(calipers)): resolve each caliper variable to its column index
    # in `vars` (the same order as left_mat/right_mat) and record it, rather
    # than an O(n*m*length(calipers)) dense scan.
    new_calipers <- cost_matrix$calipers
    for (var_name in names(calipers)) {
      var_index <- match(var_name, vars)
      if (is.na(var_index)) next  # not a matching variable, same as dense path
      new_calipers[[length(new_calipers) + 1]] <- list(
        var_index = var_index, threshold = calipers[[var_name]]
      )
    }
    cost_matrix$calipers <- new_calipers
    return(cost_matrix)
  }

  n_left <- nrow(left)
  n_right <- nrow(right)

  # For each variable with a caliper
  for (var_name in names(calipers)) {
    if (!(var_name %in% vars)) {
      next  # Skip if not in matching variables
    }

    caliper_value <- calipers[[var_name]]

    left_vals <- left[[var_name]]
    right_vals <- right[[var_name]]

    # Compute absolute differences for this variable
    for (i in seq_len(n_left)) {
      for (j in seq_len(n_right)) {
        abs_diff <- abs(left_vals[i] - right_vals[j])
        if (abs_diff > caliper_value) {
          cost_matrix[i, j] <- Inf
        }
      }
    }
  }

  cost_matrix
}

#' Mark forbidden pairs
#'
#' Generic function to mark specific pairs as forbidden.
#'
#' @return Modified cost matrix with forbidden pairs marked.
#' @keywords internal
mark_forbidden_pairs <- function(cost_matrix, forbidden_indices) {
  if (is.null(forbidden_indices) || nrow(forbidden_indices) == 0) {
    return(cost_matrix)
  }

  if (is_lazy_cost_spec(cost_matrix)) {
    stop("Explicit forbidden pairs are not supported with memory_mode = \"lazy\" yet; ",
         "use memory_mode = \"dense\".", call. = FALSE)
  }

  # forbidden_indices should be a 2-column matrix of (row, col) indices
  for (k in seq_len(nrow(forbidden_indices))) {
    i <- forbidden_indices[k, 1]
    j <- forbidden_indices[k, 2]
    cost_matrix[i, j] <- Inf
  }

  cost_matrix
}

#' Apply all constraints to cost matrix
#'
#' Main entry point for applying constraints.
#'
#' @return Modified cost matrix with all constraints applied.
#' @keywords internal
apply_all_constraints <- function(cost_matrix, left, right, vars,
                                  max_distance = Inf, calipers = NULL,
                                  forbidden = NULL) {
  # Apply max_distance
  cost_matrix <- apply_max_distance(cost_matrix, max_distance)

  # Apply calipers
  cost_matrix <- apply_calipers(cost_matrix, left, right, calipers, vars)

  # Apply custom forbidden pairs if provided
  cost_matrix <- mark_forbidden_pairs(cost_matrix, forbidden)

  cost_matrix
}

#' Check if any valid pairs exist
#'
#' @return Logical indicating whether any valid pairs exist.
#' @keywords internal
has_valid_pairs <- function(cost_matrix) {
  if (is_lazy_cost_spec(cost_matrix)) {
    # Deferred to solve time: an O(n*m) scan here to answer this precisely
    # would cost exactly what the lazy path exists to avoid. If the
    # constraints actually leave no valid pairs, the solve attempt itself
    # throws an InfeasibleException, which callers (.solve_with_partial_feasibility)
    # already catch and translate into the same "no valid pairs" outcome.
    return(TRUE)
  }
  any(is.finite(cost_matrix) & cost_matrix < BIG_COST)
}

#' Count valid pairs in cost matrix
#'
#' @return Integer count of valid (non-forbidden) pairs.
#' @keywords internal
count_valid_pairs <- function(cost_matrix) {
  sum(is.finite(cost_matrix) & cost_matrix < BIG_COST)
}
