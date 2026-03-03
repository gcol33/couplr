# ==============================================================================
# Matching Distance - Distance computation and scaling
# ==============================================================================

#' Compute pairwise distance matrix
#'
#' @param left_mat Numeric matrix of left units (rows = units, cols = variables).
#' @param right_mat Numeric matrix of right units (rows = units, cols = variables).
#' @param distance Character string specifying distance metric, or a function.
#' @param sigma Optional covariance matrix for Mahalanobis distance. If NULL
#'   (default), the pooled covariance is estimated from data.
#'
#' @return Numeric matrix of pairwise distances (n_left x n_right).
#' @keywords internal
compute_distance_matrix <- function(left_mat, right_mat, distance = "euclidean",
                                    sigma = NULL) {
  n_left <- nrow(left_mat)
  n_right <- nrow(right_mat)
  n_vars <- ncol(left_mat)

  # Validate dimensions
  if (ncol(right_mat) != n_vars) {
    stop("left_mat and right_mat must have same number of columns", call. = FALSE)
  }

  # Handle distance specification
  if (is.function(distance)) {
    # User-provided distance function
    # Assume it takes two matrices and returns a distance matrix
    return(distance(left_mat, right_mat))
  }

  # Built-in distance metrics
  distance <- tolower(as.character(distance)[1])

  dist_matrix <- matrix(0, nrow = n_left, ncol = n_right)

  if (distance %in% c("euclidean", "l2")) {
    # Euclidean distance: sqrt(sum((x - y)^2))
    for (i in seq_len(n_left)) {
      for (j in seq_len(n_right)) {
        diff <- left_mat[i, ] - right_mat[j, ]
        dist_matrix[i, j] <- sqrt(sum(diff^2))
      }
    }
  } else if (distance %in% c("manhattan", "l1", "cityblock")) {
    # Manhattan distance: sum(|x - y|)
    for (i in seq_len(n_left)) {
      for (j in seq_len(n_right)) {
        diff <- abs(left_mat[i, ] - right_mat[j, ])
        dist_matrix[i, j] <- sum(diff)
      }
    }
  } else if (distance %in% c("squared_euclidean", "sqeuclidean", "sq")) {
    # Squared Euclidean distance: sum((x - y)^2)
    for (i in seq_len(n_left)) {
      for (j in seq_len(n_right)) {
        diff <- left_mat[i, ] - right_mat[j, ]
        dist_matrix[i, j] <- sum(diff^2)
      }
    }
  } else if (distance %in% c("chebyshev", "chebychev", "maximum", "max")) {
    # Chebyshev distance: max(|x - y|)
    for (i in seq_len(n_left)) {
      for (j in seq_len(n_right)) {
        diff <- abs(left_mat[i, ] - right_mat[j, ])
        dist_matrix[i, j] <- max(diff)
      }
    }
  } else if (distance %in% c("mahalanobis", "maha")) {
    # Mahalanobis distance: sqrt((x-y)' * Sigma^-1 * (x-y))
    if (!is.null(sigma)) {
      # User-supplied covariance matrix
      if (!is.matrix(sigma) || nrow(sigma) != n_vars || ncol(sigma) != n_vars) {
        stop(sprintf("sigma must be a %d x %d matrix matching the number of variables",
                     n_vars, n_vars), call. = FALSE)
      }
      cov_mat <- sigma
    } else {
      # Estimate pooled covariance from data
      combined <- rbind(left_mat, right_mat)
      cov_mat <- stats::cov(combined)
    }

    # Robust singularity check using rcond (not det == 0)
    inv_cov <- tryCatch(
      solve(cov_mat),
      error = function(e) NULL
    )
    if (is.null(inv_cov) || rcond(cov_mat) < .Machine$double.eps) {
      stop("Covariance matrix is singular or near-singular; cannot compute Mahalanobis distance. ",
           "Consider removing collinear variables or supplying a regularized sigma.",
           call. = FALSE)
    }

    # Vectorized computation: for each left row, compute distances to all right rows
    # D_ij = sqrt( (L_i - R_j) %*% inv_cov %*% (L_i - R_j)' )
    for (i in seq_len(n_left)) {
      diff_mat <- sweep(right_mat, 2, left_mat[i, ])  # n_right x n_vars
      dist_matrix[i, ] <- sqrt(pmax(rowSums(diff_mat * (diff_mat %*% inv_cov)), 0))
    }
  } else {
    stop(sprintf("Unknown distance metric: %s", distance), call. = FALSE)
  }

  dist_matrix
}

#' Apply scaling to matching variables
#'
#' @return List with scaled left/right matrices and scaling parameters.
#' @keywords internal
apply_scaling <- function(left_mat, right_mat, method = "standardize") {
  if (method == FALSE || method == "none" || is.null(method)) {
    return(list(left = left_mat, right = right_mat, params = NULL))
  }

  # Compute scaling parameters from combined data
  combined <- rbind(left_mat, right_mat)

  if (method == TRUE || method == "standardize" || method == "scale") {
    # Standardize to mean 0, sd 1
    means <- colMeans(combined)
    sds <- apply(combined, 2, stats::sd)

    # Avoid division by zero
    sds[sds == 0] <- 1

    left_scaled <- scale(left_mat, center = means, scale = sds)
    right_scaled <- scale(right_mat, center = means, scale = sds)

    params <- list(method = "standardize", means = means, sds = sds)
  } else if (method == "range" || method == "minmax") {
    # Scale to [0, 1] based on combined range
    mins <- apply(combined, 2, min)
    maxs <- apply(combined, 2, max)
    ranges <- maxs - mins

    # Avoid division by zero
    ranges[ranges == 0] <- 1

    left_scaled <- sweep(sweep(left_mat, 2, mins, "-"), 2, ranges, "/")
    right_scaled <- sweep(sweep(right_mat, 2, mins, "-"), 2, ranges, "/")

    params <- list(method = "range", mins = mins, maxs = maxs, ranges = ranges)
  } else if (method == "robust") {
    # Robust scaling using median and MAD (median absolute deviation)
    medians <- apply(combined, 2, stats::median)
    mads <- apply(combined, 2, stats::mad)

    # Avoid division by zero
    mads[mads == 0] <- 1

    left_scaled <- sweep(sweep(left_mat, 2, medians, "-"), 2, mads, "/")
    right_scaled <- sweep(sweep(right_mat, 2, medians, "-"), 2, mads, "/")

    params <- list(method = "robust", medians = medians, mads = mads)
  } else {
    stop(sprintf("Unknown scaling method: %s", method), call. = FALSE)
  }

  # Remove attributes added by scale()
  left_scaled <- as.matrix(left_scaled)
  right_scaled <- as.matrix(right_scaled)
  attr(left_scaled, "scaled:center") <- NULL
  attr(left_scaled, "scaled:scale") <- NULL
  attr(right_scaled, "scaled:center") <- NULL
  attr(right_scaled, "scaled:scale") <- NULL

  list(left = left_scaled, right = right_scaled, params = params)
}

#' Apply weights to matching variables
#'
#' @return Numeric matrix with columns weighted.
#' @keywords internal
apply_weights <- function(mat, weights) {
  if (is.null(weights) || all(weights == 1)) {
    return(mat)
  }

  if (length(weights) != ncol(mat)) {
    stop("Length of weights must match number of columns in matrix", call. = FALSE)
  }

  # Apply weights by scaling columns
  # For distance calculation, we multiply each variable by sqrt(weight)
  # so that squared differences are weighted correctly
  sweep(mat, 2, sqrt(weights), "*")
}

#' Build cost matrix for matching
#'
#' This is the main entry point for distance computation.
#'
#' @return Numeric matrix of distances with optional scaling/weights applied.
#' @keywords internal
build_cost_matrix <- function(left, right, vars, distance = "euclidean",
                               weights = NULL, scale = FALSE, sigma = NULL) {
  # Extract variable matrices
  left_mat <- extract_matching_vars(left, vars)
  right_mat <- extract_matching_vars(right, vars)

  # Validate and normalize weights
  weights <- validate_weights(weights, vars)

  # Apply scaling if requested
  if (!identical(scale, FALSE)) {
    scaled <- apply_scaling(left_mat, right_mat, method = scale)
    left_mat <- scaled$left
    right_mat <- scaled$right
    scaling_params <- scaled$params
  } else {
    scaling_params <- NULL
  }

  # Apply weights
  left_mat <- apply_weights(left_mat, weights)
  right_mat <- apply_weights(right_mat, weights)

  # Compute distance matrix
  dist_matrix <- compute_distance_matrix(left_mat, right_mat, distance,
                                         sigma = sigma)

  # Add metadata as attributes
  attr(dist_matrix, "distance") <- distance
  attr(dist_matrix, "weights") <- weights
  attr(dist_matrix, "scaling") <- scaling_params
  attr(dist_matrix, "vars") <- vars

  dist_matrix
}
