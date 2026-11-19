# ==============================================================================
# Matching Distance - Distance computation and scaling
# ==============================================================================

# The built-in metrics are evaluated pair by pair in C++ by the code a lazy
# solve uses, written straight into the result: a dense matrix then holds the
# distances a lazy specification of the same problem evaluates, and building it
# allocates nothing else of its size. The sum over dimensions of (L_ik - R_jk)^2
# keeps every digit the coordinates carry, which the Gram-matrix identity
# ||L||^2 + ||R||^2 - 2 L.R would not when the coordinates are large next to the
# distance between them.
#
# Everything a built-in distance is computed from is plain double arithmetic in
# one C++ header (src/core/lap_plain_arith.h): the scaling statistics, the
# covariance and its inverse, and the distance of a pair. Each sum runs in a
# fixed order, in doubles, with no fused multiply-add and no BLAS or LAPACK
# routine, so the distances are the same bits on every platform. R's mean(),
# sd(), cov() and chol() are not: they accumulate in long double where the
# platform has one and call the platform's LAPACK.
.builtin_metrics <- c("euclidean", "l2", "manhattan", "l1", "cityblock",
                      "squared_euclidean", "sqeuclidean", "sq", "chebyshev",
                      "chebychev", "maximum", "max", "mahalanobis", "maha")

#' Compute pairwise distance matrix
#'
#' @param left_mat Numeric matrix of left units (rows = units, cols = variables).
#' @param right_mat Numeric matrix of right units (rows = units, cols = variables).
#' @param distance Character string specifying distance metric, or a function.
#' @param sigma Optional covariance matrix for Mahalanobis distance. If `NULL`
#'   (default), the pooled within-group covariance is estimated from `left_mat`
#'   and `right_mat`: \eqn{\Sigma = ((n_L-1)\,S_L + (n_R-1)\,S_R) / (n_L+n_R-2)}.
#'   This matches the convention used by `optmatch::match_on()` for
#'   treated/control Mahalanobis matching. If either group has fewer than two
#'   rows, falls back to the overall-sample covariance of the two groups
#'   stacked together.
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

  if (!distance %in% .builtin_metrics) {
    stop(sprintf("Unknown distance metric: %s", distance), call. = FALSE)
  }

  inv_cov <- NULL
  if (distance %in% c("mahalanobis", "maha")) {
    inv_cov <- mahalanobis_inv_cov(left_mat, right_mat, sigma)
  }

  storage.mode(left_mat) <- "double"
  storage.mode(right_mat) <- "double"
  cpp_lazy_dense_matrix(left_mat, right_mat, distance, inv_cov)
}

#' Inverse covariance matrix of a Mahalanobis distance between two groups
#'
#' The matrix `A` in `sqrt((x - y)' A (x - y))`: the inverse of `sigma` when it
#' is supplied, and otherwise of the pooled within-group covariance of the two
#' groups. Pooling within groups is the convention for treated/control
#' matching; the covariance of the stacked sample would be inflated by the
#' difference between the groups and under-weight the variables on which they
#' differ most. When a group has fewer than two rows the two groups are taken
#' as one sample. The dense matrix and a lazy specification both read their
#' matrix from here.
#'
#' @param left_mat,right_mat Numeric matrices of the two groups, one row per
#'   unit and one column per variable.
#' @param sigma Optional covariance matrix, one row and column per variable.
#' @return The `p x p` inverse covariance matrix.
#' @keywords internal
mahalanobis_inv_cov <- function(left_mat, right_mat, sigma = NULL) {
  n_vars <- ncol(left_mat)
  if (is.null(sigma)) {
    return(mahalanobis_precision(cpp_group_covariance(left_mat, right_mat)))
  }
  if (!is.matrix(sigma) || nrow(sigma) != n_vars || ncol(sigma) != n_vars) {
    stop(sprintf("sigma must be a %d x %d matrix matching the number of variables",
                 n_vars, n_vars), call. = FALSE)
  }
  mahalanobis_precision(sigma)
}

#' Precision matrix of a covariance matrix, through its Cholesky factor
#'
#' The Mahalanobis distances need the inverse of a symmetric positive
#' definite matrix. The matrix is factored as `L L'`, the inverse of `L` is
#' formed by forward substitution and the precision matrix as its
#' cross-product, in plain double arithmetic. A pivot that is not positive, or
#' a factor whose squared reciprocal condition number is below machine
#' precision, marks a singular matrix.
#'
#' @return The `p x p` inverse of `cov_mat`.
#' @keywords internal
mahalanobis_precision <- function(cov_mat) {
  cov_mat <- as.matrix(cov_mat)
  inv <- if (is.numeric(cov_mat)) cpp_precision_matrix(cov_mat)
  if (is.null(inv)) {
    stop("Covariance matrix is singular or near-singular; cannot compute Mahalanobis distance. ",
         "Consider removing collinear variables or supplying a regularized sigma.",
         call. = FALSE)
  }
  dimnames(inv) <- dimnames(cov_mat)
  inv
}

#' Apply scaling to matching variables
#'
#' @return List with scaled left/right matrices and scaling parameters.
#' @keywords internal
apply_scaling <- function(left_mat, right_mat, method = "standardize") {
  if (method == FALSE || method == "none" || is.null(method)) {
    return(list(left = left_mat, right = right_mat, params = NULL))
  }

  # The centre and scale of each variable over the left and right units
  # together. A variable without spread keeps its values: its scale is set to
  # one.
  if (method == TRUE || method == "standardize" || method == "scale") {
    st <- cpp_scaling_statistics(left_mat, right_mat, "standardize")
    st$scale[st$scale == 0] <- 1
    params <- list(method = "standardize", means = st$center, sds = st$scale)
  } else if (method == "range" || method == "minmax") {
    st <- cpp_scaling_statistics(left_mat, right_mat, "range")
    st$scale[st$scale == 0] <- 1
    params <- list(method = "range", mins = st$center, maxs = st$upper,
                   ranges = st$scale)
  } else if (method == "robust") {
    st <- cpp_scaling_statistics(left_mat, right_mat, "robust")
    st$scale[st$scale == 0] <- 1
    params <- list(method = "robust", medians = st$center, mads = st$scale)
  } else {
    stop(sprintf("Unknown scaling method: %s", method), call. = FALSE)
  }
  params[-1L] <- lapply(params[-1L], stats::setNames, colnames(left_mat))

  # One subtraction and one division per value, each a single IEEE operation.
  rescale <- function(mat) {
    sweep(sweep(mat, 2, st$center, "-"), 2, st$scale, "/")
  }

  list(left = rescale(left_mat), right = rescale(right_mat), params = params)
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
#' @param memory_mode One of "auto" (default), "dense", "lazy" or "implicit".
#'   "auto" warns (or, when the caller supports it, switches) when the dense
#'   matrix would consume a large fraction of free system RAM. `memory_mode =
#'   "lazy"` returns a `lazy_cost_spec` instead of a matrix when the calling
#'   path and distance metric support it (built-in metrics via `assignment()`
#'   with `method = "jv"`/`"auction"`); otherwise it errors clearly rather
#'   than silently falling back to dense. `memory_mode = "implicit"` returns
#'   the same specification marked for the edge-generation loop, which solves
#'   it without building the pair set at all.
#' @param caller_supports_lazy Whether the calling code path can actually
#'   consume a `lazy_cost_spec` result for a solve over every pair. Defaults
#'   to `TRUE`; a caller with no such solve (e.g. `full_match()`, whose flow
#'   over every pair would hold every pair) passes `FALSE` so
#'   `memory_mode = "auto"` never promotes to lazy for it, and an explicit
#'   `memory_mode = "lazy"` request errors clearly instead of returning a
#'   `lazy_cost_spec` the caller cannot use.
#' @param caller_supports_implicit Whether the calling path's design is one the
#'   edge-generation loop solves. Defaults to whatever the caller says about
#'   lazy, since the loop reads the same specification.
#' @return Numeric matrix of distances with optional scaling/weights applied.
#' @keywords internal
build_cost_matrix <- function(left, right, vars, distance = "euclidean",
                               weights = NULL, scale = FALSE, sigma = NULL,
                               memory_mode = "auto", caller_supports_lazy = TRUE,
                               caller_supports_implicit = caller_supports_lazy) {
  # Extract variable matrices
  left_mat <- extract_matching_vars(left, vars)
  right_mat <- extract_matching_vars(right, vars)

  # A custom distance FUNCTION reaches the lazy and implicit paths through a
  # cost source that calls it on a block of left rows against every right unit
  # and keeps a few of those blocks, which is the contract the dense path
  # already calls it under, with the R call paid per block rather than per pair.
  resolved <- resolve_memory_mode(
    nrow(left_mat), nrow(right_mat), memory_mode,
    solver_supports_lazy = caller_supports_lazy,
    solver_supports_implicit = caller_supports_implicit
  )

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

  if (resolved %in% c("lazy", "implicit")) {
    return(new_lazy_cost_spec(left_mat, right_mat, distance, sigma, weights, vars,
                              mode = resolved))
  }

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
