# ==============================================================================
# Matching Lazy - lazy_cost_spec S3 class for memory_mode = "lazy"/"implicit"
# ==============================================================================
# When build_cost_matrix() resolves to lazy mode, it returns a lazy_cost_spec
# instead of a dense matrix: a lightweight list holding the (already scaled/
# weighted) feature matrices and enough metadata to build a
# lap::LazyCostMatrix in C++ on demand, without ever materializing an
# n_left x n_right matrix in R.
#
# The same object states the complete implicit problem the edge-generation loop
# solves, so `mode` records which of the two the caller asked for and travels
# with the spec to the solve. Threading it as a separate argument instead would
# mean every function between build_cost_matrix() and assignment() carrying a
# field of the object it already holds.

#' Construct a lazy cost specification
#'
#' `mode` is the memory mode that resolved to this specification: `"lazy"`,
#' solved over every pair, or `"implicit"`, solved by generating the pairs the
#' answer turns out to need.
#'
#' @return An object of class "lazy_cost_spec".
#' @keywords internal
new_lazy_cost_spec <- function(left_mat, right_mat, distance, sigma, weights, vars,
                               mode = c("lazy", "implicit")) {
  mode <- match.arg(mode)
  structure(
    list(
      left_mat = left_mat,
      right_mat = right_mat,
      distance = tolower(as.character(distance)[1]),
      sigma = sigma,
      weights = weights,
      vars = vars,
      n_left = nrow(left_mat),
      n_right = nrow(right_mat),
      max_distance = Inf,
      calipers = list(),
      mode = mode
    ),
    class = "lazy_cost_spec"
  )
}

#' @keywords internal
is_lazy_cost_spec <- function(x) inherits(x, "lazy_cost_spec")

#' The memory mode a lazy cost specification was built for
#'
#' A spec built before `mode` existed, or by hand, is a lazy one: solving every
#' pair is what the class has always meant.
#'
#' @return "lazy" or "implicit".
#' @keywords internal
lazy_cost_spec_mode <- function(spec) {
  mode <- spec$mode
  if (is.null(mode)) "lazy" else mode
}

#' @export
dim.lazy_cost_spec <- function(x) c(x$n_left, x$n_right)

#' Swap left/right in a lazy cost spec
#'
#' A cheap metadata field-swap (left_mat <-> right_mat, n_left <-> n_right),
#' unlike the dense path's `t()` matrix copy. Calipers/max_distance are
#' unaffected: a caliper's `var_index` refers to a matching VARIABLE
#' (a column shared by both sides), not a left/right unit index, so it does
#' not need to change when the roles of left/right are swapped.
#'
#' @keywords internal
transpose_lazy_cost_spec <- function(spec) {
  transposed <- spec
  transposed$left_mat <- spec$right_mat
  transposed$right_mat <- spec$left_mat
  transposed$n_left <- spec$n_right
  transposed$n_right <- spec$n_left
  transposed
}

#' Compute paired (not cross) distances for specific matched pairs
#'
#' Given matched row/column index pairs (as produced by a solve), reports each
#' pair's distance. The evaluation is the solver's own: the same C++ routine the
#' lazy path priced the pair with is called on that pair, rather than the
#' formula being written a second time here. Two implementations of one metric
#' agree to rounding and not to the last bit, and the difference is visible
#' where it matters most -- a caliper set at a distance the package reported can
#' exclude the pair it was read from.
#'
#' This is cheap regardless of n_left/n_right: the number of matched pairs never
#' exceeds min(n_left, n_right), so it never approaches the O(n*m) cost the lazy
#' path exists to avoid.
#'
#' @return Numeric vector of length length(matched_rows).
#' @keywords internal
lazy_pair_distances <- function(spec, matched_rows, matched_cols) {
  cpp_lazy_pair_distances(
    left_mat = spec$left_mat,
    right_mat = spec$right_mat,
    metric = spec$distance,
    inv_cov = lazy_cost_spec_inv_cov(spec),
    rows = as.integer(matched_rows),
    cols = as.integer(matched_cols)
  )
}

#' Precompute the Mahalanobis inverse covariance matrix for a lazy cost spec
#'
#' Mirrors compute_distance_matrix()'s pooled within-group covariance logic
#' exactly (R/matching_distance.R) -- computed once in R rather than
#' reimplemented in C++, so the two code paths can't drift apart.
#'
#' @return p x p inverse covariance matrix, or NULL if distance != "mahalanobis".
#' @keywords internal
lazy_cost_spec_inv_cov <- function(spec) {
  if (!identical(spec$distance, "mahalanobis") && !identical(spec$distance, "maha")) {
    return(NULL)
  }
  n_left <- spec$n_left
  n_right <- spec$n_right
  if (!is.null(spec$sigma)) {
    cov_mat <- spec$sigma
  } else if (n_left >= 2L && n_right >= 2L) {
    S_L <- stats::cov(spec$left_mat)
    S_R <- stats::cov(spec$right_mat)
    cov_mat <- ((n_left - 1L) * S_L + (n_right - 1L) * S_R) / (n_left + n_right - 2L)
  } else {
    cov_mat <- stats::cov(rbind(spec$left_mat, spec$right_mat))
  }
  inv_cov <- tryCatch(solve(cov_mat), error = function(e) NULL)
  if (is.null(inv_cov) || rcond(cov_mat) < .Machine$double.eps) {
    stop("Covariance matrix is singular or near-singular; cannot compute Mahalanobis distance. ",
         "Consider removing collinear variables or supplying a regularized sigma.",
         call. = FALSE)
  }
  inv_cov
}

#' Calipers of a lazy cost spec, keyed by variable name
#'
#' The C++ lazy cost source takes its calipers as a named list of thresholds,
#' while the spec stores them as records carrying an index into `spec$vars`.
#'
#' @return Named list of numeric thresholds, one per caliper.
#' @keywords internal
lazy_cost_spec_calipers <- function(spec) {
  stats::setNames(
    lapply(spec$calipers, function(cal) cal$threshold),
    vapply(spec$calipers, function(cal) spec$vars[[cal$var_index]], character(1))
  )
}
