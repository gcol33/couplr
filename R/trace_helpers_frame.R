# ==============================================================================
# Shared helpers for trace construction
# ==============================================================================
# Frame and meta constructors used by every R/trace_*.R file. Single source of
# truth for the on-the-wire shape consumed by inst/htmlwidgets/lap_animate.js.
#
# Why these exist: before extraction, the frame field layout was inlined in
# each trace's local emit() closure - eight near-duplicates that could drift
# (and would drift the moment we added a new field like alpha or layer). With
# make_frame(), adding a field is one edit.
#
# Per-trace state (matching vectors, duals, prices, scanned sets, ...) still
# lives in the trace function's lexical scope; make_frame() is only the
# constructor for one trace frame, called from each trace's local emit().
# ==============================================================================

#' Construct a single trace frame
#'
#' Returns a list with the canonical fields consumed by lap_animate's
#' JavaScript renderer. Optional fields default to NULL or empty list so the
#' caller only names what is meaningful for the current algorithm.
#'
#' @keywords internal
#' @noRd
make_frame <- function(step, phase, description, matching,
                       dual_u = NULL, dual_v = NULL,
                       active_edges = list(), path = list()) {
  list(
    step         = as.integer(step),
    phase        = phase,
    description  = description,
    matching     = matching,
    dual_u       = dual_u,
    dual_v       = dual_v,
    active_edges = active_edges,
    path         = path
  )
}

#' Construct the meta block for a trace
#'
#' Wraps the algorithm-level constants (matrix, objective, descriptive text)
#' into the canonical meta layout. Single source of truth for meta fields.
#'
#' @keywords internal
#' @noRd
make_meta <- function(algorithm, n_rows, n_cols, cost_matrix, maximize,
                      total_cost, description) {
  list(
    algorithm   = algorithm,
    n_rows      = as.integer(n_rows),
    n_cols      = as.integer(n_cols),
    cost_matrix = cost_matrix,
    maximize    = isTRUE(maximize),
    total_cost  = as.numeric(total_cost),
    description = description
  )
}

#' Sum of matched edge costs (handles partial matchings)
#'
#' Used by traces to compute the objective from a matching vector. For
#' bottleneck algorithms compute max/min directly - this helper is sum only.
#'
#' @keywords internal
#' @noRd
matching_total_cost <- function(cost, matching) {
  m_int <- as.integer(matching)
  matched <- which(m_int > 0L)
  if (length(matched) == 0L) return(0)
  sum(cost[cbind(matched, m_int[matched])], na.rm = TRUE)
}

#' Convert NA/Inf cost entries to a finite big-M with masking
#'
#' Most dual-based algorithms can't operate on Inf or NA arithmetic, so each
#' trace replaces forbidden cells with a large finite penalty for its inner
#' loop while remembering which entries were forbidden. This helper centralises
#' the dance.
#'
#' Returns a list with:
#'   cost_work    - the work copy, sign-flipped for maximize, big-M for forbidden
#'   finite_mask  - logical, TRUE where original cost was finite
#'   big_m        - the penalty value substituted for forbidden cells
#'   scale        - the largest absolute finite cost (useful for eps choice)
#'
#' @keywords internal
#' @noRd
prepare_cost_work <- function(cost, maximize = FALSE) {
  cost_work <- if (maximize) -cost else cost
  finite_mask <- is.finite(cost_work)
  if (!any(finite_mask)) {
    stop("`cost` has no finite entries.", call. = FALSE)
  }
  scale <- max(abs(cost_work[finite_mask]))
  n <- nrow(cost); m <- ncol(cost)
  big <- (scale + 1) * (n + m + 1)
  cost_work[!finite_mask] <- big
  list(
    cost_work   = cost_work,
    finite_mask = finite_mask,
    big_m       = big,
    scale       = scale
  )
}

#' Standard input validation shared by every trace_*() function
#'
#' Throws on empty / non-numeric / NaN cost. Returns the matrix coerced to
#' numeric matrix form along with n, m.
#'
#' @keywords internal
#' @noRd
validate_cost_input <- function(cost, fn_name) {
  cost <- as.matrix(cost)
  if (!is.numeric(cost)) {
    stop(sprintf("%s: `cost` must be a numeric matrix.", fn_name), call. = FALSE)
  }
  n <- nrow(cost); m <- ncol(cost)
  if (n == 0L || m == 0L) {
    stop(sprintf("%s: cost matrix must have at least one row and one column.", fn_name),
         call. = FALSE)
  }
  if (any(is.nan(cost))) {
    stop(sprintf("%s: NaN not allowed in `cost`.", fn_name), call. = FALSE)
  }
  list(cost = cost, n = n, m = m)
}
