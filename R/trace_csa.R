# ==============================================================================
# The trace for the "csa" production method
# ==============================================================================
# src/solvers/solve_csa.cpp runs the epsilon-scaling auction: the same
# epsilon-scaling outer loop as solve_auction_scaled_impl, with alpha = 7 and
# eps_final = min(1e-6, 1/n^2), around the same bid-by-reduced-cost inner loop
# that lowers a price by gamma + eps. Goldberg-Kennedy's cost-scaling
# assignment discharges excess with push and relabel on a residual graph
# instead; assignment(method = "push_relabel") runs that inner loop.
#
# A trace shows what the production solver does, so this one shares
# trace_auction_scaled's body and states in its meta block which of the two
# inner loops ran.
# ==============================================================================

#' @keywords internal
#' @noRd
trace_csa <- function(cost, maximize = FALSE, ...) {
  out <- trace_auction_scaled(cost, maximize = maximize, ...)
  out$meta$algorithm <- "csa"
  out$meta$description <- paste0(
    "Epsilon-scaling auction (Bertsekas & Eckstein 1988), which is what ",
    "assignment(method = \"csa\") dispatches: the same solver as ",
    "method = \"auction_scaled\". The outer structure is cost scaling - a large ",
    "eps for big moves, refined toward eps < 1/n where an eps-optimal answer on ",
    "integer costs is optimal - and the inner loop is the auction, where an ",
    "unassigned person bids and the price of the object it takes falls by the ",
    "bid margin plus eps. Goldberg-Kennedy's cost-scaling assignment keeps that ",
    "outer structure and discharges excess with push and relabel instead; ",
    "lap_animate(cost, method = \"push_relabel\") shows that inner loop."
  )
  out
}

register_trace("csa", trace_csa)
