# ==============================================================================
# Reference trace for the "csa" production method (Goldberg-Kennedy cost-scaling
# assignment - actually implemented as epsilon-scaled auction in couplr)
# ==============================================================================
# Important: src/solvers/solve_csa.cpp is *misnamed* in the current couplr
# codebase. The file header says "Goldberg-Kennedy Cost-Scaling Assignment (CSA)
# Algorithm" but the implementation is structurally identical to
# src/solvers/solve_auction_rcpp.cpp::solve_auction_scaled_impl: same
# epsilon-scaling outer loop with alpha = 7, same eps_final = min(1e-6, 1/n^2),
# same inner bid-by-reduced-cost auction (decrease price by gamma + eps).
# Goldberg-Kennedy's actual CSA is push-relabel-based (push and relabel on a
# residual graph), not auction-based.
#
# Because lap_animate must faithfully reflect what the production solver does,
# this trace shares trace_auction_scaled's algorithm body and only differs in
# the meta block, where the description is honest about the situation.
#
# A followup PR could either (a) rename src/solvers/solve_csa.cpp to
# something like solve_scaled_auction_csa.cpp and document it as a CSA variant,
# (b) implement Goldberg-Kennedy's actual cost-scaling push-relabel, or
# (c) recognise csa and auction_scaled as aliases at the dispatch layer.
# ==============================================================================

#' @keywords internal
#' @noRd
trace_csa <- function(cost, maximize = FALSE, ...) {
  out <- trace_auction_scaled(cost, maximize = maximize, ...)
  out$meta$algorithm <- "csa"
  out$meta$description <- paste0(
    "NOTE: the production C++ solver dispatched by assignment(method = \"csa\") ",
    "currently implements epsilon-scaling auction (Bertsekas & Eckstein 1988), ",
    "structurally identical to method = \"auction_scaled\". Goldberg-Kennedy's ",
    "actual CSA algorithm uses push-relabel rather than auction in the inner loop. ",
    "This trace honestly reflects what the C++ does. The pedagogically distinct ",
    "outer epsilon-scaling structure - large eps for big moves, refined toward ",
    "eps < 1/n for exact integer optimality - is the same in both formulations."
  )
  out
}

register_trace("csa", trace_csa)
