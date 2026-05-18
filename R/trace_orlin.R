# ==============================================================================
# Reference trace for the "orlin" production method (Orlin-Ahuja
# strongly-polynomial LAP - actually implemented as plain SSP in couplr)
# ==============================================================================
# Important: src/solvers/orlin_ahuja/orlin_solve.cpp is *misnamed* in the
# current couplr codebase. The file claims to be the "Orlin-Ahuja LAP solver"
# but the implementation calls ssp_augment_once in a loop with no scaling
# phases and no auction warm-up - i.e. it's plain successive shortest paths
# with Dijkstra. The `alpha` and `auction_rounds` parameters of solve_orlin
# are declared but unused. The true Orlin-Ahuja algorithm (Orlin & Ahuja
# 1992) interleaves SSP with eps-scaling phases for O(sqrt(V) * E * log(VC))
# strongly-polynomial running time, which is not what is implemented here.
#
# Because lap_animate must faithfully reflect what the production solver does,
# this trace shares trace_csflow's algorithm body and only differs in the
# meta block.
#
# A followup PR could either rename src/solvers/orlin_ahuja/orlin_solve.cpp,
# remove the unused alpha/auction_rounds parameters, or implement the actual
# Orlin-Ahuja scaling algorithm.
# ==============================================================================

#' @keywords internal
#' @noRd
trace_orlin <- function(cost, maximize = FALSE, ...) {
  out <- trace_csflow(cost, maximize = maximize, ...)
  out$meta$algorithm <- "orlin"
  out$meta$description <- paste0(
    "NOTE: the production C++ solver dispatched by assignment(method = \"orlin\") ",
    "currently implements plain successive shortest paths (Dijkstra + Johnson ",
    "potentials), not the Orlin-Ahuja (1992) eps-scaling SSP that the name ",
    "suggests. The alpha and auction_rounds parameters declared by solve_orlin ",
    "are unused. This trace honestly reflects what the C++ does. The actual ",
    "Orlin-Ahuja algorithm would interleave SSP with scaling phases for ",
    "strongly-polynomial O(sqrt(V) * E * log(VC)) running time."
  )
  out
}

register_trace("orlin", trace_orlin)
