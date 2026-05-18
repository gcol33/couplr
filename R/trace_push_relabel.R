# ==============================================================================
# Reference trace for the "push_relabel" production method
# ==============================================================================
# Important: src/solvers/solve_push_relabel.cpp is *misnamed* in the current
# couplr codebase. The C++ file header says "Push-Relabel LAP solver" but the
# implementation is actually successive shortest paths with Dijkstra + Johnson
# potentials - the same algorithm as src/solvers/solve_csflow.cpp. See line 80
# of solve_push_relabel.cpp: "Push-Relabel with cost optimization / Uses
# successive shortest paths (Dijkstra with potentials)".
#
# Because lap_animate must faithfully reflect what the production solver does,
# this trace shares trace_csflow's algorithm body and only differs in the
# meta block. The pedagogically distinct Goldberg-Tarjan / Goldberg-Kennedy
# cost-scaling push-relabel ("CSA") lives in R/trace_csa.R: that file
# implements the canonical eps-relaxation with explicit eps-scaling phases,
# which is the algorithm the literature actually means by "push-relabel for
# min-cost flow".
#
# A followup PR could either rename src/solvers/solve_push_relabel.cpp to
# something accurate (e.g. solve_ssp_potentials.cpp) or implement actual
# push-relabel - either way this trace would be revised to match.
# ==============================================================================

#' @keywords internal
#' @noRd
trace_push_relabel <- function(cost, maximize = FALSE, ...) {
  out <- trace_csflow(cost, maximize = maximize, ...)
  out$meta$algorithm <- "push_relabel"
  out$meta$description <- paste0(
    "NOTE: the production C++ solver dispatched by assignment(method = \"push_relabel\") ",
    "currently implements successive shortest paths with Johnson potentials, not the ",
    "Goldberg-Tarjan push-relabel algorithm. This trace mirrors the C++ behavior. ",
    "For the canonical push-relabel / eps-relaxation algorithm with cost-scaling phases, ",
    "see lap_animate(cost, method = \"csa\")."
  )
  out
}

register_trace("push_relabel", trace_push_relabel)
