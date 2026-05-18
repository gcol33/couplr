# ==============================================================================
# Placeholder Trace Functions
# ==============================================================================
# These stub traces emit a two-frame trace (empty -> final matching) for each
# algorithm so the lap_animate() htmlwidget renders end-to-end before real
# step-by-step traces are written. They route through assignment() to obtain
# the final matching, so correctness is inherited from the production solver.
#
# Replace these algorithm-by-algorithm with real R reference implementations
# that emit a frame per algorithmic step (R/trace_hungarian.R, R/trace_jv.R,
# R/trace_auction.R, R/trace_gabow_tarjan.R, ...).
# ==============================================================================

#' @keywords internal
#' @noRd
make_stub_trace <- function(algo_name, description) {
  force(algo_name); force(description)
  function(cost, maximize = FALSE, ...) {
    cost <- as.matrix(cost)
    n <- nrow(cost); m <- ncol(cost)

    res <- assignment(cost, maximize = maximize, method = algo_name)

    matching_final <- integer(n)
    src_match <- as.integer(res$match)
    if (length(src_match) >= 1L) {
      matching_final[seq_along(src_match)] <- src_match
    }
    matching_empty <- integer(n)

    list(
      meta = list(
        algorithm   = algo_name,
        n_rows      = n,
        n_cols      = m,
        cost_matrix = cost,
        maximize    = maximize,
        total_cost  = as.numeric(res$total_cost),
        description = description
      ),
      frames = list(
        list(
          step         = 0L,
          phase        = "init",
          description  = "Initial state: no assignments.",
          matching     = matching_empty,
          dual_u       = NULL,
          dual_v       = NULL,
          active_edges = list(),
          path         = list()
        ),
        list(
          step         = 1L,
          phase        = "final",
          description  = sprintf(
            "Optimal assignment (total cost %.6g). Stub trace — real step-by-step animation pending for this algorithm.",
            res$total_cost
          ),
          matching     = matching_final,
          dual_u       = NULL,
          dual_v       = NULL,
          active_edges = list(),
          path         = list()
        )
      )
    )
  }
}

# ------------------------------------------------------------------------------
# Algorithm metadata (1-line teaching descriptions, no numerics or hype)
# ------------------------------------------------------------------------------
.algo_descriptions <- list(
  hungarian      = "Kuhn-Munkres primal-dual: maintain dual potentials u, v; augment matching through tight (u+v=c) edges; update duals when blocked.",
  munkres        = "Matrix-form Kuhn-Munkres (Munkres 1957): star and prime zeros; cover lines until a feasible assignment of zeros is found.",
  jv             = "Jonker-Volgenant: column reduction grabs cheap assignments first, reduction transfer fixes collisions, then Dijkstra-style shortest-path augmentation completes the matching.",
  auction        = "Bertsekas auction: unassigned workers bid on their most-valued job; price rises by bid margin plus epsilon; iterate to epsilon-equilibrium.",
  auction_scaled = "Auction with epsilon-scaling: start with coarse epsilon for big moves, refine epsilon downward across phases until optimal.",
  gabow_tarjan   = "Gabow-Tarjan bit-scaling: process integer costs bit-by-bit from most-significant to least-significant; each phase only needs 1-feasibility of the dual."
)

# ------------------------------------------------------------------------------
# Register stubs at package load, but ONLY if no real trace has registered
# itself for the same method. This way real trace files (R/trace_hungarian.R,
# R/trace_jv.R, ...) can register before *or* after this file is sourced
# without worrying about alphabetical file ordering.
# ------------------------------------------------------------------------------
local({
  for (algo in names(.algo_descriptions)) {
    if (!exists(algo, envir = .trace_registry, inherits = FALSE)) {
      register_trace(algo, make_stub_trace(algo, .algo_descriptions[[algo]]))
    }
  }
})
