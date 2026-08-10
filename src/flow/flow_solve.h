// src/flow/flow_solve.h
// The one min-cost flow solver every design routes through.
// Pure C++ - NO Rcpp dependencies, same rule as lap_types.h.
//
// Successive shortest paths with Johnson potentials over a residual graph.
// Capacities are integral, so every augmentation moves a whole number of units
// and the flow returned is exact rather than a rounded interior point.
//
// The dual variables come back with the flow because they are what a
// certificate and a warm start are made of, and because a design that is not a
// one-to-one assignment has no other way to produce them.
#pragma once

#include "flow_problem.h"

#include <cstdint>
#include <string>
#include <vector>

namespace lap {

struct FlowResult {
    // Aligned to prob.arcs after expansion, so block_arcs[b].rc reads a matched
    // pair straight out of it.
    std::vector<int64_t> flow;

    // One per node, normalized so potential[0] == 0. Potentials are defined up
    // to a constant per connected component; the assignment lowering needs one
    // particular representative, so fixing the gauge is part of the contract
    // rather than an implementation detail. Empty when FlowOptions asked for no
    // potentials.
    std::vector<double> potential;

    double  total_cost = 0.0;

    // What the b-flow had to move, and what it managed to move. Equal exactly
    // when the problem has a feasible flow, because the reduction to a
    // max-flow makes feasibility and saturation the same question.
    int64_t flow_sent = 0;
    int64_t flow_required = 0;

    // From solver_status_values(): "optimal", "partial", "infeasible" or
    // "iteration_limit". Derived from the termination state, never asserted.
    std::string status = "infeasible";

    int64_t n_augmentations = 0;
};

struct FlowOptions {
    // Zero threshold for the complementary-slackness repair a warm start needs.
    double tol = 1e-12;

    // Dijkstra relaxation slack: an arc relaxes when
    // dist[tail] + cbar + relax_eps < dist[head]. The value decides which of
    // several equally-cheap shortest paths is taken and therefore which of
    // several optimal flows comes back, so it belongs to the caller rather than
    // to the solver: two callers with different predicates give different but
    // equally optimal answers, and neither is free to adopt the other's.
    double relax_eps = 1e-18;

    // 0 derives the bound from the problem: every augmentation moves at least
    // one unit, so flow_required augmentations suffice and no more are
    // possible.
    int64_t max_augmentations = 0;

    bool return_potentials = true;
};

// Solve `prob`, expanding its blocks in place if that has not happened yet.
//
// Throws lap::DimensionException on a malformed problem. A problem with no
// feasible flow is not malformed: it comes back with status "partial" or
// "infeasible" and with the maximum flow that could be placed.
FlowResult solve_min_cost_flow(FlowProblem& prob,
                               const FlowOptions& opts = FlowOptions());

}  // namespace lap
