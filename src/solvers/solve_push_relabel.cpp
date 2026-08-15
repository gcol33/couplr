// src/solvers/solve_push_relabel.cpp
// Pure C++ Push-Relabel LAP solver - NO Rcpp dependencies
//
// The network is the flow model's one-to-one design and the search is
// solve_min_cost_flow(). What belongs to this solver is how the cost matrix is
// prepared, the relaxation tolerance its answers were produced under, and what
// it calls a shortfall.

#include "solve_push_relabel.h"
#include "../core/lap_error.h"
#include "../core/lap_utils.h"
#include "../flow/flow_assign.h"
#include <cmath>
#include <vector>

namespace lap {

LapResult solve_push_relabel(const CostMatrix& cost, bool maximize) {
    const int n = static_cast<int>(cost.nrow);
    const int m = static_cast<int>(cost.ncol);

    // Handle empty case
    if (n == 0) {
        return LapResult({}, 0.0, "optimal");
    }

    lap::require_rows_fit_cols(n, m);

    // Prepare working costs (negated if maximize, BIG for forbidden)
    CostMatrix work = prepare_for_solve(cost, maximize);

    // Check feasibility: each row must have at least one allowed column
    ensure_each_row_has_option(work.mask, n, m);

    // A path relaxes on an improvement of more than TOL, which is a third
    // predicate again: csflow requires 1e-18 and ssp requires nothing beyond
    // strictness. It selects which of several equally-cheap shortest paths is
    // taken, so it stays with this solver.
    FlowOptions opts;
    opts.relax_eps = TOL;
    opts.return_potentials = false;

    SourceOracle<CostMatrix> oracle(work);
    const AssignmentFlow flow = solve_assignment_flow(oracle, opts);

    if (flow.n_matched < n) {
        LAP_THROW_INFEASIBLE("Infeasible: could not find augmenting path");
    }

    std::vector<int> assignment = flow.match;

    // Verify matching and compute total cost using ORIGINAL costs
    double total = 0.0;
    for (int i = 0; i < n; ++i) {
        int j = assignment[i];
        if (j < 0) {
            LAP_THROW_INFEASIBLE("Could not find full matching");
        }
        if (!cost.allowed(i, j)) {
            LAP_THROW_INFEASIBLE("Chosen forbidden edge");
        }
        double c = cost.at(i, j);
        if (!std::isfinite(c)) {
            LAP_THROW_INFEASIBLE("Chosen edge has non-finite cost");
        }
        total += c;
    }

    return LapResult(std::move(assignment), total, "optimal");
}

}  // namespace lap
