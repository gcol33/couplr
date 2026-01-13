// src/solvers/network_simplex/solve_network_simplex.cpp
// Pure C++ Network Simplex LAP solver - NO Rcpp dependencies

#include "solve_network_simplex.h"
#include "../../core/lap_error.h"
#include "../../core/lap_utils.h"
#include "ns_types.h"
#include "ns_graph.h"
#include "ns_init.h"
#include "ns_pivot.h"
#include <vector>
#include <limits>
#include <cmath>

namespace lap {

// Convert CostMatrix to flat array for network simplex functions
// Network simplex expects column-major layout (R convention)
static std::vector<double> convert_to_column_major(const CostMatrix& cost) {
    int n = cost.nrow;
    int m = cost.ncol;
    std::vector<double> result(n * m);

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            // cost.at(i,j) is row-major access
            // result[i + j*n] is column-major storage
            result[i + j * n] = cost.at(i, j);
        }
    }

    return result;
}

LapResult solve_network_simplex(const CostMatrix& cost, bool maximize) {
    const int n_rows = cost.nrow;
    const int n_cols = cost.ncol;

    // Handle empty case
    if (n_rows == 0) {
        return LapResult({}, 0.0, "optimal");
    }

    // Dimension check
    if (n_rows > n_cols) {
        LAP_THROW_DIMENSION("Infeasible: number of rows greater than number of columns");
    }

    // Prepare working costs (negated if maximize, BIG for forbidden)
    CostMatrix work = prepare_for_solve(cost, maximize);

    // Check feasibility
    ensure_each_row_has_option(work.mask, n_rows, n_cols);

    // Convert to column-major for network simplex internal functions
    std::vector<double> cost_data = convert_to_column_major(work);

    // Build network
    couplr::ns::NSState state;
    couplr::ns::build_assignment_network(state, cost_data.data(), n_rows, n_cols);

    // Initialize spanning tree with greedy solution
    couplr::ns::initialize_spanning_tree_greedy(state);

    // Compute initial potentials
    couplr::ns::compute_potentials(state);

    // Main simplex loop
    int max_iterations = state.num_arcs * state.num_nodes;

    for (int iter = 0; iter < max_iterations; ++iter) {
        // Find entering arc
        int entering = couplr::ns::find_entering_arc(state);

        if (entering == couplr::ns::NO_ARC) {
            // Optimal: no arc with negative reduced cost
            break;
        }

        // Find leaving arc and compute delta
        couplr::ns::PivotInfo info = couplr::ns::find_leaving_arc(state, entering);

        // Perform pivot
        couplr::ns::do_pivot(state, info);
    }

    // Extract assignment from solution
    couplr::ns::NSResult ns_result = couplr::ns::extract_assignment(state);

    // Convert NSResult to LapResult
    std::vector<int> assignment = std::move(ns_result.assignment);

    // Verify matching and compute total cost using ORIGINAL costs
    double total = 0.0;
    for (int i = 0; i < n_rows; ++i) {
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
