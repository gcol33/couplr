// Network Simplex algorithm for the assignment problem
// Implementation based on LEMON library and Király-Kovács (2012)

#include "ns_types.h"
#include "ns_graph.h"
#include "ns_init.h"
#include "ns_pivot.h"
#include <Rcpp.h>

namespace couplr {
namespace ns {

// Main solver function
NSResult solve_network_simplex_impl(const Rcpp::NumericMatrix& cost_matrix) {
    int n_rows = cost_matrix.nrow();
    int n_cols = cost_matrix.ncol();

    // Handle edge cases
    if (n_rows == 0 || n_cols == 0) {
        NSResult result;
        result.optimal = false;
        result.status = "empty";
        result.total_cost = 0.0;
        result.pivot_count = 0;
        return result;
    }

    if (n_rows > n_cols) {
        // More rows than columns - some rows cannot be matched
        NSResult result;
        result.optimal = false;
        result.status = "infeasible";
        result.total_cost = 0.0;
        result.pivot_count = 0;
        result.assignment.resize(n_rows, -1);
        return result;
    }

    // Build network
    NSState state;
    build_assignment_network(state, &cost_matrix[0], n_rows, n_cols);

    // Initialize spanning tree with greedy solution
    initialize_spanning_tree_greedy(state);

    // Compute initial potentials
    compute_potentials(state);

    // Main simplex loop
    int max_iterations = state.num_arcs * state.num_nodes;

    for (int iter = 0; iter < max_iterations; ++iter) {
        // Find entering arc
        int entering = find_entering_arc(state);

        if (entering == NO_ARC) {
            // Optimal: no arc with negative reduced cost
            break;
        }

        // Find leaving arc and compute delta
        PivotInfo info = find_leaving_arc(state, entering);

        // Perform pivot
        do_pivot(state, info);
    }

    // Extract assignment from solution
    return extract_assignment(state);
}

} // namespace ns
} // namespace couplr

// Implementation function called from rcpp_interface.cpp
Rcpp::List solve_network_simplex_rcpp(const Rcpp::NumericMatrix& cost_matrix) {
    couplr::ns::NSResult result = couplr::ns::solve_network_simplex_impl(cost_matrix);

    // Convert to 1-indexed for R
    Rcpp::IntegerVector assignment(result.assignment.size());
    for (size_t i = 0; i < result.assignment.size(); ++i) {
        assignment[i] = result.assignment[i] + 1;  // 1-indexed
    }

    return Rcpp::List::create(
        Rcpp::Named("match") = assignment,
        Rcpp::Named("total_cost") = result.total_cost,
        Rcpp::Named("status") = result.status,
        Rcpp::Named("method_used") = "network_simplex",
        Rcpp::Named("n_pivots") = result.pivot_count
    );
}
