// src/solvers/solve_jv_duals.cpp
// Rcpp wrapper for JV duals solver - calls pure C++ implementation

#include <Rcpp.h>
#include "solve_jv_duals.h"
#include "../core/lap_error.h"
#include "../core/lap_utils_rcpp.h"

// Helper: Convert Rcpp::NumericMatrix to lap::CostMatrix
static lap::CostMatrix rcpp_to_cost_matrix(const Rcpp::NumericMatrix& cost) {
    const int n = cost.nrow();
    const int m = cost.ncol();

    lap::CostMatrix cm(n, m);

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            double v = cost(i, j);
            cm.at(i, j) = v;
            cm.mask[i * m + j] = (R_finite(v)) ? 1 : 0;
        }
    }

    return cm;
}

// Helper: Convert lap::DualResult to Rcpp::List (with 1-based indexing for assignment)
static Rcpp::List dual_result_to_rcpp(const lap::DualResult& result,
                                       const Rcpp::NumericMatrix& original_cost) {
    const int n = static_cast<int>(result.solution.assignment.size());

    // Convert 0-based to 1-based for assignment
    Rcpp::IntegerVector match(n);
    for (int i = 0; i < n; ++i) {
        match[i] = (result.solution.assignment[i] >= 0) ? (result.solution.assignment[i] + 1) : 0;
    }

    // Use compute_total_cost for consistency with other solvers
    double total = compute_total_cost(original_cost, match);

    // Dual variables stay 0-based
    Rcpp::NumericVector u(result.u.begin(), result.u.end());
    Rcpp::NumericVector v(result.v.begin(), result.v.end());

    return Rcpp::List::create(
        Rcpp::Named("match") = match,
        Rcpp::Named("total_cost") = total,
        Rcpp::Named("u") = u,
        Rcpp::Named("v") = v
    );
}

// Rcpp-exported wrapper
Rcpp::List solve_jv_duals_impl(Rcpp::NumericMatrix cost, bool maximize) {
    try {
        // Convert to pure C++ types
        lap::CostMatrix cm = rcpp_to_cost_matrix(cost);

        // Call pure C++ solver
        lap::DualResult result = lap::solve_jv_duals(cm, maximize);

        // Convert back to Rcpp
        return dual_result_to_rcpp(result, cost);

    } catch (const lap::LapException& e) {
        Rcpp::stop(e.what());
    }

    // Should never reach here
    return Rcpp::List();
}
