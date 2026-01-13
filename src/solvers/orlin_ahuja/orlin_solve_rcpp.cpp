// src/solvers/orlin_ahuja/orlin_solve.cpp
// Rcpp wrapper for Orlin-Ahuja solver - calls pure C++ implementation

#include <Rcpp.h>
#include "orlin_solve.h"
#include "../../core/lap_error.h"
#include "../../core/lap_utils_rcpp.h"

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

// Helper: Convert lap::LapResult to Rcpp::List (with 1-based indexing)
static Rcpp::List lap_result_to_rcpp(const lap::LapResult& result,
                                      const Rcpp::NumericMatrix& original_cost) {
    const int n = static_cast<int>(result.assignment.size());

    // Convert 0-based to 1-based
    Rcpp::IntegerVector match(n);
    for (int i = 0; i < n; ++i) {
        match[i] = (result.assignment[i] >= 0) ? (result.assignment[i] + 1) : 0;
    }

    // Use compute_total_cost for consistency with other solvers
    double total = compute_total_cost(original_cost, match);

    return make_result(match, total);
}

// Rcpp-exported wrapper
Rcpp::List oa_solve_impl(Rcpp::NumericMatrix cost, bool maximize = false,
                         double alpha = 5.0, int auction_rounds = 10) {
    try {
        // Convert to pure C++ types
        lap::CostMatrix cm = rcpp_to_cost_matrix(cost);

        // Call pure C++ solver
        lap::LapResult result = lap::solve_orlin(cm, maximize, alpha, auction_rounds);

        // Convert back to Rcpp
        return lap_result_to_rcpp(result, cost);

    } catch (const lap::LapException& e) {
        Rcpp::stop(e.what());
    }

    // Should never reach here
    return Rcpp::List();
}
