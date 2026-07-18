// src/solvers/orlin_ahuja/orlin_solve.cpp
// Rcpp wrapper for Orlin-Ahuja solver - calls pure C++ implementation

#include <Rcpp.h>
#include "orlin_solve.h"
#include "../../core/lap_error.h"
#include "../../core/lap_utils_rcpp.h"

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
