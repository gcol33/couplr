// src/solvers/network_simplex/solve_network_simplex_rcpp.cpp
// Rcpp wrapper for the network-simplex solver - delegates to the pure C++
// implementation. The algorithm lives in solve_network_simplex.cpp
// (lap::solve_network_simplex), the single source of truth exercised by
// cpp_tests, so the shipped path and the tested path are identical. The pure
// copy carries the O(n^2) pivot bound and the maximize/forbidden handling; the
// R-level wrapper negates for maximize and marks forbidden cells before calling.

#include <Rcpp.h>
#include "solve_network_simplex.h"
#include "../../core/lap_error.h"
#include "../../core/lap_utils_rcpp.h"

// Implementation function called from rcpp_interface.cpp
Rcpp::List solve_network_simplex_rcpp(const Rcpp::NumericMatrix& cost_matrix) {
    try {
        lap::CostMatrix cm = rcpp_to_cost_matrix(cost_matrix);
        lap::LapResult result = lap::solve_network_simplex(cm, false);

        Rcpp::List out = lap_result_to_rcpp(result, cost_matrix);
        out["status"] = "optimal";
        out["method_used"] = "network_simplex";
        return out;
    } catch (const lap::LapException& e) {
        Rcpp::stop(e.what());
    }
    return Rcpp::List();  // unreachable
}
