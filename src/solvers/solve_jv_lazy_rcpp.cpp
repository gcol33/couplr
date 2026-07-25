// src/solvers/solve_jv_lazy_rcpp.cpp
// Rcpp wrapper for the lazy (on-demand) JV solver - calls the pure C++
// lap::solve_jv(LazyCostMatrix) overload instead of materializing a dense
// cost matrix at the R/C++ boundary. Kept as a separate file (mirroring
// solve_jv_rcpp.cpp) rather than adding a branch to the existing wrapper,
// for a smaller, easier-to-review diff.

#include <Rcpp.h>
#include "solve_jv.h"
#include "../core/lap_error.h"
#include "../core/lap_utils_rcpp.h"

Rcpp::List solve_jv_lazy_impl(Rcpp::NumericMatrix left_mat, Rcpp::NumericMatrix right_mat,
                              std::string metric, Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                              double max_distance, Rcpp::List calipers,
                              Rcpp::CharacterVector var_names, bool maximize) {
    try {
        lap::LazyCostMatrix cm = rcpp_to_lazy_cost_matrix(
            left_mat, right_mat, metric, inv_cov, max_distance, calipers, var_names, maximize);

        lap::LapResult result = lap::solve_jv(cm);

        // total_cost is already computed from original (unnegated) distances
        // inside solve_jv(LazyCostMatrix) -- no separate "recompute from
        // original cost matrix" step is needed (there is no materialized
        // matrix to recompute from).
        std::vector<int> match(result.assignment.size());
        for (size_t i = 0; i < result.assignment.size(); ++i) {
            match[i] = (result.assignment[i] >= 0) ? (result.assignment[i] + 1) : 0;
        }
        return make_result(match, result.total_cost);

    } catch (const lap::LapException& e) {
        Rcpp::stop(e.what());
    }

    return Rcpp::List();
}
