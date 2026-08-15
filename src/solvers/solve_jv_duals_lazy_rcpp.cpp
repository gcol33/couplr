// src/solvers/solve_jv_duals_lazy_rcpp.cpp
// Rcpp wrapper for the lazy (on-demand) JV duals solver - calls the pure C++
// lap::solve_jv_duals(LazyCostMatrix) overload. Mirrors solve_jv_lazy_rcpp.cpp,
// and returns the u/v the dense wrapper returns so verify_assignment() reads
// the same shape on either cost source.

#include <Rcpp.h>
#include "solve_jv_duals.h"
#include "../core/lap_error.h"
#include "../core/lap_utils_rcpp.h"

Rcpp::List solve_jv_duals_lazy_impl(Rcpp::NumericMatrix left_mat,
                                    Rcpp::NumericMatrix right_mat,
                                    std::string metric,
                                    Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                                    double max_distance, Rcpp::List calipers,
                                    Rcpp::CharacterVector var_names, bool maximize) {
    try {
        lap::LazyCostMatrix cm = rcpp_to_lazy_cost_matrix(
            left_mat, right_mat, metric, inv_cov, max_distance, calipers, var_names, maximize);

        lap::DualResult result = lap::solve_jv_duals(cm);

        // total_cost is computed from the original (unnegated) distances inside
        // solve_jv_duals(LazyCostMatrix); there is no materialized matrix to
        // recompute it from, which is the point of the lazy path.
        const int n = static_cast<int>(result.solution.assignment.size());
        Rcpp::IntegerVector match(n);
        for (int i = 0; i < n; ++i) {
            match[i] = (result.solution.assignment[i] >= 0)
                           ? (result.solution.assignment[i] + 1)
                           : 0;
        }

        return Rcpp::List::create(
            Rcpp::Named("match") = match,
            Rcpp::Named("total_cost") = result.solution.total_cost,
            Rcpp::Named("u") = Rcpp::NumericVector(result.u.begin(), result.u.end()),
            Rcpp::Named("v") = Rcpp::NumericVector(result.v.begin(), result.v.end())
        );

    } catch (const lap::LapException& e) {
        Rcpp::stop(e.what());
    }

    return Rcpp::List();
}
