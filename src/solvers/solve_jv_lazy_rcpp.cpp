// src/solvers/solve_jv_lazy_rcpp.cpp
// Rcpp wrapper for the lazy (on-demand) JV solver. The specification names a
// built-in metric or carries a user's distance function, and the same solve runs
// over whichever cost source that is.

#include <Rcpp.h>
#include "solve_jv_duals_impl.h"
#include "../core/lap_error.h"
#include "../core/lap_utils_rcpp.h"

#include <variant>

Rcpp::List solve_jv_lazy_impl(Rcpp::NumericMatrix left_mat, Rcpp::NumericMatrix right_mat,
                              SEXP metric, Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                              double max_distance, Rcpp::List calipers,
                              Rcpp::CharacterVector var_names, bool maximize) {
    try {
        const LazySource source = rcpp_lazy_source(left_mat, right_mat, metric, inv_cov,
                                                   max_distance, calipers, var_names,
                                                   maximize);

        // The total is computed from the original (unnegated) distances inside
        // the solve; there is no materialized matrix to recompute it from.
        const lap::LapResult result = std::visit([](const auto& cm) {
            return lap::solve_jv_duals_prepared(cm).solution;
        }, source);

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
