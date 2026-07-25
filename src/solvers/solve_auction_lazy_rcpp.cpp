// src/solvers/solve_auction_lazy_rcpp.cpp
// Rcpp wrapper for the lazy (on-demand) Auction solver - calls the pure C++
// lap::solve_auction(LazyCostMatrix) overload. Mirrors solve_jv_lazy_rcpp.cpp.

#include <Rcpp.h>
#include "solve_auction.h"
#include "../core/lap_error.h"
#include "../core/lap_utils_rcpp.h"

Rcpp::List solve_auction_lazy_impl(Rcpp::NumericMatrix left_mat, Rcpp::NumericMatrix right_mat,
                                   std::string metric, Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                                   double max_distance, Rcpp::List calipers,
                                   Rcpp::CharacterVector var_names, bool maximize,
                                   Rcpp::Nullable<double> eps) {
    try {
        lap::LazyCostMatrix cm = rcpp_to_lazy_cost_matrix(
            left_mat, right_mat, metric, inv_cov, max_distance, calipers, var_names, maximize);

        double eps_in = eps.isNotNull() ? Rcpp::as<double>(eps.get())
                                        : std::numeric_limits<double>::quiet_NaN();
        lap::LapResult result = lap::solve_auction(cm, eps_in);

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
