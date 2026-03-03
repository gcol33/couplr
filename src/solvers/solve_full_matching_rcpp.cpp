// src/solvers/solve_full_matching_rcpp.cpp
// Rcpp wrapper for optimal full matching via min-cost max-flow.

// [[Rcpp::plugins(cpp17)]]
#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <climits>
#include "solve_full_matching.h"

using namespace Rcpp;

Rcpp::List solve_full_matching_impl(
    Rcpp::NumericMatrix cost,
    int min_controls,
    int max_controls_val)
{
    int n_left = cost.nrow();
    int n_right = cost.ncol();

    // Convert R matrix to row-major vector (cost[i * n_right + j])
    std::vector<double> cost_vec(n_left * n_right);
    for (int i = 0; i < n_left; ++i) {
        for (int j = 0; j < n_right; ++j) {
            double val = cost(i, j);
            // R's NA and NaN -> Inf (forbidden)
            if (ISNAN(val)) {
                cost_vec[i * n_right + j] = std::numeric_limits<double>::infinity();
            } else {
                cost_vec[i * n_right + j] = val;
            }
        }
    }

    lap::FullMatchResult res = lap::solve_full_matching(
        cost_vec, n_left, n_right, min_controls, max_controls_val
    );

    // Convert to 1-based R indexing: -1 (unmatched) -> 0, 0-based -> 1-based
    Rcpp::IntegerVector group_of_right(n_right);
    for (int j = 0; j < n_right; ++j) {
        group_of_right[j] = (res.right_to_group[j] >= 0) ? res.right_to_group[j] + 1 : 0;
    }
    Rcpp::IntegerVector group_of_left(n_left);
    for (int i = 0; i < n_left; ++i) {
        group_of_left[i] = (res.left_to_group[i] >= 0) ? res.left_to_group[i] + 1 : 0;
    }

    return Rcpp::List::create(
        Rcpp::Named("group_of_right") = group_of_right,
        Rcpp::Named("group_of_left") = group_of_left,
        Rcpp::Named("total_cost") = res.total_cost,
        Rcpp::Named("status") = res.status,
        Rcpp::Named("n_groups") = res.n_groups
    );
}
