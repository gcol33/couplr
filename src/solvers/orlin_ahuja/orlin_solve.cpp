// src/solvers/orlin_ahuja/orlin_solve.cpp
// Production Orlin-Ahuja solver implementation

#include <Rcpp.h>
#include <chrono>
#include "orlin_types.h"
#include "orlin_scaling.h"

using namespace Rcpp;
using namespace orlin;

// Full Orlin-Ahuja solve with detailed statistics
List oa_solve_impl(NumericMatrix cost_r, double alpha = 5.0,
                   int auction_rounds = 10) {
    int n = cost_r.nrow();
    int m = cost_r.ncol();

    std::vector<Cost> cost(n * m);
    for (int ii = 0; ii < n; ++ii) {
        for (int jj = 0; jj < m; ++jj) {
            cost[ii * m + jj] = cost_r(ii, jj);
        }
    }

    ScalingParams params;
    params.alpha = alpha;
    params.auction_rounds_per_scale = auction_rounds;

    auto start = std::chrono::high_resolution_clock::now();
    OrlinResult result = solve_orlin_ahuja(cost, n, m, params);
    auto end = std::chrono::high_resolution_clock::now();

    double time_us = std::chrono::duration<double, std::micro>(end - start).count();

    // Convert matching to R format (1-based)
    IntegerVector row_to_col_r(n);
    for (int i = 0; i < n; ++i) {
        row_to_col_r[i] = result.row_to_col[i] == UNASSIGNED ? 0 : result.row_to_col[i] + 1;
    }

    // Extract per-scale statistics
    int n_scales = result.scales.size();
    IntegerVector scale_nums(n_scales);
    NumericVector scale_eps(n_scales);
    IntegerVector scale_auction_rounds(n_scales);
    IntegerVector scale_auction_bids(n_scales);
    IntegerVector scale_ssp_augs(n_scales);
    IntegerVector scale_edges(n_scales);

    for (int s = 0; s < n_scales; ++s) {
        scale_nums[s] = result.scales[s].scale_number;
        scale_eps[s] = result.scales[s].epsilon;
        scale_auction_rounds[s] = result.scales[s].auction_rounds;
        scale_auction_bids[s] = result.scales[s].auction_bids;
        scale_ssp_augs[s] = result.scales[s].ssp_augmentations;
        scale_edges[s] = result.scales[s].edges_scanned;
    }

    DataFrame scale_stats = DataFrame::create(
        Named("scale") = scale_nums,
        Named("epsilon") = scale_eps,
        Named("auction_rounds") = scale_auction_rounds,
        Named("auction_bids") = scale_auction_bids,
        Named("ssp_augmentations") = scale_ssp_augs,
        Named("edges_scanned") = scale_edges
    );

    return List::create(
        Named("row_to_col") = row_to_col_r,
        Named("total_cost") = result.total_cost,
        Named("optimal") = result.optimal,
        Named("n_scales") = result.total_scales,
        Named("total_augmentations") = result.total_augmentations,
        Named("scale_stats") = scale_stats,
        Named("time_us") = time_us
    );
}
