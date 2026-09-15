// src/solvers/solve_csa.cpp
// Cost-scaling assignment entry point - NO Rcpp dependencies

#include "solve_csa.h"
#include "solve_auction.h"

namespace lap {

LapResult solve_csa(const CostMatrix& cost, bool maximize) {
    return solve_auction_scaled_params(cost, maximize,
                                       /*initial_epsilon_factor=*/1.0,
                                       /*alpha=*/7.0,
                                       /*final_epsilon=*/-1.0);
}

}  // namespace lap
