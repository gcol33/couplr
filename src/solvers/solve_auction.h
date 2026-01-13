// src/solvers/solve_auction.h
// Pure C++ Auction LAP solver - NO Rcpp dependencies
#pragma once

#include "../core/lap_types.h"
#include <string>

namespace lap {

// Solve LAP using basic auction algorithm
// Parameters:
//   cost: Cost matrix (row-major, with mask for forbidden edges)
//   maximize: If true, find maximum weight matching (costs negated internally)
//   eps: Epsilon parameter for bidding. If <= 0, uses adaptive epsilon based on cost spread
//
// Returns:
//   LapResult with 0-based assignment and total cost (using original costs)
//
// Throws:
//   InfeasibleException if no valid matching exists
//   DimensionException if nrow > ncol
//   ConvergenceException if iteration limit exceeded
LapResult solve_auction(const CostMatrix& cost, bool maximize = false, double eps = -1.0);

// Solve LAP using scaled-epsilon auction algorithm
// Parameters:
//   cost: Cost matrix (row-major, with mask for forbidden edges)
//   maximize: If true, find maximum weight matching (costs negated internally)
//   schedule: Scaling schedule - "alpha7" (default), "pow2", "halves"
//
// Returns:
//   LapResult with 0-based assignment and total cost (using original costs)
//
// Throws:
//   InfeasibleException if no valid matching exists
//   DimensionException if nrow > ncol
//   ConvergenceException if iteration limit exceeded
LapResult solve_auction_scaled(const CostMatrix& cost, bool maximize = false,
                                const std::string& schedule = "alpha7");

// Solve LAP using Gauss-Seidel auction algorithm
// Parameters:
//   cost: Cost matrix (row-major, with mask for forbidden edges)
//   maximize: If true, find maximum weight matching (costs negated internally)
//   eps: Epsilon parameter for bidding. If <= 0, uses adaptive epsilon based on cost spread
//
// Returns:
//   LapResult with 0-based assignment and total cost (using original costs)
//
// Throws:
//   InfeasibleException if no valid matching exists
//   DimensionException if nrow > ncol (auto-transposes)
//   ConvergenceException if iteration limit exceeded
LapResult solve_auction_gs(const CostMatrix& cost, bool maximize = false, double eps = -1.0);

// Solve LAP using scaled-epsilon auction with custom parameters
// Parameters:
//   cost: Cost matrix (row-major, with mask for forbidden edges)
//   maximize: If true, find maximum weight matching (costs negated internally)
//   initial_epsilon_factor: Multiplier for initial epsilon (default 1.0)
//   alpha: Epsilon reduction factor each phase (default 7.0)
//   final_epsilon: Stopping epsilon (if <= 0, uses 1e-6 or 1/(n²), whichever is smaller)
//
// Returns:
//   LapResult with 0-based assignment and total cost (using original costs)
//
// Throws:
//   InfeasibleException if no valid matching exists
//   DimensionException if nrow > ncol
//   ConvergenceException if iteration limit exceeded
LapResult solve_auction_scaled_params(const CostMatrix& cost, bool maximize = false,
                                       double initial_epsilon_factor = 1.0,
                                       double alpha = 7.0,
                                       double final_epsilon = -1.0);

}  // namespace lap
