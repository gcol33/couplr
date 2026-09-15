// src/solvers/solve_csa.h
// Cost-scaling assignment entry point - NO Rcpp dependencies
#pragma once

#include "../core/lap_types.h"

namespace lap {

// Solve LAP by epsilon-scaling: a large epsilon for coarse moves, divided by 7
// each phase, around the forward-auction bid. This is the core behind
// solve_auction_scaled_params() at its default schedule, including the repair
// that turns the final epsilon-optimal assignment into an optimal one, so
// real-valued costs are solved as supplied.
//
// Parameters:
//   cost: Cost matrix (row-major, with mask for forbidden edges)
//   maximize: If true, find maximum weight matching (costs negated internally)
//
// Returns:
//   LapResult with 0-based assignment and total cost (using original costs)
//
// Throws:
//   InfeasibleException if no valid matching exists
//   DimensionException if nrow > ncol
LapResult solve_csa(const CostMatrix& cost, bool maximize = false);

}  // namespace lap
