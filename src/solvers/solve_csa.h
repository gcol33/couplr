// src/solvers/solve_csa.h
// Pure C++ Goldberg-Kennedy Cost-Scaling Assignment (CSA) Algorithm - NO Rcpp dependencies
#pragma once

#include "../core/lap_types.h"

namespace lap {

// Solve LAP using Goldberg-Kennedy Cost-Scaling Algorithm
// Reference: Goldberg & Kennedy (1995) "An efficient cost scaling algorithm
//            for the assignment problem"
// Complexity: O(sqrt(n) * m * log(nC)) where m = number of edges, C = max cost
//
// CSA uses epsilon-scaling auction approach. For minimization, we find objects
// with minimum reduced cost (c_ij - price_j) and decrease prices to maintain
// epsilon-complementary slackness.
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
