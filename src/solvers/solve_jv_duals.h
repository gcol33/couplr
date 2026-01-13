// src/solvers/solve_jv_duals.h
// Pure C++ Jonker-Volgenant solver with dual variables - NO Rcpp dependencies
#pragma once

#include "../core/lap_types.h"

namespace lap {

// Solve LAP using Jonker-Volgenant and return dual variables
// Handles rectangular matrices where nrow <= ncol
//
// For an optimal assignment, the duals satisfy complementary slackness:
//   u[i] + v[j] <= c[i,j] for all (i,j) (minimization)
//   u[i] + v[j] >= c[i,j] for all (i,j) (maximization)
//   u[i] + v[j] = c[i,j] for assigned pairs
//
// Parameters:
//   cost: Cost matrix (row-major, with mask for forbidden edges)
//   maximize: If true, find maximum weight matching (costs negated internally)
//
// Returns:
//   DualResult with:
//     - solution: LapResult with 0-based assignment and total cost (using original costs)
//     - u: row potentials (0-based indexing)
//     - v: column potentials (0-based indexing)
//
// Throws:
//   InfeasibleException if no valid matching exists
//   DimensionException if nrow > ncol
DualResult solve_jv_duals(const CostMatrix& cost, bool maximize = false);

}  // namespace lap
