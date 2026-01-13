// src/solvers/solve_jv.h
// Pure C++ Jonker-Volgenant LAP solver - NO Rcpp dependencies
#pragma once

#include "../core/lap_types.h"

namespace lap {

// Solve LAP using Jonker-Volgenant (Hungarian-style with potentials)
// Handles rectangular matrices where nrow <= ncol
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
LapResult solve_jv(const CostMatrix& cost, bool maximize = false);

}  // namespace lap
