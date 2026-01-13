// src/solvers/solve_push_relabel.h
// Pure C++ Push-Relabel LAP solver - NO Rcpp dependencies
#pragma once

#include "../core/lap_types.h"

namespace lap {

// Solve LAP using Push-Relabel with successive shortest paths
// Reference: Goldberg & Tarjan (1988) "A new approach to the maximum-flow problem"
//            Extended to min-cost flow for assignment
// Complexity: O(n²m) for assignment problems
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
LapResult solve_push_relabel(const CostMatrix& cost, bool maximize = false);

}  // namespace lap
