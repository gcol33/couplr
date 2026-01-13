// src/solvers/orlin_ahuja/orlin_solve.h
// Pure C++ Orlin-Ahuja LAP solver - NO Rcpp dependencies
#pragma once

#include "../../core/lap_types.h"

namespace lap {

// Solve LAP using Orlin-Ahuja algorithm with epsilon-scaling
// Uses successive shortest path (SSP) approach for dense graphs
//
// Parameters:
//   cost: Cost matrix (row-major, with mask for forbidden edges)
//   maximize: If true, find maximum weight matching (costs negated internally)
//   alpha: Scaling factor (typical: 2-7, default: 5.0)
//   auction_rounds: Number of auction rounds per scale before switching to SSP
//
// Returns:
//   LapResult with 0-based assignment and total cost (using original costs)
//
// Throws:
//   InfeasibleException if no valid matching exists
//   DimensionException if nrow > ncol
//
// Complexity: O(n * m * log n) using pure SSP approach
// Note: This implementation uses SSP only (no epsilon-scaling) for simplicity
LapResult solve_orlin(const CostMatrix& cost, bool maximize = false,
                      double alpha = 5.0, int auction_rounds = 10);

}  // namespace lap
