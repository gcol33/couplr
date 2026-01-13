// src/solvers/network_simplex/solve_network_simplex.h
// Pure C++ Network Simplex LAP solver - NO Rcpp dependencies
#pragma once

#include "../../core/lap_types.h"

namespace lap {

// Solve LAP using Network Simplex algorithm
// Based on LEMON library and Király-Kovács (2012)
//
// Network Simplex maintains a spanning tree of the flow network and performs
// pivots to improve the solution. For assignment problems, the network has:
//   - Source node (supplies n units)
//   - n row nodes (transshipment)
//   - m column nodes (transshipment)
//   - Sink node (demands n units)
//
// The algorithm iteratively finds entering arcs with negative reduced cost
// and performs basis exchanges until optimality.
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
LapResult solve_network_simplex(const CostMatrix& cost, bool maximize = false);

}  // namespace lap
