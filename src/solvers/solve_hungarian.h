// src/solvers/solve_hungarian.h
// Pure C++ Hungarian (Kuhn-Munkres) LAP solver - NO Rcpp dependencies
#pragma once

#include "../core/lap_types.h"

namespace lap {

// Classic Hungarian (Kuhn-Munkres) algorithm for n <= m with forbidden edge support
// Returns optimal assignment with 0-based indexing
LapResult solve_hungarian(const CostMatrix& cost, bool maximize = false);

}  // namespace lap
