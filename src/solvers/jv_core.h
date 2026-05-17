// src/solvers/jv_core.h
// Pure C++ shortest-augmenting-path Hungarian core.
// Shared by solve_jv, solve_jv_duals, and solve_hungarian.
// NO Rcpp dependencies.
#pragma once

#include "../core/lap_types.h"
#include <vector>

namespace lap {
namespace detail {

struct JvCoreOpts {
    // Reserved for LAPJV pre-stages (column reduction + augmenting row reduction).
    // Currently unused; off by default. Will be wired up when warm-start lands.
    bool use_warm_start = false;
};

struct JvCoreResult {
    std::vector<int> assignment;  // size nrow, 0-based, -1 = unmatched
    std::vector<double> u;        // size nrow, row potentials
    std::vector<double> v;        // size ncol, column potentials
};

// Solve LAP on a prepared cost matrix.
// Precondition: `work` came from prepare_for_solve() — forbidden cells = BIG,
// negated if the caller wants maximize, padded if needed.
// Caller is responsible for feasibility checks (ensure_each_row_has_option),
// dimension checks (n <= m), and computing total_cost from the original matrix.
JvCoreResult jv_core(const CostMatrix& work, const JvCoreOpts& opts = {});

}  // namespace detail
}  // namespace lap
