// src/solvers/solve_hungarian.cpp
// Classic Hungarian in shortest-augmenting-path form. Delegates to jv_core
// with LAPJV pre-stages disabled — what gets you the "textbook" O(n^3) Hungarian.
// solve_jv calls the same core with pre-stages enabled.

#include "solve_hungarian.h"
#include "jv_core.h"
#include "../core/lap_error.h"
#include "../core/lap_utils.h"
#include <cmath>
#include <utility>

namespace lap {

LapResult solve_hungarian(const CostMatrix& cost, bool maximize) {
    const int n = cost.nrow;
    const int m = cost.ncol;

    if (n == 0) {
        return LapResult({}, 0.0, "optimal");
    }
    if (n > m) {
        LAP_THROW_DIMENSION("Infeasible: number of rows greater than number of columns");
    }

    CostMatrix work = prepare_for_solve(cost, maximize);
    ensure_each_row_has_option(work.mask, n, m);

    detail::JvCoreOpts opts;
    opts.use_warm_start = false;  // classic Hungarian: no LAPJV pre-stages
    auto core = detail::jv_core(work, opts);

    double total = 0.0;
    for (int i = 0; i < n; ++i) {
        int j = core.assignment[i];
        if (j < 0) {
            LAP_THROW_INFEASIBLE("Could not find full matching");
        }
        if (!cost.allowed(i, j)) {
            LAP_THROW_INFEASIBLE("Chosen forbidden edge");
        }
        double c = cost.at(i, j);
        if (!std::isfinite(c)) {
            LAP_THROW_INFEASIBLE("Chosen edge has non-finite cost");
        }
        total += c;
    }

    return LapResult(std::move(core.assignment), total, "optimal");
}

}  // namespace lap
