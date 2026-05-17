// src/solvers/solve_jv_duals.cpp
// Pure C++ Jonker-Volgenant solver with dual variables - NO Rcpp dependencies.
// Thin wrapper that prepares the matrix, delegates to detail::jv_core(), and
// also returns the row/column potentials.

#include "solve_jv_duals.h"
#include "jv_core.h"
#include "../core/lap_error.h"
#include "../core/lap_utils.h"
#include <cmath>
#include <utility>

namespace lap {

DualResult solve_jv_duals(const CostMatrix& cost, bool maximize) {
    const int n = cost.nrow;
    const int m = cost.ncol;

    if (n == 0) {
        DualResult result;
        result.solution = LapResult({}, 0.0, "optimal");
        return result;
    }
    if (n > m) {
        LAP_THROW_DIMENSION("Infeasible: number of rows greater than number of columns");
    }

    CostMatrix work = prepare_for_solve(cost, maximize);
    ensure_each_row_has_option(work.mask, n, m);

    auto core = detail::jv_core(work);

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

    // For maximization, the solver ran on negated costs; flip duals back.
    if (maximize) {
        for (auto& x : core.u) x = -x;
        for (auto& x : core.v) x = -x;
    }

    DualResult result;
    result.solution = LapResult(std::move(core.assignment), total, "optimal");
    result.u = std::move(core.u);
    result.v = std::move(core.v);
    return result;
}

}  // namespace lap
