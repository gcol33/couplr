// src/solvers/solve_jv.cpp
// Pure C++ Jonker-Volgenant LAP solver - NO Rcpp dependencies.
// Thin wrapper that prepares the matrix and delegates to detail::jv_core().

#include "solve_jv.h"
#include "jv_core.h"
#include "../core/lap_error.h"
#include "../core/lap_utils.h"
#include <cmath>
#include <utility>

namespace lap {

LapResult solve_jv(const CostMatrix& cost, bool maximize) {
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

    return LapResult(std::move(core.assignment), total, "optimal");
}

}  // namespace lap
