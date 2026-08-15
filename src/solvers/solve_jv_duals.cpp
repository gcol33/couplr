// src/solvers/solve_jv_duals.cpp
// Pure C++ Jonker-Volgenant solver with dual variables - NO Rcpp dependencies.
// Thin wrapper that prepares the cost source, delegates to detail::jv_core(),
// and also returns the row/column potentials.

#include "solve_jv_duals.h"
#include "jv_core.h"
#include "../core/lap_error.h"
#include "../core/lap_utils.h"
#include <cmath>
#include <utility>

namespace lap {

namespace {

// The body the dense and lazy entry points share. Preparation is what differs
// and stays with the caller: a dense matrix is negated and masked into a
// separate `work` matrix while the original stays behind to price the result,
// a lazy one is prepared at construction and is its own reporter.
//
// `report_negated` says whether `report` holds negated costs, so the total is
// always reported on the original scale. `solved_negated` says whether the core
// ran on negated costs, which is what the duals have to be flipped back from.
template <typename SolveSourceT, typename ReportSourceT>
DualResult jv_duals_from(const SolveSourceT& work, const ReportSourceT& report,
                         bool report_negated, bool solved_negated) {
    const int n = static_cast<int>(work.nrow);

    detail::JvCoreOpts opts;
    // LAPJV pre-stages: column reduction + ARR. Inert for a LazyCostMatrix
    // (see jv_core.cpp).
    opts.use_warm_start = true;
    auto core = detail::jv_core(work, opts);

    double total = 0.0;
    for (int i = 0; i < n; ++i) {
        const int j = core.assignment[i];
        if (j < 0) {
            LAP_THROW_INFEASIBLE("Could not find full matching");
        }
        if (!report.allowed(i, j)) {
            LAP_THROW_INFEASIBLE("Chosen forbidden edge");
        }
        double c = report.at(i, j);
        if (report_negated) c = -c;
        if (!std::isfinite(c)) {
            LAP_THROW_INFEASIBLE("Chosen edge has non-finite cost");
        }
        total += c;
    }

    if (solved_negated) {
        for (auto& x : core.u) x = -x;
        for (auto& x : core.v) x = -x;
    }

    DualResult result;
    result.solution = LapResult(std::move(core.assignment), total, "optimal");
    result.u = std::move(core.u);
    result.v = std::move(core.v);
    return result;
}

}  // namespace

DualResult solve_jv_duals(const CostMatrix& cost, bool maximize) {
    const int n = static_cast<int>(cost.nrow);
    const int m = static_cast<int>(cost.ncol);

    if (n == 0) {
        DualResult result;
        result.solution = LapResult({}, 0.0, "optimal");
        return result;
    }
    lap::require_rows_fit_cols(n, m);

    CostMatrix work = prepare_for_solve(cost, maximize);
    ensure_each_row_has_option(work.mask, n, m);

    return jv_duals_from(work, cost,
                         /*report_negated=*/false,
                         /*solved_negated=*/maximize);
}

DualResult solve_jv_duals(const LazyCostMatrix& cost) {
    const int n = static_cast<int>(cost.nrow);
    const int m = static_cast<int>(cost.ncol);

    if (n == 0) {
        DualResult result;
        result.solution = LapResult({}, 0.0, "optimal");
        return result;
    }
    lap::require_rows_fit_cols(n, m);

    // No prepare_for_solve() step: the LazyCostMatrix is already "prepared"
    // (forbidden -> BIG via at(), negated if maximize) at construction, so it
    // is both the source the core runs on and the source the result is priced
    // from.
    ensure_each_row_has_option(cost);

    const bool negated = cost.is_negated();
    return jv_duals_from(cost, cost,
                         /*report_negated=*/negated,
                         /*solved_negated=*/negated);
}

}  // namespace lap
