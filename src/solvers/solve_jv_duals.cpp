// src/solvers/solve_jv_duals.cpp
// Pure C++ Jonker-Volgenant solver with dual variables - NO Rcpp dependencies

#include "solve_jv_duals.h"
#include "../core/lap_error.h"
#include "../core/lap_utils.h"
#include <vector>
#include <limits>
#include <cmath>

namespace lap {

DualResult solve_jv_duals(const CostMatrix& cost, bool maximize) {
    const int n = cost.nrow;
    const int m = cost.ncol;

    // Handle empty case
    if (n == 0) {
        DualResult result;
        result.solution = LapResult({}, 0.0, "optimal");
        result.u = std::vector<double>();
        result.v = std::vector<double>();
        return result;
    }

    // Dimension check
    if (n > m) {
        LAP_THROW_DIMENSION("Infeasible: number of rows greater than number of columns");
    }

    // For dual variable extraction, we work with negated costs for maximization
    // This allows clean dual transformations (just negate at the end)
    CostMatrix work = prepare_for_solve(cost, maximize);

    // Check feasibility
    ensure_each_row_has_option(work.mask, n, m);

    // Hungarian (rectangular n <= m) with potentials
    // Using 1-based indexing internally for the algorithm
    std::vector<double> u(n + 1, 0.0), v(m + 1, 0.0);
    std::vector<int> p(m + 1, 0), way(m + 1, 0);

    for (int i = 1; i <= n; ++i) {
        p[0] = i;
        int j0 = 0;
        std::vector<double> minv(m + 1, std::numeric_limits<double>::infinity());
        std::vector<char> used(m + 1, 0);
        way[0] = 0;

        while (true) {
            used[j0] = 1;
            int i0 = p[j0];
            double delta = std::numeric_limits<double>::infinity();
            int j1 = 0;

            for (int j = 1; j <= m; ++j) {
                if (used[j]) continue;
                double cij = work.at(i0 - 1, j - 1);
                if (!std::isfinite(cij)) cij = BIG;
                double cur = cij - u[i0] - v[j];
                if (cur < minv[j]) {
                    minv[j] = cur;
                    way[j] = j0;
                }
                if (minv[j] < delta) {
                    delta = minv[j];
                    j1 = j;
                }
            }

            for (int j = 0; j <= m; ++j) {
                if (used[j]) {
                    u[p[j]] += delta;
                    v[j] -= delta;
                } else {
                    minv[j] -= delta;
                }
            }

            j0 = j1;
            if (p[j0] == 0) break;  // found augmenting column
        }

        // Augment
        while (true) {
            int j1 = way[j0];
            p[j0] = p[j1];
            j0 = j1;
            if (j0 == 0) break;
        }
    }

    // Build assignment: row -> column (0-based)
    std::vector<int> assignment(n, -1);
    for (int j = 1; j <= m; ++j) {
        if (p[j] != 0 && p[j] <= n) {
            int row = p[j] - 1;
            assignment[row] = j - 1;  // Convert to 0-based
        }
    }

    // Verify matching and compute total cost using ORIGINAL costs
    double total = 0.0;
    for (int i = 0; i < n; ++i) {
        int j = assignment[i];
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

    // Extract dual variables (convert from 1-indexed internal to 0-indexed output)
    // For maximization, negate the duals back
    std::vector<double> out_u(n), out_v(m);

    if (maximize) {
        // When maximizing, we solved min(-c), so duals need negation
        for (int i = 0; i < n; ++i) {
            out_u[i] = -u[i + 1];
        }
        for (int j = 0; j < m; ++j) {
            out_v[j] = -v[j + 1];
        }
    } else {
        for (int i = 0; i < n; ++i) {
            out_u[i] = u[i + 1];
        }
        for (int j = 0; j < m; ++j) {
            out_v[j] = v[j + 1];
        }
    }

    // Build result
    DualResult result;
    result.solution = LapResult(std::move(assignment), total, "optimal");
    result.u = std::move(out_u);
    result.v = std::move(out_v);

    return result;
}

}  // namespace lap
