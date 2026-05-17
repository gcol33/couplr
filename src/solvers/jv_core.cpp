// src/solvers/jv_core.cpp
// Shortest-augmenting-path Hungarian (Kuhn-Munkres with potentials).
// Single Dijkstra-style augmentation per row; O(n*m^2) worst case, O(n*m) typical
// after the row/column potentials warm up.

#include "jv_core.h"
#include "../core/lap_types.h"
#include <vector>
#include <limits>
#include <cmath>
#include <algorithm>

namespace lap {
namespace detail {

JvCoreResult jv_core(const CostMatrix& work, const JvCoreOpts& /*opts*/) {
    const int n = work.nrow;
    const int m = work.ncol;

    JvCoreResult result;
    result.assignment.assign(n, -1);
    result.u.assign(n, 0.0);
    result.v.assign(m, 0.0);

    if (n == 0) return result;

    // 1-based internal indexing.
    std::vector<double> u(n + 1, 0.0), v(m + 1, 0.0);
    std::vector<int> p(m + 1, 0), way(m + 1, 0);

    const double INF = std::numeric_limits<double>::infinity();

    for (int i = 1; i <= n; ++i) {
        p[0] = i;
        int j0 = 0;
        std::vector<double> minv(m + 1, INF);
        std::vector<char> used(m + 1, 0);
        way[0] = 0;

        while (true) {
            used[j0] = 1;
            int i0 = p[j0];
            double delta = INF;
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
            if (p[j0] == 0) break;  // augmenting column found
        }

        // Augment along the predecessor chain.
        while (true) {
            int j1 = way[j0];
            p[j0] = p[j1];
            j0 = j1;
            if (j0 == 0) break;
        }
    }

    for (int j = 1; j <= m; ++j) {
        if (p[j] != 0 && p[j] <= n) {
            int row = p[j] - 1;
            result.assignment[row] = j - 1;
        }
    }
    for (int i = 0; i < n; ++i) result.u[i] = u[i + 1];
    for (int j = 0; j < m; ++j) result.v[j] = v[j + 1];

    return result;
}

}  // namespace detail
}  // namespace lap
