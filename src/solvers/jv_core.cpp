// src/solvers/jv_core.cpp
// Shortest-augmenting-path Hungarian (Kuhn-Munkres with potentials).
// Single Dijkstra-style augmentation per row. With opts.use_warm_start = true,
// also runs the three Jonker-Volgenant 1987 pre-stages (Column Reduction,
// Reduction Transfer, Augmenting Row Reduction) so the main augmentation only
// has to process the remaining unassigned rows — typically 1/3 to 1/10 of n.
//
// Reference: R. Jonker & A. Volgenant, "A Shortest Augmenting Path Algorithm
// for Dense and Sparse Linear Assignment Problems," Computing 38 (1987)
// 325-340. Pre-stage pseudocode mirrors the canonical C implementation.
//
// Precondition: `work` is the output of prepare_for_solve() — forbidden cells
// = BIG, negated for maximize, mask correctly set. ensure_each_row_has_option
// must have been called on the caller's side so every row has at least one
// allowed column.

#include "jv_core.h"
#include "../core/lap_types.h"
#include <vector>
#include <limits>
#include <cmath>
#include <algorithm>
#include <utility>

namespace lap {
namespace detail {

JvCoreResult jv_core(const CostMatrix& work, const JvCoreOpts& opts) {
    const int n = work.nrow;
    const int m = work.ncol;

    JvCoreResult result;
    result.assignment.assign(n, -1);
    result.u.assign(n, 0.0);
    result.v.assign(m, 0.0);

    if (n == 0) return result;

    const double INF = std::numeric_limits<double>::infinity();

    // 1-based arrays used by the augmentation loop. u[1..n], v[1..m].
    // p[j] (1-based) = row currently assigned to col j (1-based), or 0 if free.
    std::vector<double> u(n + 1, 0.0), v(m + 1, 0.0);
    std::vector<int> p(m + 1, 0), way(m + 1, 0);

    // Rows that still need an augmenting path. 1-based.
    std::vector<int> free_rows;
    free_rows.reserve(n);

    // LAPJV pre-stages assume the matrix is square and fully connected
    // (every cell allowed). For rectangular n < m the standard pre-stages
    // produce v[j] > 0 on unmatched columns, which violates the unbalanced-
    // LAP dual constraint v[j] <= 0 and lets augmentation converge to a
    // sub-optimal balanced-LAP matching. For matrices with forbidden cells
    // the same column-min computation gets contaminated by BIG. Gate the
    // warm-start to the safe regime and fall back otherwise.
    bool safe_warm_start = opts.use_warm_start && (n == m);
    if (safe_warm_start) {
        for (int k = 0; k < n * m; ++k) {
            if (work.mask[k] == 0) { safe_warm_start = false; break; }
        }
    }

    if (safe_warm_start) {
        // --- LAPJV pre-stages (0-based working arrays) ---
        std::vector<int> rowsol(n, -1);   // row -> col, -1 = unassigned
        std::vector<int> colsol(m, -1);   // col -> row, -1 = unassigned
        std::vector<int> matches(n, 0);   // how many CR cols claimed this row

        // --- (1) COLUMN REDUCTION ---
        // For each column j (iterated back-to-front for parity with the
        // reference C code), find the row imin minimizing work[i,j] over
        // allowed entries. Set v[j] to that min and tentatively assign
        // (imin, j); resolve conflicts when the same row is claimed twice.
        // BIG-only columns are skipped — their v[j] stays at 0 so that
        // c[i,j] - v[j] = BIG correctly marks them as unattractive later.
        for (int j = m - 1; j >= 0; --j) {
            double mn = INF;
            int imin = -1;
            for (int i = 0; i < n; ++i) {
                if (!work.allowed(i, j)) continue;
                double c = work.at(i, j);
                if (c < mn) {
                    mn = c;
                    imin = i;
                }
            }
            if (imin < 0) {
                colsol[j] = -1;
                continue;  // column has no allowed entry
            }
            v[j + 1] = mn;
            ++matches[imin];
            if (matches[imin] == 1) {
                rowsol[imin] = j;
                colsol[j] = imin;
            } else if (v[j + 1] < v[rowsol[imin] + 1]) {
                // New column j is cheaper for imin than the previously claimed j1.
                int j1 = rowsol[imin];
                rowsol[imin] = j;
                colsol[j] = imin;
                colsol[j1] = -1;
            } else {
                colsol[j] = -1;
            }
        }

        // --- (2) REDUCTION TRANSFER ---
        // Rows with matches[i] == 0 are free. Rows with matches[i] == 1 keep
        // their assignment but push v[rowsol[i]] down by the gap to the
        // next-cheapest column for that row, tightening the dual.
        std::vector<int> free0;
        free0.reserve(n);
        for (int i = 0; i < n; ++i) {
            if (matches[i] == 0) {
                free0.push_back(i);
            } else if (matches[i] == 1) {
                int j1 = rowsol[i];
                double mn = INF;
                for (int j = 0; j < m; ++j) {
                    if (j == j1) continue;
                    if (!work.allowed(i, j)) continue;
                    double h = work.at(i, j) - v[j + 1];
                    if (h < mn) mn = h;
                }
                if (std::isfinite(mn)) {
                    v[j1 + 1] -= mn;
                }
            }
        }

        // --- (3) AUGMENTING ROW REDUCTION ---
        // Two passes over the free list. For each free row, find umin
        // (best reduced cost) and usubmin (second-best). Reassign the row
        // to its best column, displacing any prior owner. When best and
        // second-best tie, swap to second-best to avoid a useless ping-pong.
        for (int loopcnt = 0; loopcnt < 2 && !free0.empty(); ++loopcnt) {
            std::vector<int> cur = std::move(free0);
            free0.clear();
            int k = 0;
            const int prv = static_cast<int>(cur.size());
            while (k < prv) {
                int i = cur[k++];

                // Find umin / usubmin among allowed columns.
                double umin = INF;
                double usubmin = INF;
                int j1 = -1, j2 = -1;
                for (int j = 0; j < m; ++j) {
                    if (!work.allowed(i, j)) continue;
                    double h = work.at(i, j) - v[j + 1];
                    if (h < usubmin) {
                        if (h >= umin) {
                            usubmin = h;
                            j2 = j;
                        } else {
                            usubmin = umin;
                            j2 = j1;
                            umin = h;
                            j1 = j;
                        }
                    }
                }
                if (j1 < 0) {
                    // No allowed column at all — shouldn't happen given
                    // ensure_each_row_has_option, but stay safe.
                    free0.push_back(i);
                    continue;
                }

                int i0 = colsol[j1];
                if (j2 >= 0 && umin < usubmin) {
                    v[j1 + 1] -= (usubmin - umin);
                } else if (i0 >= 0) {
                    // Tie on umin: try the second-best column instead.
                    if (j2 >= 0) {
                        j1 = j2;
                        i0 = colsol[j2];
                    }
                }

                rowsol[i] = j1;
                colsol[j1] = i;
                if (i0 >= 0) {
                    // Canonical LAPJV: displaced rows always go to the next
                    // pass via free0. Reprocessing in the same pass via
                    // cur[--k] = i0 can ping-pong indefinitely on inputs
                    // where two rows alternately displace each other and
                    // floating-point ordering makes the umin/usubmin gap
                    // borderline (observed on macOS-arm64 with a 10x10
                    // dense Euclidean matrix from test-cardinality.R). The
                    // outer 2-pass cap still bounds total work to O(n).
                    free0.push_back(i0);
                }
            }
        }

        // Compute row potentials u from u[i] = c[i, rowsol[i]] - v[rowsol[i]].
        // Skip rows whose rowsol is stale (i.e., that column is now owned by
        // someone else) — those rows must be in free0 and will get u during
        // augmentation. Leaving u[i] = 0 still satisfies CS for them, since
        // v[j] was only ever decreased and c[i,j] - v[j] >= 0 by invariant.
        for (int i = 0; i < n; ++i) {
            int j = rowsol[i];
            if (j >= 0 && colsol[j] == i && work.allowed(i, j)) {
                u[i + 1] = work.at(i, j) - v[j + 1];
            }
        }

        // Wire the partial assignment into the 1-based p[] used by the
        // augmentation phase.
        for (int j = 0; j < m; ++j) {
            int i = colsol[j];
            p[j + 1] = (i >= 0) ? (i + 1) : 0;
        }
        for (int i : free0) free_rows.push_back(i + 1);
    } else {
        // No warm-start: every row needs augmenting.
        for (int i = 1; i <= n; ++i) free_rows.push_back(i);
    }

    // --- Main augmentation: Dijkstra-style shortest path on reduced costs. ---
    std::vector<double> minv(m + 1, 0.0);
    std::vector<char> used(m + 1, 0);

    for (int i_aug : free_rows) {
        p[0] = i_aug;
        int j0 = 0;
        std::fill(minv.begin(), minv.end(), INF);
        std::fill(used.begin(), used.end(), 0);
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
            if (p[j0] == 0) break;
        }

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
