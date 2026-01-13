// src/solvers/solve_hungarian.cpp
// Pure C++ Hungarian (Kuhn-Munkres) LAP solver - NO Rcpp dependencies

#include "solve_hungarian.h"
#include "../core/lap_error.h"
#include "../core/lap_utils.h"
#include <vector>
#include <limits>
#include <algorithm>
#include <cmath>

namespace lap {

LapResult solve_hungarian(const CostMatrix& cost, bool maximize) {
    const int n = cost.nrow;
    const int m = cost.ncol;

    // Handle empty case
    if (n == 0) {
        return LapResult({}, 0.0, "optimal");
    }

    // Dimension check
    if (n > m) {
        LAP_THROW_DIMENSION("Infeasible: number of rows greater than number of columns");
    }

    // Prepare working costs (negated if maximize, BIG for forbidden)
    CostMatrix work = prepare_for_solve(cost, maximize);

    // Check feasibility
    ensure_each_row_has_option(work.mask, n, m);

    const double INF = std::numeric_limits<double>::infinity();

    // Working copy: forbidden -> INF
    std::vector<double> C(n * m);
    for (int k = 0; k < n * m; ++k) {
        C[k] = work.mask[k] ? work.data[k] : INF;
    }

    // Helper functions
    auto row_min = [&](int i) {
        double mn = INF;
        for (int j = 0; j < m; ++j) {
            if (std::isfinite(C[i * m + j])) {
                mn = std::min(mn, C[i * m + j]);
            }
        }
        return mn;
    };

    auto col_min = [&](int j) {
        double mn = INF;
        for (int i = 0; i < n; ++i) {
            if (std::isfinite(C[i * m + j])) {
                mn = std::min(mn, C[i * m + j]);
            }
        }
        return mn;
    };

    // Row reduction (for n <= m)
    for (int i = 0; i < n; ++i) {
        double mn = row_min(i);
        if (!std::isfinite(mn)) {
            LAP_THROW_INFEASIBLE("Row has no finite values (all forbidden)");
        }
        for (int j = 0; j < m; ++j) {
            if (std::isfinite(C[i * m + j])) {
                C[i * m + j] -= mn;
            }
        }
    }

    // Masks and covers
    std::vector<int> star_row_of_col(m, -1);
    std::vector<int> star_col_of_row(n, -1);
    std::vector<int> prime_col_of_row(n, -1);
    std::vector<char> row_cov(n, 0);
    std::vector<char> col_cov(m, 0);

    auto is_zero = [&](int i, int j) {
        double x = C[i * m + j];
        return std::isfinite(x) && std::abs(x) <= TOL;
    };

    // Initial greedy starring (cover columns only for n <= m)
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            if (is_zero(i, j) && col_cov[j] == 0) {
                star_row_of_col[j] = i;
                star_col_of_row[i] = j;
                col_cov[j] = 1;
                break;
            }
        }
    }

    // Recompute column covers from stars to ensure clean invariant
    std::fill(col_cov.begin(), col_cov.end(), 0);
    for (int j = 0; j < m; ++j) {
        if (star_row_of_col[j] != -1) {
            col_cov[j] = 1;
        }
    }

    auto find_uncovered_zero = [&]() -> std::pair<int, int> {
        for (int i = 0; i < n; ++i) {
            if (!row_cov[i]) {
                for (int j = 0; j < m; ++j) {
                    if (!col_cov[j] && is_zero(i, j)) {
                        return {i, j};
                    }
                }
            }
        }
        return {-1, -1};
    };

    auto smallest_uncovered = [&]() {
        double mn = INF;
        for (int i = 0; i < n; ++i) {
            if (!row_cov[i]) {
                for (int j = 0; j < m; ++j) {
                    if (!col_cov[j] && std::isfinite(C[i * m + j])) {
                        mn = std::min(mn, C[i * m + j]);
                    }
                }
            }
        }
        return mn;
    };

    // Main algorithm loop
    const long long max_outer = n * m * 100LL;
    long long outer_iter = 0;

    while (true) {
        if (++outer_iter > max_outer) {
            LAP_THROW_CONVERGENCE("Exceeded maximum iterations");
        }

        // Count stars to check termination
        int num_stars = 0;
        for (int j = 0; j < m; ++j) {
            if (star_row_of_col[j] >= 0) {
                num_stars++;
            }
        }

        // Terminate when we have n stars (complete matching)
        if (num_stars >= n) {
            break;
        }

        // Reset covers: uncover all rows, cover only columns with stars
        std::fill(row_cov.begin(), row_cov.end(), 0);
        std::fill(col_cov.begin(), col_cov.end(), 0);
        for (int j = 0; j < m; ++j) {
            if (star_row_of_col[j] != -1) {
                col_cov[j] = 1;
            }
        }
        std::fill(prime_col_of_row.begin(), prime_col_of_row.end(), -1);

        // Step 3: Find uncovered zeros and build augmenting paths
        const long long max_inner = n * m * 100LL;
        long long inner_iter = 0;

        while (true) {
            if (++inner_iter > max_inner) {
                LAP_THROW_CONVERGENCE("Inner loop exceeded maximum iterations");
            }

            auto z = find_uncovered_zero();
            if (z.first == -1) {
                // No uncovered zero found - adjust matrix (step 5)
                double d = smallest_uncovered();
                if (!std::isfinite(d)) {
                    LAP_THROW_INFEASIBLE("No uncovered finite values");
                }

                // Adjustment: add to covered rows, subtract from uncovered columns
                for (int i = 0; i < n; ++i) {
                    for (int j = 0; j < m; ++j) {
                        if (row_cov[i] && std::isfinite(C[i * m + j])) {
                            C[i * m + j] += d;
                        }
                        if (!col_cov[j] && std::isfinite(C[i * m + j])) {
                            C[i * m + j] -= d;
                        }
                    }
                }
                // After adjustment, continue to look for zeros again
                continue;
            }

            // Found an uncovered zero at (i, j) - prime it
            int i = z.first;
            int j = z.second;
            prime_col_of_row[i] = j;

            if (star_col_of_row[i] == -1) {
                // No star in this row - we found an augmenting path!
                // Augment along alternating path starting from (i, j)
                int ii = i;
                int jj = j;
                std::vector<std::pair<int, int>> path;
                path.emplace_back(ii, jj);

                while (true) {
                    int i_star = star_row_of_col[jj];
                    if (i_star == -1) {
                        break;
                    }
                    path.emplace_back(i_star, jj);
                    int j_prime = prime_col_of_row[i_star];
                    jj = j_prime;
                    path.emplace_back(i_star, jj);
                }

                // Flip stars along path
                // CRITICAL: Unstar all the starred entries first to avoid overwriting
                for (size_t k = 0; k < path.size(); ++k) {
                    if (k % 2 == 1) {  // star -> unset
                        int r = path[k].first;
                        int c = path[k].second;
                        star_row_of_col[c] = -1;
                        star_col_of_row[r] = -1;
                    }
                }
                // Now star all the primed entries
                for (size_t k = 0; k < path.size(); ++k) {
                    if (k % 2 == 0) {  // prime -> star
                        int r = path[k].first;
                        int c = path[k].second;
                        star_row_of_col[c] = r;
                        star_col_of_row[r] = c;
                    }
                }

                // After augmentation, go back to outer loop to check if done
                break;
            } else {
                // There's a star in this row - cover the row and uncover the star's column
                row_cov[i] = 1;
                col_cov[star_col_of_row[i]] = 0;
                // Continue in the inner loop to find more zeros
            }
        }
    }

    // Build assignment from stars (0-based)
    std::vector<int> assignment(n, -1);
    int num_matched = 0;
    for (int i = 0; i < n; ++i) {
        int j = star_col_of_row[i];
        if (j >= 0) {
            assignment[i] = j;
            num_matched++;
        }
    }

    // Verify we have a complete matching
    if (num_matched < n) {
        LAP_THROW_INFEASIBLE("Could not find full matching");
    }

    // Compute total cost using ORIGINAL costs
    double total = 0.0;
    for (int i = 0; i < n; ++i) {
        int j = assignment[i];
        if (j < 0) {
            LAP_THROW_INFEASIBLE("Incomplete matching");
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

    return LapResult(std::move(assignment), total, "optimal");
}

}  // namespace lap
