// src/solvers/solve_auction.cpp
// Pure C++ Auction LAP solver - NO Rcpp dependencies

#include "solve_auction.h"
#include "../core/lap_error.h"
#include "../core/lap_utils.h"
#include <vector>
#include <limits>
#include <algorithm>
#include <cmath>

namespace lap {

// Compute cost spread among allowed entries (max - min)
static inline double allowed_spread(const CostMatrix& cost) {
    double cmin = std::numeric_limits<double>::infinity();
    double cmax = -std::numeric_limits<double>::infinity();

    for (int i = 0; i < cost.nrow; ++i) {
        for (int j = 0; j < cost.ncol; ++j) {
            if (cost.allowed(i, j)) {
                double v = cost.at(i, j);
                if (std::isfinite(v)) {
                    if (v < cmin) cmin = v;
                    if (v > cmax) cmax = v;
                }
            }
        }
    }

    if (!std::isfinite(cmin) || !std::isfinite(cmax)) return 0.0;
    double s = cmax - cmin;
    if (s < 0.0) s = 0.0;
    return s;
}

// Basic auction algorithm
LapResult solve_auction(const CostMatrix& cost, bool maximize, double eps_in) {
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

    // Build allowed structure (CSR)
    std::vector<int> row_ptr, cols;
    build_allowed(work.mask, n, m, row_ptr, cols);

    // Prices and assignments
    std::vector<double> price(m, 0.0);
    std::vector<int> a_of_i(n, -1), i_of_j(m, -1);
    std::vector<int> queue;
    queue.reserve(n);

    // Determine epsilon
    // Adaptive epsilon: scale with cost spread / n² to avoid tiny bids
    // Scale-invariant choice: ε = spread / (2n²)
    double eps;
    if (std::isfinite(eps_in) && eps_in > 0.0) {
        eps = eps_in;
    } else {
        const double spread = allowed_spread(work);
        const double base = (spread > 0.0) ? spread : 1.0;
        eps = base / (2.0 * static_cast<double>(n) * static_cast<double>(n));
        // Defensive clamp to prevent denormal or absurd values
        if (eps < 1e-12) eps = 1e-12;
        if (spread > 0.0 && eps > spread) eps = spread;
    }

    // Profit function (for minimization: maximize negative cost)
    auto profit = [&](int i, int j) {
        double base = -(work.at(i, j) + price[j]);
        // Tie-breaking perturbation
        unsigned key = static_cast<unsigned>((i + 1) * 1315423911u) ^
                       static_cast<unsigned>((j + 1) * 2654435761u);
        double tweak = std::ldexp(static_cast<double>(key & 0xFFFFu), -80);
        return base + tweak;
    };

    // Initialize queue with all persons
    for (int i = 0; i < n; ++i) {
        queue.push_back(i);
    }

    long long reassign_guard = 0;
    const long long max_iters = 200000000LL;

    while (!queue.empty()) {
        int i = queue.back();
        queue.pop_back();

        // Find best and second-best among allowed objects
        double best = -std::numeric_limits<double>::infinity();
        double second = -std::numeric_limits<double>::infinity();
        int jbest = -1;

        const int start = row_ptr[i];
        const int end = row_ptr[i + 1];

        for (int k = start; k < end; ++k) {
            int j = cols[k];
            double val = profit(i, j);
            if (val > best) {
                second = best;
                best = val;
                jbest = j;
            } else if (val > second) {
                second = val;
            }
        }

        if (jbest < 0) {
            LAP_THROW_INFEASIBLE("Person has no valid objects");
        }

        // Compute bid increment
        double bid = (second == -std::numeric_limits<double>::infinity()) ?
                     (2.0 * eps) : (best - second + eps);

        // Update price
        price[jbest] += bid;

        // Update assignment
        int old = i_of_j[jbest];
        i_of_j[jbest] = i;

        if (old != -1) {
            a_of_i[old] = -1;
            queue.push_back(old);
        }
        a_of_i[i] = jbest;

        if (++reassign_guard > max_iters) {
            LAP_THROW_CONVERGENCE("Auction: exceeded iteration guard");
        }
    }

    // Build assignment vector (0-based)
    std::vector<int> assignment(n, -1);
    for (int i = 0; i < n; ++i) {
        assignment[i] = a_of_i[i];
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

    return LapResult(std::move(assignment), total, "optimal");
}

// Scaled-epsilon auction algorithm
LapResult solve_auction_scaled(const CostMatrix& cost, bool maximize,
                                const std::string& schedule) {
    const int n0 = cost.nrow;
    const int m0 = cost.ncol;

    // Handle empty case
    if (n0 == 0) {
        return LapResult({}, 0.0, "optimal");
    }

    // Dimension check
    if (n0 > m0) {
        LAP_THROW_DIMENSION("Infeasible: number of rows greater than number of columns");
    }

    // C reference requires balanced graph (n == m). If n < m, pad with dummies.
    bool needs_padding = (n0 < m0);
    CostMatrix padded_cost;
    int n = n0;
    int m = m0;

    if (needs_padding) {
        // Pad with dummy rows to make it square
        n = m0;
        padded_cost = CostMatrix(n, m);

        // Copy original costs
        for (int i = 0; i < n0; ++i) {
            for (int j = 0; j < m0; ++j) {
                padded_cost.at(i, j) = cost.at(i, j);
                padded_cost.mask[i * m + j] = cost.mask[i * m + j];
            }
        }

        // Set dummy rows to very high cost (very low profit in maximize case)
        double dummy_cost = 0.0;
        for (int i = 0; i < n0; ++i) {
            for (int j = 0; j < m0; ++j) {
                if (std::isfinite(cost.at(i, j))) {
                    dummy_cost = std::max(dummy_cost, std::abs(cost.at(i, j)));
                }
            }
        }
        dummy_cost = (dummy_cost + 1.0) * m * 10.0;

        for (int i = n0; i < n; ++i) {
            for (int j = 0; j < m; ++j) {
                padded_cost.at(i, j) = maximize ? -dummy_cost : dummy_cost;
                padded_cost.mask[i * m + j] = 1;  // Allow dummy edges
            }
        }
    } else {
        padded_cost = cost;
    }

    // Prepare working costs (negated if maximize, BIG for forbidden)
    CostMatrix work = prepare_for_solve(padded_cost, maximize);

    // Build CSR for the balanced (possibly padded) matrix
    std::vector<int> row_ptr, cols;
    build_allowed(work.mask, n, m, row_ptr, cols);

    // Find max cost for epsilon initialization
    double max_abs_cost = 0.0;
    for (int i = 0; i < n; ++i) {
        const int start = row_ptr[i];
        const int end = row_ptr[i + 1];
        for (int k = start; k < end; ++k) {
            int j = cols[k];
            double c = std::abs(work.at(i, j));
            if (std::isfinite(c) && c > max_abs_cost) {
                max_abs_cost = c;
            }
        }
    }

    // Determine alpha (scaling factor)
    double alpha = 7.0;
    if (schedule == "pow2" || schedule == "halves") {
        alpha = 4.0;
    }

    double epsilon = std::max(1.0, max_abs_cost);
    double eps_final = std::min(1e-6, 1.0 / (static_cast<double>(n) * static_cast<double>(n)));

    // State: prices persist, matching rebuilt each phase
    std::vector<double> price(m, 0.0);
    std::vector<int> a_of_i(n, -1);
    std::vector<int> i_of_j(m, -1);

    // Price bounds to prevent overflow (scaled relative to problem size)
    const double price_bound = std::max(1e12, max_abs_cost * n * 1000.0);

    // C reference formulation: MINIMIZE (cost - price)
    auto reduced_cost = [&](int i, int j) {
        return work.at(i, j) - price[j];
    };

    // Find best (MINIMUM reduced cost) and second-best
    auto find_best = [&](int i, double& best_rc, double& second_rc, int& best_j) {
        const int start = row_ptr[i];
        const int end = row_ptr[i + 1];
        best_rc = std::numeric_limits<double>::infinity();
        second_rc = std::numeric_limits<double>::infinity();
        best_j = -1;

        if (end - start == 1) {
            best_j = cols[start];
            best_rc = reduced_cost(i, best_j);
            return;
        }

        for (int k = start; k < end; ++k) {
            int j = cols[k];
            double rc = reduced_cost(i, j);
            if (rc < best_rc) {
                second_rc = best_rc;
                best_rc = rc;
                best_j = j;
            } else if (rc < second_rc) {
                second_rc = rc;
            }
        }
    };

    int phase = 0;
    const long long max_iter = static_cast<long long>(n) * m * 100;

    // Epsilon-scaling loop
    do {
        phase++;

        // Reduce epsilon (C reference does this first each phase)
        epsilon /= alpha;
        if (epsilon < eps_final) epsilon = eps_final;

        // Discard matching
        std::fill(a_of_i.begin(), a_of_i.end(), -1);
        std::fill(i_of_j.begin(), i_of_j.end(), -1);

        // Rebuild matching - all n persons (including dummies if padded)
        std::vector<int> unmatched;
        unmatched.reserve(n);
        for (int i = 0; i < n; ++i) {
            unmatched.push_back(i);
        }

        long long iter = 0;
        while (!unmatched.empty()) {
            if (++iter > max_iter) {
                LAP_THROW_CONVERGENCE("Auction(scaled): iteration guard exceeded at phase " +
                                     std::to_string(phase));
            }

            int i = unmatched.back();
            unmatched.pop_back();

            double best_rc, second_rc;
            int best_j;
            find_best(i, best_rc, second_rc, best_j);

            if (best_j < 0) {
                LAP_THROW_INFEASIBLE("Person has no valid neighbors");
            }

            // Compute gamma - use epsilon-scaled default when only one option
            double gamma;
            if (second_rc == std::numeric_limits<double>::infinity() || !std::isfinite(second_rc)) {
                // Only one valid option: use small multiple of epsilon
                gamma = epsilon;
            } else {
                gamma = second_rc - best_rc;
                // Clamp gamma to prevent runaway prices
                if (gamma > price_bound) gamma = price_bound;
                if (gamma < 0.0) gamma = 0.0;  // Should not happen, but defensive
            }

            // DECREASE price with overflow protection
            double new_price = price[best_j] - (gamma + epsilon);
            if (new_price < -price_bound) new_price = -price_bound;
            price[best_j] = new_price;

            // Assignment
            int old = i_of_j[best_j];
            i_of_j[best_j] = i;
            if (old != -1) {
                a_of_i[old] = -1;
                unmatched.push_back(old);
            }
            a_of_i[i] = best_j;
        }
    } while (epsilon > eps_final);

    // Build assignment vector - only for ORIGINAL n0 rows (not dummy rows)
    std::vector<int> assignment(n0, -1);
    for (int i = 0; i < n0; ++i) {
        assignment[i] = a_of_i[i];
    }

    // Verify matching and compute total cost using ORIGINAL costs
    double total = 0.0;
    for (int i = 0; i < n0; ++i) {
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

    return LapResult(std::move(assignment), total, "optimal");
}

// Transpose a cost matrix
static CostMatrix transpose_cost(const CostMatrix& cost) {
    const int n = cost.nrow;
    const int m = cost.ncol;
    CostMatrix result(m, n);

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            result.at(j, i) = cost.at(i, j);
            result.mask[j * n + i] = cost.mask[i * m + j];
        }
    }

    return result;
}

// Scaled-epsilon auction with custom parameters
LapResult solve_auction_scaled_params(const CostMatrix& cost, bool maximize,
                                       double initial_epsilon_factor,
                                       double alpha,
                                       double final_epsilon) {
    const int n0 = cost.nrow;
    const int m0 = cost.ncol;

    // Handle empty case
    if (n0 == 0) {
        return LapResult({}, 0.0, "optimal");
    }

    // Dimension check
    if (n0 > m0) {
        LAP_THROW_DIMENSION("Infeasible: number of rows greater than number of columns");
    }

    // C reference requires balanced graph (n == m). If n < m, pad with dummies.
    bool needs_padding = (n0 < m0);
    CostMatrix padded_cost;
    int n = n0;
    int m = m0;

    if (needs_padding) {
        // Pad with dummy rows to make it square
        n = m0;
        padded_cost = CostMatrix(n, m);

        // Copy original costs
        for (int i = 0; i < n0; ++i) {
            for (int j = 0; j < m0; ++j) {
                padded_cost.at(i, j) = cost.at(i, j);
                padded_cost.mask[i * m + j] = cost.mask[i * m + j];
            }
        }

        // Set dummy rows to very high cost (very low profit in maximize case)
        double dummy_cost = 0.0;
        for (int i = 0; i < n0; ++i) {
            for (int j = 0; j < m0; ++j) {
                if (std::isfinite(cost.at(i, j))) {
                    dummy_cost = std::max(dummy_cost, std::abs(cost.at(i, j)));
                }
            }
        }
        dummy_cost = (dummy_cost + 1.0) * m * 10.0;

        for (int i = n0; i < n; ++i) {
            for (int j = 0; j < m; ++j) {
                padded_cost.at(i, j) = maximize ? -dummy_cost : dummy_cost;
                padded_cost.mask[i * m + j] = 1;  // Allow dummy edges
            }
        }
    } else {
        padded_cost = cost;
    }

    // Prepare working costs (negated if maximize, BIG for forbidden)
    CostMatrix work = prepare_for_solve(padded_cost, maximize);

    // Build CSR for the balanced (possibly padded) matrix
    std::vector<int> row_ptr, cols;
    build_allowed(work.mask, n, m, row_ptr, cols);

    // Find max cost for epsilon initialization
    double max_abs_cost = 0.0;
    for (int i = 0; i < n; ++i) {
        const int start = row_ptr[i];
        const int end = row_ptr[i + 1];
        for (int k = start; k < end; ++k) {
            int j = cols[k];
            double c = std::abs(work.at(i, j));
            if (std::isfinite(c) && c > max_abs_cost) {
                max_abs_cost = c;
            }
        }
    }

    double epsilon = std::max(1.0, max_abs_cost * initial_epsilon_factor);
    double eps_final = (final_epsilon > 0.0) ? final_epsilon :
                       std::min(1e-6, 1.0 / (static_cast<double>(n) * static_cast<double>(n)));

    // State: prices persist, matching rebuilt each phase
    std::vector<double> price(m, 0.0);
    std::vector<int> a_of_i(n, -1);
    std::vector<int> i_of_j(m, -1);

    // Price bounds to prevent overflow (scaled relative to problem size)
    const double price_bound = std::max(1e12, max_abs_cost * n * 1000.0);

    // C reference formulation: MINIMIZE (cost - price)
    auto reduced_cost = [&](int i, int j) {
        return work.at(i, j) - price[j];
    };

    // Find best (MINIMUM reduced cost) and second-best
    auto find_best = [&](int i, double& best_rc, double& second_rc, int& best_j) {
        const int start = row_ptr[i];
        const int end = row_ptr[i + 1];
        best_rc = std::numeric_limits<double>::infinity();
        second_rc = std::numeric_limits<double>::infinity();
        best_j = -1;

        if (end - start == 1) {
            best_j = cols[start];
            best_rc = reduced_cost(i, best_j);
            return;
        }

        for (int k = start; k < end; ++k) {
            int j = cols[k];
            double rc = reduced_cost(i, j);
            if (rc < best_rc) {
                second_rc = best_rc;
                best_rc = rc;
                best_j = j;
            } else if (rc < second_rc) {
                second_rc = rc;
            }
        }
    };

    int phase = 0;
    const long long max_iter = static_cast<long long>(n) * m * 100;

    // Epsilon-scaling loop
    do {
        phase++;

        // Reduce epsilon (C reference does this first each phase)
        epsilon /= alpha;
        if (epsilon < eps_final) epsilon = eps_final;

        // Discard matching
        std::fill(a_of_i.begin(), a_of_i.end(), -1);
        std::fill(i_of_j.begin(), i_of_j.end(), -1);

        // Rebuild matching - all n persons (including dummies if padded)
        std::vector<int> unmatched;
        unmatched.reserve(n);
        for (int i = 0; i < n; ++i) {
            unmatched.push_back(i);
        }

        long long iter = 0;
        while (!unmatched.empty()) {
            if (++iter > max_iter) {
                LAP_THROW_CONVERGENCE("Auction(scaled): iteration guard exceeded at phase " +
                                     std::to_string(phase));
            }

            int i = unmatched.back();
            unmatched.pop_back();

            double best_rc, second_rc;
            int best_j;
            find_best(i, best_rc, second_rc, best_j);

            if (best_j < 0) {
                LAP_THROW_INFEASIBLE("Person has no valid neighbors");
            }

            // Compute gamma - use epsilon-scaled default when only one option
            double gamma;
            if (second_rc == std::numeric_limits<double>::infinity() || !std::isfinite(second_rc)) {
                // Only one valid option: use small multiple of epsilon
                gamma = epsilon;
            } else {
                gamma = second_rc - best_rc;
                // Clamp gamma to prevent runaway prices
                if (gamma > price_bound) gamma = price_bound;
                if (gamma < 0.0) gamma = 0.0;  // Should not happen, but defensive
            }

            // DECREASE price with overflow protection
            double new_price = price[best_j] - (gamma + epsilon);
            if (new_price < -price_bound) new_price = -price_bound;
            price[best_j] = new_price;

            // Assignment
            int old = i_of_j[best_j];
            i_of_j[best_j] = i;
            if (old != -1) {
                a_of_i[old] = -1;
                unmatched.push_back(old);
            }
            a_of_i[i] = best_j;
        }
    } while (epsilon > eps_final);

    // Build assignment vector - only for ORIGINAL n0 rows (not dummy rows)
    std::vector<int> assignment(n0, -1);
    for (int i = 0; i < n0; ++i) {
        assignment[i] = a_of_i[i];
    }

    // Verify matching and compute total cost using ORIGINAL costs
    double total = 0.0;
    for (int i = 0; i < n0; ++i) {
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

    return LapResult(std::move(assignment), total, "optimal");
}

// Gauss-Seidel auction algorithm
LapResult solve_auction_gs(const CostMatrix& cost, bool maximize, double eps_in) {
    const int n0 = cost.nrow;
    const int m0 = cost.ncol;

    // Handle empty case
    if (n0 == 0) {
        return LapResult({}, 0.0, "optimal");
    }

    // Auto-transpose if n > m
    bool transposed = false;
    CostMatrix work_cost = cost;
    int n = n0;
    int m = m0;

    if (n0 > m0) {
        transposed = true;
        work_cost = transpose_cost(cost);
        n = work_cost.nrow;  // == m0
        m = work_cost.ncol;  // == n0
    }

    // Prepare working costs (negated if maximize, BIG for forbidden)
    CostMatrix work = prepare_for_solve(work_cost, maximize);

    // Check feasibility
    ensure_each_row_has_option(work.mask, n, m);

    // Build allowed structure (CSR)
    std::vector<int> row_ptr, cols;
    build_allowed(work.mask, n, m, row_ptr, cols);

    // Prices and assignments
    std::vector<double> price(m, 0.0);
    std::vector<int> a_of_i(n, -1), i_of_j(m, -1);

    // Determine epsilon
    // Adaptive epsilon: scale with cost spread / n² to match regular auction precision
    double eps;
    if (std::isfinite(eps_in) && eps_in > 0.0) {
        eps = eps_in;
    } else {
        const double spread = allowed_spread(work);
        const double base = (spread > 0.0) ? spread : 1.0;
        eps = base / (2.0 * static_cast<double>(n) * static_cast<double>(n));
        // Defensive clamp to prevent denormal or absurd values
        if (eps < 1e-12) eps = 1e-12;
        if (spread > 0.0 && eps > spread) eps = spread;
    }

    // Profit function (for minimization: maximize negative cost)
    auto profit = [&](int i, int j) {
        double base = -(work.at(i, j) + price[j]);
        // Tie-breaking perturbation
        unsigned key = static_cast<unsigned>((i + 1) * 1315423911u) ^
                       static_cast<unsigned>((j + 1) * 2654435761u);
        double tweak = std::ldexp(static_cast<double>(key & 0xFFFFu), -80);
        return base + tweak;
    };

    // Gauss-Seidel: iterate until all persons are matched
    long long total_bids = 0;
    const long long max_bids = 200000000LL;

    bool converged = false;
    while (!converged) {
        converged = true;

        for (int i = 0; i < n; ++i) {
            // Check if person i is still matched
            if (a_of_i[i] >= 0) {
                int j = a_of_i[i];
                if (i_of_j[j] == i) continue;
            }

            // Person i needs to bid
            converged = false;

            // Find best and second-best objects
            double best = -std::numeric_limits<double>::infinity();
            double second = -std::numeric_limits<double>::infinity();
            int jbest = -1;

            const int start = row_ptr[i];
            const int end = row_ptr[i + 1];

            for (int k = start; k < end; ++k) {
                int j = cols[k];
                double val = profit(i, j);
                if (val > best) {
                    second = best;
                    best = val;
                    jbest = j;
                } else if (val > second) {
                    second = val;
                }
            }

            if (jbest < 0) {
                LAP_THROW_INFEASIBLE("Person has no valid objects");
            }

            // Compute bid increment and update price immediately (GS difference)
            double bid = (second == -std::numeric_limits<double>::infinity()) ?
                        (2.0 * eps) : (best - second + eps);
            price[jbest] += bid;

            // Update assignment
            int old = i_of_j[jbest];
            i_of_j[jbest] = i;
            a_of_i[i] = jbest;

            if (old != -1 && old != i) {
                a_of_i[old] = -1;  // old person becomes unmatched
            }

            if (++total_bids > max_bids) {
                LAP_THROW_CONVERGENCE("Auction (Gauss-Seidel): exceeded iteration guard");
            }
        }
    }

    // Extract matching (in work orientation)
    std::vector<int> match_work(n, -1);
    for (int i = 0; i < n; ++i) {
        match_work[i] = a_of_i[i];
    }

    // Map back to original orientation if transposed
    std::vector<int> assignment;
    double total = 0.0;

    if (!transposed) {
        assignment = match_work;

        // Compute cost on original scale
        for (int i = 0; i < n; ++i) {
            int j = assignment[i];
            if (j < 0) {
                LAP_THROW_INFEASIBLE("Could not find full matching");
            }
            if (!work_cost.allowed(i, j)) {
                LAP_THROW_INFEASIBLE("Chosen forbidden edge");
            }
            double c = work_cost.at(i, j);
            if (!std::isfinite(c)) {
                LAP_THROW_INFEASIBLE("Chosen edge has non-finite cost");
            }
            total += c;
        }
    } else {
        // work is m0 x n0; match_work length m0
        assignment.assign(n0, -1);
        for (int i = 0; i < m0; ++i) {
            int j = match_work[i];
            if (j >= 0 && j < n0) {
                assignment[j] = i;
            }
        }

        // Compute cost on original scale
        for (int i = 0; i < n0; ++i) {
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
    }

    return LapResult(std::move(assignment), total, "optimal");
}

}  // namespace lap
