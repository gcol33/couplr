// src/solvers/solve_csa.cpp
// Pure C++ Goldberg-Kennedy Cost-Scaling Assignment (CSA) Algorithm - NO Rcpp dependencies

#include "solve_csa.h"
#include "../core/lap_error.h"
#include "../core/lap_utils.h"
#include <vector>
#include <limits>
#include <algorithm>
#include <cmath>

namespace lap {

LapResult solve_csa(const CostMatrix& cost, bool maximize) {
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
    const int nn = work.nrow;  // May be padded to square

    // Check feasibility
    ensure_each_row_has_option(work.mask, nn, m);

    // Find maximum absolute cost for epsilon initialization
    double max_abs_cost = 0.0;
    for (int i = 0; i < nn; ++i) {
        for (int j = 0; j < m; ++j) {
            if (work.allowed(i, j)) {
                double av = std::abs(work.at(i, j));
                if (av > max_abs_cost) max_abs_cost = av;
            }
        }
    }

    // Build CSR-style allowed lists for efficient iteration
    std::vector<int> row_ptr, cols;
    build_allowed(work.mask, nn, m, row_ptr, cols);

    // Dual variables (prices for objects)
    std::vector<double> price(m, 0.0);

    // Assignment arrays
    std::vector<int> a_of_i(nn, -1);  // person i -> object
    std::vector<int> i_of_j(m, -1);   // object j -> person

    // Reduced cost for minimization
    auto reduced_cost = [&](int i, int j) -> double {
        return work.at(i, j) - price[j];
    };

    // Find best (min reduced cost) and second-best for person i
    auto find_best = [&](int i, double& best_rc, double& second_rc, int& best_j) {
        const int start = row_ptr[i], end = row_ptr[i + 1];
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

    // Epsilon scaling parameters
    double epsilon = std::max(1.0, max_abs_cost);
    const double alpha = 7.0;  // Scaling factor
    const double eps_final = std::min(1e-6, 1.0 / (static_cast<double>(nn) * nn));

    int phase = 0;
    const long long max_iter = static_cast<long long>(nn) * m * 100;

    // Main epsilon-scaling loop (do-while ensures at least one phase at eps_final)
    do {
        phase++;

        // Reduce epsilon first
        epsilon /= alpha;
        if (epsilon < eps_final) epsilon = eps_final;

        // Discard matching
        std::fill(a_of_i.begin(), a_of_i.end(), -1);
        std::fill(i_of_j.begin(), i_of_j.end(), -1);

        // Rebuild matching for all persons
        std::vector<int> unmatched;
        unmatched.reserve(nn);
        for (int i = 0; i < nn; ++i) unmatched.push_back(i);

        long long iter = 0;
        while (!unmatched.empty()) {
            if (++iter > max_iter) {
                LAP_THROW("CSA: iteration limit exceeded");
            }

            int i = unmatched.back();
            unmatched.pop_back();

            double best_rc, second_rc;
            int best_j;
            find_best(i, best_rc, second_rc, best_j);

            if (best_j < 0) {
                LAP_THROW_INFEASIBLE("Infeasible: a person has no valid objects");
            }

            // Compute gamma (bid increment)
            double gamma = (!std::isfinite(second_rc)) ? 1e6 : (second_rc - best_rc);

            // DECREASE price (makes object more "expensive" in auction sense)
            price[best_j] -= (gamma + epsilon);

            // Handle displacement
            int old = i_of_j[best_j];
            i_of_j[best_j] = i;
            if (old != -1) {
                a_of_i[old] = -1;
                unmatched.push_back(old);
            }
            a_of_i[i] = best_j;
        }
    } while (epsilon > eps_final);

    // Build assignment: row -> column (0-based, only real rows, not dummies)
    std::vector<int> assignment(n, -1);
    for (int i = 0; i < n; ++i) {
        int j = a_of_i[i];
        assignment[i] = j;  // Already 0-based
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

}  // namespace lap
