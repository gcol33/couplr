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
    const int n = static_cast<int>(cost.nrow);
    const int m = static_cast<int>(cost.ncol);

    // Handle empty case
    if (n == 0) {
        return LapResult({}, 0.0, "optimal");
    }

    lap::require_rows_fit_cols(n, m);

    // Prepare working costs (negated if maximize, BIG for forbidden)
    CostMatrix work = prepare_for_solve(cost, maximize);

    // Check feasibility on the real rows
    ensure_each_row_has_option(work.mask, n, m);

    // Find maximum absolute cost for epsilon initialization, and detect whether
    // all allowed costs are already integers.
    double max_abs_cost = 0.0;
    bool all_integer = true;
    double lo = std::numeric_limits<double>::infinity();
    double hi = -std::numeric_limits<double>::infinity();
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            if (work.allowed(i, j)) {
                double v = work.at(i, j);
                double av = std::abs(v);
                if (av > max_abs_cost) max_abs_cost = av;
                if (v < lo) lo = v;
                if (v > hi) hi = v;
                if (all_integer && std::abs(v - std::round(v)) > 1e-9) all_integer = false;
            }
        }
    }

    // CSA's epsilon-scaling optimality guarantee (eps < 1/n at the final scale)
    // holds only for integer costs; for real-valued inputs, competing
    // assignments can differ by less than n*eps and terminate suboptimally with
    // no error. Scale the allowed costs to integers first (as gabow_tarjan
    // does) so distinct assignment sums differ by >= 1 on the scaled problem.
    // Only allowed cells are touched, so the forbidden big-M sentinel is left
    // intact; the reported total is still computed from the ORIGINAL costs
    // below.
    //
    // What is scaled is the SPAN of the costs, after shifting the smallest to
    // zero, and not their distance from the origin. A complete assignment
    // matches every row exactly once, so subtracting a constant from every
    // allowed cell moves all of them by the same amount and leaves the optimum
    // where it was, while leaving the fixed-point scale the whole span to work
    // with. Scaling the magnitude instead spends the resolution on the offset:
    // costs spread over a decade but sitting near 1e9 keep three digits of it,
    // and a heavy-tailed matrix whose smallest entries are a billionth of its
    // largest rounds those entries to a common zero, after which the solver
    // cannot order them and returns whichever of the tied assignments it
    // reaches first.
    //
    // The span is bounded twice over. A double carries integers exactly to
    // 2^53, and padding gives a dummy row a cost of span * m * 10 that an
    // assignment sums m of, so span * m^2 * 10 has to stay inside that. The
    // auction sets the other bound: its work grows with the ratio of the cost
    // scale to the final epsilon, so a span taken as large as the exactness
    // bound allows makes the price wars of the last phases run past the
    // iteration limit. 1e9 is three orders above the flat 1e6 this used to
    // take and still leaves the phase count near where it was.
    if (!all_integer && hi > lo) {
        constexpr double EXACT_INT = 9007199254740992.0;  // 2^53
        const double md = static_cast<double>(m);
        const double span_limit = EXACT_INT / (10.0 * md * md);
        const double target_span = std::min(1e9, span_limit);
        const double scale = target_span / (hi - lo);

        // Rounding merges costs that sit closer together than the span's
        // resolution. Merging two large costs is the ordinary error of a
        // fixed-point conversion and is bounded. Merging costs onto the
        // MINIMUM is not: the solver then cannot order the cheapest pairs,
        // which are the ones an optimal assignment is made of, and it returns
        // whichever of the tied matchings it reaches first with no indication
        // that it did. A matrix whose entries span more than the resolution
        // can express, a heavy-tailed one above all, is refused here rather
        // than answered wrongly.
        bool bottom_collapsed = false;
        for (int i = 0; i < n; ++i) {
            for (int j = 0; j < m; ++j) {
                if (!work.allowed(i, j)) continue;
                const double v = work.at(i, j);
                const double scaled = std::round((v - lo) * scale);
                if (scaled == 0.0 && v > lo) bottom_collapsed = true;
                work.at(i, j) = scaled;
            }
        }
        if (bottom_collapsed) {
            LAP_THROW("csa: the cost range is too wide for its cost scaling to "
                      "order the cheapest pairs; the smallest costs round "
                      "together at a span of 1e9. Use method = 'jv' or "
                      "'auction', which read the costs as supplied.");
        }
        max_abs_cost = target_span;
    } else if (!all_integer && max_abs_cost > 0.0) {
        // Every allowed cost is the same value, so there is no span to scale
        // and the shift alone settles it.
        for (int i = 0; i < n; ++i) {
            for (int j = 0; j < m; ++j) {
                if (work.allowed(i, j)) work.at(i, j) = 0.0;
            }
        }
        max_abs_cost = 0.0;
    }

    // For rectangular problems (n < m) the auction has more objects than
    // persons, so free objects stay at price 0 and no real price competition
    // forms: each person greedily takes its cheapest object and the result is
    // suboptimal. Pad to a square m x m problem with dummy persons whose edges
    // all cost far more than any real assignment, restoring full competition
    // over all m objects. Only rows 0..n-1 are extracted below.
    const int nn = m;
    CostMatrix sq(nn, m);
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            sq.at(i, j) = work.at(i, j);
            sq.mask[static_cast<size_t>(flat_index(i, j, m))] = work.mask[static_cast<size_t>(flat_index(i, j, m))];
        }
    }
    if (n < nn) {
        const double dummy_cost = (max_abs_cost + 1.0) * m * 10.0;
        for (int i = n; i < nn; ++i) {
            for (int j = 0; j < m; ++j) {
                sq.at(i, j) = dummy_cost;
                sq.mask[static_cast<size_t>(flat_index(i, j, m))] = 1;
            }
        }
    }
    work = std::move(sq);

    // Build CSR-style allowed lists for efficient iteration
    std::vector<int64_t> row_ptr;
    std::vector<int> cols;
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
        const int64_t start = row_ptr[i], end = row_ptr[i + 1];
        best_rc = std::numeric_limits<double>::infinity();
        second_rc = std::numeric_limits<double>::infinity();
        best_j = -1;

        if (end - start == 1) {
            best_j = cols[start];
            best_rc = reduced_cost(i, best_j);
            return;
        }

        for (int64_t k = start; k < end; ++k) {
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
