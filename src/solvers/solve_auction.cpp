// src/solvers/solve_auction.cpp
// Pure C++ Auction LAP solver - NO Rcpp dependencies
//
// All public variants (basic, Gauss-Seidel, scaled) run one shared
// epsilon-scaling forward-auction core. Epsilon-scaling is what makes the
// auction exact rather than merely epsilon-optimal: a single fixed epsilon
// leaves a duality-gap slack of up to n*eps, so a coarse epsilon returns a
// suboptimal assignment on closely-spaced costs. Scaling epsilon down to a tiny
// final value drives that slack below the smallest achievable cost gap.
//
// Rectangular problems (n < m) are padded to a square graph with dummy rows.
// The padding matters for correctness, not just balance: with warm-started
// prices carried across phases, a person facing no competition for its object
// would otherwise flee it when the matching is rebuilt at the next (smaller)
// epsilon, so the auction oscillates instead of converging. Dummy rows supply
// that competition.
//
// The variants differ only in bidding discipline and exposed parameters:
//   - solve_auction        : queue drain, default schedule
//   - solve_auction_gs     : Gauss-Seidel sweep, default schedule
//   - solve_auction_scaled : queue drain with a caller-chosen alpha schedule
//
// The dense (CostMatrix) and lazy (LazyCostMatrix) entry points share one
// templated bidding loop, auction_core_impl<CostSourceT>. Dense padding
// still materializes a padded CostMatrix copy exactly as before (unchanged
// behavior/performance); lazy padding uses PaddedCostView to fake the extra
// dummy rows without copying the underlying feature data.

#include "solve_auction.h"
#include "../core/lap_error.h"
#include "../core/lap_utils.h"
#include "../core/lap_cost_view.h"
#include <vector>
#include <limits>
#include <algorithm>
#include <cmath>

namespace lap {

// Terminal epsilon small enough that n*eps stays below the smallest achievable
// cost gap on realistic inputs, lifting the auction from eps-optimal to exact.
static inline double default_eps_final(int n) {
    return std::min(1e-6, 1.0 / (static_cast<double>(n) * static_cast<double>(n)));
}

template <typename CostSourceT>
struct AuctionCoreResult {
    std::vector<int> a_of_i;  // padded size n: person -> object (0-based)
    long long iter;
};

// Shared epsilon-scaling forward-auction bidding loop. `work` must already be
// square, padded, and "prepared" (forbidden reads as BIG via at(), negated
// if the caller wants maximize) -- CostMatrix via prepare_for_solve(), or a
// LazyCostMatrix/PaddedCostView<LazyCostMatrix> which bake that in at
// construction. Returns the assignment over ALL padded rows; callers extract
// the real (unpadded) rows and verify/report using their own original-cost
// object.
template <typename CostSourceT>
static AuctionCoreResult<CostSourceT> auction_core_impl(const CostSourceT& work,
                                                         bool gauss_seidel,
                                                         double initial_epsilon_factor,
                                                         double alpha,
                                                         double final_epsilon) {
    const int n = static_cast<int>(work.nrow);
    const int m = static_cast<int>(work.ncol);

    ensure_each_row_has_option(work);
    std::vector<int64_t> row_ptr;
    std::vector<int> cols;
    build_allowed(work, row_ptr, cols);

    double max_abs_cost = 0.0;
    for (int i = 0; i < n; ++i) {
        for (int64_t k = row_ptr[i]; k < row_ptr[i + 1]; ++k) {
            double c = std::abs(work.at(i, cols[k]));
            if (std::isfinite(c) && c > max_abs_cost) max_abs_cost = c;
        }
    }

    double epsilon = std::max(1.0, max_abs_cost * initial_epsilon_factor);
    const double eps_final = (final_epsilon > 0.0) ? final_epsilon : default_eps_final(n);
    const double price_bound = std::max(1e12, max_abs_cost * n * 1000.0);

    std::vector<double> price(m, 0.0);
    std::vector<int> a_of_i(n, -1), i_of_j(m, -1);

    // Minimize reduced cost (cost - price); the bidder decreases the winning
    // object's price so contenders see it as more expensive.
    auto find_best = [&](int i, double& best_rc, double& second_rc, int& best_j) {
        best_rc = std::numeric_limits<double>::infinity();
        second_rc = std::numeric_limits<double>::infinity();
        best_j = -1;
        for (int64_t k = row_ptr[i]; k < row_ptr[i + 1]; ++k) {
            int j = cols[k];
            double rc = work.at(i, j) - price[j];
            if (rc < best_rc) { second_rc = best_rc; best_rc = rc; best_j = j; }
            else if (rc < second_rc) { second_rc = rc; }
        }
    };

    double eps_cur = eps_final;
    const long long max_iter = static_cast<long long>(n) * m * 200 + 1000;
    long long iter = 0;

    auto bid_person = [&](int i) -> int {
        double best_rc, second_rc;
        int best_j;
        find_best(i, best_rc, second_rc, best_j);
        if (best_j < 0) LAP_THROW_INFEASIBLE("Person has no valid objects");

        double gamma;
        if (!std::isfinite(second_rc)) {
            gamma = eps_cur;  // only one option
        } else {
            gamma = second_rc - best_rc;
            if (gamma > price_bound) gamma = price_bound;
            if (gamma < 0.0) gamma = 0.0;
        }
        double new_price = price[best_j] - (gamma + eps_cur);
        if (new_price < -price_bound) new_price = -price_bound;
        price[best_j] = new_price;

        int old = i_of_j[best_j];
        i_of_j[best_j] = i;
        a_of_i[i] = best_j;
        if (old != -1 && old != i) a_of_i[old] = -1;
        return old;
    };

    for (;;) {
        epsilon = std::max(epsilon / alpha, eps_final);
        eps_cur = epsilon;

        std::fill(a_of_i.begin(), a_of_i.end(), -1);
        std::fill(i_of_j.begin(), i_of_j.end(), -1);

        if (!gauss_seidel) {
            std::vector<int> queue;
            queue.reserve(n);
            for (int i = 0; i < n; ++i) queue.push_back(i);
            while (!queue.empty()) {
                int i = queue.back();
                queue.pop_back();
                int old = bid_person(i);
                if (old != -1) queue.push_back(old);
                if (++iter > max_iter)
                    LAP_THROW_CONVERGENCE("Auction: iteration guard exceeded");
            }
        } else {
            bool converged = false;
            while (!converged) {
                converged = true;
                for (int i = 0; i < n; ++i) {
                    if (a_of_i[i] >= 0 && i_of_j[a_of_i[i]] == i) continue;
                    converged = false;
                    bid_person(i);
                    if (++iter > max_iter)
                        LAP_THROW_CONVERGENCE("Auction (Gauss-Seidel): iteration guard exceeded");
                }
            }
        }

        if (epsilon <= eps_final) break;
    }

    return AuctionCoreResult<CostSourceT>{std::move(a_of_i), iter};
}

// Shared epsilon-scaling forward-auction core (dense CostMatrix).
//   initial_epsilon_factor : multiplies the starting epsilon
//   alpha                  : epsilon reduction factor per phase (> 1)
//   final_epsilon          : terminal epsilon (<= 0 selects default_eps_final)
//   gauss_seidel           : false drains a queue of unmatched persons; true
//                            sweeps all persons each round until none move
// Returns a LapResult over the ORIGINAL (unpadded) rows.
static LapResult auction_core(const CostMatrix& cost, bool maximize,
                              double initial_epsilon_factor, double alpha,
                              double final_epsilon, bool gauss_seidel,
                              long long* out_bids = nullptr) {
    const int n0 = static_cast<int>(cost.nrow);
    const int m0 = static_cast<int>(cost.ncol);

    if (n0 == 0) return LapResult({}, 0.0, "optimal");
    lap::require_rows_fit_cols(n0, m0);
    if (alpha <= 1.0) alpha = 7.0;

    // Pad rectangular problems to square with dummy rows (see file header).
    const bool needs_padding = (n0 < m0);
    int n = n0, m = m0;
    CostMatrix padded;

    if (needs_padding) {
        n = m0;
        padded = CostMatrix(n, m);
        double dummy_cost = 0.0;
        for (int i = 0; i < n0; ++i) {
            for (int j = 0; j < m0; ++j) {
                padded.at(i, j) = cost.at(i, j);
                padded.mask[static_cast<size_t>(lap::flat_index(i, j, m))] =
                    cost.mask[static_cast<size_t>(lap::flat_index(i, j, m))];
                if (cost.allowed(i, j) && std::isfinite(cost.at(i, j))) {
                    dummy_cost = std::max(dummy_cost, std::abs(cost.at(i, j)));
                }
            }
        }
        // Dummy rows: high cost (minimize) / very low profit (maximize), allowed
        // everywhere so they absorb the surplus columns.
        dummy_cost = (dummy_cost + 1.0) * m * 10.0;
        for (int i = n0; i < n; ++i) {
            for (int j = 0; j < m; ++j) {
                padded.at(i, j) = maximize ? -dummy_cost : dummy_cost;
                padded.mask[static_cast<size_t>(lap::flat_index(i, j, m))] = 1;  // allowed
            }
        }
    }
    const CostMatrix& base = needs_padding ? padded : cost;

    // Prepare working costs (negated if maximize, forbidden excluded via mask).
    CostMatrix work = prepare_for_solve(base, maximize);

    auto core = auction_core_impl(work, gauss_seidel, initial_epsilon_factor,
                                  alpha, final_epsilon);
    if (out_bids != nullptr) *out_bids = core.iter;

    // Verify the ORIGINAL rows and total on the ORIGINAL costs.
    std::vector<int> assignment(n0, -1);
    double total = 0.0;
    for (int i = 0; i < n0; ++i) {
        int j = core.a_of_i[i];
        if (j < 0) LAP_THROW_INFEASIBLE("Could not find full matching");
        if (!cost.allowed(i, j)) LAP_THROW_INFEASIBLE("Chosen forbidden edge");
        double c = cost.at(i, j);
        if (!std::isfinite(c)) LAP_THROW_INFEASIBLE("Chosen edge has non-finite cost");
        assignment[i] = j;
        total += c;
    }
    return LapResult(std::move(assignment), total, "optimal");
}

// Lazy cost-source variant. `cost` is already "prepared" (negate/caliper/
// max_distance baked in at construction) -- no prepare_for_solve() step.
static LapResult auction_core_lazy(const LazyCostMatrix& cost,
                                   double initial_epsilon_factor, double alpha,
                                   double final_epsilon, bool gauss_seidel,
                                   long long* out_bids = nullptr) {
    const int64_t n0 = cost.nrow;
    const int64_t m0 = cost.ncol;

    if (n0 == 0) return LapResult({}, 0.0, "optimal");
    lap::require_rows_fit_cols(n0, m0);
    if (alpha <= 1.0) alpha = 7.0;

    const bool needs_padding = (n0 < m0);

    if (!needs_padding) {
        auto core = auction_core_impl(cost, gauss_seidel, initial_epsilon_factor,
                                      alpha, final_epsilon);
        if (out_bids != nullptr) *out_bids = core.iter;

        std::vector<int> assignment(static_cast<size_t>(n0), -1);
        double total = 0.0;
        for (int64_t i = 0; i < n0; ++i) {
            int j = core.a_of_i[static_cast<size_t>(i)];
            if (j < 0) LAP_THROW_INFEASIBLE("Could not find full matching");
            if (!cost.allowed(i, j)) LAP_THROW_INFEASIBLE("Chosen forbidden edge");
            double c = cost.at(i, j);
            if (cost.is_negated()) c = -c;
            if (!std::isfinite(c)) LAP_THROW_INFEASIBLE("Chosen edge has non-finite cost");
            assignment[static_cast<size_t>(i)] = j;
            total += c;
        }
        return LapResult(std::move(assignment), total, "optimal");
    }

    // Rectangular: find the dummy cost (largest allowed |cost|) via one pass
    // over the real rows -- mirrors the dense padding loop's dummy_cost scan,
    // expressed via at()/allowed() instead of raw array access.
    double dummy_cost = 0.0;
    for (int64_t i = 0; i < n0; ++i) {
        for (int64_t j = 0; j < m0; ++j) {
            if (cost.allowed(i, j)) {
                double c = cost.at(i, j);
                if (std::isfinite(c)) dummy_cost = std::max(dummy_cost, std::abs(c));
            }
        }
    }
    dummy_cost = (dummy_cost + 1.0) * static_cast<double>(m0) * 10.0;
    // cost.at() already negates for maximize; the dummy rows must match sign
    // convention (very expensive to route flow through, in whichever
    // direction "expensive" means under the source's own negation).
    if (cost.is_negated()) dummy_cost = -dummy_cost;

    PaddedCostView<LazyCostMatrix> padded(cost, n0, dummy_cost);
    auto core = auction_core_impl(padded, gauss_seidel, initial_epsilon_factor,
                                  alpha, final_epsilon);
    if (out_bids != nullptr) *out_bids = core.iter;

    std::vector<int> assignment(static_cast<size_t>(n0), -1);
    double total = 0.0;
    for (int64_t i = 0; i < n0; ++i) {
        int j = core.a_of_i[static_cast<size_t>(i)];
        if (j < 0) LAP_THROW_INFEASIBLE("Could not find full matching");
        if (!cost.allowed(i, j)) LAP_THROW_INFEASIBLE("Chosen forbidden edge");
        double c = cost.at(i, j);
        if (cost.is_negated()) c = -c;
        if (!std::isfinite(c)) LAP_THROW_INFEASIBLE("Chosen edge has non-finite cost");
        assignment[static_cast<size_t>(i)] = j;
        total += c;
    }
    return LapResult(std::move(assignment), total, "optimal");
}

// Basic auction algorithm (queue drain, epsilon-scaled)
LapResult solve_auction(const CostMatrix& cost, bool maximize, double eps_in) {
    // An explicit epsilon becomes the terminal epsilon (the requested precision).
    double final_eps = (std::isfinite(eps_in) && eps_in > 0.0) ? eps_in : -1.0;
    return auction_core(cost, maximize, /*initial_epsilon_factor=*/1.0, /*alpha=*/7.0,
                        final_eps, /*gauss_seidel=*/false);
}

// Gauss-Seidel auction algorithm (sweep discipline, epsilon-scaled)
LapResult solve_auction_gs(const CostMatrix& cost, bool maximize, double eps_in,
                           long long* out_bids) {
    double final_eps = (std::isfinite(eps_in) && eps_in > 0.0) ? eps_in : -1.0;
    return auction_core(cost, maximize, /*initial_epsilon_factor=*/1.0, /*alpha=*/7.0,
                        final_eps, /*gauss_seidel=*/true, out_bids);
}

// Scaled-epsilon auction with custom parameters (queue drain)
LapResult solve_auction_scaled_params(const CostMatrix& cost, bool maximize,
                                       double initial_epsilon_factor,
                                       double alpha,
                                       double final_epsilon) {
    return auction_core(cost, maximize, initial_epsilon_factor, alpha,
                        final_epsilon, /*gauss_seidel=*/false);
}

// Scaled-epsilon auction. Maps the named schedule to a numeric alpha and
// forwards to solve_auction_scaled_params with default epsilon bounds.
LapResult solve_auction_scaled(const CostMatrix& cost, bool maximize,
                                const std::string& schedule) {
    double alpha = 7.0;
    if (schedule == "pow2" || schedule == "halves") {
        alpha = 4.0;
    }
    return solve_auction_scaled_params(cost, maximize,
                                       /*initial_epsilon_factor=*/1.0,
                                       alpha,
                                       /*final_epsilon=*/-1.0);
}

// Lazy cost-source overload: basic auction (queue drain, epsilon-scaled).
// `maximize` is baked into `cost` at construction; `eps_in` becomes the
// terminal epsilon, matching solve_auction()'s dense contract.
LapResult solve_auction(const LazyCostMatrix& cost, double eps_in) {
    double final_eps = (std::isfinite(eps_in) && eps_in > 0.0) ? eps_in : -1.0;
    return auction_core_lazy(cost, /*initial_epsilon_factor=*/1.0, /*alpha=*/7.0,
                             final_eps, /*gauss_seidel=*/false);
}

}  // namespace lap
