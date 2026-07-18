// Randomized ground-truth verification harness.
//
// Every pure lap::solve_* that claims to return an OPTIMAL sum-assignment is
// compared against lap::solve_bruteforce (exact enumeration) over thousands of
// random matrices spanning integer, fractional, rectangular, maximize, and
// forbidden-edge inputs. lap::solve_bottleneck is checked against a brute-force
// minimax reference instead (different objective).
//
// This is the gate that decides whether a solver's Rcpp wrapper may delegate to
// the pure copy: a pure solver that disagrees with brute force here carries a
// latent optimality bug and must be fixed before its shipped path is pointed at
// it. Fixed seeds make every failure reproducible.

#include <catch2/catch_test_macros.hpp>
#include <catch2/catch_approx.hpp>

#include <algorithm>
#include <cmath>
#include <functional>
#include <limits>
#include <random>
#include <sstream>
#include <string>
#include <vector>

#include "core/lap_types.h"
#include "core/lap_error.h"

#include "solvers/solve_bruteforce.h"
#include "solvers/solve_jv.h"
#include "solvers/solve_csa.h"
#include "solvers/solve_ssp.h"
#include "solvers/solve_csflow.h"
#include "solvers/solve_hk01.h"
#include "solvers/solve_ssap_bucket.h"
#include "solvers/solve_cycle_cancel.h"
#include "solvers/solve_push_relabel.h"
#include "solvers/solve_auction.h"
#include "solvers/solve_bottleneck.h"
#include "network_simplex/solve_network_simplex.h"

using Catch::Approx;

namespace {

using Solver = std::function<lap::LapResult(const lap::CostMatrix&, bool)>;

struct NamedSolver {
    std::string name;
    Solver fn;
};

// Every pure solver that must return the sum-optimal assignment.
// jv is the anchor: it is independently verified elsewhere, so if jv passes on
// a matrix and another solver fails, the harness itself is sound.
std::vector<NamedSolver> optimal_solvers() {
    return {
        {"jv",              [](const lap::CostMatrix& c, bool mx){ return lap::solve_jv(c, mx); }},
        {"csa",             [](const lap::CostMatrix& c, bool mx){ return lap::solve_csa(c, mx); }},
        {"ssp",             [](const lap::CostMatrix& c, bool mx){ return lap::solve_ssp(c, mx); }},
        {"csflow",          [](const lap::CostMatrix& c, bool mx){ return lap::solve_csflow(c, mx); }},
        {"cycle_cancel",    [](const lap::CostMatrix& c, bool mx){ return lap::solve_cycle_cancel(c, mx); }},
        {"push_relabel",    [](const lap::CostMatrix& c, bool mx){ return lap::solve_push_relabel(c, mx); }},
        {"ssap_bucket",     [](const lap::CostMatrix& c, bool mx){ return lap::solve_ssap_bucket(c, mx); }},
        // hk01 is specialized to {0,1}/uniform costs and throws by contract on
        // general costs (both pure and rcpp), so it is exercised by its own
        // binary-cost test below, not this general-cost sweep.
        {"network_simplex", [](const lap::CostMatrix& c, bool mx){ return lap::solve_network_simplex(c, mx); }},
        {"auction",         [](const lap::CostMatrix& c, bool mx){ return lap::solve_auction(c, mx); }},
        {"auction_scaled",  [](const lap::CostMatrix& c, bool mx){ return lap::solve_auction_scaled(c, mx); }},
        {"auction_gs",      [](const lap::CostMatrix& c, bool mx){ return lap::solve_auction_gs(c, mx); }},
    };
}

std::string dump(const lap::CostMatrix& c) {
    std::ostringstream os;
    os << "\n" << c.nrow << "x" << c.ncol << " cost (BIG=forbidden):\n";
    for (int i = 0; i < c.nrow; ++i) {
        for (int j = 0; j < c.ncol; ++j) {
            if (!c.allowed(i, j)) os << "   .   ";
            else os << "  " << c.at(i, j) << " ";
        }
        os << "\n";
    }
    return os.str();
}

// Recompute the cost of a returned assignment and validate that it is an
// injective partial permutation using only allowed edges. Returns NaN and sets
// `ok=false` if the assignment is structurally invalid.
double assignment_cost(const lap::CostMatrix& c, const lap::LapResult& r, bool& ok) {
    ok = true;
    const int n = c.nrow, m = c.ncol;
    if (static_cast<int>(r.assignment.size()) != n) { ok = false; return std::nan(""); }
    std::vector<char> used(m, 0);
    double total = 0.0;
    int matched = 0;
    for (int i = 0; i < n; ++i) {
        int j = r.assignment[i];
        if (j < 0) continue;              // partial rows allowed only if n>m, never here
        if (j >= m || used[j] || !c.allowed(i, j)) { ok = false; return std::nan(""); }
        used[j] = 1;
        total += c.at(i, j);
        ++matched;
    }
    if (matched != n) { ok = false; return std::nan(""); }  // n<=m => every row matched
    return total;
}

// Random cost matrix with n <= m (the shipped contract; assignment() transposes
// n>m away before dispatch). `frac` selects sub-integer costs; `forbid` plants a
// guaranteed-feasible permutation then forbids a random subset of other edges.
lap::CostMatrix random_matrix(std::mt19937& rng, int n, int m, bool frac, bool forbid) {
    lap::CostMatrix c(n, m);
    std::uniform_int_distribution<int> icost(0, 9);
    std::uniform_int_distribution<int> qcost(0, 12);   // /4 -> 0.00 .. 3.00 in 0.25 steps
    for (int i = 0; i < n; ++i)
        for (int j = 0; j < m; ++j)
            c.at(i, j) = frac ? (qcost(rng) / 4.0) : static_cast<double>(icost(rng));

    if (forbid) {
        // Plant a feasible assignment: pick n distinct columns, keep those edges.
        std::vector<int> cols(m);
        for (int j = 0; j < m; ++j) cols[j] = j;
        std::shuffle(cols.begin(), cols.end(), rng);
        std::vector<char> keep(n * m, 0);
        for (int i = 0; i < n; ++i) keep[i * m + cols[i]] = 1;

        std::bernoulli_distribution forbid_p(0.4);
        for (int i = 0; i < n; ++i)
            for (int j = 0; j < m; ++j)
                if (!keep[i * m + j] && forbid_p(rng)) c.forbid(i, j);
    }
    return c;
}

// Compare one solver against brute force on one matrix. Non-fatal (CHECK) so a
// single run surfaces every mismatch across every solver.
void expect_optimal(const NamedSolver& s, const lap::CostMatrix& c, bool maximize) {
    lap::LapResult truth;
    bool truth_ok = true;
    try {
        truth = lap::solve_bruteforce(c, maximize);
    } catch (const lap::LapException&) {
        truth_ok = false;  // infeasible / unsupported -> nothing to compare
    }

    lap::LapResult got;
    bool threw = false;
    try {
        got = s.fn(c, maximize);
    } catch (const lap::LapException&) {
        threw = true;
    }

    if (!truth_ok) {
        INFO(s.name << " maximize=" << maximize << dump(c));
        CHECK(threw);        // solver must also report infeasibility
        return;
    }

    INFO(s.name << " maximize=" << maximize << dump(c));
    REQUIRE_FALSE(threw);

    bool valid = true;
    double recomputed = assignment_cost(c, got, valid);
    CHECK(valid);
    if (!valid) return;

    // Reported cost matches the assignment it returned...
    CHECK(got.total_cost == Approx(recomputed).margin(1e-9));
    // ...and that cost is the true optimum.
    CHECK(recomputed == Approx(truth.total_cost).margin(1e-9));
}

// Brute-force minimax value for the bottleneck objective.
double brute_minimax(const lap::CostMatrix& c, bool maximize, bool& feasible) {
    const int n = c.nrow, m = c.ncol;
    feasible = false;
    double best = maximize ? -std::numeric_limits<double>::infinity()
                           :  std::numeric_limits<double>::infinity();
    // Enumerate C(m,n) column subsets, then permutations.
    std::vector<int> choose(m, 0);
    std::fill(choose.begin(), choose.begin() + n, 1);
    std::sort(choose.rbegin(), choose.rend());
    do {
        std::vector<int> chosen;
        for (int j = 0; j < m; ++j) if (choose[j]) chosen.push_back(j);
        std::sort(chosen.begin(), chosen.end());
        do {
            double worst = maximize ? std::numeric_limits<double>::infinity()
                                    : -std::numeric_limits<double>::infinity();
            bool ok = true;
            for (int i = 0; i < n; ++i) {
                int j = chosen[i];
                if (!c.allowed(i, j) || !std::isfinite(c.at(i, j))) { ok = false; break; }
                double v = c.at(i, j);
                if (maximize) worst = std::min(worst, v);
                else          worst = std::max(worst, v);
            }
            if (ok) {
                feasible = true;
                if (maximize) best = std::max(best, worst);
                else          best = std::min(best, worst);
            }
        } while (std::next_permutation(chosen.begin(), chosen.end()));
    } while (std::prev_permutation(choose.begin(), choose.end()));
    return best;
}

}  // namespace

TEST_CASE("Ground truth: sum-optimal solvers vs brute force", "[ground_truth][optimal]") {
    auto solvers = optimal_solvers();

    struct Cat { const char* name; bool frac; bool forbid; };
    const std::vector<Cat> cats = {
        {"integer",    false, false},
        {"fractional", true,  false},
        {"forbidden",  false, true},
        {"frac+forbid",true,  true},
    };

    for (const auto& s : solvers) {
        DYNAMIC_SECTION("solver=" << s.name) {
            for (const auto& cat : cats) {
                DYNAMIC_SECTION("costs=" << cat.name) {
                    std::mt19937 rng(0xC0FFEEu);  // fixed per (solver,cat): reproducible
                    std::uniform_int_distribution<int> ndist(1, 5);
                    for (int trial = 0; trial < 300; ++trial) {
                        int n = ndist(rng);
                        int m = n + std::uniform_int_distribution<int>(0, 2)(rng);
                        bool maximize = (trial % 2 == 0);
                        auto c = random_matrix(rng, n, m, cat.frac, cat.forbid);
                        expect_optimal(s, c, maximize);
                    }
                }
            }
        }
    }
}

TEST_CASE("Ground truth: hk01 on binary/constant costs", "[ground_truth][hk01]") {
    // hk01's design point: {0,1} and uniform costs. It must be exactly optimal here.
    std::mt19937 rng(0xB1247u);
    std::uniform_int_distribution<int> ndist(1, 6);
    std::bernoulli_distribution bit(0.5);
    for (int trial = 0; trial < 400; ++trial) {
        int n = ndist(rng);
        int m = n + std::uniform_int_distribution<int>(0, 2)(rng);
        bool maximize = (trial % 2 == 0);
        lap::CostMatrix c(n, m);
        bool constant = (trial % 5 == 0);
        for (int i = 0; i < n; ++i)
            for (int j = 0; j < m; ++j)
                c.at(i, j) = constant ? 1.0 : static_cast<double>(bit(rng));
        NamedSolver hk{"hk01", [](const lap::CostMatrix& cc, bool mx){ return lap::solve_hk01(cc, mx); }};
        expect_optimal(hk, c, maximize);
    }
}

TEST_CASE("Ground truth: bottleneck vs brute-force minimax", "[ground_truth][bottleneck]") {
    std::mt19937 rng(0xB07u);
    std::uniform_int_distribution<int> ndist(1, 5);
    for (int trial = 0; trial < 600; ++trial) {
        int n = ndist(rng);
        int m = n + std::uniform_int_distribution<int>(0, 2)(rng);
        bool maximize = (trial % 2 == 0);
        bool frac = (trial % 3 == 0);
        bool forbid = (trial % 2 == 1);
        auto c = random_matrix(rng, n, m, frac, forbid);

        bool feasible = false;
        double truth = brute_minimax(c, maximize, feasible);

        lap::LapResult got;
        bool threw = false;
        try { got = lap::solve_bottleneck(c, maximize); }
        catch (const lap::LapException&) { threw = true; }

        INFO("bottleneck maximize=" << maximize << dump(c));
        if (!feasible) { CHECK(threw); continue; }
        REQUIRE_FALSE(threw);

        // Achieved bottleneck = extreme chosen edge over the returned assignment.
        bool valid = true;
        double worst = maximize ? std::numeric_limits<double>::infinity()
                                : -std::numeric_limits<double>::infinity();
        REQUIRE(static_cast<int>(got.assignment.size()) == n);
        std::vector<char> used(m, 0);
        for (int i = 0; i < n; ++i) {
            int j = got.assignment[i];
            if (j < 0 || j >= m || used[j] || !c.allowed(i, j)) { valid = false; break; }
            used[j] = 1;
            double v = c.at(i, j);
            if (maximize) worst = std::min(worst, v);
            else          worst = std::max(worst, v);
        }
        CHECK(valid);
        if (valid) {
            CHECK(worst == Approx(truth).margin(1e-9));
            CHECK(got.total_cost == Approx(truth).margin(1e-9));  // bottleneck returns the minimax value
        }
    }
}
