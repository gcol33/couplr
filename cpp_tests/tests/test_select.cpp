// cpp_tests/tests/test_select.cpp
// The unit-selection branch and bound, against enumeration.
//
// Every instance is small enough to list all 0/1 points, so the optimum, the
// feasibility of the answer and the validity of every bound are checked
// against a statement of the problem that never touches the solver. The rows
// are a cardinality-style equation, which makes the swap search's classes
// non-trivial, and dense inequalities with continuous coefficients, the shape
// a moment row takes.

#include <catch2/catch_test_macros.hpp>

#include "core/lap_exact.h"
#include "select/select_bb.h"
#include "select/select_bound.h"

#include <cmath>
#include <cstdint>
#include <random>
#include <vector>

namespace {

namespace sel = lap::select;
namespace ex = lap::exact;

struct Instance {
    int64_t n_left = 0;
    int64_t n_right = 0;
    std::vector<double> c;
    std::vector<std::vector<double>> a;  // inequality rows, a.x <= 0
};

Instance random_instance(uint32_t seed) {
    std::mt19937 rng(seed);
    std::uniform_int_distribution<int> size(2, 6);
    std::normal_distribution<double> normal(0.0, 1.0);
    Instance in;
    in.n_left = size(rng);
    in.n_right = size(rng);
    const int64_t n = in.n_left + in.n_right;
    in.c.assign(static_cast<std::size_t>(n), 0.0);
    for (int64_t j = 0; j < in.n_left; ++j) in.c[static_cast<std::size_t>(j)] = -1.0;
    const int rows = 1 + static_cast<int>(rng() % 3);
    for (int r = 0; r < rows; ++r) {
        std::vector<double> row(static_cast<std::size_t>(n));
        const double b = 0.2 + 0.3 * std::fabs(normal(rng));
        for (int64_t j = 0; j < in.n_left; ++j) row[static_cast<std::size_t>(j)] = normal(rng) + 0.4 - b;
        for (int64_t j = in.n_left; j < n; ++j) row[static_cast<std::size_t>(j)] = -normal(rng);
        in.a.push_back(row);
    }
    return in;
}

sel::RowSpec row_of(const std::vector<double>& coef, double lo, double hi) {
    sel::RowSpec spec;
    spec.lo = lo;
    spec.hi = hi;
    for (double v : coef) spec.coef.push_back(v == 0.0 ? ex::Expansion() : ex::Expansion{v});
    return spec;
}

sel::SelectProblem build(const Instance& in) {
    const int64_t n = in.n_left + in.n_right;
    sel::SelectProblem prob(n);
    prob.c = in.c;
    prob.lower.assign(static_cast<std::size_t>(n), 0.0);
    prob.upper.assign(static_cast<std::size_t>(n), 1.0);
    prob.integer.assign(static_cast<std::size_t>(n), 1);
    prob.integral_objective = true;
    prob.perturb = true;
    std::vector<double> balance(static_cast<std::size_t>(n), 0.0);
    for (int64_t j = 0; j < in.n_left; ++j) balance[static_cast<std::size_t>(j)] = 1.0;
    for (int64_t j = in.n_left; j < n; ++j) balance[static_cast<std::size_t>(j)] = -1.0;
    prob.rows.add(row_of(balance, 0.0, 0.0));
    for (const auto& row : in.a) prob.rows.add(row_of(row, -sel::kInf, 0.0));
    return prob;
}

// The best objective by enumeration, or +1 when only the empty point is
// feasible (it always is: every row is zero there).
double brute_force(const Instance& in) {
    const int64_t n = in.n_left + in.n_right;
    double best = 0.0;
    for (uint32_t mask = 0; mask < (1u << n); ++mask) {
        int64_t nl = 0;
        int64_t nr = 0;
        for (int64_t j = 0; j < n; ++j) {
            if (!(mask >> j & 1u)) continue;
            (j < in.n_left ? nl : nr) += 1;
        }
        if (nl != nr) continue;
        bool ok = true;
        for (const auto& row : in.a) {
            ex::Expansion act;
            for (int64_t j = 0; j < n; ++j) {
                if (mask >> j & 1u) act = ex::expansion_sum(act, ex::Expansion{row[static_cast<std::size_t>(j)]});
            }
            if (ex::sign(act) > 0) ok = false;
        }
        if (ok) best = std::min(best, -static_cast<double>(nl));
    }
    return best;
}

bool feasible(const Instance& in, const std::vector<double>& x) {
    const int64_t n = in.n_left + in.n_right;
    double nl = 0.0;
    double nr = 0.0;
    for (int64_t j = 0; j < n; ++j) (j < in.n_left ? nl : nr) += x[static_cast<std::size_t>(j)];
    if (nl != nr) return false;
    for (const auto& row : in.a) {
        ex::Expansion act;
        for (int64_t j = 0; j < n; ++j) {
            if (x[static_cast<std::size_t>(j)] != 0.0) {
                act = ex::expansion_sum(act, ex::Expansion{row[static_cast<std::size_t>(j)]});
            }
        }
        if (ex::sign(act) > 0) return false;
    }
    return true;
}

}  // namespace

TEST_CASE("the search returns the enumerated optimum and a feasible point",
          "[select]") {
    for (uint32_t seed = 1; seed <= 200; ++seed) {
        const Instance in = random_instance(seed);
        sel::SelectProblem prob = build(in);
        sel::Limits limits;
        sel::BranchAndBound bb(prob, nullptr, limits);
        const int64_t n = prob.n();
        bb.set_incumbent(std::vector<double>(static_cast<std::size_t>(n), 0.0), ex::Expansion());
        const sel::SearchResult res = bb.run();
        INFO("seed " << seed);
        REQUIRE(res.stopped == sel::Stop::optimality);
        REQUIRE(res.has_incumbent);
        REQUIRE(feasible(in, res.x));
        REQUIRE(ex::compare(res.value, brute_force(in)) == 0);
        REQUIRE(res.bound <= brute_force(in));
    }
}

TEST_CASE("weak-duality bounds never exceed a feasible point, at any duals",
          "[select]") {
    std::mt19937 rng(7);
    std::normal_distribution<double> normal(0.0, 1.0);
    for (uint32_t seed = 1; seed <= 60; ++seed) {
        const Instance in = random_instance(seed);
        sel::SelectProblem prob = build(in);
        const double best = brute_force(in);
        const sel::Box box{&prob.lower, &prob.upper};
        for (int draw = 0; draw < 5; ++draw) {
            std::vector<double> y(static_cast<std::size_t>(prob.rows.m()));
            for (double& v : y) v = normal(rng);
            const sel::RigorousBound rb = sel::rigorous_bound(prob.c, prob.rows, box, y);
            bool finite = true;
            const ex::Expansion eb = sel::exact_bound(prob.c, prob.rows, box, y, finite);
            INFO("seed " << seed << " draw " << draw);
            REQUIRE(rb.value <= best);
            if (finite) {
                REQUIRE(ex::compare(eb, best) <= 0);
                REQUIRE(ex::compare(eb, rb.value) >= 0);
            }
        }
    }
}

TEST_CASE("rows an oracle adds are kept and the optimum respects them",
          "[select]") {
    // The oracle forbids taking left units 0 and 1 together, the shape of a
    // Hall row, by answering any point that does with the row x0 + x1 <= 1.
    for (uint32_t seed = 1; seed <= 80; ++seed) {
        const Instance in = random_instance(seed);
        if (in.n_left < 2) continue;
        sel::SelectProblem prob = build(in);
        const int64_t n = prob.n();
        int64_t calls = 0;
        sel::Oracle oracle = [&](const std::vector<double>& x) {
            ++calls;
            sel::Verdict v;
            if (x[0] + x[1] > 1.0) {
                std::vector<double> coef(static_cast<std::size_t>(n), 0.0);
                coef[0] = 1.0;
                coef[1] = 1.0;
                v.rows.push_back(row_of(coef, -sel::kInf, 1.0));
                return v;
            }
            v.feasible = true;
            return v;
        };
        sel::Limits limits;
        sel::BranchAndBound bb(prob, oracle, limits);
        bb.set_incumbent(std::vector<double>(static_cast<std::size_t>(n), 0.0), ex::Expansion());
        const sel::SearchResult res = bb.run();

        // Enumeration with the oracle's row stated directly.
        double best = 0.0;
        for (uint32_t mask = 0; mask < (1u << n); ++mask) {
            if ((mask & 3u) == 3u) continue;
            std::vector<double> x(static_cast<std::size_t>(n), 0.0);
            for (int64_t j = 0; j < n; ++j) x[static_cast<std::size_t>(j)] = (mask >> j & 1u) ? 1.0 : 0.0;
            if (!feasible(in, x)) continue;
            double z = 0.0;
            for (int64_t j = 0; j < in.n_left; ++j) z -= x[static_cast<std::size_t>(j)];
            best = std::min(best, z);
        }
        INFO("seed " << seed);
        REQUIRE(res.stopped == sel::Stop::optimality);
        REQUIRE(!(res.x[0] == 1.0 && res.x[1] == 1.0));
        REQUIRE(ex::compare(res.value, best) == 0);
    }
}

TEST_CASE("small moment coefficients preserve the optimum under row scaling",
          "[select]") {
    for (int exponent : {-900, -40, -31, -10, 0, 10, 900}) {
        for (bool fixed_pivot : {false, true}) {
            const int64_t n = 64;
            sel::SelectProblem prob(n);
            prob.c.assign(n, -1.0);
            prob.lower.assign(n, 0.0);
            prob.upper.assign(n, 1.0);
            prob.integer.assign(n, 1);
            prob.integral_objective = true;
            std::vector<double> moment(n, 0.0);
            for (int64_t j = 0; j < n / 2; ++j) moment[j] = std::ldexp(1.0, exponent);
            if (fixed_pivot) {
                prob.upper.back() = 0.0;
                moment.back() = std::ldexp(1.0, exponent + 31);
            }
            prob.rows.add(row_of(moment, -sel::kInf, 0.0));
            sel::Limits limits;
            sel::BranchAndBound bb(prob, nullptr, limits);
            bb.set_incumbent(std::vector<double>(n, 0.0), ex::Expansion());
            const sel::SearchResult res = bb.run();
            const double optimum = fixed_pivot ? -31.0 : -32.0;
            INFO("exponent " << exponent << " fixed pivot " << fixed_pivot);
            REQUIRE(res.stopped == sel::Stop::optimality);
            REQUIRE(res.bound_certified);
            REQUIRE(ex::compare(res.value, optimum) == 0);
            REQUIRE(res.bound <= optimum);
            for (int64_t j = 0; j < n / 2; ++j) REQUIRE(res.x[j] == 0.0);
        }
    }
}

TEST_CASE("a bounded search retains a conservative bound with small rows",
          "[select]") {
    sel::SelectProblem prob(64);
    prob.c.assign(64, -1.0);
    prob.lower.assign(64, 0.0);
    prob.upper.assign(64, 1.0);
    prob.integer.assign(64, 1);
    prob.integral_objective = true;
    prob.upper.back() = 0.0;
    std::vector<double> moment(64, 0.0);
    for (int j = 0; j < 32; ++j) moment[j] = std::ldexp(1.0, -31);
    moment.back() = 1.0;
    prob.rows.add(row_of(moment, -sel::kInf, 0.0));
    sel::Limits limits;
    limits.node_limit = 3;
    sel::BranchAndBound bb(prob, nullptr, limits);
    bb.set_incumbent(std::vector<double>(64, 0.0), ex::Expansion());
    const sel::SearchResult res = bb.run();
    REQUIRE((res.stopped == sel::Stop::node_limit || res.stopped == sel::Stop::optimality));
    REQUIRE(res.n_nodes <= limits.node_limit);
    REQUIRE(res.bound <= -31.0);
    REQUIRE(ex::compare(res.value, -31.0) >= 0);
}

TEST_CASE("an unresolved continuous leaf retains its inherited bound",
          "[select]") {
    sel::SelectProblem prob(1);
    prob.c = {-1.0};
    prob.lower = {0.0};
    prob.upper = {1.0};
    prob.integer = {0};
    sel::Limits limits;
    limits.lp_iteration_limit = 0;
    sel::BranchAndBound bb(prob, nullptr, limits);
    bb.set_incumbent({0.0}, ex::Expansion());
    const sel::SearchResult res = bb.run();
    REQUIRE_FALSE(res.bound_certified);
    REQUIRE(res.bound <= -1.0);
}

TEST_CASE("row normalization preserves exact residuals and finite endpoints",
          "[select]") {
    sel::RowSpec spec;
    spec.coef = {{std::ldexp(1.0, -90), std::ldexp(1.0, -40)},
                 {-std::ldexp(1.0, -40)}};
    spec.lo = -sel::kInf;
    spec.hi = 0.0;
    sel::ExactRows rows(2);
    rows.add(spec);
    REQUIRE(ex::sign(rows.activity(0, {0, 1}, {1.0, 1.0})) > 0);
    REQUIRE(ex::compare(rows.entry(0, 1), -1.0) == 0);

    spec.coef = {{std::numeric_limits<double>::denorm_min()}};
    spec.hi = std::numeric_limits<double>::max();
    const int exponent = sel::row_scale_exponent(spec);
    const double endpoint = std::scalbn(spec.hi, exponent);
    REQUIRE(std::isfinite(endpoint));
    REQUIRE(std::scalbn(endpoint, -exponent) == spec.hi);
    const double component = std::scalbn(spec.coef[0][0], exponent);
    REQUIRE(std::scalbn(component, -exponent) == spec.coef[0][0]);
}

TEST_CASE("a scaled nonzero distance cut closes its integral root exactly",
          "[select]") {
    for (int exponent : {-20, -10, 0, 10, 30}) {
        const double a = std::ldexp(1.0, exponent);
        sel::SelectProblem prob(3);
        prob.c = {0.0, 0.0, 1.0};
        prob.lower = {0.0, 0.0, 0.0};
        prob.upper = {1.0, 1.0, sel::kInf};
        prob.integer = {1, 1, 0};
        prob.theta = 2;
        prob.rows.add(row_of({1.0, 1.0, 0.0}, 1.0, 1.0));
        auto cut = row_of({-a, -2.0 * a, 1.0}, 0.0, sel::kInf);
        cut.hard = false;
        prob.rows.add(cut);
        sel::Oracle oracle = [a](const std::vector<double>& x) {
            sel::Verdict verdict;
            verdict.feasible = true;
            verdict.has_value = true;
            verdict.value = ex::Expansion{a * x[0] + 2.0 * a * x[1]};
            return verdict;
        };
        sel::Limits limits;
        limits.node_limit = 1;
        sel::BranchAndBound bb(prob, oracle, limits);
        bb.set_incumbent({0.0, 1.0, 2.0 * a}, ex::Expansion{2.0 * a});
        const sel::SearchResult res = bb.run();
        INFO("distance exponent " << exponent);
        REQUIRE(res.stopped == sel::Stop::optimality);
        REQUIRE(res.bound_certified);
        REQUIRE(ex::compare(res.value, a) == 0);
        REQUIRE(res.bound == a);
        REQUIRE(res.n_nodes == 1);
    }
}
