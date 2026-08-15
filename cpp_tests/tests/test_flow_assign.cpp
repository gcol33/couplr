// Test suite for solve_assignment_flow(): the part five solvers share once
// their own residual graphs are gone. Exercised without Rcpp.

#include <catch2/catch_test_macros.hpp>
#include <catch2/catch_approx.hpp>

#include "core/lap_types.h"
#include "flow/flow_assign.h"
#include "flow/flow_oracle.h"
#include "flow/flow_problem.h"
#include "solvers/solve_jv.h"

#include <cstddef>
#include <cstdint>
#include <random>
#include <set>
#include <vector>

using Catch::Approx;

namespace {

lap::CostMatrix random_cost(int64_t nr, int64_t nc, uint32_t seed) {
    std::mt19937 rng(seed);
    std::uniform_real_distribution<double> unif(-5.0, 20.0);
    lap::CostMatrix c(nr, nc);
    for (int64_t i = 0; i < nr; ++i) {
        for (int64_t j = 0; j < nc; ++j) c.at(i, j) = unif(rng);
    }
    return c;
}

double cost_of(const lap::CostMatrix& c, const std::vector<int>& match) {
    double total = 0.0;
    for (std::size_t i = 0; i < match.size(); ++i) {
        if (match[i] >= 0) total += c.at(static_cast<int64_t>(i), match[i]);
    }
    return total;
}

// Every matched pair admissible, and no column taken twice.
void require_valid(const lap::CostMatrix& c, const std::vector<int>& match) {
    std::set<int> used;
    for (std::size_t i = 0; i < match.size(); ++i) {
        if (match[i] < 0) continue;
        REQUIRE(match[i] < static_cast<int>(c.ncol));
        REQUIRE(c.allowed(static_cast<int64_t>(i), match[i]));
        REQUIRE(used.insert(match[i]).second);
    }
}

}  // namespace

TEST_CASE("solve_assignment_flow - reaches the assignment optimum", "[flow][assign]") {
    struct Shape { int64_t nr; int64_t nc; };
    const Shape shapes[] = {{1, 1}, {4, 4}, {5, 9}, {8, 8}, {3, 20}, {12, 15}};

    uint32_t seed = 4100u;
    for (const Shape& shape : shapes) {
        const lap::CostMatrix c = random_cost(shape.nr, shape.nc, seed++);

        lap::SourceOracle<lap::CostMatrix> oracle(c);
        const lap::AssignmentFlow flow = lap::solve_assignment_flow(oracle);

        REQUIRE(flow.status == "optimal");
        REQUIRE(flow.n_matched == shape.nr);
        require_valid(c, flow.match);

        const lap::LapResult jv = lap::solve_jv(c, false);
        REQUIRE(cost_of(c, flow.match) == Approx(jv.total_cost).epsilon(1e-12));
    }
}

TEST_CASE("solve_assignment_flow - forbidden pairs are not arcs", "[flow][assign]") {
    lap::CostMatrix c = random_cost(5, 7, 4200u);

    // Leave every row one admissible column, and make it an expensive one, so a
    // solve that ignored the mask would return a cheaper answer than the
    // constrained optimum rather than the same one.
    for (int64_t i = 0; i < c.nrow; ++i) {
        for (int64_t j = 0; j < c.ncol; ++j) {
            if (j != i) c.forbid(i, j);
        }
        c.at(i, i) = 100.0 + static_cast<double>(i);
    }

    lap::SourceOracle<lap::CostMatrix> oracle(c);
    const lap::AssignmentFlow flow = lap::solve_assignment_flow(oracle);

    REQUIRE(flow.status == "optimal");
    REQUIRE(flow.n_matched == 5);
    require_valid(c, flow.match);
    for (int i = 0; i < 5; ++i) REQUIRE(flow.match[static_cast<std::size_t>(i)] == i);
}

TEST_CASE("solve_assignment_flow - a row with no admissible column goes unmatched",
          "[flow][assign]") {
    lap::CostMatrix c = random_cost(4, 6, 4300u);
    for (int64_t j = 0; j < c.ncol; ++j) c.forbid(2, j);

    lap::SourceOracle<lap::CostMatrix> oracle(c);
    const lap::AssignmentFlow flow = lap::solve_assignment_flow(oracle);

    REQUIRE(flow.n_matched == 3);
    REQUIRE(flow.status == "partial");
    REQUIRE(flow.match[2] == -1);
    require_valid(c, flow.match);
}

TEST_CASE("solve_assignment_flow - more rows than columns places what it can",
          "[flow][assign]") {
    const lap::CostMatrix c = random_cost(6, 4, 4400u);

    lap::SourceOracle<lap::CostMatrix> oracle(c);
    const lap::AssignmentFlow flow = lap::solve_assignment_flow(oracle);

    REQUIRE(flow.n_matched == 4);
    REQUIRE(flow.status == "partial");
    REQUIRE(flow.match.size() == 6u);
    require_valid(c, flow.match);
}

TEST_CASE("solve_assignment_flow - an empty side is not a malformed problem",
          "[flow][assign]") {
    SECTION("no rows") {
        const lap::CostMatrix c(0, 5);
        lap::SourceOracle<lap::CostMatrix> oracle(c);
        const lap::AssignmentFlow flow = lap::solve_assignment_flow(oracle);
        REQUIRE(flow.match.empty());
        REQUIRE(flow.n_matched == 0);
        REQUIRE(flow.status == "optimal");
    }

    SECTION("no columns") {
        const lap::CostMatrix c(3, 0);
        lap::SourceOracle<lap::CostMatrix> oracle(c);
        const lap::AssignmentFlow flow = lap::solve_assignment_flow(oracle);
        REQUIRE(flow.match.size() == 3u);
        REQUIRE(flow.n_matched == 0);
        REQUIRE(flow.status == "infeasible");
    }
}

TEST_CASE("solve_assignment_flow - each caller's relaxation predicate is honoured",
          "[flow][assign]") {
    // The three predicates the migrated solvers carry. They may select
    // different optima where two are equally cheap, which is why each caller
    // keeps its own; what none of them may do is return a worse one.
    const double slacks[] = {0.0, 1e-18, lap::TOL};

    uint32_t seed = 4500u;
    for (int rep = 0; rep < 6; ++rep) {
        const lap::CostMatrix c = random_cost(7, 11, seed++);
        const lap::LapResult jv = lap::solve_jv(c, false);

        for (double slack : slacks) {
            lap::FlowOptions opts;
            opts.relax_eps = slack;

            lap::SourceOracle<lap::CostMatrix> oracle(c);
            const lap::AssignmentFlow flow = lap::solve_assignment_flow(oracle, opts);

            REQUIRE(flow.status == "optimal");
            require_valid(c, flow.match);
            REQUIRE(cost_of(c, flow.match) == Approx(jv.total_cost).epsilon(1e-12));
        }
    }
}

TEST_CASE("solve_assignment_flow - potentials come back with the matching",
          "[flow][assign]") {
    const lap::CostMatrix c = random_cost(5, 8, 4600u);

    lap::SourceOracle<lap::CostMatrix> oracle(c);
    const lap::AssignmentFlow flow = lap::solve_assignment_flow(oracle);

    REQUIRE(flow.potential.size() == static_cast<std::size_t>(2 + c.nrow + c.ncol));
    REQUIRE(flow.potential[lap::FLOW_SOURCE] == Approx(0.0));

    lap::FlowOptions opts;
    opts.return_potentials = false;
    const lap::AssignmentFlow bare = lap::solve_assignment_flow(oracle, opts);
    REQUIRE(bare.potential.empty());
    REQUIRE(bare.match == flow.match);
}
