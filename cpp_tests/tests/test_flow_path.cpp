// Test suite for the design path: a sequence of problems differing in one
// knob, solved as one loop that carries its arcs, its flow and its structure
// from each value to the next.
//
// The claim every equality case makes is that a point of the path is the answer
// a cold solve at that value gives. The path reaches it warm, over an arc set
// that has been growing since the first value; the cold solve reaches it from an
// empty candidate set. The two are free to name different optima whenever the
// optimum is not unique, so the assertion is on the cost and the status.

#include <catch2/catch_test_macros.hpp>
#include <catch2/catch_approx.hpp>

#include "core/lap_error.h"
#include "core/lap_lazy_types.h"
#include "flow/flow_candidates.h"
#include "flow/flow_compile.h"
#include "flow/flow_implicit.h"
#include "flow/flow_oracle.h"
#include "flow/flow_path.h"
#include "flow/flow_problem.h"

#include <cstdint>
#include <random>
#include <vector>

using Catch::Approx;

namespace {

constexpr int64_t kVars = 2;

std::vector<double> random_points(int64_t n, std::mt19937& rng) {
    std::normal_distribution<double> norm(0.0, 1.0);
    std::vector<double> out(static_cast<std::size_t>(n * kVars));
    for (double& x : out) x = norm(rng);
    return out;
}

lap::LazyCostMatrix lazy_source(const std::vector<double>& left,
                                const std::vector<double>& right,
                                double max_distance) {
    return lap::LazyCostMatrix(left, right, kVars, lap::DistanceMetric::Euclidean,
                               std::vector<double>(), max_distance,
                               std::vector<lap::CaliperSpec>(), false);
}

// The loop run from cold at one value, which is what a caller who did not want
// a path would get.
lap::ImplicitResult solve_cold(const std::vector<double>& left,
                               const std::vector<double>& right,
                               double max_distance) {
    lap::LazyCostMatrix src = lazy_source(left, right, max_distance);
    lap::SourceOracle<lap::LazyCostMatrix> oracle(src);
    lap::CompiledDesign design = lap::compile_one_to_one(oracle, {});
    lap::CandidateSet cand(src.nrow, src.ncol);
    return lap::solve_implicit_assignment(src, design.problem, cand);
}

// The knob: the distance cut, and nothing else on the source moves with it.
auto widen_cut = [](lap::LazyCostMatrix& src, lap::FlowProblem&, double v) {
    src.set_max_distance(v);
};

lap::PathResult run_path(const std::vector<double>& left,
                         const std::vector<double>& right,
                         const std::vector<double>& values) {
    // An empty sweep is one of the things solve_path() refuses, so the source
    // is built at a value the refusal never reads.
    lap::LazyCostMatrix src =
        lazy_source(left, right, values.empty() ? 0.0 : values.front());
    lap::SourceOracle<lap::LazyCostMatrix> oracle(src);
    lap::CompiledDesign design = lap::compile_one_to_one(oracle, {});
    lap::CandidateSet cand(src.nrow, src.ncol);
    return lap::solve_path(src, design.problem, cand, values, widen_cut);
}

}  // namespace

TEST_CASE("A path point is the answer a cold solve at that value gives",
          "[flow][path]") {
    std::mt19937 rng(90210);
    const int64_t nr = 25;
    const int64_t nc = 120;
    const std::vector<double> left = random_points(nr, rng);
    const std::vector<double> right = random_points(nc, rng);

    const std::vector<double> values = {0.5, 1.0, 1.5, 2.5,
                                        std::numeric_limits<double>::infinity()};
    const lap::PathResult path = run_path(left, right, values);

    REQUIRE(path.points.size() == values.size());
    for (std::size_t k = 0; k < values.size(); ++k) {
        const lap::PathPoint& pt = path.points[k];
        const lap::ImplicitResult cold = solve_cold(left, right, values[k]);

        CHECK(pt.value == values[k]);
        CHECK(pt.status == cold.status);
        if (pt.status == "optimal") {
            CHECK(pt.total_cost == Approx(cold.total_cost).epsilon(1e-12));
            CHECK(pt.n_matched == nr);
            CHECK(pt.certified);
            CHECK(pt.certificate.certified_optimal);
        }
    }
}

TEST_CASE("A path's last point is the unconstrained problem", "[flow][path]") {
    std::mt19937 rng(90211);
    const std::vector<double> left = random_points(18, rng);
    const std::vector<double> right = random_points(80, rng);

    const double inf = std::numeric_limits<double>::infinity();
    const lap::PathResult path = run_path(left, right, {1.0, 2.0, inf});
    const lap::ImplicitResult cold = solve_cold(left, right, inf);

    REQUIRE(path.points.back().status == "optimal");
    CHECK(path.points.back().total_cost == Approx(cold.total_cost).epsilon(1e-12));
}

TEST_CASE("The candidate set carries across points and only ever grows",
          "[flow][path]") {
    std::mt19937 rng(90212);
    const std::vector<double> left = random_points(20, rng);
    const std::vector<double> right = random_points(90, rng);

    const double inf = std::numeric_limits<double>::infinity();
    const lap::PathResult path = run_path(left, right, {0.8, 1.2, 2.0, inf});

    int64_t running = 0;
    int64_t evaluated = 0;
    for (const lap::PathPoint& pt : path.points) {
        CHECK(pt.pairs_added >= 0);
        running += pt.pairs_added;
        evaluated += pt.edges_evaluated;
        CHECK(pt.candidate_edges == running);
        CHECK(pt.rounds.size() >= 1u);
    }
    CHECK(path.candidate_edges == running);
    CHECK(path.edges_evaluated == evaluated);
    CHECK(path.possible_edges == 20 * 90);
}

TEST_CASE("A later point costs fewer rounds than the same value from cold",
          "[flow][path]") {
    std::mt19937 rng(90213);
    const std::vector<double> left = random_points(30, rng);
    const std::vector<double> right = random_points(150, rng);

    const double inf = std::numeric_limits<double>::infinity();
    const std::vector<double> values = {1.0, 1.5, 2.0, 3.0, inf};
    const lap::PathResult path = run_path(left, right, values);
    const lap::ImplicitResult cold = solve_cold(left, right, inf);

    // The last point starts from a matching that is already optimal for a cut
    // just below it, so what is left to do is price the pairs the last widening
    // admitted. From cold the same value starts from nothing.
    CHECK(path.points.back().rounds.size() <= cold.rounds.size());
}

TEST_CASE("A descending sweep is refused before anything is solved",
          "[flow][path]") {
    std::mt19937 rng(90214);
    const std::vector<double> left = random_points(10, rng);
    const std::vector<double> right = random_points(40, rng);

    CHECK_THROWS_AS(run_path(left, right, {2.0, 1.0}), lap::DimensionException);
    CHECK_THROWS_AS(run_path(left, right, {1.0, 1.0}), lap::DimensionException);
    CHECK_THROWS_AS(run_path(left, right, std::vector<double>()),
                    lap::DimensionException);
}

TEST_CASE("Starting and continuing are told apart by the problem's own state",
          "[flow][path]") {
    std::mt19937 rng(90215);
    const int64_t nr = 12;
    const int64_t nc = 50;
    const std::vector<double> left = random_points(nr, rng);
    const std::vector<double> right = random_points(nc, rng);

    lap::LazyCostMatrix src = lazy_source(left, right, 1.5);
    lap::SourceOracle<lap::LazyCostMatrix> oracle(src);
    lap::CompiledDesign design = lap::compile_one_to_one(oracle, {});
    lap::CandidateSet cand(src.nrow, src.ncol);
    lap::RowSearch<lap::LazyCostMatrix> search(src);

    // Continuing a problem no master has been solved over has nothing to
    // continue from.
    CHECK_THROWS_AS(
        lap::continue_implicit_assignment(src, design.problem, cand, search),
        lap::DimensionException);

    const lap::ImplicitResult first =
        lap::start_implicit_assignment(src, design.problem, cand, search);
    REQUIRE(first.status == "optimal");

    // Starting over an arc set that is already the candidate set's would expand
    // it a second time.
    CHECK_THROWS_AS(
        lap::start_implicit_assignment(src, design.problem, cand, search),
        lap::DimensionException);

    // The problem holds the master it stopped on, which is what the next point
    // continues from.
    CHECK(design.problem.warm_flow.size() == design.problem.arcs.size());
    CHECK(!design.problem.warm_potential.empty());

    src.set_max_distance(std::numeric_limits<double>::infinity());
    const lap::ImplicitResult second =
        lap::continue_implicit_assignment(src, design.problem, cand, search);
    const lap::ImplicitResult cold =
        solve_cold(left, right, std::numeric_limits<double>::infinity());

    REQUIRE(second.status == "optimal");
    CHECK(second.total_cost == Approx(cold.total_cost).epsilon(1e-12));
    CHECK(second.certificate.certified_optimal);
}

TEST_CASE("A point too tight to match reports the witness and the path goes on",
          "[flow][path]") {
    std::mt19937 rng(90216);
    const std::vector<double> left = random_points(30, rng);
    const std::vector<double> right = random_points(34, rng);

    const double inf = std::numeric_limits<double>::infinity();
    const lap::PathResult path = run_path(left, right, {0.02, inf});

    REQUIRE(path.points.size() == 2u);
    CHECK(path.points[0].status == "infeasible");
    CHECK(!path.points[0].witness.rows.empty());
    CHECK(path.points[0].witness_certified);

    CHECK(path.points[1].status == "optimal");
    CHECK(path.points[1].certificate.certified_optimal);
    CHECK(path.points[1].n_matched == 30);
}
