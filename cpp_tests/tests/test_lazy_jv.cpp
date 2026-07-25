// Dense vs. lazy cost-source parity tests for solve_jv().
//
// Builds the SAME underlying feature data (left_mat/right_mat), then solves
// once via a materialized dense CostMatrix and once via a LazyCostMatrix
// that computes distances on demand, and asserts identical results. This is
// the hard proof that templating jv_core<CostSourceT> didn't change the
// dense path's behavior and that the lazy path computes the same LAP.

#include <catch2/catch_test_macros.hpp>
#include <catch2/catch_approx.hpp>

#include "core/lap_types.h"
#include "core/lap_lazy_types.h"
#include "solvers/solve_jv.h"

#include <vector>
#include <cmath>
#include <random>

using Catch::Approx;
using lap::CostMatrix;
using lap::LazyCostMatrix;
using lap::DistanceMetric;
using lap::CaliperSpec;

namespace {

double raw_distance(const std::vector<double>& li, const std::vector<double>& rj,
                    DistanceMetric metric, const std::vector<double>& inv_cov) {
    size_t p = li.size();
    switch (metric) {
        case DistanceMetric::Euclidean: {
            double s = 0.0;
            for (size_t k = 0; k < p; ++k) { double d = li[k] - rj[k]; s += d * d; }
            return std::sqrt(s);
        }
        case DistanceMetric::Manhattan: {
            double s = 0.0;
            for (size_t k = 0; k < p; ++k) s += std::abs(li[k] - rj[k]);
            return s;
        }
        case DistanceMetric::SquaredEuclidean: {
            double s = 0.0;
            for (size_t k = 0; k < p; ++k) { double d = li[k] - rj[k]; s += d * d; }
            return s;
        }
        case DistanceMetric::Chebyshev: {
            double s = 0.0;
            for (size_t k = 0; k < p; ++k) s = std::max(s, std::abs(li[k] - rj[k]));
            return s;
        }
        case DistanceMetric::Mahalanobis: {
            double s = 0.0;
            for (size_t a = 0; a < p; ++a) {
                double row_sum = 0.0;
                for (size_t b = 0; b < p; ++b) row_sum += inv_cov[a * p + b] * (li[b] - rj[b]);
                s += (li[a] - rj[a]) * row_sum;
            }
            return std::sqrt(std::max(s, 0.0));
        }
    }
    return 0.0;
}

// Builds a dense CostMatrix and a LazyCostMatrix from the same feature data,
// with identical caliper/max_distance/maximize semantics baked into each.
struct ParallelCost {
    CostMatrix dense;
    LazyCostMatrix lazy;
};

ParallelCost build_parallel(const std::vector<std::vector<double>>& left,
                            const std::vector<std::vector<double>>& right,
                            DistanceMetric metric,
                            const std::vector<double>& inv_cov,
                            double max_distance,
                            const std::vector<CaliperSpec>& calipers,
                            bool maximize) {
    int64_t n = static_cast<int64_t>(left.size());
    int64_t m = static_cast<int64_t>(right.size());
    int64_t p = static_cast<int64_t>(left[0].size());

    CostMatrix dense(n, m);
    for (int64_t i = 0; i < n; ++i) {
        for (int64_t j = 0; j < m; ++j) {
            double d = raw_distance(left[static_cast<size_t>(i)], right[static_cast<size_t>(j)],
                                    metric, inv_cov);
            bool allowed = true;
            for (const auto& cal : calipers) {
                if (std::abs(left[static_cast<size_t>(i)][static_cast<size_t>(cal.var_index)] -
                            right[static_cast<size_t>(j)][static_cast<size_t>(cal.var_index)]) > cal.threshold) {
                    allowed = false;
                }
            }
            if (std::isfinite(max_distance) && d > max_distance) allowed = false;
            if (!allowed) {
                dense.forbid(i, j);
            } else {
                // Always store the RAW (un-negated) distance: solve_jv(CostMatrix,
                // maximize) negates internally via prepare_for_solve(). The lazy
                // overload has no separate maximize parameter (it bakes negation
                // into the LazyCostMatrix itself, below), so pre-negating here
                // too would double-negate the dense path relative to what
                // solve_jv(pc.dense, maximize) actually does.
                dense.at(i, j) = d;
            }
        }
    }

    std::vector<double> left_flat, right_flat;
    for (const auto& row : left) for (double v : row) left_flat.push_back(v);
    for (const auto& row : right) for (double v : row) right_flat.push_back(v);

    LazyCostMatrix lazy(left_flat, right_flat, p, metric, inv_cov,
                        max_distance, calipers, maximize);

    return ParallelCost{std::move(dense), std::move(lazy)};
}

std::vector<std::vector<double>> random_points(int n, int p, unsigned seed) {
    std::mt19937 rng(seed);
    std::uniform_real_distribution<double> dist(-10.0, 10.0);
    std::vector<std::vector<double>> pts(static_cast<size_t>(n), std::vector<double>(static_cast<size_t>(p)));
    for (auto& row : pts) for (double& v : row) v = dist(rng);
    return pts;
}

}  // namespace

TEST_CASE("solve_jv: dense and lazy agree across metrics", "[lazy][jv]") {
    auto left = random_points(6, 3, 1);
    auto right = random_points(6, 3, 2);
    std::vector<double> no_inv_cov;

    for (auto metric : {DistanceMetric::Euclidean, DistanceMetric::Manhattan,
                        DistanceMetric::SquaredEuclidean, DistanceMetric::Chebyshev}) {
        auto pc = build_parallel(left, right, metric, no_inv_cov,
                                 std::numeric_limits<double>::infinity(), {}, false);
        auto dense_res = lap::solve_jv(pc.dense, false);
        auto lazy_res = lap::solve_jv(pc.lazy);

        REQUIRE(dense_res.total_cost == Approx(lazy_res.total_cost).margin(1e-9));
        REQUIRE(dense_res.assignment == lazy_res.assignment);
    }
}

TEST_CASE("solve_jv: dense and lazy agree for Mahalanobis", "[lazy][jv]") {
    auto left = random_points(5, 2, 3);
    auto right = random_points(5, 2, 4);
    // Simple diagonal inverse-covariance (identity-like, weighting dim 0 more).
    std::vector<double> inv_cov = {2.0, 0.0, 0.0, 1.0};

    auto pc = build_parallel(left, right, DistanceMetric::Mahalanobis, inv_cov,
                             std::numeric_limits<double>::infinity(), {}, false);
    auto dense_res = lap::solve_jv(pc.dense, false);
    auto lazy_res = lap::solve_jv(pc.lazy);

    REQUIRE(dense_res.total_cost == Approx(lazy_res.total_cost).margin(1e-9));
    REQUIRE(dense_res.assignment == lazy_res.assignment);
}

TEST_CASE("solve_jv: dense and lazy agree on rectangular problems", "[lazy][jv]") {
    auto left = random_points(4, 3, 5);
    auto right = random_points(9, 3, 6);
    std::vector<double> no_inv_cov;

    auto pc = build_parallel(left, right, DistanceMetric::Euclidean, no_inv_cov,
                             std::numeric_limits<double>::infinity(), {}, false);
    auto dense_res = lap::solve_jv(pc.dense, false);
    auto lazy_res = lap::solve_jv(pc.lazy);

    REQUIRE(dense_res.total_cost == Approx(lazy_res.total_cost).margin(1e-9));
    REQUIRE(dense_res.assignment == lazy_res.assignment);
}

// Deterministic fixture (not random): left[i] and right[i] are close (an
// "identity" pairing always available at small distance), while every
// cross pair i != j is far apart. This guarantees a feasible bipartite
// matching exists regardless of the forbidding threshold used below --
// random points can, by bad luck of the seed, produce a max_distance/caliper
// combination with NO perfect matching among allowed edges at all, which
// would make solve_jv() correctly throw for BOTH dense and lazy (not a
// dense-vs-lazy mismatch, just an unrelated infeasible fixture).
std::vector<std::vector<double>> identity_plus_offset(int n, double offset) {
    std::vector<std::vector<double>> pts(static_cast<size_t>(n), std::vector<double>(2));
    for (int i = 0; i < n; ++i) {
        pts[static_cast<size_t>(i)] = {10.0 * i, offset};
    }
    return pts;
}

TEST_CASE("solve_jv: dense and lazy agree with max_distance forbidding pairs", "[lazy][jv]") {
    auto left = identity_plus_offset(6, 0.0);
    auto right = identity_plus_offset(6, 1.0);  // identity pairs at distance 1
    std::vector<double> no_inv_cov;

    // max_distance = 5 leaves only the identity pairing allowed (cross pairs
    // are >= 9 apart) -- a single, deterministic, always-feasible matching.
    auto pc = build_parallel(left, right, DistanceMetric::Euclidean, no_inv_cov,
                             5.0, {}, false);
    auto dense_res = lap::solve_jv(pc.dense, false);
    auto lazy_res = lap::solve_jv(pc.lazy);

    REQUIRE(dense_res.total_cost == Approx(lazy_res.total_cost).margin(1e-9));
    REQUIRE(dense_res.assignment == lazy_res.assignment);
    REQUIRE(dense_res.total_cost == Approx(6.0).margin(1e-9));  // 6 identity pairs at dist 1
}

TEST_CASE("solve_jv: dense and lazy agree with a per-variable caliper", "[lazy][jv]") {
    // Variable 0 encodes "index" (identity pairs match exactly, cross pairs
    // differ by >= 10); variable 1 differs by only 1 for every pair. A
    // caliper on variable 0 with threshold 3 forbids every cross pair while
    // always leaving the identity pairing allowed -- deterministic and
    // always feasible, unlike a caliper applied to random points.
    auto left = identity_plus_offset(6, 0.0);
    auto right = identity_plus_offset(6, 1.0);
    std::vector<double> no_inv_cov;
    std::vector<CaliperSpec> calipers = {{0, 3.0}};  // caliper on variable index 0

    auto pc = build_parallel(left, right, DistanceMetric::Euclidean, no_inv_cov,
                             std::numeric_limits<double>::infinity(), calipers, false);
    auto dense_res = lap::solve_jv(pc.dense, false);
    auto lazy_res = lap::solve_jv(pc.lazy);

    REQUIRE(dense_res.total_cost == Approx(lazy_res.total_cost).margin(1e-9));
    REQUIRE(dense_res.assignment == lazy_res.assignment);
}

TEST_CASE("solve_jv: dense and lazy agree for maximize = TRUE", "[lazy][jv]") {
    auto left = random_points(5, 2, 11);
    auto right = random_points(5, 2, 12);
    std::vector<double> no_inv_cov;

    auto pc = build_parallel(left, right, DistanceMetric::Euclidean, no_inv_cov,
                             std::numeric_limits<double>::infinity(), {}, true);
    auto dense_res = lap::solve_jv(pc.dense, true);
    auto lazy_res = lap::solve_jv(pc.lazy);

    REQUIRE(dense_res.total_cost == Approx(lazy_res.total_cost).margin(1e-9));
    REQUIRE(dense_res.assignment == lazy_res.assignment);
}

TEST_CASE("solve_jv: lazy matches bruteforce-style ground truth on a tiny hand-checked case", "[lazy][jv]") {
    // 2x2, hand-computable: left units at (0,0),(10,10); right at (0,1),(10,11).
    // Optimal: 0->0 (dist=1), 1->1 (dist=1), total=2 (vs the cross pairing's
    // much larger diagonal distances).
    std::vector<std::vector<double>> left = {{0, 0}, {10, 10}};
    std::vector<std::vector<double>> right = {{0, 1}, {10, 11}};
    std::vector<double> no_inv_cov;

    auto pc = build_parallel(left, right, DistanceMetric::Euclidean, no_inv_cov,
                             std::numeric_limits<double>::infinity(), {}, false);
    auto lazy_res = lap::solve_jv(pc.lazy);

    REQUIRE(lazy_res.total_cost == Approx(2.0).margin(1e-9));
}
