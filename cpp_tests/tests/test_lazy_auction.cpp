// Dense vs. lazy cost-source parity tests for solve_auction().
//
// Same approach as test_lazy_jv.cpp: build the SAME underlying feature data,
// solve once via a materialized dense CostMatrix and once via a
// LazyCostMatrix, and assert agreement. The rectangular cases specifically
// exercise PaddedCostView, since auction's padding step is the harder part
// of templating this solver (a real copy for dense, a zero-copy decorator
// for lazy).

#include <catch2/catch_test_macros.hpp>
#include <catch2/catch_approx.hpp>

#include "core/lap_types.h"
#include "core/lap_lazy_types.h"
#include "solvers/solve_auction.h"

#include <vector>
#include <cmath>
#include <random>
#include <limits>

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

struct ParallelCost {
    CostMatrix dense;
    LazyCostMatrix lazy;
};

// Always stores RAW (un-negated) distances in `dense`: solve_auction(dense,
// maximize) negates internally; the lazy path's `maximize` is baked into the
// LazyCostMatrix itself (no separate parameter), so pre-negating dense here
// too would double-negate relative to what solve_auction(pc.dense, maximize)
// actually does (same lesson as test_lazy_jv.cpp).
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

// Deterministic (not random) fixture: left[i]/right[i] are close ("identity"
// pairing always available and cheap), cross pairs are far apart. Guarantees
// a feasible matching exists regardless of forbidding threshold -- see
// test_lazy_jv.cpp for why random points are unsafe for these cases.
std::vector<std::vector<double>> identity_plus_offset(int n, double offset) {
    std::vector<std::vector<double>> pts(static_cast<size_t>(n), std::vector<double>(2));
    for (int i = 0; i < n; ++i) {
        pts[static_cast<size_t>(i)] = {10.0 * i, offset};
    }
    return pts;
}

std::vector<std::vector<double>> random_points(int n, int p, unsigned seed) {
    std::mt19937 rng(seed);
    std::uniform_real_distribution<double> dist(-10.0, 10.0);
    std::vector<std::vector<double>> pts(static_cast<size_t>(n), std::vector<double>(static_cast<size_t>(p)));
    for (auto& row : pts) for (double& v : row) v = dist(rng);
    return pts;
}

// Auction is only epsilon-optimal in principle; the implementation's
// epsilon-scaling drives error toward zero, but allow a small tolerance
// wider than JV's exact comparison.
constexpr double AUCTION_TOL = 1e-6;

}  // namespace

TEST_CASE("solve_auction: dense and lazy agree across metrics (square)", "[lazy][auction]") {
    auto left = random_points(6, 3, 21);
    auto right = random_points(6, 3, 22);
    std::vector<double> no_inv_cov;

    for (auto metric : {DistanceMetric::Euclidean, DistanceMetric::Manhattan,
                        DistanceMetric::SquaredEuclidean, DistanceMetric::Chebyshev}) {
        auto pc = build_parallel(left, right, metric, no_inv_cov,
                                 std::numeric_limits<double>::infinity(), {}, false);
        auto dense_res = lap::solve_auction(pc.dense, false);
        auto lazy_res = lap::solve_auction(pc.lazy);

        REQUIRE(dense_res.total_cost == Approx(lazy_res.total_cost).margin(AUCTION_TOL));
    }
}

TEST_CASE("solve_auction: dense and lazy agree for Mahalanobis", "[lazy][auction]") {
    auto left = random_points(5, 2, 23);
    auto right = random_points(5, 2, 24);
    std::vector<double> inv_cov = {2.0, 0.0, 0.0, 1.0};

    auto pc = build_parallel(left, right, DistanceMetric::Mahalanobis, inv_cov,
                             std::numeric_limits<double>::infinity(), {}, false);
    auto dense_res = lap::solve_auction(pc.dense, false);
    auto lazy_res = lap::solve_auction(pc.lazy);

    REQUIRE(dense_res.total_cost == Approx(lazy_res.total_cost).margin(AUCTION_TOL));
}

TEST_CASE("solve_auction: dense and lazy agree on rectangular problems (padding)", "[lazy][auction]") {
    auto left = random_points(4, 3, 25);
    auto right = random_points(9, 3, 26);
    std::vector<double> no_inv_cov;

    auto pc = build_parallel(left, right, DistanceMetric::Euclidean, no_inv_cov,
                             std::numeric_limits<double>::infinity(), {}, false);
    auto dense_res = lap::solve_auction(pc.dense, false);
    auto lazy_res = lap::solve_auction(pc.lazy);

    REQUIRE(dense_res.total_cost == Approx(lazy_res.total_cost).margin(AUCTION_TOL));
    REQUIRE(dense_res.assignment.size() == lazy_res.assignment.size());
}

TEST_CASE("solve_auction: dense and lazy agree with max_distance (padding + forbidding)", "[lazy][auction]") {
    auto left = identity_plus_offset(4, 0.0);
    auto right = identity_plus_offset(7, 1.0);  // rectangular: exercises padding
    std::vector<double> no_inv_cov;

    // max_distance = 5 leaves only the identity pairing allowed for the 4
    // real rows (cross pairs are >= 9 apart); the 3 extra columns go unmatched.
    auto pc = build_parallel(left, right, DistanceMetric::Euclidean, no_inv_cov,
                             5.0, {}, false);
    auto dense_res = lap::solve_auction(pc.dense, false);
    auto lazy_res = lap::solve_auction(pc.lazy);

    REQUIRE(dense_res.total_cost == Approx(lazy_res.total_cost).margin(AUCTION_TOL));
    REQUIRE(dense_res.total_cost == Approx(4.0).margin(AUCTION_TOL));  // 4 identity pairs at dist 1
}

TEST_CASE("solve_auction: dense and lazy agree with a per-variable caliper", "[lazy][auction]") {
    auto left = identity_plus_offset(6, 0.0);
    auto right = identity_plus_offset(6, 1.0);
    std::vector<double> no_inv_cov;
    std::vector<CaliperSpec> calipers = {{0, 3.0}};

    auto pc = build_parallel(left, right, DistanceMetric::Euclidean, no_inv_cov,
                             std::numeric_limits<double>::infinity(), calipers, false);
    auto dense_res = lap::solve_auction(pc.dense, false);
    auto lazy_res = lap::solve_auction(pc.lazy);

    REQUIRE(dense_res.total_cost == Approx(lazy_res.total_cost).margin(AUCTION_TOL));
}

TEST_CASE("solve_auction: dense and lazy agree for maximize = TRUE", "[lazy][auction]") {
    auto left = random_points(5, 2, 27);
    auto right = random_points(5, 2, 28);
    std::vector<double> no_inv_cov;

    auto pc = build_parallel(left, right, DistanceMetric::Euclidean, no_inv_cov,
                             std::numeric_limits<double>::infinity(), {}, true);
    auto dense_res = lap::solve_auction(pc.dense, true);
    auto lazy_res = lap::solve_auction(pc.lazy);

    REQUIRE(dense_res.total_cost == Approx(lazy_res.total_cost).margin(AUCTION_TOL));
}

TEST_CASE("solve_auction: dense and lazy agree for maximize = TRUE with padding", "[lazy][auction]") {
    // Rectangular AND maximize: exercises the sign-adjusted dummy_cost in
    // both the dense padding loop and PaddedCostView's lazy equivalent.
    auto left = random_points(3, 2, 29);
    auto right = random_points(6, 2, 30);
    std::vector<double> no_inv_cov;

    auto pc = build_parallel(left, right, DistanceMetric::Euclidean, no_inv_cov,
                             std::numeric_limits<double>::infinity(), {}, true);
    auto dense_res = lap::solve_auction(pc.dense, true);
    auto lazy_res = lap::solve_auction(pc.lazy);

    REQUIRE(dense_res.total_cost == Approx(lazy_res.total_cost).margin(AUCTION_TOL));
}

TEST_CASE("solve_auction: lazy matches a hand-checked tiny case", "[lazy][auction]") {
    std::vector<std::vector<double>> left = {{0, 0}, {10, 10}};
    std::vector<std::vector<double>> right = {{0, 1}, {10, 11}};
    std::vector<double> no_inv_cov;

    auto pc = build_parallel(left, right, DistanceMetric::Euclidean, no_inv_cov,
                             std::numeric_limits<double>::infinity(), {}, false);
    auto lazy_res = lap::solve_auction(pc.lazy);

    REQUIRE(lazy_res.total_cost == Approx(2.0).margin(AUCTION_TOL));
}
