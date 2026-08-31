// Test suite for the ball tree over the control units: the leaf order, the two
// bounding structures, and the per-node column dual.
//
// Every bound is asserted against the members it claims to bound, read one at
// a time out of the source itself. A bound that is merely plausible is a
// pricer that silently drops violators, so the assertions are one-sided in the
// direction the pruning reads: the lower bound never exceeds any member's
// cost, and an "out" verdict is never returned while some member is in.

#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_floating_point.hpp>

#include "core/lap_error.h"
#include "core/lap_lazy_types.h"
#include "flow/flow_balltree.h"

#include <cmath>
#include <cstdint>
#include <limits>
#include <random>
#include <vector>

namespace {

constexpr double kInf = std::numeric_limits<double>::infinity();
constexpr double kTol = 1e-9;

// The bounds are compared against distances that can be large under a poorly
// conditioned covariance, so the slack is relative to the value being bounded.
bool at_most(double bound, double value) {
    return bound <= value + kTol * (1.0 + std::abs(value));
}

bool at_least(double bound, double value) {
    return bound >= value - kTol * (1.0 + std::abs(value));
}

std::vector<double> random_coords(int64_t n_units, int64_t n_vars,
                                  std::mt19937& rng) {
    std::uniform_real_distribution<double> unif(-3.0, 3.0);
    std::vector<double> out(static_cast<std::size_t>(n_units * n_vars));
    for (double& x : out) x = unif(rng);
    return out;
}

// B'B + n I, symmetric positive definite by construction, row-major.
std::vector<double> spd_matrix(int64_t n, std::mt19937& rng) {
    std::uniform_real_distribution<double> unif(-1.0, 1.0);
    std::vector<double> b(static_cast<std::size_t>(n * n));
    for (double& x : b) x = unif(rng);

    std::vector<double> a(static_cast<std::size_t>(n * n), 0.0);
    for (int64_t i = 0; i < n; ++i) {
        for (int64_t j = 0; j < n; ++j) {
            double s = 0.0;
            for (int64_t k = 0; k < n; ++k) {
                s += b[static_cast<std::size_t>(k * n + i)] *
                     b[static_cast<std::size_t>(k * n + j)];
            }
            a[static_cast<std::size_t>(i * n + j)] = s;
        }
        a[static_cast<std::size_t>(i * n + i)] += static_cast<double>(n);
    }
    return a;
}

struct SourceSpec {
    int64_t nrow = 12;
    int64_t ncol = 200;
    int64_t n_vars = 3;
    lap::DistanceMetric metric = lap::DistanceMetric::Euclidean;
    double max_distance = kInf;
    std::vector<lap::CaliperSpec> calipers;
    bool negate = false;
    std::uint32_t seed = 1;
};

lap::LazyCostMatrix make_source(const SourceSpec& spec) {
    std::mt19937 rng(spec.seed);
    std::vector<double> left = random_coords(spec.nrow, spec.n_vars, rng);
    std::vector<double> right = random_coords(spec.ncol, spec.n_vars, rng);
    std::vector<double> inv_cov;
    if (spec.metric == lap::DistanceMetric::Mahalanobis) {
        inv_cov = spd_matrix(spec.n_vars, rng);
    }
    return lap::LazyCostMatrix(std::move(left), std::move(right), spec.n_vars,
                               spec.metric, std::move(inv_cov),
                               spec.max_distance, spec.calipers, spec.negate);
}

// The whitened separation the tree's centres and radii are stated in.
double whitened_distance(const lap::BallTree& tree, const double* q_whitened,
                         int32_t unit) {
    const double* w = tree.unit_whitened(unit);
    double s = 0.0;
    for (int64_t k = 0; k < tree.n_vars; ++k) {
        const double d = q_whitened[k] - w[k];
        s += d * d;
    }
    return std::sqrt(s);
}

}  // namespace

TEST_CASE("Ball tree - the leaf order is a permutation and the spans partition") {
    const lap::LazyCostMatrix src = make_source(SourceSpec{});
    const lap::BallTree tree = lap::build_ball_tree(src, 8);

    REQUIRE_FALSE(tree.empty());
    REQUIRE(tree.n_units == static_cast<int32_t>(src.ncol));
    REQUIRE(static_cast<int64_t>(tree.perm.size()) == src.ncol);

    std::vector<int> seen(static_cast<std::size_t>(src.ncol), 0);
    for (int32_t unit : tree.perm) {
        REQUIRE(unit >= 0);
        REQUIRE(unit < tree.n_units);
        ++seen[static_cast<std::size_t>(unit)];
    }
    for (int count : seen) REQUIRE(count == 1);

    REQUIRE(tree.lo[0] == 0);
    REQUIRE(tree.hi[0] == tree.n_units);

    int32_t leaf_total = 0;
    for (int32_t id = 0; id < tree.n_nodes(); ++id) {
        const int32_t a = tree.lo[static_cast<std::size_t>(id)];
        const int32_t b = tree.hi[static_cast<std::size_t>(id)];
        REQUIRE(a < b);
        if (tree.is_leaf(id)) {
            leaf_total += b - a;
            continue;
        }
        // A parent is recorded before either child, which is what makes the
        // reverse sweep in build and in refresh_max_v a valid bottom-up pass.
        const int32_t l = tree.left[static_cast<std::size_t>(id)];
        const int32_t r = tree.right[static_cast<std::size_t>(id)];
        REQUIRE(l > id);
        REQUIRE(r > id);
        REQUIRE(tree.lo[static_cast<std::size_t>(l)] == a);
        REQUIRE(tree.hi[static_cast<std::size_t>(l)] ==
                tree.lo[static_cast<std::size_t>(r)]);
        REQUIRE(tree.hi[static_cast<std::size_t>(r)] == b);
    }
    REQUIRE(leaf_total == tree.n_units);
}

TEST_CASE("Ball tree - whitening reproduces the metric the source measures") {
    for (lap::DistanceMetric metric : {lap::DistanceMetric::Euclidean,
                                       lap::DistanceMetric::SquaredEuclidean,
                                       lap::DistanceMetric::Mahalanobis}) {
        SourceSpec spec;
        spec.metric = metric;
        spec.ncol = 60;
        spec.seed = 7;
        const lap::LazyCostMatrix src = make_source(spec);
        const lap::BallTree tree = lap::build_ball_tree(src, 4);
        REQUIRE_FALSE(tree.empty());

        std::vector<double> q(static_cast<std::size_t>(spec.n_vars));
        for (int64_t i = 0; i < src.nrow; ++i) {
            lap::whiten_point(tree, src.left_row(i), q.data());
            for (int64_t j = 0; j < src.ncol; ++j) {
                const double d = whitened_distance(tree, q.data(),
                                                   static_cast<int32_t>(j));
                const double cost = lap::metric_cost_of(metric, d);
                REQUIRE_THAT(cost, Catch::Matchers::WithinAbs(src.at(i, j), 1e-9));
            }
        }
    }
}

TEST_CASE("Ball tree - the ball encloses every member of its node") {
    SourceSpec spec;
    spec.metric = lap::DistanceMetric::Mahalanobis;
    spec.ncol = 150;
    spec.n_vars = 4;
    spec.seed = 11;
    const lap::LazyCostMatrix src = make_source(spec);
    const lap::BallTree tree = lap::build_ball_tree(src, 6);
    REQUIRE_FALSE(tree.empty());

    std::vector<double> q(static_cast<std::size_t>(spec.n_vars));
    for (int64_t i = 0; i < src.nrow; ++i) {
        const double q_g = lap::whiten_point(tree, src.left_row(i), q.data());
        for (int32_t id = 0; id < tree.n_nodes(); ++id) {
            const lap::BallBounds b =
                lap::node_ball_bounds(tree, q.data(), q_g, id);
            REQUIRE(b.d_lo >= 0.0);
            REQUIRE(b.d_lo <= b.d_hi + kTol);
            for (int32_t t = tree.lo[static_cast<std::size_t>(id)];
                 t < tree.hi[static_cast<std::size_t>(id)]; ++t) {
                const double d = whitened_distance(
                    tree, q.data(), tree.perm[static_cast<std::size_t>(t)]);
                REQUIRE(at_most(b.d_lo, d));
                REQUIRE(at_least(b.d_hi, d));
            }
        }
    }
}

TEST_CASE("Ball tree - node_cost_lo is under every admissible member's cost") {
    for (lap::DistanceMetric metric : {lap::DistanceMetric::Euclidean,
                                       lap::DistanceMetric::SquaredEuclidean,
                                       lap::DistanceMetric::Mahalanobis}) {
        for (bool negate : {false, true}) {
            SourceSpec spec;
            spec.metric = metric;
            spec.negate = negate;
            spec.ncol = 120;
            spec.max_distance = 2.5;
            spec.calipers = {lap::CaliperSpec{0, 2.0}};
            spec.seed = 23;
            const lap::LazyCostMatrix src = make_source(spec);
            const lap::BallTree tree = lap::build_ball_tree(src, 5);
            REQUIRE_FALSE(tree.empty());

            std::vector<double> q(static_cast<std::size_t>(spec.n_vars));
            for (int64_t i = 0; i < src.nrow; ++i) {
                const double q_g =
                    lap::whiten_point(tree, src.left_row(i), q.data());
                for (int32_t id = 0; id < tree.n_nodes(); ++id) {
                    const double lo =
                        lap::node_cost_lo(tree, src, q.data(), q_g, id);
                    for (int32_t t = tree.lo[static_cast<std::size_t>(id)];
                         t < tree.hi[static_cast<std::size_t>(id)]; ++t) {
                        const int64_t j = static_cast<int64_t>(
                            tree.perm[static_cast<std::size_t>(t)]);
                        double cost = 0.0;
                        if (!src.admissible(i, j, cost)) continue;
                        REQUIRE(at_most(lo, cost));
                    }
                }
            }
        }
    }
}

TEST_CASE("Ball tree - an out verdict never leaves a member in") {
    SourceSpec spec;
    spec.metric = lap::DistanceMetric::Euclidean;
    spec.ncol = 180;
    spec.n_vars = 3;
    // Tight enough that both verdicts fire on some node and loose enough that
    // neither fires on all of them.
    spec.max_distance = 2.0;
    spec.calipers = {lap::CaliperSpec{0, 1.0}, lap::CaliperSpec{2, 1.5}};
    spec.seed = 31;
    const lap::LazyCostMatrix src = make_source(spec);
    const lap::BallTree tree = lap::build_ball_tree(src, 7);
    REQUIRE_FALSE(tree.empty());

    int64_t distance_fired = 0;
    int64_t caliper_fired = 0;

    std::vector<double> q(static_cast<std::size_t>(spec.n_vars));
    for (int64_t i = 0; i < src.nrow; ++i) {
        const double q_g = lap::whiten_point(tree, src.left_row(i), q.data());
        const double* x = src.left_row(i);
        for (int32_t id = 0; id < tree.n_nodes(); ++id) {
            const bool d_out =
                lap::node_distance_out(tree, src, q.data(), q_g, id);
            const bool c_out = lap::node_caliper_out(tree, src, x, id);
            if (d_out) ++distance_fired;
            if (c_out) ++caliper_fired;
            if (!d_out && !c_out) continue;

            for (int32_t t = tree.lo[static_cast<std::size_t>(id)];
                 t < tree.hi[static_cast<std::size_t>(id)]; ++t) {
                const int64_t j = static_cast<int64_t>(
                    tree.perm[static_cast<std::size_t>(t)]);
                REQUIRE_FALSE(src.allowed(i, j));
            }
        }
    }

    REQUIRE(distance_fired > 0);
    REQUIRE(caliper_fired > 0);
}

TEST_CASE("Ball tree - the caliper box is read in the original covariates") {
    // Whitening rotates, so a box built in whitened coordinates would bound the
    // wrong differences. Under a covariance that mixes the variables the two
    // boxes disagree, and the verdict has to follow the original one.
    SourceSpec spec;
    spec.metric = lap::DistanceMetric::Mahalanobis;
    spec.ncol = 200;
    spec.n_vars = 3;
    spec.calipers = {lap::CaliperSpec{1, 0.4}};
    spec.seed = 41;
    const lap::LazyCostMatrix src = make_source(spec);
    const lap::BallTree tree = lap::build_ball_tree(src, 6);
    REQUIRE_FALSE(tree.empty());

    int64_t fired = 0;
    std::vector<double> q(static_cast<std::size_t>(spec.n_vars));
    for (int64_t i = 0; i < src.nrow; ++i) {
        lap::whiten_point(tree, src.left_row(i), q.data());
        for (int32_t id = 0; id < tree.n_nodes(); ++id) {
            if (!lap::node_caliper_out(tree, src, src.left_row(i), id)) continue;
            ++fired;
            for (int32_t t = tree.lo[static_cast<std::size_t>(id)];
                 t < tree.hi[static_cast<std::size_t>(id)]; ++t) {
                const int64_t j = static_cast<int64_t>(
                    tree.perm[static_cast<std::size_t>(t)]);
                REQUIRE_FALSE(src.allowed(i, j));
            }
        }
    }
    REQUIRE(fired > 0);
}

TEST_CASE("Ball tree - refresh_max_v is the largest dual in the subtree") {
    const lap::LazyCostMatrix src = make_source(SourceSpec{});
    lap::BallTree tree = lap::build_ball_tree(src, 8);
    REQUIRE_FALSE(tree.empty());

    std::mt19937 rng(97);
    std::uniform_real_distribution<double> unif(-5.0, 5.0);
    std::vector<double> v(static_cast<std::size_t>(src.ncol));
    for (double& x : v) x = unif(rng);

    lap::refresh_max_v(tree, v);

    for (int32_t id = 0; id < tree.n_nodes(); ++id) {
        double expected = -kInf;
        for (int32_t t = tree.lo[static_cast<std::size_t>(id)];
             t < tree.hi[static_cast<std::size_t>(id)]; ++t) {
            const double x = v[static_cast<std::size_t>(
                tree.perm[static_cast<std::size_t>(t)])];
            if (x > expected) expected = x;
        }
        REQUIRE(tree.max_v[static_cast<std::size_t>(id)] == expected);
    }

    // A second round writes over the first rather than accumulating with it.
    for (double& x : v) x = unif(rng);
    lap::refresh_max_v(tree, v);
    double root_expected = -kInf;
    for (double x : v) root_expected = x > root_expected ? x : root_expected;
    REQUIRE(tree.max_v[0] == root_expected);

    std::vector<double> wrong(static_cast<std::size_t>(src.ncol) - 1, 0.0);
    REQUIRE_THROWS_AS(lap::refresh_max_v(tree, wrong), lap::DimensionException);
}

TEST_CASE("Ball tree - a metric no ball bounds gets no tree") {
    for (lap::DistanceMetric metric : {lap::DistanceMetric::Manhattan,
                                       lap::DistanceMetric::Chebyshev}) {
        REQUIRE_FALSE(lap::metric_has_ball_bound(metric));
        SourceSpec spec;
        spec.metric = metric;
        const lap::LazyCostMatrix src = make_source(spec);
        const lap::BallTree tree = lap::build_ball_tree(src, 8);
        REQUIRE(tree.empty());
        REQUIRE(tree.n_nodes() == 0);
    }
}

TEST_CASE("Ball tree - a covariance with no whitening gets no tree") {
    std::vector<double> singular(9, 0.0);
    std::vector<double> factor;
    REQUIRE_FALSE(lap::cholesky_lower(singular, 3, factor));

    // Rank one: the first pivot is fine and the second is zero.
    const std::vector<double> x = {1.0, 2.0};
    std::vector<double> rank_one(4, 0.0);
    for (int64_t i = 0; i < 2; ++i) {
        for (int64_t j = 0; j < 2; ++j) {
            rank_one[static_cast<std::size_t>(i * 2 + j)] = x[static_cast<std::size_t>(i)] *
                                                            x[static_cast<std::size_t>(j)];
        }
    }
    REQUIRE_FALSE(lap::cholesky_lower(rank_one, 2, factor));

    std::mt19937 rng(3);
    lap::LazyCostMatrix src(random_coords(5, 2, rng), random_coords(40, 2, rng), 2,
                            lap::DistanceMetric::Mahalanobis, rank_one, kInf, {},
                            false);
    REQUIRE(lap::build_ball_tree(src, 4).empty());

    // An inv_cov of the wrong size is the same routing answer.
    lap::LazyCostMatrix short_cov(random_coords(5, 2, rng), random_coords(40, 2, rng),
                                  2, lap::DistanceMetric::Mahalanobis,
                                  std::vector<double>{1.0, 0.0}, kInf, {}, false);
    REQUIRE(lap::build_ball_tree(short_cov, 4).empty());
}

TEST_CASE("Ball tree - the shapes a split cannot cut") {
    std::mt19937 rng(5);

    SECTION("one column") {
        lap::LazyCostMatrix src(random_coords(3, 2, rng), random_coords(1, 2, rng), 2,
                                lap::DistanceMetric::Euclidean, {}, kInf, {}, false);
        const lap::BallTree tree = lap::build_ball_tree(src, 8);
        REQUIRE(tree.n_nodes() == 1);
        REQUIRE(tree.radius[0] == 0.0);
        REQUIRE(tree.is_leaf(0));
    }

    SECTION("no columns") {
        lap::LazyCostMatrix src(random_coords(3, 2, rng), {}, 2,
                                lap::DistanceMetric::Euclidean, {}, kInf, {}, false);
        REQUIRE(lap::build_ball_tree(src, 8).empty());
    }

    SECTION("every column at the same point") {
        const int64_t n_units = 50;
        std::vector<double> right(static_cast<std::size_t>(n_units * 2));
        for (int64_t j = 0; j < n_units; ++j) {
            right[static_cast<std::size_t>(j * 2)] = 1.5;
            right[static_cast<std::size_t>(j * 2 + 1)] = -0.5;
        }
        lap::LazyCostMatrix src(random_coords(3, 2, rng), std::move(right), 2,
                                lap::DistanceMetric::Euclidean, {}, kInf, {}, false);
        const lap::BallTree tree = lap::build_ball_tree(src, 8);
        // No dimension to cut on, so the whole set stays one leaf, and its
        // radius is zero rather than the spread of a split that never happened.
        REQUIRE(tree.n_nodes() == 1);
        REQUIRE(tree.radius[0] == 0.0);
        REQUIRE(tree.hi[0] == static_cast<int32_t>(n_units));

        std::vector<double> q(2);
        const double q_g = lap::whiten_point(tree, src.left_row(0), q.data());
        const lap::BallBounds b = lap::node_ball_bounds(tree, q.data(), q_g, 0);
        REQUIRE_THAT(b.d_lo, Catch::Matchers::WithinAbs(b.d_hi, 1e-12));
        REQUIRE_THAT(b.d_lo, Catch::Matchers::WithinAbs(src.at(0, 0), 1e-12));
    }

    SECTION("a leaf size of one still terminates") {
        lap::LazyCostMatrix src(random_coords(3, 2, rng), random_coords(33, 2, rng), 2,
                                lap::DistanceMetric::Euclidean, {}, kInf, {}, false);
        const lap::BallTree tree = lap::build_ball_tree(src, 1);
        REQUIRE_FALSE(tree.empty());
        int32_t leaves = 0;
        for (int32_t id = 0; id < tree.n_nodes(); ++id) {
            if (tree.is_leaf(id)) ++leaves;
        }
        REQUIRE(leaves == 33);
        for (int32_t id = 0; id < tree.n_nodes(); ++id) {
            if (tree.is_leaf(id)) REQUIRE(tree.radius[static_cast<std::size_t>(id)] == 0.0);
        }
    }
}
