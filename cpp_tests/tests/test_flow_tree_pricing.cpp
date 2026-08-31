// Test suite for tree pricing: the same answer as price_block(), for less work.
//
// The tree is judged against the grid scan and not against itself. Every case
// runs both pricers over the same source, the same duals and the same candidate
// set, and asserts the violator set, the per-row minimum and the termination
// count agree. Comparing only the minimum would hide a pruning bug that drops
// one row's violators, so the kept pairs are compared one at a time.

#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_floating_point.hpp>

#include "core/lap_error.h"
#include "core/lap_lazy_types.h"
#include "flow/flow_balltree.h"
#include "flow/flow_candidates.h"
#include "flow/flow_pricing.h"
#include "flow/flow_tree_pricing.h"

#include <cmath>
#include <cstdint>
#include <limits>
#include <random>
#include <string>
#include <vector>

namespace {

constexpr double kInf = std::numeric_limits<double>::infinity();
constexpr double kTol = 1e-9;

std::vector<double> random_coords(int64_t n_units, int64_t n_vars,
                                  std::mt19937& rng) {
    std::uniform_real_distribution<double> unif(-3.0, 3.0);
    std::vector<double> out(static_cast<std::size_t>(n_units * n_vars));
    for (double& x : out) x = unif(rng);
    return out;
}

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

struct Case {
    int64_t nrow = 20;
    int64_t ncol = 250;
    int64_t n_vars = 3;
    lap::DistanceMetric metric = lap::DistanceMetric::Euclidean;
    double max_distance = kInf;
    std::vector<lap::CaliperSpec> calipers;
    bool negate = false;
    double dual_scale = 1.0;   // how far the duals push pairs below zero
    double cand_fraction = 0.1;
    int keep_per_row = 3;
    int leaf_size = 8;
    // Multiplies every coordinate, so a case can sit far from the exponent
    // range the bound was written at without changing the geometry.
    double coord_scale = 1.0;
    // Adds a translation after the scaling, which leaves every pairwise
    // difference alone while removing the leading digits that carry it.
    double coord_shift = 0.0;
    // The smallest eigenvalue the generated covariance is pushed down to. A
    // value near zero is a matrix whose Cholesky factor is barely defined.
    double spd_floor = -1.0;
    std::uint32_t seed = 1;
    std::string label;
};

lap::LazyCostMatrix make_source(const Case& c, std::mt19937& rng) {
    std::vector<double> left = random_coords(c.nrow, c.n_vars, rng);
    std::vector<double> right = random_coords(c.ncol, c.n_vars, rng);
    if (c.coord_scale != 1.0 || c.coord_shift != 0.0) {
        for (double& x : left) x = x * c.coord_scale + c.coord_shift;
        for (double& x : right) x = x * c.coord_scale + c.coord_shift;
    }
    std::vector<double> inv_cov;
    if (c.metric == lap::DistanceMetric::Mahalanobis) {
        inv_cov = spd_matrix(c.n_vars, rng);
        if (c.spd_floor >= 0.0) {
            // Shrink the diagonal boost the generator adds, which is what
            // keeps the matrix comfortably away from singular.
            for (int64_t i = 0; i < c.n_vars; ++i) {
                const std::size_t d = static_cast<std::size_t>(i * c.n_vars + i);
                inv_cov[d] -= static_cast<double>(c.n_vars);
                inv_cov[d] += c.spd_floor;
            }
        }
    }
    return lap::LazyCostMatrix(std::move(left), std::move(right), c.n_vars,
                               c.metric, std::move(inv_cov), c.max_distance,
                               c.calipers, c.negate);
}

// Both pricers see the same candidate set, so which pairs are in it only has to
// be arbitrary, not meaningful. The pairs are drawn once and each pricer gets
// its own set built from them, because a pricer records the pairs it evaluated
// on the set it was handed.
std::vector<lap::CandidateSet::Pair> candidate_pairs(const Case& c,
                                                     std::mt19937& rng) {
    std::uniform_real_distribution<double> unif(0.0, 1.0);
    std::vector<lap::CandidateSet::Pair> pairs;
    for (int64_t i = 0; i < c.nrow; ++i) {
        for (int64_t j = 0; j < c.ncol; ++j) {
            if (unif(rng) < c.cand_fraction) {
                pairs.emplace_back(static_cast<int32_t>(i), static_cast<int32_t>(j));
            }
        }
    }
    return pairs;
}

lap::CandidateSet make_candidates(const Case& c,
                                  const std::vector<lap::CandidateSet::Pair>& pairs) {
    lap::CandidateSet cand(c.nrow, c.ncol);
    cand.add_pairs(pairs);
    return cand;
}

void compare(const lap::BlockPricing& block, const lap::BlockPricing& tree,
             const std::string& label) {
    INFO(label);
    REQUIRE(tree.row_min.size() == block.row_min.size());
    for (std::size_t i = 0; i < block.row_min.size(); ++i) {
        INFO("row " << i);
        REQUIRE(tree.row_min[i] == block.row_min[i]);
    }
    REQUIRE(tree.min_reduced_cost == block.min_reduced_cost);
    REQUIRE(tree.arg_i == block.arg_i);
    REQUIRE(tree.arg_j == block.arg_j);
    REQUIRE(tree.n_violators == block.n_violators);
    REQUIRE(tree.violators.size() == block.violators.size());
    for (std::size_t t = 0; t < block.violators.size(); ++t) {
        INFO("violator " << t);
        REQUIRE(tree.violators[t].i == block.violators[t].i);
        REQUIRE(tree.violators[t].j == block.violators[t].j);
        REQUIRE(tree.violators[t].cbar == block.violators[t].cbar);
    }
}

// Duals with the shape a restricted master produces: a row dual near the row's
// own cheapest cost, so that a fraction of the omitted pairs price negative and
// the rest do not.
void make_duals(const lap::LazyCostMatrix& src, const Case& c, std::mt19937& rng,
                std::vector<double>& u, std::vector<double>& v) {
    std::uniform_real_distribution<double> unif(0.0, 1.0);
    v.assign(static_cast<std::size_t>(src.ncol), 0.0);
    for (double& x : v) x = 0.2 * unif(rng);

    u.assign(static_cast<std::size_t>(src.nrow), 0.0);
    for (int64_t i = 0; i < src.nrow; ++i) {
        double best = kInf;
        for (int64_t j = 0; j < src.ncol; ++j) {
            double cost = 0.0;
            if (!src.admissible(i, j, cost)) continue;
            if (cost < best) best = cost;
        }
        if (!std::isfinite(best)) best = 0.0;
        u[static_cast<std::size_t>(i)] = best + c.dual_scale * unif(rng);
    }
}

void run_case(const Case& c) {
    std::mt19937 rng(c.seed);
    const lap::LazyCostMatrix src = make_source(c, rng);
    lap::BallTree tree = lap::build_ball_tree(src, c.leaf_size);
    REQUIRE_FALSE(tree.empty());

    std::vector<double> u, v;
    make_duals(src, c, rng, u, v);

    const std::vector<lap::CandidateSet::Pair> pairs = candidate_pairs(c, rng);
    lap::CandidateSet cand_block = make_candidates(c, pairs);
    lap::CandidateSet cand_tree = make_candidates(c, pairs);

    const lap::BlockPricing block =
        lap::price_block(src, u, v, cand_block, c.keep_per_row, kTol);
    const lap::BlockPricing from_tree =
        lap::price_tree(src, tree, u, v, cand_tree, c.keep_per_row, kTol);

    compare(block, from_tree, c.label);

    // The pruning is the point, so a case that reads every pair anyway is a
    // case that proves nothing about it.
    INFO(c.label << ": tree evaluated " << from_tree.n_evaluated << " of "
                 << block.n_evaluated);
    REQUIRE(from_tree.n_scanned <= block.n_scanned);
}

}  // namespace

TEST_CASE("Tree pricing - the same answer as the grid scan") {
    std::vector<Case> cases;

    for (std::uint32_t seed = 1; seed <= 3; ++seed) {
        Case euclid;
        euclid.seed = seed;
        euclid.label = "euclidean seed " + std::to_string(seed);
        cases.push_back(euclid);

        Case maha;
        maha.metric = lap::DistanceMetric::Mahalanobis;
        maha.n_vars = 4;
        maha.seed = seed + 10;
        maha.label = "mahalanobis seed " + std::to_string(seed);
        cases.push_back(maha);

        Case squared;
        squared.metric = lap::DistanceMetric::SquaredEuclidean;
        squared.seed = seed + 20;
        squared.label = "squared euclidean seed " + std::to_string(seed);
        cases.push_back(squared);
    }

    Case capped;
    capped.max_distance = 1.5;
    capped.seed = 31;
    capped.label = "max_distance";
    cases.push_back(capped);

    Case calipered;
    calipered.calipers = {lap::CaliperSpec{0, 1.0}, lap::CaliperSpec{2, 1.2}};
    calipered.seed = 32;
    calipered.label = "calipers";
    cases.push_back(calipered);

    Case both;
    both.metric = lap::DistanceMetric::Mahalanobis;
    both.n_vars = 4;
    both.max_distance = 2.0;
    both.calipers = {lap::CaliperSpec{1, 0.9}};
    both.seed = 33;
    both.label = "mahalanobis with both limits";
    cases.push_back(both);

    Case maximize;
    maximize.negate = true;
    maximize.max_distance = 2.5;
    maximize.seed = 34;
    maximize.label = "maximize";
    cases.push_back(maximize);

    Case wide;
    wide.dual_scale = 6.0;   // most omitted pairs price negative
    wide.seed = 35;
    wide.label = "duals that leave almost every pair a violator";
    cases.push_back(wide);

    Case tight;
    tight.dual_scale = 0.0;  // only the row's own cheapest pair can tie zero
    tight.seed = 36;
    tight.label = "duals that leave almost none";
    cases.push_back(tight);

    Case empty_cand;
    empty_cand.cand_fraction = 0.0;
    empty_cand.seed = 37;
    empty_cand.label = "no candidates yet";
    cases.push_back(empty_cand);

    Case full_cand;
    full_cand.cand_fraction = 1.0;
    full_cand.seed = 38;
    full_cand.label = "every pair already a candidate";
    cases.push_back(full_cand);

    Case keep_none;
    keep_none.keep_per_row = 0;
    keep_none.seed = 39;
    keep_none.label = "counting violators and keeping none";
    cases.push_back(keep_none);

    Case keep_many;
    keep_many.keep_per_row = 40;
    keep_many.dual_scale = 4.0;
    keep_many.seed = 40;
    keep_many.label = "keeping more than a row offers";
    cases.push_back(keep_many);

    Case leafy;
    leafy.leaf_size = 1;
    leafy.seed = 41;
    leafy.label = "one column per leaf";
    cases.push_back(leafy);

    Case bushy;
    bushy.leaf_size = 512;   // one leaf holding everything
    bushy.seed = 42;
    bushy.label = "one leaf holding every column";
    cases.push_back(bushy);

    Case one_var;
    one_var.n_vars = 1;
    one_var.seed = 43;
    one_var.label = "one covariate";
    cases.push_back(one_var);

    Case tall;
    tall.nrow = 120;
    tall.ncol = 130;
    tall.seed = 44;
    tall.label = "nearly square";
    cases.push_back(tall);

    for (const Case& c : cases) run_case(c);
}

TEST_CASE("Tree pricing - the grid scan is matched at extreme scales") {
    // The bound is arithmetic over coordinates, so the cases that can break it
    // are the ones where the arithmetic is worst: coordinates whose exponent
    // sits far from one, differences that survive only in the trailing bits of
    // a large translation, and a covariance whose Cholesky factor is barely
    // defined. A prune that fires where the grid scan finds a violator shows up
    // here as a disagreement, because both pricers see the same source.
    std::vector<Case> cases;

    for (int e = -150; e <= 150; e += 50) {
        if (e == 0) continue;
        Case scaled;
        scaled.coord_scale = std::pow(10.0, static_cast<double>(e));
        scaled.seed = static_cast<std::uint32_t>(200 + e);
        scaled.label = "euclidean coordinates at 1e" + std::to_string(e);
        cases.push_back(scaled);
    }

    for (int e = -60; e <= 60; e += 30) {
        if (e == 0) continue;
        Case scaled;
        scaled.metric = lap::DistanceMetric::Mahalanobis;
        scaled.n_vars = 4;
        scaled.coord_scale = std::pow(10.0, static_cast<double>(e));
        scaled.seed = static_cast<std::uint32_t>(400 + e);
        scaled.label = "mahalanobis coordinates at 1e" + std::to_string(e);
        cases.push_back(scaled);
    }

    // A large translation leaves every pairwise difference unchanged while
    // spending the leading digits of each coordinate on the offset.
    for (double shift : {1e6, 1e9, 1e12}) {
        Case shifted;
        shifted.coord_shift = shift;
        shifted.seed = 500;
        shifted.label = "coordinates translated by " + std::to_string(shift);
        cases.push_back(shifted);

        Case shifted_maha;
        shifted_maha.metric = lap::DistanceMetric::Mahalanobis;
        shifted_maha.n_vars = 4;
        shifted_maha.coord_shift = shift;
        shifted_maha.seed = 501;
        shifted_maha.label = "mahalanobis translated by " + std::to_string(shift);
        cases.push_back(shifted_maha);
    }

    // A covariance approaching singular, where the factor the tree whitens by
    // and the matrix the source reads diverge fastest.
    for (double floor_v : {1e-2, 1e-5, 1e-8}) {
        Case ill;
        ill.metric = lap::DistanceMetric::Mahalanobis;
        ill.n_vars = 4;
        ill.spd_floor = floor_v;
        ill.seed = 600;
        ill.label = "mahalanobis with a diagonal floor of " + std::to_string(floor_v);
        cases.push_back(ill);
    }

    for (const Case& c : cases) {
        std::mt19937 rng(c.seed);
        const lap::LazyCostMatrix src = make_source(c, rng);
        lap::BallTree tree = lap::build_ball_tree(src, c.leaf_size);
        // An ill-conditioned covariance may leave no Cholesky factor at all,
        // which is the scan rather than a failure.
        if (tree.empty()) continue;

        std::vector<double> u, v;
        make_duals(src, c, rng, u, v);

        const std::vector<lap::CandidateSet::Pair> pairs = candidate_pairs(c, rng);
        lap::CandidateSet cand_block = make_candidates(c, pairs);
        lap::CandidateSet cand_tree = make_candidates(c, pairs);

        const lap::BlockPricing block =
            lap::price_block(src, u, v, cand_block, c.keep_per_row, kTol);
        const lap::BlockPricing from_tree =
            lap::price_tree(src, tree, u, v, cand_tree, c.keep_per_row, kTol);

        compare(block, from_tree, c.label);
    }
}

TEST_CASE("Tree pricing - a tie at the keep boundary is settled the same way") {
    // Costs off a lattice, so a row offers many pairs at exactly the same
    // reduced cost and the kept set is decided by the order they are offered
    // in rather than by their keys.
    const int64_t n_vars = 1;
    const int64_t nrow = 6;
    const int64_t ncol = 120;

    std::vector<double> left(static_cast<std::size_t>(nrow), 0.0);
    std::vector<double> right(static_cast<std::size_t>(ncol));
    for (int64_t j = 0; j < ncol; ++j) {
        right[static_cast<std::size_t>(j)] = static_cast<double>(j % 5);
    }
    lap::LazyCostMatrix src(std::move(left), std::move(right), n_vars,
                            lap::DistanceMetric::Euclidean, {}, kInf, {}, false);
    lap::BallTree tree = lap::build_ball_tree(src, 4);
    REQUIRE_FALSE(tree.empty());

    const std::vector<double> u(static_cast<std::size_t>(nrow), 3.0);
    const std::vector<double> v(static_cast<std::size_t>(ncol), 0.0);

    lap::CandidateSet cand_block(nrow, ncol);
    lap::CandidateSet cand_tree(nrow, ncol);

    const lap::BlockPricing block = lap::price_block(src, u, v, cand_block, 7, kTol);
    const lap::BlockPricing from_tree = lap::price_tree(src, tree, u, v, cand_tree, 7, kTol);

    // Distances are 0, 1, 2, 3, 4 in equal shares and every row dual is 3, so a
    // row offers 24 pairs at each of -3, -2 and -1. Keeping 7 cuts into the
    // first group, which is the boundary a tie has to be settled at.
    REQUIRE(block.n_violators == nrow * (ncol / 5) * 3);
    REQUIRE(block.violators.size() == static_cast<std::size_t>(nrow) * 7u);
    for (const lap::PricedPair& p : block.violators) {
        REQUIRE(p.cbar == -3.0);
    }
    compare(block, from_tree, "lattice ties");
}

TEST_CASE("Tree pricing - price_pairs routes a metric with no tree to the grid") {
    std::mt19937 rng(5);
    Case c;
    c.metric = lap::DistanceMetric::Manhattan;
    const lap::LazyCostMatrix src = make_source(c, rng);
    lap::BallTree tree = lap::build_ball_tree(src, c.leaf_size);
    REQUIRE(tree.empty());

    std::vector<double> u, v;
    make_duals(src, c, rng, u, v);
    const std::vector<lap::CandidateSet::Pair> pairs = candidate_pairs(c, rng);
    lap::CandidateSet cand_a = make_candidates(c, pairs);
    lap::CandidateSet cand_b = make_candidates(c, pairs);

    const lap::BlockPricing direct =
        lap::price_block(src, u, v, cand_a, c.keep_per_row, kTol);
    const lap::BlockPricing routed =
        lap::price_pairs(src, tree, u, v, cand_b, c.keep_per_row, kTol);

    compare(direct, routed, "manhattan routed to price_block");
    REQUIRE(routed.n_scanned == direct.n_scanned);
}

TEST_CASE("Tree pricing - a mismatched shape throws rather than pricing nothing") {
    std::mt19937 rng(9);
    Case c;
    const lap::LazyCostMatrix src = make_source(c, rng);
    lap::BallTree tree = lap::build_ball_tree(src, 8);
    REQUIRE_FALSE(tree.empty());

    std::vector<double> u, v;
    make_duals(src, c, rng, u, v);
    lap::CandidateSet cand(c.nrow, c.ncol);

    std::vector<double> short_u(u.begin(), u.end() - 1);
    REQUIRE_THROWS_AS(lap::price_tree(src, tree, short_u, v, cand, 3, kTol),
                      lap::DimensionException);

    std::vector<double> short_v(v.begin(), v.end() - 1);
    REQUIRE_THROWS_AS(lap::price_tree(src, tree, u, short_v, cand, 3, kTol),
                      lap::DimensionException);

    lap::CandidateSet wrong_shape(c.nrow, c.ncol + 1);
    REQUIRE_THROWS_AS(lap::price_tree(src, tree, u, v, wrong_shape, 3, kTol),
                      lap::DimensionException);

    lap::BallTree no_tree;
    REQUIRE_THROWS_AS(lap::price_tree(src, no_tree, u, v, cand, 3, kTol),
                      lap::DimensionException);
}
