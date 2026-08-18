// Test suite for the tree-backed re-seed: the same columns the row scan keeps.
//
// The descent is judged against the scan it replaces. A seed that kept cheaper
// columns would still look reasonable on its own, so every case runs both over
// the same source, the same excluded set and the same width, and compares the
// kept columns one at a time. Ties at the width boundary are settled by the
// column index, and a lattice of repeated costs is what proves that here.

#include <catch2/catch_test_macros.hpp>

#include "core/lap_lazy_types.h"
#include "flow/flow_balltree.h"
#include "flow/flow_candidates.h"
#include "flow/flow_feasibility.h"
#include "flow/flow_row_search.h"
#include "flow/flow_topk.h"
#include "flow/flow_tree_nearest.h"

#include <cmath>
#include <cstdint>
#include <limits>
#include <random>
#include <string>
#include <utility>
#include <vector>

namespace {

constexpr double kInf = std::numeric_limits<double>::infinity();

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
    int64_t nrow = 12;
    int64_t ncol = 200;
    int64_t n_vars = 3;
    lap::DistanceMetric metric = lap::DistanceMetric::Euclidean;
    double max_distance = kInf;
    std::vector<lap::CaliperSpec> calipers;
    bool negate = false;
    double skip_fraction = 0.2;
    int width = 5;
    int leaf_size = 8;
    std::uint32_t seed = 1;
    std::string label;
};

lap::LazyCostMatrix make_source(const Case& c, std::mt19937& rng) {
    std::vector<double> left = random_coords(c.nrow, c.n_vars, rng);
    std::vector<double> right = random_coords(c.ncol, c.n_vars, rng);
    std::vector<double> inv_cov;
    if (c.metric == lap::DistanceMetric::Mahalanobis) {
        inv_cov = spd_matrix(c.n_vars, rng);
    }
    return lap::LazyCostMatrix(std::move(left), std::move(right), c.n_vars,
                               c.metric, std::move(inv_cov), c.max_distance,
                               c.calipers, c.negate);
}

// One row's kept columns, ascending, as (column, cost).
using Kept = std::vector<std::pair<int32_t, double>>;

Kept emit_one(lap::detail::RowTopK& keep) {
    Kept out;
    keep.emit([&out](int32_t, int32_t j, double key) { out.emplace_back(j, key); });
    return out;
}

Kept scan_row(const lap::LazyCostMatrix& src, int64_t i,
              const std::vector<char>& skip, int width, lap::RowScanWork& work) {
    lap::detail::RowTopK keep(1, width);
    lap::row_search_detail::scan_cheapest_outside(src, i, skip, keep, 0, work);
    return emit_one(keep);
}

Kept tree_row(const lap::LazyCostMatrix& src, const lap::BallTree& tree, int64_t i,
              const std::vector<char>& skip, int width, lap::RowScanWork& work) {
    lap::detail::RowTopK keep(1, width);
    lap::tree_cheapest_outside(src, tree, i, skip, keep, 0, work);
    return emit_one(keep);
}

void run_case(const Case& c) {
    INFO("case: " << c.label);
    std::mt19937 rng(c.seed);
    const lap::LazyCostMatrix src = make_source(c, rng);
    const lap::BallTree tree = lap::build_ball_tree(src, c.leaf_size);
    REQUIRE_FALSE(tree.empty());

    std::uniform_real_distribution<double> unif(0.0, 1.0);
    std::vector<char> skip(static_cast<std::size_t>(c.ncol), 0);
    for (int64_t j = 0; j < c.ncol; ++j) {
        if (unif(rng) < c.skip_fraction) skip[static_cast<std::size_t>(j)] = 1;
    }

    lap::RowScanWork scan_work;
    lap::RowScanWork tree_work;
    for (int64_t i = 0; i < c.nrow; ++i) {
        INFO("row " << i);
        const Kept want = scan_row(src, i, skip, c.width, scan_work);
        const Kept got = tree_row(src, tree, i, skip, c.width, tree_work);
        REQUIRE(got.size() == want.size());
        for (std::size_t t = 0; t < want.size(); ++t) {
            REQUIRE(got[t].first == want[t].first);
            REQUIRE(got[t].second == want[t].second);
        }
    }
    // The descent is allowed to cost the same as the scan, never more.
    REQUIRE(tree_work.n_scanned <= scan_work.n_scanned);
}

}  // namespace

TEST_CASE("tree reseed keeps the columns the row scan keeps", "[flow][tree_nearest]") {
    for (std::uint32_t seed = 1; seed <= 3; ++seed) {
        Case euclid;
        euclid.seed = seed;
        euclid.label = "euclidean seed " + std::to_string(seed);
        run_case(euclid);

        Case maha;
        maha.metric = lap::DistanceMetric::Mahalanobis;
        maha.seed = seed;
        maha.label = "mahalanobis seed " + std::to_string(seed);
        run_case(maha);

        Case sq;
        sq.metric = lap::DistanceMetric::SquaredEuclidean;
        sq.seed = seed;
        sq.label = "squared euclidean seed " + std::to_string(seed);
        run_case(sq);
    }
}

TEST_CASE("tree reseed matches the scan under every admissibility rule",
          "[flow][tree_nearest]") {
    Case bounded;
    bounded.max_distance = 1.5;
    bounded.label = "max distance";
    run_case(bounded);

    Case capered;
    capered.calipers = {lap::CaliperSpec{0, 1.0}, lap::CaliperSpec{2, 0.8}};
    capered.label = "calipers";
    run_case(capered);

    Case both;
    both.max_distance = 2.0;
    both.calipers = {lap::CaliperSpec{1, 1.2}};
    both.metric = lap::DistanceMetric::Mahalanobis;
    both.label = "distance limit and caliper";
    run_case(both);

    Case maximize;
    maximize.negate = true;
    maximize.label = "maximize";
    run_case(maximize);

    Case narrow;
    narrow.max_distance = 0.35;
    narrow.skip_fraction = 0.5;
    narrow.label = "fewer admissible columns than the width";
    run_case(narrow);
}

TEST_CASE("tree reseed matches the scan at every width and leaf size",
          "[flow][tree_nearest]") {
    for (int width : {1, 2, 7, 64, 400}) {
        Case c;
        c.width = width;
        c.label = "width " + std::to_string(width);
        run_case(c);
    }
    for (int leaf : {1, 2, 512}) {
        Case c;
        c.leaf_size = leaf;
        c.label = "leaf size " + std::to_string(leaf);
        run_case(c);
    }

    Case skip_none;
    skip_none.skip_fraction = 0.0;
    skip_none.label = "nothing excluded";
    run_case(skip_none);

    Case one_var;
    one_var.n_vars = 1;
    one_var.label = "one covariate";
    run_case(one_var);
}

TEST_CASE("tree reseed settles a width-boundary tie by the column index",
          "[flow][tree_nearest]") {
    // Columns sit on a five-point lattice, so five columns share every cost a
    // row can see and the width falls inside a tie whatever the row is.
    const int64_t nrow = 6, ncol = 200, width = 7;
    std::vector<double> left(static_cast<std::size_t>(nrow), 0.0);
    std::vector<double> right(static_cast<std::size_t>(ncol));
    for (int64_t j = 0; j < ncol; ++j) {
        right[static_cast<std::size_t>(j)] = static_cast<double>(j % 5);
    }
    lap::LazyCostMatrix src(std::move(left), std::move(right), 1,
                            lap::DistanceMetric::Euclidean, {}, kInf, {}, false);
    const lap::BallTree tree = lap::build_ball_tree(src, 4);
    REQUIRE_FALSE(tree.empty());

    const std::vector<char> skip(static_cast<std::size_t>(ncol), 0);
    lap::RowScanWork work;
    for (int64_t i = 0; i < nrow; ++i) {
        const Kept want = scan_row(src, i, skip, width, work);
        const Kept got = tree_row(src, tree, i, skip, width, work);
        REQUIRE(got.size() == static_cast<std::size_t>(width));
        REQUIRE(got.size() == want.size());
        for (std::size_t t = 0; t < want.size(); ++t) {
            REQUIRE(got[t].first == want[t].first);
            REQUIRE(got[t].second == want[t].second);
        }
        // Cost zero is reached by forty columns and the width keeps seven, so
        // the kept set is the seven smallest indices among them.
        for (std::size_t t = 0; t < got.size(); ++t) {
            REQUIRE(got[t].second == 0.0);
            REQUIRE(got[t].first == static_cast<int32_t>(t) * 5);
        }
    }
}

TEST_CASE("an empty tree offers nothing and a zero width keeps nothing",
          "[flow][tree_nearest]") {
    Case c;
    c.label = "empty tree";
    std::mt19937 rng(c.seed);
    const lap::LazyCostMatrix src = make_source(c, rng);
    const lap::BallTree tree = lap::build_ball_tree(src, c.leaf_size);
    const std::vector<char> skip(static_cast<std::size_t>(c.ncol), 0);

    lap::RowScanWork work;
    lap::detail::RowTopK none(1, c.width);
    lap::tree_cheapest_outside(src, lap::BallTree(), 0, skip, none, 0, work);
    REQUIRE(emit_one(none).empty());
    REQUIRE(work.n_scanned == 0);
    REQUIRE(work.n_evaluated == 0);

    lap::detail::RowTopK zero(1, 0);
    lap::tree_cheapest_outside(src, tree, 0, skip, zero, 0, work);
    REQUIRE(emit_one(zero).empty());
    REQUIRE(work.n_scanned == 0);
}

TEST_CASE("a ball pays for a quadratic distance at any width and a linear one to a point",
          "[flow][tree_nearest]") {
    auto vars_source = [](lap::DistanceMetric metric, int64_t n_vars) {
        std::mt19937 rng(4);
        Case c;
        c.metric = metric;
        c.n_vars = n_vars;
        return make_source(c, rng);
    };

    REQUIRE(lap::ball_tree_pays(vars_source(lap::DistanceMetric::Euclidean, 3)));
    REQUIRE(lap::ball_tree_pays(vars_source(lap::DistanceMetric::Euclidean,
                                            lap::kBallTreeLinearVarLimit)));
    REQUIRE_FALSE(lap::ball_tree_pays(
        vars_source(lap::DistanceMetric::Euclidean, lap::kBallTreeLinearVarLimit + 1)));
    REQUIRE_FALSE(lap::ball_tree_pays(vars_source(lap::DistanceMetric::SquaredEuclidean,
                                                  lap::kBallTreeLinearVarLimit + 1)));
    REQUIRE(lap::ball_tree_pays(vars_source(lap::DistanceMetric::Mahalanobis, 12)));
    REQUIRE_FALSE(lap::ball_tree_pays(vars_source(lap::DistanceMetric::Manhattan, 2)));
    REQUIRE_FALSE(lap::ball_tree_pays(vars_source(lap::DistanceMetric::Chebyshev, 2)));
}

TEST_CASE("a feasibility round re-seeds the same pairs through the tree",
          "[flow][tree_nearest]") {
    Case c;
    c.nrow = 30;
    c.ncol = 300;
    c.metric = lap::DistanceMetric::Mahalanobis;
    c.label = "feasibility round";
    std::mt19937 rng(c.seed);
    const lap::LazyCostMatrix src = make_source(c, rng);

    lap::RowSearch<lap::LazyCostMatrix> with_tree(src);
    REQUIRE_FALSE(with_tree.tree.empty());
    lap::RowSearch<lap::LazyCostMatrix> without_tree(src);
    without_tree.tree = lap::BallTree();

    for (int width : {1, 4, 11}) {
        INFO("width " << width);
        // An empty candidate set leaves every row deficient, which is the seed
        // the loop takes over the whole source.
        lap::CandidateSet a(c.nrow, c.ncol);
        lap::CandidateSet b(c.nrow, c.ncol);

        const lap::FeasibilityRound scanned =
            lap::feasibility_round(src, a, width, without_tree);
        const lap::FeasibilityRound walked =
            lap::feasibility_round(src, b, width, with_tree);

        REQUIRE(walked.status == scanned.status);
        REQUIRE(walked.added.size() == scanned.added.size());
        for (std::size_t t = 0; t < scanned.added.size(); ++t) {
            REQUIRE(walked.added[t].first == scanned.added[t].first);
            REQUIRE(walked.added[t].second == scanned.added[t].second);
        }
        REQUIRE(walked.witness.rows == scanned.witness.rows);
        REQUIRE(walked.n_scanned <= scanned.n_scanned);
    }
}
