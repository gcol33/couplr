// Test suite for block pricing: which omitted pairs a restricted master wants.

#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_floating_point.hpp>

#include "core/lap_certify.h"
#include "core/lap_error.h"
#include "core/lap_types.h"
#include "flow/flow_candidates.h"
#include "flow/flow_pricing.h"

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <limits>
#include <random>
#include <set>
#include <utility>
#include <vector>

namespace {

using Pair = lap::CandidateSet::Pair;

constexpr double kInf = std::numeric_limits<double>::infinity();
constexpr double kTol = 1e-9;

// An independent model of the same question, written the obvious way: no
// blocking, no heap, no cursor, a std::set per row for membership.
struct Reference {
    std::vector<double> row_min;
    std::vector<std::vector<std::pair<double, int32_t>>> row_violators;  // ascending j
    double  min_cbar = kInf;
    int64_t n_violators = 0;
    int64_t n_scanned = 0;
    int64_t n_evaluated = 0;
};

Reference price_reference(const lap::CostMatrix& src,
                          const std::vector<double>& u,
                          const std::vector<double>& v,
                          const std::vector<std::set<int32_t>>& cand,
                          double tol) {
    Reference r;
    r.row_min.assign(static_cast<std::size_t>(src.nrow), kInf);
    r.row_violators.resize(static_cast<std::size_t>(src.nrow));

    for (int64_t i = 0; i < src.nrow; ++i) {
        for (int64_t j = 0; j < src.ncol; ++j) {
            if (cand[static_cast<std::size_t>(i)].count(static_cast<int32_t>(j)) != 0u) continue;
            ++r.n_scanned;
            if (!src.allowed(i, j)) continue;
            ++r.n_evaluated;
            const double cbar = src.at(i, j) - u[static_cast<std::size_t>(i)] -
                                v[static_cast<std::size_t>(j)];
            if (cbar < r.row_min[static_cast<std::size_t>(i)]) {
                r.row_min[static_cast<std::size_t>(i)] = cbar;
            }
            if (cbar < -tol) {
                ++r.n_violators;
                r.row_violators[static_cast<std::size_t>(i)]
                    .emplace_back(cbar, static_cast<int32_t>(j));
            }
        }
        if (r.row_min[static_cast<std::size_t>(i)] < r.min_cbar) {
            r.min_cbar = r.row_min[static_cast<std::size_t>(i)];
        }
    }
    return r;
}

lap::CandidateSet build_candidates(int64_t nrow, int64_t ncol,
                                   const std::vector<std::set<int32_t>>& rows) {
    lap::CandidateSet cand(nrow, ncol);
    std::vector<Pair> pairs;
    for (int64_t i = 0; i < nrow; ++i) {
        for (int32_t j : rows[static_cast<std::size_t>(i)]) {
            pairs.emplace_back(static_cast<int32_t>(i), j);
        }
    }
    cand.add_pairs(pairs);
    return cand;
}

// Two rng() calls as arguments to one call have unspecified order, so every
// draw is taken into a named local before it is used.
void forbid_random(lap::CostMatrix& src, std::mt19937& rng, int64_t count) {
    for (int64_t k = 0; k < count; ++k) {
        const int64_t i = static_cast<int64_t>(rng() % static_cast<unsigned>(src.nrow));
        const int64_t j = static_cast<int64_t>(rng() % static_cast<unsigned>(src.ncol));
        src.forbid(i, j);
    }
}

void seed_random(std::vector<std::set<int32_t>>& rows, std::mt19937& rng,
                 int64_t ncol, int64_t count) {
    for (int64_t k = 0; k < count; ++k) {
        const std::size_t i = rng() % static_cast<unsigned>(rows.size());
        const int32_t j = static_cast<int32_t>(rng() % static_cast<unsigned>(ncol));
        rows[i].insert(j);
    }
}

std::vector<double> cbars_of_row(const lap::BlockPricing& out, int32_t i) {
    std::vector<double> got;
    for (const lap::PricedPair& p : out.violators) {
        if (p.i == i) got.push_back(p.cbar);
    }
    std::sort(got.begin(), got.end());
    return got;
}

}  // namespace

TEST_CASE("Pricing reads the omitted pairs and leaves the candidates alone",
          "[flow][pricing]") {
    // cbar_ij = c_ij - u_i - v_j, with u = v = 0, so cbar is the cost.
    lap::CostMatrix src(std::vector<std::vector<double>>{
        {-3.0,  1.0, -2.0,  4.0},
        { 5.0, -7.0,  6.0,  0.5},
        { 2.0,  3.0,  1.0,  8.0}});

    const std::vector<double> u(3, 0.0);
    const std::vector<double> v(4, 0.0);

    // Row 0 already holds its two negatives; row 1 holds nothing.
    lap::CandidateSet cand = build_candidates(3, 4, {{0, 2}, {}, {}});

    const lap::BlockPricing out = lap::price_block(src, u, v, cand, 4, kTol);

    // 12 pairs less the two candidates.
    REQUIRE(out.n_scanned == 10);
    REQUIRE(out.n_evaluated == 10);
    REQUIRE(out.n_violators == 1);          // only (1, 1) at -7
    REQUIRE(out.violators.size() == 1u);
    REQUIRE(out.violators[0].i == 1);
    REQUIRE(out.violators[0].j == 1);
    REQUIRE(out.violators[0].cbar == -7.0);

    REQUIRE(out.min_reduced_cost == -7.0);
    REQUIRE(out.row_min[0] == 1.0);          // -3 and -2 are candidates, so 1.0 is the row's least
    REQUIRE(out.row_min[1] == -7.0);
    REQUIRE(out.row_min[2] == 1.0);
}

TEST_CASE("A row whose every pair is a candidate prices to nothing",
          "[flow][pricing]") {
    lap::CostMatrix src(std::vector<std::vector<double>>{
        {-1.0, -2.0},
        {-3.0, -4.0}});
    const std::vector<double> u(2, 0.0);
    const std::vector<double> v(2, 0.0);

    lap::CandidateSet cand = build_candidates(2, 2, {{0, 1}, {}});

    const lap::BlockPricing out = lap::price_block(src, u, v, cand, 4, kTol);

    REQUIRE(out.row_min[0] == kInf);
    REQUIRE(out.row_min[1] == -4.0);
    REQUIRE(out.min_reduced_cost == -4.0);
    for (const lap::PricedPair& p : out.violators) REQUIRE(p.i == 1);
}

TEST_CASE("With nothing in the candidate set, pricing sees what the certificate scan sees",
          "[flow][pricing]") {
    std::mt19937 rng(20260816u);
    std::uniform_real_distribution<double> cost(-5.0, 5.0);
    std::uniform_real_distribution<double> dual(-2.0, 2.0);

    const int64_t nr = 17;
    const int64_t nc = 23;

    lap::CostMatrix src(nr, nc);
    for (int64_t i = 0; i < nr; ++i) {
        for (int64_t j = 0; j < nc; ++j) src.at(i, j) = cost(rng);
    }
    forbid_random(src, rng, 40);

    std::vector<double> u(static_cast<std::size_t>(nr));
    std::vector<double> v(static_cast<std::size_t>(nc));
    for (double& x : u) x = dual(rng);
    for (double& x : v) x = dual(rng);

    lap::CandidateSet cand(nr, nc);
    const lap::BlockPricing out = lap::price_block(src, u, v, cand, 1, kTol);
    const lap::ReducedCostScan scan = lap::scan_reduced_costs(src, u, v, kTol);

    // Every pair is omitted, so the two are scanning the same set.
    REQUIRE(out.n_scanned == nr * nc);
    REQUIRE(out.n_evaluated == scan.n_admissible);
    REQUIRE(out.n_violators == scan.n_violations);
    REQUIRE(out.min_reduced_cost == scan.min_reduced_cost);
    REQUIRE(out.row_min[static_cast<std::size_t>(scan.arg_i)] == scan.min_reduced_cost);
}

TEST_CASE("A forbidden pair is scanned, never evaluated, never a violator",
          "[flow][pricing]") {
    lap::CostMatrix src(std::vector<std::vector<double>>{{-9.0, 1.0, -8.0}});
    src.forbid(0, 0);   // the most negative pair in the row

    const std::vector<double> u(1, 0.0);
    const std::vector<double> v(3, 0.0);
    lap::CandidateSet cand(1, 3);

    const lap::BlockPricing out = lap::price_block(src, u, v, cand, 4, kTol);

    REQUIRE(out.n_scanned == 3);
    REQUIRE(out.n_evaluated == 2);
    REQUIRE(out.n_violators == 1);
    REQUIRE(out.violators.size() == 1u);
    REQUIRE(out.violators[0].j == 2);
    REQUIRE(out.min_reduced_cost == -8.0);
    REQUIRE(cand.edges_evaluated() == 2);
}

TEST_CASE("keep_per_row bounds a row and keeps its most negative",
          "[flow][pricing]") {
    lap::CostMatrix src(std::vector<std::vector<double>>{
        {-1.0, -5.0, -3.0, -2.0, -4.0,  6.0},
        {-8.0,  7.0, -8.0, -8.0,  1.0,  2.0}});
    const std::vector<double> u(2, 0.0);
    const std::vector<double> v(6, 0.0);

    SECTION("two per row") {
        lap::CandidateSet cand(2, 6);
        const lap::BlockPricing out = lap::price_block(src, u, v, cand, 2, kTol);

        REQUIRE(out.n_violators == 8);       // five in row 0, three in row 1
        REQUIRE(out.violators.size() == 4u);
        REQUIRE(cbars_of_row(out, 0) == std::vector<double>{-5.0, -4.0});
        // Row 1's three-way tie at -8 keeps two of them, whichever two.
        REQUIRE(cbars_of_row(out, 1) == std::vector<double>{-8.0, -8.0});
    }

    SECTION("more per row than the row has") {
        lap::CandidateSet cand(2, 6);
        const lap::BlockPricing out = lap::price_block(src, u, v, cand, 50, kTol);

        REQUIRE(out.violators.size() == 8u);
        REQUIRE(static_cast<int64_t>(out.violators.size()) == out.n_violators);
    }

    SECTION("none per row still counts them") {
        lap::CandidateSet cand(2, 6);
        const lap::BlockPricing out = lap::price_block(src, u, v, cand, 0, kTol);

        REQUIRE(out.violators.empty());
        REQUIRE(out.n_violators == 8);
        REQUIRE(out.min_reduced_cost == -8.0);
    }
}

TEST_CASE("Violators come back ascending by row and column", "[flow][pricing]") {
    lap::CostMatrix src(std::vector<std::vector<double>>{
        {-1.0, -9.0, -5.0},
        { 0.0, -2.0, -7.0}});
    const std::vector<double> u(2, 0.0);
    const std::vector<double> v(3, 0.0);
    lap::CandidateSet cand(2, 3);

    const lap::BlockPricing out = lap::price_block(src, u, v, cand, 3, kTol);

    for (std::size_t k = 1; k < out.violators.size(); ++k) {
        const lap::PricedPair& a = out.violators[k - 1];
        const lap::PricedPair& b = out.violators[k];
        REQUIRE((a.i < b.i || (a.i == b.i && a.j < b.j)));
    }
    REQUIRE(lap::violator_pairs(out.violators) ==
            std::vector<Pair>{{0, 0}, {0, 1}, {0, 2}, {1, 1}, {1, 2}});
}

TEST_CASE("A candidate at either end of a row is skipped like any other",
          "[flow][pricing]") {
    // The cursor walk has to survive a candidate at column 0, a candidate at
    // the last column, and adjacent candidates.
    lap::CostMatrix src(std::vector<std::vector<double>>{
        {-1.0, -2.0, -3.0, -4.0, -5.0}});
    const std::vector<double> u(1, 0.0);
    const std::vector<double> v(5, 0.0);

    lap::CandidateSet cand = build_candidates(1, 5, {{0, 1, 4}});
    const lap::BlockPricing out = lap::price_block(src, u, v, cand, 5, kTol);

    REQUIRE(out.n_scanned == 2);
    REQUIRE(lap::violator_pairs(out.violators) == std::vector<Pair>{{0, 2}, {0, 3}});
    REQUIRE(out.min_reduced_cost == -4.0);
}

TEST_CASE("Pricing agrees with the obvious implementation over many shapes",
          "[flow][pricing]") {
    std::mt19937 rng(20260818u);
    std::uniform_real_distribution<double> cost(-4.0, 4.0);
    std::uniform_real_distribution<double> dual(-2.0, 2.0);

    for (int trial = 0; trial < 30; ++trial) {
        const int64_t nr = 1 + static_cast<int64_t>(rng() % 13u);
        const int64_t nc = 1 + static_cast<int64_t>(rng() % 31u);
        const int keep = static_cast<int>(rng() % 5u);

        lap::CostMatrix src(nr, nc);
        for (int64_t i = 0; i < nr; ++i) {
            for (int64_t j = 0; j < nc; ++j) {
                // Coarse costs, so ties are common and the tie handling is exercised.
                src.at(i, j) = std::round(cost(rng));
            }
        }
        forbid_random(src, rng, static_cast<int64_t>(rng() % static_cast<unsigned>(nr * nc + 1)));

        std::vector<double> u(static_cast<std::size_t>(nr));
        std::vector<double> v(static_cast<std::size_t>(nc));
        for (double& x : u) x = dual(rng);
        for (double& x : v) x = dual(rng);

        std::vector<std::set<int32_t>> rows(static_cast<std::size_t>(nr));
        seed_random(rows, rng, nc,
                    static_cast<int64_t>(rng() % static_cast<unsigned>(nr * nc + 1)));

        lap::CandidateSet cand = build_candidates(nr, nc, rows);
        const lap::BlockPricing got = lap::price_block(src, u, v, cand, keep, kTol);
        const Reference want = price_reference(src, u, v, rows, kTol);

        REQUIRE(got.n_scanned == want.n_scanned);
        REQUIRE(got.n_evaluated == want.n_evaluated);
        REQUIRE(got.n_violators == want.n_violators);
        REQUIRE(got.min_reduced_cost == want.min_cbar);
        REQUIRE(got.row_min == want.row_min);
        REQUIRE(cand.edges_evaluated() == want.n_evaluated);

        for (int64_t i = 0; i < nr; ++i) {
            std::vector<std::pair<double, int32_t>> all =
                want.row_violators[static_cast<std::size_t>(i)];
            std::sort(all.begin(), all.end());

            const std::size_t expect =
                std::min(static_cast<std::size_t>(keep > 0 ? keep : 0), all.size());
            const std::vector<double> kept = cbars_of_row(got, static_cast<int32_t>(i));
            REQUIRE(kept.size() == expect);

            // The kept reduced costs are the row's `keep` smallest. Which of a
            // tied set was kept is not asserted; that they are the smallest is.
            for (std::size_t t = 0; t < expect; ++t) REQUIRE(kept[t] == all[t].first);

            // And each kept pair is a real violator of this row.
            for (const lap::PricedPair& p : got.violators) {
                if (p.i != static_cast<int32_t>(i)) continue;
                REQUIRE(p.cbar < -kTol);
                REQUIRE_FALSE(cand.contains(i, p.j));
                REQUIRE(src.allowed(i, p.j));
                REQUIRE(p.cbar == src.at(i, p.j) - u[static_cast<std::size_t>(i)] -
                                      v[static_cast<std::size_t>(p.j)]);
            }
        }
    }
}

TEST_CASE("A priced violator, once added, is not offered again",
          "[flow][pricing]") {
    lap::CostMatrix src(std::vector<std::vector<double>>{
        {-1.0, -5.0, -3.0,  2.0},
        {-4.0,  1.0, -6.0, -2.0}});
    const std::vector<double> u(2, 0.0);
    const std::vector<double> v(4, 0.0);

    lap::CandidateSet cand(2, 4);

    const lap::BlockPricing first = lap::price_block(src, u, v, cand, 1, kTol);
    REQUIRE(first.violators.size() == 2u);
    REQUIRE(first.min_reduced_cost == -6.0);

    const std::vector<Pair> added = cand.add_pairs(lap::violator_pairs(first.violators));
    REQUIRE(added == std::vector<Pair>{{0, 1}, {1, 2}});

    const lap::BlockPricing second = lap::price_block(src, u, v, cand, 1, kTol);
    REQUIRE(second.n_scanned == 6);
    for (const lap::PricedPair& p : second.violators) {
        REQUIRE_FALSE((p.i == 0 && p.j == 1));
        REQUIRE_FALSE((p.i == 1 && p.j == 2));
    }
    REQUIRE(second.min_reduced_cost == -4.0);

    // Both rounds' evaluations, and the second round priced two pairs fewer.
    REQUIRE(cand.edges_evaluated() == 8 + 6);
}

TEST_CASE("Pricing runs to termination on duals that leave nothing negative",
          "[flow][pricing]") {
    lap::CostMatrix src(std::vector<std::vector<double>>{
        {4.0, 5.0},
        {6.0, 7.0}});
    // Row duals at each row's minimum leave every reduced cost at or above zero.
    const std::vector<double> u{4.0, 6.0};
    const std::vector<double> v(2, 0.0);
    lap::CandidateSet cand(2, 2);

    const lap::BlockPricing out = lap::price_block(src, u, v, cand, 4, kTol);

    REQUIRE(out.n_violators == 0);
    REQUIRE(out.violators.empty());
    REQUIRE(out.min_reduced_cost == 0.0);
}

TEST_CASE("An empty problem prices to an empty answer", "[flow][pricing]") {
    lap::CostMatrix src(0, 0);
    lap::CandidateSet cand(0, 0);
    const lap::BlockPricing out =
        lap::price_block(src, std::vector<double>{}, std::vector<double>{}, cand, 4, kTol);

    REQUIRE(out.violators.empty());
    REQUIRE(out.row_min.empty());
    REQUIRE(out.min_reduced_cost == kInf);
    REQUIRE(out.n_scanned == 0);
}

TEST_CASE("Pricing names a shape it cannot price", "[flow][pricing]") {
    lap::CostMatrix src(3, 4);
    lap::CandidateSet cand(3, 4);
    const std::vector<double> u(3, 0.0);
    const std::vector<double> v(4, 0.0);

    lap::CandidateSet wrong(3, 5);
    REQUIRE_THROWS_AS(lap::price_block(src, u, v, wrong, 1, kTol),
                      lap::DimensionException);
    REQUIRE_THROWS_AS(lap::price_block(src, std::vector<double>(2, 0.0), v, cand, 1, kTol),
                      lap::DimensionException);
    REQUIRE_THROWS_AS(lap::price_block(src, u, std::vector<double>(5, 0.0), cand, 1, kTol),
                      lap::DimensionException);
}
