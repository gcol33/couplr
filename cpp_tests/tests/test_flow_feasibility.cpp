// Test suite for the feasibility phase: Hall's witness over the restricted arc
// set, the re-seed it aims, and the certificate that ends the ladder.

#include <catch2/catch_test_macros.hpp>

#include "core/lap_error.h"
#include "core/lap_hall.h"
#include "core/lap_types.h"
#include "flow/flow_candidates.h"
#include "flow/flow_feasibility.h"

#include <algorithm>
#include <cstdint>
#include <random>
#include <set>
#include <utility>
#include <vector>

namespace {

using Pair   = lap::CandidateSet::Pair;
using Status = lap::FeasibilityRound::Status;

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

// The same graph written densely: every pair the candidate set omits is
// forbidden outright, so a matching over it reads the grid where the restricted
// graph reads the candidates.
lap::CostMatrix densify(const lap::CostMatrix& src, const lap::CandidateSet& cand) {
    lap::CostMatrix out = src;
    for (int64_t i = 0; i < src.nrow; ++i) {
        for (int64_t j = 0; j < src.ncol; ++j) {
            if (!cand.contains(i, j)) out.forbid(i, j);
        }
    }
    return out;
}

std::vector<int32_t> columns_of_row(const std::vector<Pair>& pairs, int32_t i) {
    std::vector<int32_t> got;
    for (const Pair& p : pairs) {
        if (p.first == i) got.push_back(p.second);
    }
    std::sort(got.begin(), got.end());
    return got;
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

}  // namespace

TEST_CASE("The restricted graph is matched exactly as the same graph written densely",
          "[flow][feasibility]") {
    std::mt19937 rng(20260816u);
    std::uniform_real_distribution<double> cost(0.0, 10.0);

    for (int trial = 0; trial < 30; ++trial) {
        const int64_t nr = 1 + static_cast<int64_t>(rng() % 11u);
        const int64_t nc = 1 + static_cast<int64_t>(rng() % 17u);

        lap::CostMatrix src(nr, nc);
        for (int64_t i = 0; i < nr; ++i) {
            for (int64_t j = 0; j < nc; ++j) src.at(i, j) = cost(rng);
        }
        forbid_random(src, rng, static_cast<int64_t>(rng() % static_cast<unsigned>(nr * nc + 1)));

        std::vector<std::set<int32_t>> rows(static_cast<std::size_t>(nr));
        const int64_t seeds = static_cast<int64_t>(rng() % static_cast<unsigned>(nr * nc + 1));
        for (int64_t k = 0; k < seeds; ++k) {
            const std::size_t i = rng() % static_cast<unsigned>(nr);
            const int32_t j = static_cast<int32_t>(rng() % static_cast<unsigned>(nc));
            rows[i].insert(j);
        }
        lap::CandidateSet cand = build_candidates(nr, nc, rows);

        const lap::DeficiencySet sparse =
            lap::hall_witness(lap::CandidateGraph<lap::CostMatrix>(src, cand));
        const lap::CostMatrix dense_src = densify(src, cand);
        const lap::DeficiencySet dense = lap::hall_witness(dense_src);

        REQUIRE(sparse.max_cardinality == dense.max_cardinality);
        REQUIRE(sparse.deficiency == dense.deficiency);
        REQUIRE(sparse.row_perfect == dense.row_perfect);
        REQUIRE(sparse.verified == dense.verified);
        REQUIRE(sparse.rows == dense.rows);
        REQUIRE(sparse.cols == dense.cols);
        // Same algorithm over the same neighbourhoods in the same order, so it
        // is the same matching and not only the same cardinality.
        REQUIRE(sparse.matching == dense.matching);
    }
}

TEST_CASE("A restricted set that matches every row is feasible and grows by nothing",
          "[flow][feasibility]") {
    lap::CostMatrix src(3, 4);
    for (int64_t i = 0; i < 3; ++i) {
        for (int64_t j = 0; j < 4; ++j) src.at(i, j) = 1.0;
    }
    lap::CandidateSet cand = build_candidates(3, 4, {{0}, {1}, {2}});

    const lap::FeasibilityRound out = lap::feasibility_round(src, cand, 4);

    REQUIRE(out.status == Status::feasible);
    REQUIRE(out.witness.row_perfect);
    REQUIRE(out.witness.rows.empty());
    REQUIRE(out.witness.cols.empty());
    REQUIRE(out.added.empty());
    REQUIRE(out.n_scanned == 0);
    REQUIRE(out.certified == false);
    REQUIRE(cand.n_arcs() == 3);
    REQUIRE(cand.edges_evaluated() == 0);
}

TEST_CASE("Only the deficient rows are re-seeded, and only outside their neighbourhood",
          "[flow][feasibility]") {
    // Rows 0 and 1 both reach column 0 alone, so one of them is unmatched and
    // the witness is S = {0, 1}, N(S) = {0}. Row 2 is matched and pays nothing.
    lap::CostMatrix src(3, 4);
    for (int64_t i = 0; i < 3; ++i) {
        for (int64_t j = 0; j < 4; ++j) src.at(i, j) = static_cast<double>(10 * i + j);
    }
    lap::CandidateSet cand = build_candidates(3, 4, {{0}, {0}, {2}});

    const lap::FeasibilityRound out = lap::feasibility_round(src, cand, 1);

    REQUIRE(out.status == Status::reseeded);
    REQUIRE(out.witness.deficiency == 1);
    REQUIRE(out.witness.rows == std::vector<int64_t>{0, 1});
    REQUIRE(out.witness.cols == std::vector<int64_t>{0});
    REQUIRE(out.witness.verified);

    // Three columns outside N(S), for each of the two deficient rows.
    REQUIRE(out.n_scanned == 6);
    REQUIRE(out.n_evaluated == 6);

    // One column each, the cheapest outside N(S), which is column 1 in both rows.
    REQUIRE(out.added == std::vector<Pair>{{0, 1}, {1, 1}});
    REQUIRE(cand.row_size(2) == 1);          // untouched
    REQUIRE(cand.edges_evaluated() == 6);

    // Column 1 is what the deficiency needed, so the next round matches.
    const lap::FeasibilityRound again = lap::feasibility_round(src, cand, 1);
    REQUIRE(again.status == Status::feasible);
}

TEST_CASE("The re-seed takes a row's cheapest admissible columns", "[flow][feasibility]") {
    // Both rows hold column 0 and nothing else, so N(S) is column 0 and the
    // scan sees columns 1 through 5. Costs fall towards the far end, so a scan
    // keeping the first columns it met would be caught.
    lap::CostMatrix src(2, 6);
    const double row0[6] = {0.0, 9.0, 8.0, 7.0, 6.0, 5.0};
    for (int64_t j = 0; j < 6; ++j) {
        src.at(0, j) = row0[j];
        src.at(1, j) = 100.0 + static_cast<double>(j);
    }
    src.forbid(0, 5);                        // row 0's cheapest is forbidden

    lap::CandidateSet cand = build_candidates(2, 6, {{0}, {0}});

    const lap::FeasibilityRound out = lap::feasibility_round(src, cand, 2);

    REQUIRE(out.status == Status::reseeded);
    REQUIRE(out.n_scanned == 10);
    REQUIRE(out.n_evaluated == 9);
    REQUIRE(columns_of_row(out.added, 0) == std::vector<int32_t>{3, 4});
    REQUIRE(columns_of_row(out.added, 1) == std::vector<int32_t>{1, 2});
}

TEST_CASE("A candidate the source forbids is neither an edge nor re-offered",
          "[flow][feasibility]") {
    lap::CostMatrix src(1, 3);
    for (int64_t j = 0; j < 3; ++j) src.at(0, j) = 1.0 + static_cast<double>(j);
    src.forbid(0, 0);

    lap::CandidateSet cand = build_candidates(1, 3, {{0}});

    const lap::FeasibilityRound out = lap::feasibility_round(src, cand, 3);

    // The candidate carries no edge, so the row is unmatched and N(S) is empty.
    REQUIRE(out.status == Status::reseeded);
    REQUIRE(out.witness.max_cardinality == 0);
    REQUIRE(out.witness.rows == std::vector<int64_t>{0});
    REQUIRE(out.witness.cols.empty());

    REQUIRE(out.added == std::vector<Pair>{{0, 1}, {0, 2}});
    REQUIRE(cand.contains(0, 0));            // still a candidate, still no arc
}

TEST_CASE("An infeasible problem comes back with a certificate rather than a wider ladder",
          "[flow][feasibility]") {
    // Three rows, four columns, but every row reaches columns 0 and 1 alone.
    lap::CostMatrix src(3, 4);
    for (int64_t i = 0; i < 3; ++i) {
        for (int64_t j = 0; j < 4; ++j) {
            src.at(i, j) = static_cast<double>(j);
            if (j >= 2) src.forbid(i, j);
        }
    }

    lap::CandidateSet cand(3, 4);
    int rounds = 0;
    lap::FeasibilityRound out = lap::feasibility_round(src, cand, 4);
    while (out.status == Status::reseeded) {
        REQUIRE(++rounds < 20);
        out = lap::feasibility_round(src, cand, 4);
    }

    REQUIRE(out.status == Status::infeasible);
    REQUIRE(out.certified);
    REQUIRE(out.witness.rows == std::vector<int64_t>{0, 1, 2});
    REQUIRE(out.witness.cols == std::vector<int64_t>{0, 1});
    REQUIRE(out.witness.deficiency == 1);

    // The certificate is the one the full source produces on its own.
    const lap::DeficiencySet full = lap::hall_witness(src);
    REQUIRE(full.rows == out.witness.rows);
    REQUIRE(full.cols == out.witness.cols);
}

TEST_CASE("More rows than columns is infeasible on the first witness",
          "[flow][feasibility]") {
    lap::CostMatrix src(4, 2);
    for (int64_t i = 0; i < 4; ++i) {
        for (int64_t j = 0; j < 2; ++j) src.at(i, j) = 1.0;
    }
    lap::CandidateSet cand(4, 2);

    lap::FeasibilityRound out = lap::feasibility_round(src, cand, 2);
    REQUIRE(out.status == Status::reseeded);       // the empty set seeds first
    out = lap::feasibility_round(src, cand, 2);

    REQUIRE(out.status == Status::infeasible);
    REQUIRE(out.certified);
    REQUIRE(out.witness.rows.size() == 4u);
    REQUIRE(out.witness.cols.size() == 2u);
}

TEST_CASE("Rounds run to the verdict the full source's own witness gives",
          "[flow][feasibility]") {
    std::mt19937 rng(20260817u);
    std::uniform_real_distribution<double> cost(0.0, 10.0);

    for (int trial = 0; trial < 40; ++trial) {
        const int64_t nr = 1 + static_cast<int64_t>(rng() % 9u);
        const int64_t nc = 1 + static_cast<int64_t>(rng() % 11u);
        const int width  = 1 + static_cast<int>(rng() % 3u);

        lap::CostMatrix src(nr, nc);
        for (int64_t i = 0; i < nr; ++i) {
            for (int64_t j = 0; j < nc; ++j) src.at(i, j) = cost(rng);
        }
        // Enough forbidding that a good share of the trials are infeasible.
        forbid_random(src, rng, static_cast<int64_t>(rng() % static_cast<unsigned>(nr * nc + 1)));

        lap::CandidateSet cand(nr, nc);
        lap::FeasibilityRound out = lap::feasibility_round(src, cand, width);
        int64_t rounds = 0;
        while (out.status == Status::reseeded) {
            // Every re-seeded round adds at least one pair, so the round count
            // is bounded by the grid.
            REQUIRE_FALSE(out.added.empty());
            REQUIRE(++rounds <= nr * nc + 1);
            out = lap::feasibility_round(src, cand, width);
        }

        const lap::DeficiencySet full = lap::hall_witness(src);
        REQUIRE((out.status == Status::feasible) == full.row_perfect);

        if (out.status == Status::infeasible) {
            REQUIRE(out.certified);
            REQUIRE(out.witness.rows.size() > out.witness.cols.size());
            // No row of S reaches a column outside N(S) in the full source.
            std::set<int64_t> neighbourhood(out.witness.cols.begin(), out.witness.cols.end());
            for (int64_t i : out.witness.rows) {
                for (int64_t j = 0; j < nc; ++j) {
                    if (src.allowed(i, j)) REQUIRE(neighbourhood.count(j) == 1u);
                }
            }
        } else {
            // Feasible means the candidate set alone carries a row-perfect
            // matching, over pairs the source admits.
            REQUIRE(out.witness.matching.size() == static_cast<std::size_t>(nr));
            for (int64_t i = 0; i < nr; ++i) {
                const int j = out.witness.matching[static_cast<std::size_t>(i)];
                REQUIRE(j >= 0);
                REQUIRE(cand.contains(i, j));
                REQUIRE(src.allowed(i, j));
            }
        }
    }
}

TEST_CASE("A round names what it cannot run", "[flow][feasibility]") {
    lap::CostMatrix src(3, 4);
    lap::CandidateSet cand(3, 4);
    lap::CandidateSet wrong(3, 5);

    REQUIRE_THROWS_AS(lap::feasibility_round(src, wrong, 1), lap::DimensionException);
    REQUIRE_THROWS_AS(lap::feasibility_round(src, cand, 0), lap::DimensionException);
    REQUIRE_THROWS_AS(lap::feasibility_round(src, cand, -4), lap::DimensionException);
}

TEST_CASE("An empty problem is feasible and asks nothing of the source",
          "[flow][feasibility]") {
    lap::CostMatrix src(0, 0);
    lap::CandidateSet cand(0, 0);

    const lap::FeasibilityRound out = lap::feasibility_round(src, cand, 1);

    REQUIRE(out.status == Status::feasible);
    REQUIRE(out.added.empty());
    REQUIRE(out.n_scanned == 0);
}

TEST_CASE("A problem with rows and no columns is infeasible with the rows named",
          "[flow][feasibility]") {
    lap::CostMatrix src(2, 0);
    lap::CandidateSet cand(2, 0);

    const lap::FeasibilityRound out = lap::feasibility_round(src, cand, 1);

    REQUIRE(out.status == Status::infeasible);
    REQUIRE(out.certified);
    REQUIRE(out.witness.rows == std::vector<int64_t>{0, 1});
    REQUIRE(out.witness.cols.empty());
}
