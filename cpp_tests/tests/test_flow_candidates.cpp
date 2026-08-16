// Test suite for the candidate set: the pairs a restricted master may use.

#include <catch2/catch_test_macros.hpp>

#include "core/lap_error.h"
#include "flow/flow_candidates.h"

#include <algorithm>
#include <cstdint>
#include <random>
#include <set>
#include <utility>
#include <vector>

namespace {

using Pair = lap::CandidateSet::Pair;

std::vector<int32_t> row_of(const lap::CandidateSet& cand, int64_t i) {
    return std::vector<int32_t>(cand.row_begin(i), cand.row_end(i));
}

}  // namespace

TEST_CASE("An empty candidate set holds nothing", "[flow][candidates]") {
    lap::CandidateSet cand(3, 4);

    REQUIRE(cand.nrow() == 3);
    REQUIRE(cand.ncol() == 4);
    REQUIRE(cand.n_arcs() == 0);
    REQUIRE(cand.edges_evaluated() == 0);

    for (int64_t i = 0; i < 3; ++i) {
        REQUIRE(cand.row_size(i) == 0);
        REQUIRE(cand.row_begin(i) == cand.row_end(i));
        for (int64_t j = 0; j < 4; ++j) REQUIRE_FALSE(cand.contains(i, j));
    }
}

TEST_CASE("Adding pairs reports the ones that were not already in",
          "[flow][candidates]") {
    lap::CandidateSet cand(3, 5);

    const std::vector<Pair> added = cand.add_pairs({{1, 4}, {0, 2}, {1, 0}, {0, 2}});

    // Row order, and the repeated (0, 2) reported once.
    REQUIRE(added == std::vector<Pair>{{0, 2}, {1, 0}, {1, 4}});
    REQUIRE(cand.n_arcs() == 3);
    REQUIRE(cand.row_size(0) == 1);
    REQUIRE(cand.row_size(1) == 2);
    REQUIRE(cand.row_size(2) == 0);
    REQUIRE(row_of(cand, 1) == std::vector<int32_t>{0, 4});

    SECTION("a second call reports only what it brought") {
        const std::vector<Pair> again = cand.add_pairs({{0, 2}, {1, 2}, {2, 3}});

        REQUIRE(again == std::vector<Pair>{{1, 2}, {2, 3}});
        REQUIRE(cand.n_arcs() == 5);
        // Merged in place: every row stays ascending.
        REQUIRE(row_of(cand, 1) == std::vector<int32_t>{0, 2, 4});
        REQUIRE(row_of(cand, 2) == std::vector<int32_t>{3});
    }

    SECTION("adding nothing new changes nothing") {
        const std::vector<Pair> again = cand.add_pairs({{0, 2}, {1, 0}, {1, 4}});

        REQUIRE(again.empty());
        REQUIRE(cand.n_arcs() == 3);
        REQUIRE(row_of(cand, 1) == std::vector<int32_t>{0, 4});
    }

    SECTION("membership agrees with the rows") {
        for (int64_t i = 0; i < 3; ++i) {
            const std::vector<int32_t> row = row_of(cand, i);
            for (int64_t j = 0; j < 5; ++j) {
                const bool in = std::find(row.begin(), row.end(),
                                          static_cast<int32_t>(j)) != row.end();
                REQUIRE(cand.contains(i, j) == in);
            }
        }
    }
}

TEST_CASE("The candidate set names an index it cannot hold", "[flow][candidates]") {
    REQUIRE_THROWS_AS(lap::CandidateSet(-1, 4), lap::DimensionException);
    REQUIRE_THROWS_AS(lap::CandidateSet(3, -1), lap::DimensionException);

    lap::CandidateSet cand(3, 4);
    REQUIRE_THROWS_AS(cand.add_pairs({{3, 0}}), lap::DimensionException);
    REQUIRE_THROWS_AS(cand.add_pairs({{0, 4}}), lap::DimensionException);
    REQUIRE_THROWS_AS(cand.add_pairs({{-1, 0}}), lap::DimensionException);
    REQUIRE_THROWS_AS(cand.contains(0, 4), lap::DimensionException);
    REQUIRE_THROWS_AS(cand.row_size(3), lap::DimensionException);

    // A rejected batch leaves the set as it was.
    REQUIRE(cand.n_arcs() == 0);
}

TEST_CASE("The evaluated counter runs across rounds", "[flow][candidates]") {
    lap::CandidateSet cand(2, 2);

    cand.note_evaluated(4);
    cand.note_evaluated(3);

    REQUIRE(cand.edges_evaluated() == 7);
    // It counts pairs priced, not pairs held, so adding does not move it.
    cand.add_pairs({{0, 0}});
    REQUIRE(cand.edges_evaluated() == 7);
}

TEST_CASE("The candidate set agrees with a set of pairs over many rounds",
          "[flow][candidates]") {
    const int64_t nr = 40;
    const int64_t nc = 60;
    lap::CandidateSet cand(nr, nc);
    std::set<Pair> ref;

    std::mt19937 rng(20260816u);
    std::uniform_int_distribution<int32_t> row(0, static_cast<int32_t>(nr - 1));
    std::uniform_int_distribution<int32_t> col(0, static_cast<int32_t>(nc - 1));

    for (int round = 0; round < 12; ++round) {
        std::vector<Pair> batch;
        for (int k = 0; k < 50; ++k) batch.emplace_back(row(rng), col(rng));

        const std::vector<Pair> added = cand.add_pairs(batch);

        // Every reported pair was absent and is now present.
        for (const Pair& p : added) {
            REQUIRE(ref.count(p) == 0u);
            REQUIRE(cand.contains(p.first, p.second));
            ref.insert(p);
        }
        for (const Pair& p : batch) ref.insert(p);

        REQUIRE(cand.n_arcs() == static_cast<int64_t>(ref.size()));
        for (int64_t i = 0; i < nr; ++i) {
            const std::vector<int32_t> r = row_of(cand, i);
            REQUIRE(std::is_sorted(r.begin(), r.end()));
            REQUIRE(std::adjacent_find(r.begin(), r.end()) == r.end());
        }
    }

    for (int64_t i = 0; i < nr; ++i) {
        for (int64_t j = 0; j < nc; ++j) {
            REQUIRE(cand.contains(i, j) ==
                    (ref.count(Pair(static_cast<int32_t>(i),
                                    static_cast<int32_t>(j))) == 1u));
        }
    }
}
