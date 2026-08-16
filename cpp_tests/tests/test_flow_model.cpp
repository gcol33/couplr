// Test suite for the internal flow model: validation, block expansion and the
// min-cost flow solver, exercised without Rcpp.

#include <catch2/catch_test_macros.hpp>
#include <catch2/catch_approx.hpp>

#include "core/lap_certify.h"
#include "core/lap_error.h"
#include "core/lap_types.h"
#include "flow/flow_candidates.h"
#include "flow/flow_certify.h"
#include "flow/flow_oracle.h"
#include "flow/flow_problem.h"
#include "flow/flow_solve.h"
#include "solvers/solve_jv.h"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <random>
#include <vector>

using Catch::Approx;

namespace {

lap::CostMatrix make_cost(std::initializer_list<std::initializer_list<double>> rows) {
    std::vector<std::vector<double>> data;
    for (const auto& row : rows) data.push_back(std::vector<double>(row));
    return lap::CostMatrix(data);
}

// Node layout of flow_problem.h: FLOW_SOURCE, FLOW_SINK, then nr row nodes from
// FLOW_FIRST_ROW, then nc column nodes.
int32_t col_base(int64_t nr) { return static_cast<int32_t>(lap::FLOW_FIRST_ROW + nr); }

// Unit-capacity bipartite assignment: `n_match` units leave the source, cross
// one row and one column, and are absorbed by the sink.
lap::FlowProblem make_assignment(int64_t nr, int64_t nc,
                                 const lap::CostOracle* costs,
                                 int64_t n_match) {
    lap::FlowProblem prob;
    prob.n_nodes = static_cast<int32_t>(2 + nr + nc);
    prob.supply.assign(static_cast<std::size_t>(prob.n_nodes), 0);
    prob.supply[static_cast<std::size_t>(lap::FLOW_SOURCE)] = n_match;
    prob.supply[static_cast<std::size_t>(lap::FLOW_SINK)]   = -n_match;

    for (int64_t i = 0; i < nr; ++i) {
        prob.arcs.emplace_back(lap::FLOW_SOURCE,
                               static_cast<int32_t>(lap::FLOW_FIRST_ROW + i), 0, 1, 0.0);
    }
    for (int64_t j = 0; j < nc; ++j) {
        prob.arcs.emplace_back(static_cast<int32_t>(col_base(nr) + j),
                               lap::FLOW_SINK, 0, 1, 0.0);
    }

    lap::BipartiteBlock blk;
    blk.row_base = lap::FLOW_FIRST_ROW;
    blk.col_base = col_base(nr);
    blk.lower    = 0;
    blk.upper    = 1;
    blk.costs    = costs;
    prob.blocks.push_back(blk);

    return prob;
}

// Read the matching out of a solved flow through the block's (i, j) metadata.
std::vector<int> matching_of(const lap::FlowProblem& prob, const lap::FlowResult& res,
                             int64_t nr) {
    std::vector<int> match(static_cast<std::size_t>(nr), -1);
    REQUIRE(prob.block_arcs.size() == 1u);
    const lap::BlockArcRange& range = prob.block_arcs[0];
    for (int64_t k = 0; k < range.n_arcs; ++k) {
        if (res.flow[static_cast<std::size_t>(range.first_arc + k)] <= 0) continue;
        const auto rc = range.rc[static_cast<std::size_t>(k)];
        match[static_cast<std::size_t>(rc.first)] = static_cast<int>(rc.second);
    }
    return match;
}

// A solved flow read back as a matching and a pair of assignment duals, for a
// unit-capacity bipartite problem in the layout above. The gauge measuring both
// vectors from the sink, and the clamp on v, are derived in
// flow/flow_certify.h.
//
// The equality is on the rows. Every problem reaching this helper sends nr
// units across nr unit-capacity source arcs, so each row places exactly one
// unit and the columns carry sum_i x_ij <= 1; that is the orientation whose
// anchor is the sink.
lap::AssignmentDuals flow_duals(const lap::FlowProblem& prob,
                                const lap::FlowResult& res) {
    return lap::map_assignment_duals(prob, lap::layout_of(prob), res.flow, res.potential,
                                     lap::AssignmentEquality::Rows);
}

lap::CostMatrix random_cost(int64_t nr, int64_t nc, uint32_t seed) {
    std::mt19937 rng(seed);
    std::uniform_real_distribution<double> unif(-5.0, 20.0);
    lap::CostMatrix c(nr, nc);
    for (int64_t i = 0; i < nr; ++i) {
        for (int64_t j = 0; j < nc; ++j) c.at(i, j) = unif(rng);
    }
    return c;
}

// Two independent assignments sharing the auxiliary source and sink. The point
// of two blocks is that block 1's arcs sit behind block 0's, so growing block 0
// has to move them.
lap::FlowProblem make_two_assignments(int64_t nr_a, int64_t nc_a,
                                      const lap::CostOracle* a,
                                      int64_t nr_b, int64_t nc_b,
                                      const lap::CostOracle* b) {
    const int32_t row_a = lap::FLOW_FIRST_ROW;
    const int32_t col_a = static_cast<int32_t>(row_a + nr_a);
    const int32_t row_b = static_cast<int32_t>(col_a + nc_a);
    const int32_t col_b = static_cast<int32_t>(row_b + nr_b);

    lap::FlowProblem prob;
    prob.n_nodes = static_cast<int32_t>(col_b + nc_b);
    prob.supply.assign(static_cast<std::size_t>(prob.n_nodes), 0);
    prob.supply[static_cast<std::size_t>(lap::FLOW_SOURCE)] = nr_a + nr_b;
    prob.supply[static_cast<std::size_t>(lap::FLOW_SINK)]   = -(nr_a + nr_b);

    for (int64_t i = 0; i < nr_a; ++i) {
        prob.arcs.emplace_back(lap::FLOW_SOURCE, static_cast<int32_t>(row_a + i), 0, 1, 0.0);
    }
    for (int64_t j = 0; j < nc_a; ++j) {
        prob.arcs.emplace_back(static_cast<int32_t>(col_a + j), lap::FLOW_SINK, 0, 1, 0.0);
    }
    for (int64_t i = 0; i < nr_b; ++i) {
        prob.arcs.emplace_back(lap::FLOW_SOURCE, static_cast<int32_t>(row_b + i), 0, 1, 0.0);
    }
    for (int64_t j = 0; j < nc_b; ++j) {
        prob.arcs.emplace_back(static_cast<int32_t>(col_b + j), lap::FLOW_SINK, 0, 1, 0.0);
    }

    lap::BipartiteBlock blk;
    blk.lower = 0;
    blk.upper = 1;

    blk.row_base = row_a;
    blk.col_base = col_a;
    blk.costs    = a;
    prob.blocks.push_back(blk);

    blk.row_base = row_b;
    blk.col_base = col_b;
    blk.costs    = b;
    prob.blocks.push_back(blk);

    return prob;
}

// The mapping every reader of a solved flow relies on: block b's arc
// first_arc + k is the pair rc[k], priced by the block's own cost source.
void require_block_aligned(const lap::FlowProblem& prob, std::size_t b,
                           const lap::CostMatrix& c) {
    const lap::BlockArcRange& range  = prob.block_arcs[b];
    const lap::BipartiteBlock& blk   = prob.blocks[b];
    REQUIRE(static_cast<int64_t>(range.rc.size()) == range.n_arcs);
    REQUIRE(range.first_arc >= 0);
    REQUIRE(range.first_arc + range.n_arcs <= static_cast<int64_t>(prob.arcs.size()));

    for (int64_t k = 0; k < range.n_arcs; ++k) {
        const auto rc = range.rc[static_cast<std::size_t>(k)];
        const lap::FlowArc& arc =
            prob.arcs[static_cast<std::size_t>(range.first_arc + k)];
        INFO("block " << b << " arc " << k << " pair (" << rc.first << ", "
             << rc.second << ")");
        REQUIRE(c.allowed(rc.first, rc.second));
        REQUIRE(arc.tail == blk.row_base + rc.first);
        REQUIRE(arc.head == blk.col_base + rc.second);
        REQUIRE(arc.cost == Approx(c.at(rc.first, rc.second)));
    }

    std::vector<std::pair<int32_t, int32_t>> seen = range.rc;
    std::sort(seen.begin(), seen.end());
    REQUIRE(std::adjacent_find(seen.begin(), seen.end()) == seen.end());
}

// The dense problem the restricted master is solving: the same costs with every
// pair outside the candidate set forbidden.
lap::CostMatrix restricted_to(const lap::CostMatrix& c, const lap::CandidateSet& cand) {
    lap::CostMatrix r = c;
    for (int64_t i = 0; i < c.nrow; ++i) {
        for (int64_t j = 0; j < c.ncol; ++j) {
            if (!cand.contains(i, j)) r.forbid(i, j);
        }
    }
    return r;
}

std::vector<lap::CandidateSet::Pair> every_pair(int64_t nr, int64_t nc) {
    std::vector<lap::CandidateSet::Pair> all;
    all.reserve(static_cast<std::size_t>(nr * nc));
    for (int64_t i = 0; i < nr; ++i) {
        for (int64_t j = 0; j < nc; ++j) {
            all.emplace_back(static_cast<int32_t>(i), static_cast<int32_t>(j));
        }
    }
    return all;
}

lap::CostMatrix transpose_of(const lap::CostMatrix& c) {
    lap::CostMatrix t(c.ncol, c.nrow);
    for (int64_t i = 0; i < c.nrow; ++i) {
        for (int64_t j = 0; j < c.ncol; ++j) {
            t.at(j, i) = c.at(i, j);
            if (!c.allowed(i, j)) t.forbid(j, i);
        }
    }
    return t;
}

}  // namespace

// ---------------------------------------------------------------------------
// validation
// ---------------------------------------------------------------------------

TEST_CASE("Flow validation rejects malformed problems", "[flow][validate]") {
    SECTION("supplies that do not sum to zero") {
        lap::FlowProblem prob;
        prob.n_nodes = 2;
        prob.supply = {3, -2};
        prob.arcs.emplace_back(0, 1, 0, 5, 1.0);
        REQUIRE_THROWS_AS(lap::validate(prob), lap::DimensionException);
    }

    SECTION("supply vector of the wrong length") {
        lap::FlowProblem prob;
        prob.n_nodes = 3;
        prob.supply = {0, 0};
        REQUIRE_THROWS_AS(lap::validate(prob), lap::DimensionException);
    }

    SECTION("arc endpoint out of range") {
        lap::FlowProblem prob;
        prob.n_nodes = 2;
        prob.supply = {1, -1};
        prob.arcs.emplace_back(0, 7, 0, 1, 0.0);
        REQUIRE_THROWS_AS(lap::validate(prob), lap::DimensionException);

        lap::FlowProblem neg;
        neg.n_nodes = 2;
        neg.supply = {1, -1};
        neg.arcs.emplace_back(-1, 1, 0, 1, 0.0);
        REQUIRE_THROWS_AS(lap::validate(neg), lap::DimensionException);
    }

    SECTION("lower above upper") {
        lap::FlowProblem prob;
        prob.n_nodes = 2;
        prob.supply = {1, -1};
        prob.arcs.emplace_back(0, 1, 4, 2, 0.0);
        REQUIRE_THROWS_AS(lap::validate(prob), lap::DimensionException);
    }

    SECTION("negative lower bound") {
        lap::FlowProblem prob;
        prob.n_nodes = 2;
        prob.supply = {1, -1};
        prob.arcs.emplace_back(0, 1, -1, 2, 0.0);
        REQUIRE_THROWS_AS(lap::validate(prob), lap::DimensionException);
    }

    SECTION("non-finite arc cost") {
        lap::FlowProblem prob;
        prob.n_nodes = 2;
        prob.supply = {1, -1};
        prob.arcs.emplace_back(0, 1, 0, 2,
                               std::numeric_limits<double>::infinity());
        REQUIRE_THROWS_AS(lap::validate(prob), lap::DimensionException);
    }

    SECTION("warm potential of the wrong length") {
        lap::FlowProblem prob;
        prob.n_nodes = 2;
        prob.supply = {1, -1};
        prob.arcs.emplace_back(0, 1, 0, 1, 0.0);
        prob.warm_potential = {0.0, 0.0, 0.0};
        REQUIRE_THROWS_AS(lap::validate(prob), lap::DimensionException);
    }

    SECTION("warm flow of the wrong length on a blockless problem") {
        lap::FlowProblem prob;
        prob.n_nodes = 2;
        prob.supply = {1, -1};
        prob.arcs.emplace_back(0, 1, 0, 1, 0.0);
        prob.warm_flow = {0, 0, 0};
        REQUIRE_THROWS_AS(lap::validate(prob), lap::DimensionException);
    }

    SECTION("block without a cost source") {
        lap::FlowProblem prob;
        prob.n_nodes = 4;
        prob.supply.assign(4, 0);
        lap::BipartiteBlock blk;
        blk.row_base = 0;
        blk.col_base = 2;
        prob.blocks.push_back(blk);
        REQUIRE_THROWS_AS(lap::validate(prob), lap::DimensionException);
    }

    SECTION("block node range past the end") {
        lap::CostMatrix c = make_cost({{1, 2}, {3, 4}});
        lap::SourceOracle<lap::CostMatrix> oracle(c);
        lap::FlowProblem prob;
        prob.n_nodes = 3;
        prob.supply.assign(3, 0);
        lap::BipartiteBlock blk;
        blk.row_base = 0;
        blk.col_base = 2;  // needs nodes 2 and 3, only 3 nodes exist
        blk.costs = &oracle;
        prob.blocks.push_back(blk);
        REQUIRE_THROWS_AS(lap::validate(prob), lap::DimensionException);
    }

    SECTION("two blocks claiming the same nodes") {
        lap::CostMatrix c = make_cost({{1, 2}, {3, 4}});
        lap::SourceOracle<lap::CostMatrix> oracle(c);
        lap::FlowProblem prob;
        prob.n_nodes = 6;
        prob.supply.assign(6, 0);

        lap::BipartiteBlock first;
        first.row_base = 0;
        first.col_base = 2;
        first.costs = &oracle;
        lap::BipartiteBlock second;
        second.row_base = 3;  // overlaps the first block's column nodes 2..3
        second.col_base = 4;
        second.costs = &oracle;
        prob.blocks.push_back(first);
        prob.blocks.push_back(second);

        REQUIRE_THROWS_AS(lap::validate(prob), lap::DimensionException);
    }

    SECTION("a block's own rows and columns may not share nodes") {
        lap::CostMatrix c = make_cost({{1, 2}, {3, 4}});
        lap::SourceOracle<lap::CostMatrix> oracle(c);
        lap::FlowProblem prob;
        prob.n_nodes = 3;
        prob.supply.assign(3, 0);
        lap::BipartiteBlock blk;
        blk.row_base = 0;
        blk.col_base = 1;
        blk.costs = &oracle;
        prob.blocks.push_back(blk);
        REQUIRE_THROWS_AS(lap::validate(prob), lap::DimensionException);
    }
}

TEST_CASE("Flow validation accepts problems that are merely infeasible",
          "[flow][validate][infeasible]") {
    SECTION("more supply than any flow can place") {
        lap::CostMatrix c = make_cost({{1.0}, {2.0}});
        lap::SourceOracle<lap::CostMatrix> oracle(c);
        lap::FlowProblem prob = make_assignment(2, 1, &oracle, 2);

        REQUIRE_NOTHROW(lap::validate(prob));

        lap::FlowResult res = lap::solve_min_cost_flow(prob);
        REQUIRE(res.flow_required == 2);
        REQUIRE(res.flow_sent == 1);
        REQUIRE(res.status == "partial");
        REQUIRE(res.total_cost == Approx(1.0));
    }

    SECTION("nothing admissible at all") {
        lap::CostMatrix c = make_cost({{1.0, 2.0}, {3.0, 4.0}});
        c.forbid(0, 0); c.forbid(0, 1);
        c.forbid(1, 0); c.forbid(1, 1);
        lap::SourceOracle<lap::CostMatrix> oracle(c);
        lap::FlowProblem prob = make_assignment(2, 2, &oracle, 2);

        REQUIRE_NOTHROW(lap::validate(prob));

        lap::FlowResult res = lap::solve_min_cost_flow(prob);
        REQUIRE(res.flow_required == 2);
        REQUIRE(res.flow_sent == 0);
        REQUIRE(res.status == "infeasible");
    }

    SECTION("an iteration cap below what the problem needs") {
        lap::CostMatrix c = make_cost({{1.0, 9.0}, {9.0, 1.0}});
        lap::SourceOracle<lap::CostMatrix> oracle(c);
        lap::FlowProblem prob = make_assignment(2, 2, &oracle, 2);

        lap::FlowOptions opts;
        opts.max_augmentations = 1;
        lap::FlowResult res = lap::solve_min_cost_flow(prob, opts);

        REQUIRE(res.n_augmentations == 1);
        REQUIRE(res.flow_sent == 1);
        REQUIRE(res.status == "iteration_limit");
    }
}

// ---------------------------------------------------------------------------
// expansion
// ---------------------------------------------------------------------------

TEST_CASE("Block expansion skips forbidden pairs and maps (i, j) back",
          "[flow][expand]") {
    lap::CostMatrix c = make_cost({
        {1.0, 2.0, 3.0, 4.0},
        {5.0, 6.0, 7.0, 8.0},
        {9.0, 10.0, 11.0, 12.0}
    });
    c.forbid(0, 2);
    c.forbid(2, 0);
    c.forbid(2, 3);
    lap::SourceOracle<lap::CostMatrix> oracle(c);

    lap::FlowProblem prob = make_assignment(3, 4, &oracle, 3);
    const std::size_t n_structural = prob.arcs.size();  // 3 source + 4 sink

    lap::expand_blocks(prob);

    REQUIRE(prob.expanded);
    REQUIRE(prob.block_arcs.size() == 1u);
    const lap::BlockArcRange& range = prob.block_arcs[0];

    REQUIRE(range.first_arc == static_cast<int64_t>(n_structural));
    REQUIRE(range.n_arcs == 9);  // 12 pairs less the 3 forbidden ones
    REQUIRE(range.rc.size() == 9u);
    REQUIRE(prob.arcs.size() == n_structural + 9u);

    for (int64_t k = 0; k < range.n_arcs; ++k) {
        const auto rc = range.rc[static_cast<std::size_t>(k)];
        const lap::FlowArc& arc = prob.arcs[static_cast<std::size_t>(range.first_arc + k)];
        REQUIRE(c.allowed(rc.first, rc.second));
        REQUIRE(arc.tail == lap::FLOW_FIRST_ROW + rc.first);
        REQUIRE(arc.head == col_base(3) + rc.second);
        REQUIRE(arc.cost == Approx(c.at(rc.first, rc.second)));
        REQUIRE(arc.lower == 0);
        REQUIRE(arc.upper == 1);
    }

    SECTION("a second expansion changes nothing") {
        const std::vector<lap::FlowArc> before_arcs = prob.arcs;
        const int64_t before_first = prob.block_arcs[0].first_arc;
        const int64_t before_count = prob.block_arcs[0].n_arcs;

        lap::expand_blocks(prob);

        REQUIRE(prob.arcs.size() == before_arcs.size());
        REQUIRE(prob.block_arcs.size() == 1u);
        REQUIRE(prob.block_arcs[0].first_arc == before_first);
        REQUIRE(prob.block_arcs[0].n_arcs == before_count);
        for (std::size_t a = 0; a < before_arcs.size(); ++a) {
            REQUIRE(prob.arcs[a].tail == before_arcs[a].tail);
            REQUIRE(prob.arcs[a].head == before_arcs[a].head);
            REQUIRE(prob.arcs[a].cost == Approx(before_arcs[a].cost));
        }
    }
}

TEST_CASE("Block expansion refuses to allocate past its budget", "[flow][expand]") {
    // A cost source too large to expand, reported without touching memory.
    struct HugeOracle final : public lap::CostOracle {
        double  at(int64_t, int64_t) const override { return 0.0; }
        bool    allowed(int64_t, int64_t) const override { return true; }
        int64_t nrow() const override { return 1 << 20; }
        int64_t ncol() const override { return 1 << 20; }
    } huge;

    lap::FlowProblem prob;
    prob.n_nodes = 4;
    prob.supply.assign(4, 0);
    lap::BipartiteBlock blk;
    blk.row_base = 0;
    blk.col_base = 2;
    blk.costs = &huge;
    prob.blocks.push_back(blk);

    REQUIRE_THROWS_AS(lap::expand_blocks(prob), lap::DimensionException);
}

// ---------------------------------------------------------------------------
// expansion over a candidate set, and arcs added afterwards
// ---------------------------------------------------------------------------

TEST_CASE("Subset expansion emits the candidate pairs and nothing else",
          "[flow][expand][subset]") {
    lap::CostMatrix c = make_cost({
        {1.0, 2.0, 3.0, 4.0},
        {5.0, 6.0, 7.0, 8.0},
        {9.0, 10.0, 11.0, 12.0}
    });
    c.forbid(1, 1);
    lap::SourceOracle<lap::CostMatrix> oracle(c);

    // (1, 1) is a candidate the source forbids, which is what separates the two
    // objects: it is in the set and it gets no arc.
    lap::CandidateSet cand(3, 4);
    cand.add_pairs({{0, 3}, {0, 0}, {1, 1}, {1, 2}, {2, 0}});
    REQUIRE(cand.n_arcs() == 5);

    lap::FlowProblem prob = make_assignment(3, 4, &oracle, 3);
    const std::size_t n_structural = prob.arcs.size();

    lap::expand_block_subset(prob, 0, cand);

    REQUIRE(prob.expanded);
    REQUIRE(prob.block_arcs.size() == 1u);
    REQUIRE(prob.block_arcs[0].first_arc == static_cast<int64_t>(n_structural));
    REQUIRE(prob.block_arcs[0].n_arcs == 4);
    REQUIRE(prob.arcs.size() == n_structural + 4u);
    require_block_aligned(prob, 0, c);

    // Row-major, ascending within a row, which is the order the full expansion
    // emits too.
    const std::vector<std::pair<int32_t, int32_t>> expected = {
        {0, 0}, {0, 3}, {1, 2}, {2, 0}
    };
    REQUIRE(prob.block_arcs[0].rc == expected);
}

TEST_CASE("A restricted master solves the problem its arcs describe",
          "[flow][expand][subset]") {
    const lap::CostMatrix c = random_cost(6, 8, 4242u);

    lap::CandidateSet cand(6, 8);
    std::vector<lap::CandidateSet::Pair> seed;
    for (int32_t i = 0; i < 6; ++i) {
        seed.emplace_back(i, i);
        seed.emplace_back(i, (i + 3) % 8);
        seed.emplace_back(i, (i + 5) % 8);
    }
    cand.add_pairs(seed);

    lap::SourceOracle<lap::CostMatrix> oracle(c);
    lap::FlowProblem prob = make_assignment(6, 8, &oracle, 6);
    lap::expand_block_subset(prob, 0, cand);
    const lap::FlowResult res = lap::solve_min_cost_flow(prob);
    REQUIRE(res.status == "optimal");

    // The same problem written out densely: every pair outside the candidate
    // set forbidden, expanded in full.
    const lap::CostMatrix r = restricted_to(c, cand);
    lap::SourceOracle<lap::CostMatrix> r_oracle(r);
    lap::FlowProblem dense = make_assignment(6, 8, &r_oracle, 6);
    const lap::FlowResult dense_res = lap::solve_min_cost_flow(dense);
    REQUIRE(dense_res.status == "optimal");

    REQUIRE(prob.block_arcs[0].n_arcs == dense.block_arcs[0].n_arcs);
    REQUIRE(res.total_cost == Approx(dense_res.total_cost).margin(1e-12));
    REQUIRE(matching_of(prob, res, 6) == matching_of(dense, dense_res, 6));

    const lap::AssignmentDuals duals = flow_duals(prob, res);
    REQUIRE(duals.ok());
    const lap::CertificateReport rep =
        lap::certify_assignment(r, duals.match, duals.u, duals.v, 1e-9);
    REQUIRE(rep.certified_optimal);
}

TEST_CASE("Growing the candidate set to the whole grid reaches the dense optimum",
          "[flow][expand][subset][warm]") {
    lap::CostMatrix c = random_cost(7, 10, 777u);
    c.forbid(2, 5);
    lap::SourceOracle<lap::CostMatrix> oracle(c);

    lap::CandidateSet cand(7, 10);
    std::vector<lap::CandidateSet::Pair> seed;
    for (int32_t i = 0; i < 7; ++i) {
        seed.emplace_back(i, i);
        seed.emplace_back(i, (i + 4) % 10);
    }
    cand.add_pairs(seed);

    lap::FlowProblem prob = make_assignment(7, 10, &oracle, 7);
    lap::expand_block_subset(prob, 0, cand);
    const lap::FlowResult first = lap::solve_min_cost_flow(prob);
    REQUIRE(first.status == "optimal");
    require_block_aligned(prob, 0, c);

    // Warm start from the restricted optimum, then price in every remaining
    // pair at once.
    prob.warm_flow      = first.flow;
    prob.warm_potential = first.potential;

    const std::vector<lap::CandidateSet::Pair> fresh = cand.add_pairs(every_pair(7, 10));
    REQUIRE(static_cast<int64_t>(fresh.size()) == 70 - 14);

    const int64_t added = lap::add_block_arcs(prob, 0, fresh);
    // Every fresh pair became an arc except the one the source forbids.
    REQUIRE(added == static_cast<int64_t>(fresh.size()) - 1);
    REQUIRE(prob.warm_flow.size() == prob.arcs.size());
    REQUIRE(prob.block_arcs[0].n_arcs == 69);
    require_block_aligned(prob, 0, c);

    const lap::FlowResult grown = lap::solve_min_cost_flow(prob);
    REQUIRE(grown.status == "optimal");

    lap::FlowProblem cold_prob = make_assignment(7, 10, &oracle, 7);
    const lap::FlowResult cold = lap::solve_min_cost_flow(cold_prob);
    REQUIRE(cold.status == "optimal");

    REQUIRE(grown.total_cost == Approx(cold.total_cost).margin(1e-12));
    REQUIRE(matching_of(prob, grown, 7) == matching_of(cold_prob, cold, 7));
    // The restricted optimum is over a subset of the arcs, so it cannot be
    // cheaper, and on this instance it is strictly worse.
    REQUIRE(first.total_cost > cold.total_cost);
}

TEST_CASE("Arcs added to one block move the blocks behind it",
          "[flow][expand][subset]") {
    const lap::CostMatrix a = random_cost(4, 5, 8080u);
    const lap::CostMatrix b = random_cost(3, 4, 8081u);
    lap::SourceOracle<lap::CostMatrix> a_oracle(a);
    lap::SourceOracle<lap::CostMatrix> b_oracle(b);

    lap::CandidateSet a_cand(4, 5);
    for (int32_t i = 0; i < 4; ++i) a_cand.add_pairs({{i, i}, {i, (i + 2) % 5}});
    lap::CandidateSet b_cand(3, 4);
    for (int32_t i = 0; i < 3; ++i) b_cand.add_pairs({{i, i}, {i, (i + 1) % 4}});

    lap::FlowProblem prob = make_two_assignments(4, 5, &a_oracle, 3, 4, &b_oracle);
    lap::expand_block_subset(prob, 0, a_cand);
    REQUIRE_FALSE(prob.expanded);  // block 1 is still implicit
    lap::expand_block_subset(prob, 1, b_cand);
    REQUIRE(prob.expanded);

    const int64_t b_first_before = prob.block_arcs[1].first_arc;
    const int64_t b_count        = prob.block_arcs[1].n_arcs;
    const std::vector<std::pair<int32_t, int32_t>> b_rc_before = prob.block_arcs[1].rc;

    const lap::FlowResult before = lap::solve_min_cost_flow(prob);
    REQUIRE(before.status == "optimal");
    prob.warm_flow      = before.flow;
    prob.warm_potential = before.potential;

    const std::vector<lap::CandidateSet::Pair> fresh =
        a_cand.add_pairs(every_pair(4, 5));
    const int64_t added = lap::add_block_arcs(prob, 0, fresh);
    REQUIRE(added == static_cast<int64_t>(fresh.size()));

    REQUIRE(prob.warm_flow.size() == prob.arcs.size());
    REQUIRE(prob.block_arcs[1].first_arc == b_first_before + added);
    REQUIRE(prob.block_arcs[1].n_arcs == b_count);
    REQUIRE(prob.block_arcs[1].rc == b_rc_before);
    require_block_aligned(prob, 0, a);
    require_block_aligned(prob, 1, b);

    const lap::FlowResult grown = lap::solve_min_cost_flow(prob);
    REQUIRE(grown.status == "optimal");

    // Block 0 now holds every pair and block 1 still holds its candidates, so
    // the answer is the two problems solved separately.
    lap::FlowProblem a_full = make_assignment(4, 5, &a_oracle, 4);
    const lap::FlowResult a_res = lap::solve_min_cost_flow(a_full);
    const lap::CostMatrix b_restricted = restricted_to(b, b_cand);
    lap::SourceOracle<lap::CostMatrix> b_r_oracle(b_restricted);
    lap::FlowProblem b_full = make_assignment(3, 4, &b_r_oracle, 3);
    const lap::FlowResult b_res = lap::solve_min_cost_flow(b_full);

    REQUIRE(grown.total_cost ==
            Approx(a_res.total_cost + b_res.total_cost).margin(1e-12));
}

TEST_CASE("A block grows once its arcs are in, and not before",
          "[flow][expand][subset]") {
    const lap::CostMatrix c = random_cost(3, 4, 1234u);
    lap::SourceOracle<lap::CostMatrix> oracle(c);
    lap::CandidateSet cand(3, 4);
    cand.add_pairs({{0, 0}, {1, 1}, {2, 2}});

    SECTION("adding arcs before the expansion is refused") {
        lap::FlowProblem prob = make_assignment(3, 4, &oracle, 3);
        REQUIRE_THROWS_AS(lap::add_block_arcs(prob, 0, {{0, 1}}),
                          lap::DimensionException);
    }

    SECTION("a second subset expansion of the same block is refused") {
        lap::FlowProblem prob = make_assignment(3, 4, &oracle, 3);
        lap::expand_block_subset(prob, 0, cand);
        REQUIRE_THROWS_AS(lap::expand_block_subset(prob, 0, cand),
                          lap::DimensionException);
    }

    SECTION("blocks expand in index order") {
        const lap::CostMatrix other = random_cost(3, 4, 1235u);
        lap::SourceOracle<lap::CostMatrix> other_oracle(other);
        lap::FlowProblem prob =
            make_two_assignments(3, 4, &oracle, 3, 4, &other_oracle);
        REQUIRE_THROWS_AS(lap::expand_block_subset(prob, 1, cand),
                          lap::DimensionException);
    }

    SECTION("a candidate set of the wrong shape is refused") {
        lap::FlowProblem prob = make_assignment(3, 4, &oracle, 3);
        lap::CandidateSet wrong(3, 5);
        REQUIRE_THROWS_AS(lap::expand_block_subset(prob, 0, wrong),
                          lap::DimensionException);
    }

    SECTION("a pair outside the block is refused") {
        lap::FlowProblem prob = make_assignment(3, 4, &oracle, 3);
        lap::expand_block_subset(prob, 0, cand);
        REQUIRE_THROWS_AS(lap::add_block_arcs(prob, 0, {{3, 0}}),
                          lap::DimensionException);
        REQUIRE_THROWS_AS(lap::add_block_arcs(prob, 0, {{0, 4}}),
                          lap::DimensionException);
        REQUIRE_THROWS_AS(lap::add_block_arcs(prob, 1, {{0, 0}}),
                          lap::DimensionException);
    }

    SECTION("a warm start that is already stale is named as stale") {
        lap::FlowProblem prob = make_assignment(3, 4, &oracle, 3);
        lap::expand_block_subset(prob, 0, cand);
        prob.warm_flow.assign(prob.arcs.size() - 1u, 0);
        REQUIRE_THROWS_AS(lap::add_block_arcs(prob, 0, {{0, 1}}),
                          lap::DimensionException);
    }

    SECTION("a cold problem gains arcs without gaining a warm flow") {
        lap::FlowProblem prob = make_assignment(3, 4, &oracle, 3);
        lap::expand_block_subset(prob, 0, cand);
        REQUIRE(lap::add_block_arcs(prob, 0, {{0, 1}, {2, 3}}) == 2);
        REQUIRE(prob.warm_flow.empty());
        REQUIRE(prob.block_arcs[0].n_arcs == 5);
        require_block_aligned(prob, 0, c);
    }
}

TEST_CASE("A partly expanded problem finishes its remaining blocks in full",
          "[flow][expand][subset]") {
    const lap::CostMatrix a = random_cost(4, 5, 606u);
    const lap::CostMatrix b = random_cost(3, 4, 607u);
    lap::SourceOracle<lap::CostMatrix> a_oracle(a);
    lap::SourceOracle<lap::CostMatrix> b_oracle(b);

    lap::CandidateSet a_cand(4, 5);
    for (int32_t i = 0; i < 4; ++i) a_cand.add_pairs({{i, i}, {i, (i + 2) % 5}});

    lap::FlowProblem prob = make_two_assignments(4, 5, &a_oracle, 3, 4, &b_oracle);
    lap::expand_block_subset(prob, 0, a_cand);
    const int64_t a_count = prob.block_arcs[0].n_arcs;

    // The solver expands what is left, and leaves the restricted block alone.
    const lap::FlowResult res = lap::solve_min_cost_flow(prob);
    REQUIRE(res.status == "optimal");
    REQUIRE(prob.expanded);
    REQUIRE(prob.block_arcs.size() == 2u);
    REQUIRE(prob.block_arcs[0].n_arcs == a_count);
    REQUIRE(prob.block_arcs[1].n_arcs == 12);
    require_block_aligned(prob, 0, a);
    require_block_aligned(prob, 1, b);
}

// ---------------------------------------------------------------------------
// a solve whose optimum and potentials are known by inspection
// ---------------------------------------------------------------------------

TEST_CASE("Hand-computed 2x2 assignment, optimum and potentials",
          "[flow][solve][hand]") {
    // Costs [[1, 5], [3, 4]]. The two perfect matchings cost 1 + 4 = 5 and
    // 5 + 3 = 8, so the optimum pairs row 0 with column 0 and row 1 with
    // column 1.
    lap::CostMatrix c = make_cost({{1.0, 5.0}, {3.0, 4.0}});
    lap::SourceOracle<lap::CostMatrix> oracle(c);
    lap::FlowProblem prob = make_assignment(2, 2, &oracle, 2);

    lap::FlowResult res = lap::solve_min_cost_flow(prob);

    REQUIRE(res.status == "optimal");
    REQUIRE(res.flow_required == 2);
    REQUIRE(res.flow_sent == 2);
    REQUIRE(res.n_augmentations == 2);
    REQUIRE(res.total_cost == Approx(5.0));

    const lap::AssignmentDuals duals = flow_duals(prob, res);
    REQUIRE(duals.ok());
    REQUIRE(duals.match[0] == 0);
    REQUIRE(duals.match[1] == 1);

    // The first search prices the columns at their cheapest incoming arc, so
    // pi lands on 1 for column 0 and 4 for column 1 and the sink follows the
    // cheaper of the two. The second search pays 3 more to route row 1 through
    // column 1, which is what lifts the sink to 4 and row 0 to 2.
    REQUIRE(res.potential.size() == 6u);
    REQUIRE(res.potential[0] == Approx(0.0));  // source, the fixed gauge
    REQUIRE(res.potential[1] == Approx(4.0));  // sink
    REQUIRE(res.potential[2] == Approx(2.0));  // row 0
    REQUIRE(res.potential[3] == Approx(0.0));  // row 1
    REQUIRE(res.potential[4] == Approx(3.0));  // column 0
    REQUIRE(res.potential[5] == Approx(4.0));  // column 1

    REQUIRE(duals.u[0] == Approx(2.0));
    REQUIRE(duals.u[1] == Approx(4.0));
    REQUIRE(duals.v[0] == Approx(-1.0));
    REQUIRE(duals.v[1] == Approx(0.0));

    const lap::CertificateReport rep =
        lap::certify_assignment(c, duals.match, duals.u, duals.v, 1e-9);
    REQUIRE(rep.certified_optimal);
}

// ---------------------------------------------------------------------------
// lower bounds
// ---------------------------------------------------------------------------

TEST_CASE("A lower bound forces a strictly worse optimum", "[flow][solve][lower]") {
    // Nodes: 0 source, 1 sink, 2 and 3 two routes between them. One unit has to
    // travel; route a costs 1 and route b costs 10.
    auto build = [](int64_t lower_on_b) {
        lap::FlowProblem prob;
        prob.n_nodes = 4;
        prob.supply = {1, -1, 0, 0};
        prob.arcs.emplace_back(0, 2, 0, 1, 0.0);            // source -> a
        prob.arcs.emplace_back(0, 3, lower_on_b, 1, 0.0);   // source -> b
        prob.arcs.emplace_back(2, 1, 0, 1, 1.0);            // a -> sink
        prob.arcs.emplace_back(3, 1, 0, 1, 10.0);           // b -> sink
        return prob;
    };

    SECTION("unconstrained takes the cheap route") {
        lap::FlowProblem prob = build(0);
        lap::FlowResult res = lap::solve_min_cost_flow(prob);

        REQUIRE(res.status == "optimal");
        REQUIRE(res.total_cost == Approx(1.0));
        REQUIRE(res.flow[0] == 1);
        REQUIRE(res.flow[1] == 0);
        REQUIRE(res.flow[2] == 1);
        REQUIRE(res.flow[3] == 0);
    }

    SECTION("a lower bound of one on the expensive route") {
        lap::FlowProblem prob = build(1);
        lap::FlowResult res = lap::solve_min_cost_flow(prob);

        // The mandatory unit uses up the source's whole supply, so the cheap
        // route carries nothing and the answer costs ten times as much.
        REQUIRE(res.status == "optimal");
        REQUIRE(res.flow_required == 1);
        REQUIRE(res.flow_sent == 1);
        REQUIRE(res.total_cost == Approx(10.0));
        REQUIRE(res.flow[0] == 0);
        REQUIRE(res.flow[1] == 1);
        REQUIRE(res.flow[2] == 0);
        REQUIRE(res.flow[3] == 1);
    }
}

TEST_CASE("Minimum group sizes drive the answer away from the free optimum",
          "[flow][solve][lower]") {
    // Two group centres and two units. Centre 0 is close to both units, centre 1
    // is far from both. Left alone, centre 0 takes both units for 1 + 2 = 3.
    // Requiring every centre to hold at least one unit forces centre 1 to take
    // one, and the cheaper split gives centre 0 the unit costing 2 and centre 1
    // the unit costing 50.
    //
    // Nodes: 0 source, 1 sink, 2 and 3 centres, 4 and 5 units.
    auto build = [](int64_t min_per_centre) {
        lap::FlowProblem prob;
        prob.n_nodes = 6;
        prob.supply = {2, -2, 0, 0, 0, 0};
        prob.arcs.emplace_back(0, 2, min_per_centre, 2, 0.0);
        prob.arcs.emplace_back(0, 3, min_per_centre, 2, 0.0);
        prob.arcs.emplace_back(2, 4, 0, 1, 1.0);
        prob.arcs.emplace_back(2, 5, 0, 1, 2.0);
        prob.arcs.emplace_back(3, 4, 0, 1, 50.0);
        prob.arcs.emplace_back(3, 5, 0, 1, 60.0);
        prob.arcs.emplace_back(4, 1, 0, 1, 0.0);
        prob.arcs.emplace_back(5, 1, 0, 1, 0.0);
        return prob;
    };

    SECTION("no minimum") {
        lap::FlowProblem prob = build(0);
        lap::FlowResult res = lap::solve_min_cost_flow(prob);
        REQUIRE(res.status == "optimal");
        REQUIRE(res.total_cost == Approx(3.0));
        REQUIRE(res.flow[0] == 2);
        REQUIRE(res.flow[1] == 0);
    }

    SECTION("at least one unit per centre") {
        lap::FlowProblem prob = build(1);
        lap::FlowResult res = lap::solve_min_cost_flow(prob);

        REQUIRE(res.status == "optimal");
        REQUIRE(res.total_cost == Approx(52.0));
        REQUIRE(res.flow[0] == 1);  // source -> centre 0
        REQUIRE(res.flow[1] == 1);  // source -> centre 1
        REQUIRE(res.flow[3] == 1);  // centre 0 -> unit 1, costing 2
        REQUIRE(res.flow[4] == 1);  // centre 1 -> unit 0, costing 50
    }
}

// ---------------------------------------------------------------------------
// agreement with the assignment solvers
// ---------------------------------------------------------------------------

TEST_CASE("Flow optimum agrees with Jonker-Volgenant", "[flow][solve][agree]") {
    struct Shape { int64_t nr; int64_t nc; };
    const std::vector<Shape> shapes = {
        {1, 1}, {3, 3}, {5, 5}, {8, 8},
        {3, 7}, {2, 9}, {6, 11},
        {7, 3}, {9, 2}, {11, 6}
    };

    uint32_t seed = 20260810u;
    for (const Shape& shape : shapes) {
        for (int rep = 0; rep < 4; ++rep) {
            const lap::CostMatrix c = random_cost(shape.nr, shape.nc, seed++);
            const int64_t n_match = std::min(shape.nr, shape.nc);

            lap::SourceOracle<lap::CostMatrix> oracle(c);
            lap::FlowProblem prob = make_assignment(shape.nr, shape.nc, &oracle, n_match);
            const lap::FlowResult res = lap::solve_min_cost_flow(prob);

            INFO("shape " << shape.nr << "x" << shape.nc << " seed " << (seed - 1));
            REQUIRE(res.status == "optimal");

            // Jonker-Volgenant assigns every row, so a problem with more rows
            // than columns is put to it the only way it accepts: transposed.
            // Both then match min(nr, nc) pairs and the optimum is the same set
            // of pairs read the other way round.
            const lap::CostMatrix jv_input =
                (shape.nr <= shape.nc) ? c : transpose_of(c);
            const lap::LapResult jv = lap::solve_jv(jv_input, false);
            REQUIRE(res.total_cost == Approx(jv.total_cost).margin(1e-9));
        }
    }
}

TEST_CASE("Flow optimum agrees with Jonker-Volgenant on a sparse instance",
          "[flow][solve][agree]") {
    lap::CostMatrix c = random_cost(4, 6, 77u);
    // Forbidding a diagonal leaves every row several admissible columns, so a
    // complete matching still exists and both solvers have to avoid the same
    // cells.
    for (int64_t i = 0; i < 4; ++i) c.forbid(i, i);

    lap::SourceOracle<lap::CostMatrix> oracle(c);
    lap::FlowProblem prob = make_assignment(4, 6, &oracle, 4);
    const lap::FlowResult res = lap::solve_min_cost_flow(prob);

    REQUIRE(res.status == "optimal");
    const lap::LapResult jv = lap::solve_jv(c, false);
    REQUIRE(res.total_cost == Approx(jv.total_cost).margin(1e-9));

    const std::vector<int> match = matching_of(prob, res, 4);
    for (int i = 0; i < 4; ++i) REQUIRE(match[i] != i);
}

// ---------------------------------------------------------------------------
// potentials as certified assignment duals
// ---------------------------------------------------------------------------

TEST_CASE("Flow potentials certify the assignment they came from",
          "[flow][solve][certify]") {
    struct Shape { int64_t nr; int64_t nc; };
    const std::vector<Shape> shapes = {
        {1, 1}, {4, 4}, {7, 7},
        {1, 6}, {3, 8}, {5, 12}, {2, 15}
    };

    uint32_t seed = 424242u;
    for (const Shape& shape : shapes) {
        for (int rep = 0; rep < 4; ++rep) {
            const lap::CostMatrix c = random_cost(shape.nr, shape.nc, seed++);

            lap::SourceOracle<lap::CostMatrix> oracle(c);
            lap::FlowProblem prob = make_assignment(shape.nr, shape.nc, &oracle, shape.nr);
            const lap::FlowResult res = lap::solve_min_cost_flow(prob);

            INFO("shape " << shape.nr << "x" << shape.nc << " seed " << (seed - 1));
            REQUIRE(res.status == "optimal");
            REQUIRE(res.potential.size() ==
                    static_cast<std::size_t>(2 + shape.nr + shape.nc));
            REQUIRE(res.potential[0] == Approx(0.0));

            const lap::AssignmentDuals duals = flow_duals(prob, res);
            REQUIRE(duals.ok());

            const lap::CertificateReport rep_out =
                lap::certify_assignment(c, duals.match, duals.u, duals.v, 1e-9);

            INFO("min reduced cost " << rep_out.min_reduced_cost
                 << " matched slack " << rep_out.max_matched_slack
                 << " free column dual " << rep_out.max_v_unmatched
                 << " gap " << rep_out.duality_gap);
            REQUIRE(rep_out.primal_feasible);
            REQUIRE(rep_out.dual_feasible);
            REQUIRE(rep_out.cs_matched_tight);
            REQUIRE(rep_out.cs_unmatched_free);
            REQUIRE(rep_out.certified_optimal);
        }
    }
}

namespace {

// Every arc that can still take flow prices at or above zero, and every arc
// carrying more than its lower bound prices at or below zero. That is the
// invariant successive shortest paths maintains, stated over the whole arc set
// rather than over the part the search happened to walk.
void require_reduced_costs_consistent(const lap::FlowProblem& prob,
                                      const lap::FlowResult& res, double tol) {
    for (std::size_t a = 0; a < prob.arcs.size(); ++a) {
        const lap::FlowArc& arc = prob.arcs[a];
        const int64_t fa = res.flow[a];
        const double cbar = arc.cost +
                            res.potential[static_cast<std::size_t>(arc.tail)] -
                            res.potential[static_cast<std::size_t>(arc.head)];
        INFO("arc " << a << " " << arc.tail << "->" << arc.head
             << " flow " << fa << " in [" << arc.lower << ", " << arc.upper
             << "] cbar " << cbar);
        if (fa < arc.upper) REQUIRE(cbar >= -tol);
        if (fa > arc.lower) REQUIRE(cbar <= tol);
    }
}

}  // namespace

TEST_CASE("Reduced costs stay consistent on arcs the search never reached",
          "[flow][solve][certify]") {
    // A column no admissible pair can reach is never labelled by any search, so
    // its potential only moves if unreached nodes are carried along with the
    // reached ones. Left behind, its arc into the sink still has capacity and
    // ends up priced below zero, which is a solved flow the certificate cannot
    // accept even though the flow itself is optimal.
    SECTION("a column every row forbids") {
        lap::CostMatrix c = make_cost({
            {4.0, 1.0, 2.0},
            {7.0, 6.0, 3.0}
        });
        c.forbid(0, 1);
        c.forbid(1, 0);
        c.forbid(1, 1);
        lap::SourceOracle<lap::CostMatrix> oracle(c);

        lap::FlowProblem prob = make_assignment(2, 3, &oracle, 2);
        const lap::FlowResult res = lap::solve_min_cost_flow(prob);

        REQUIRE(res.status == "optimal");
        REQUIRE(res.total_cost == Approx(7.0));

        const lap::AssignmentDuals duals = flow_duals(prob, res);
        REQUIRE(duals.ok());
        REQUIRE(duals.match[0] == 0);
        REQUIRE(duals.match[1] == 2);

        // Column 1 is the unreachable one, and it has to end up priced at or
        // above the sink for its residual arc into the sink to stay admissible.
        REQUIRE(res.potential[static_cast<std::size_t>(col_base(2) + 1)] >=
                res.potential[static_cast<std::size_t>(lap::FLOW_SINK)] - 1e-9);
        require_reduced_costs_consistent(prob, res, 1e-9);

        REQUIRE(lap::certify_assignment(c, duals.match, duals.u, duals.v, 1e-9)
                    .certified_optimal);
    }

    SECTION("randomized instances with whole columns forbidden") {
        uint32_t seed = 4242u;
        for (int rep = 0; rep < 12; ++rep) {
            lap::CostMatrix c = random_cost(4, 9, seed++);
            for (int64_t j = 1; j < 9; j += 3) {
                for (int64_t i = 0; i < 4; ++i) c.forbid(i, j);
            }
            lap::SourceOracle<lap::CostMatrix> oracle(c);
            lap::FlowProblem prob = make_assignment(4, 9, &oracle, 4);
            const lap::FlowResult res = lap::solve_min_cost_flow(prob);

            INFO("rep " << rep << " seed " << (seed - 1));
            REQUIRE(res.status == "optimal");
            require_reduced_costs_consistent(prob, res, 1e-9);

            const lap::AssignmentDuals duals = flow_duals(prob, res);
            REQUIRE(duals.ok());
            REQUIRE(lap::certify_assignment(c, duals.match, duals.u, duals.v, 1e-9)
                        .certified_optimal);
        }
    }

    SECTION("a lower-bounded network with a dead-end node") {
        // Node 4 has an arc into the sink and nothing feeding it.
        lap::FlowProblem prob;
        prob.n_nodes = 5;
        prob.supply = {2, -2, 0, 0, 0};
        prob.arcs.emplace_back(0, 2, 1, 2, 3.0);
        prob.arcs.emplace_back(0, 3, 0, 2, 1.0);
        prob.arcs.emplace_back(2, 1, 0, 2, 0.0);
        prob.arcs.emplace_back(3, 1, 0, 2, 0.0);
        prob.arcs.emplace_back(4, 1, 0, 2, 0.0);

        const lap::FlowResult res = lap::solve_min_cost_flow(prob);
        REQUIRE(res.status == "optimal");
        REQUIRE(res.total_cost == Approx(4.0));  // one unit forced at 3, one at 1
        require_reduced_costs_consistent(prob, res, 1e-9);
    }
}

TEST_CASE("The sink prices used and free columns on opposite sides",
          "[flow][solve][certify]") {
    // What makes the clamp on v in lap::map_assignment_duals() exact rather
    // than a repair. The arc column -> sink is saturated on every column the
    // flow uses, so its reverse is residual and prices pi[column] at or below
    // pi[sink]; it keeps residual capacity on every column left free, which
    // prices pi[column] at or above pi[sink]. Neither side can cross.
    uint32_t seed = 6060u;
    for (int64_t nc = 6; nc <= 14; nc += 2) {
        const int64_t nr = 4;
        const lap::CostMatrix c = random_cost(nr, nc, seed++);
        lap::SourceOracle<lap::CostMatrix> oracle(c);
        lap::FlowProblem prob = make_assignment(nr, nc, &oracle, nr);
        const lap::FlowResult res = lap::solve_min_cost_flow(prob);
        REQUIRE(res.status == "optimal");

        const std::vector<int> match = matching_of(prob, res, nr);
        std::vector<bool> used(static_cast<std::size_t>(nc), false);
        for (int64_t i = 0; i < nr; ++i) {
            used[static_cast<std::size_t>(match[static_cast<std::size_t>(i)])] = true;
        }

        const double pi_sink = res.potential[static_cast<std::size_t>(lap::FLOW_SINK)];
        bool saw_free_above_sink = false;
        for (int64_t j = 0; j < nc; ++j) {
            const double pi_col =
                res.potential[static_cast<std::size_t>(col_base(nr) + j)];
            INFO("nc " << nc << " column " << j << " pi " << pi_col
                 << " sink " << pi_sink);
            if (used[static_cast<std::size_t>(j)]) {
                REQUIRE(pi_col <= pi_sink + 1e-9);
            } else {
                REQUIRE(pi_col >= pi_sink - 1e-9);
                if (pi_col > pi_sink + 1e-9) saw_free_above_sink = true;
            }
        }
        // The clamp is doing work on these instances rather than reading as a
        // no-op: at least one free column is priced strictly above the sink.
        REQUIRE(saw_free_above_sink);
    }
}

TEST_CASE("Flow potentials certify a sparse wide assignment",
          "[flow][solve][certify]") {
    lap::CostMatrix c = random_cost(5, 9, 909u);
    for (int64_t i = 0; i < 5; ++i) {
        c.forbid(i, i);
        c.forbid(i, (i + 1) % 9);
    }

    lap::SourceOracle<lap::CostMatrix> oracle(c);
    lap::FlowProblem prob = make_assignment(5, 9, &oracle, 5);
    const lap::FlowResult res = lap::solve_min_cost_flow(prob);
    REQUIRE(res.status == "optimal");

    const lap::AssignmentDuals duals = flow_duals(prob, res);
    REQUIRE(duals.ok());

    const lap::CertificateReport rep =
        lap::certify_assignment(c, duals.match, duals.u, duals.v, 1e-9);
    REQUIRE(rep.certified_optimal);
}

// ---------------------------------------------------------------------------
// warm start
// ---------------------------------------------------------------------------

TEST_CASE("A warm start reproduces the cold answer after a cost perturbation",
          "[flow][solve][warm]") {
    const std::vector<double> deltas = {-14.0, -3.0, 3.0, 25.0};

    uint32_t seed = 5150u;
    for (int64_t n = 3; n <= 7; ++n) {
        for (const double delta : deltas) {
            const lap::CostMatrix c = random_cost(n, n + 2, seed++);
            lap::SourceOracle<lap::CostMatrix> oracle(c);

            lap::FlowProblem base = make_assignment(n, n + 2, &oracle, n);
            const lap::FlowResult cold = lap::solve_min_cost_flow(base);
            REQUIRE(cold.status == "optimal");

            // The arcs are expanded now, so the perturbation and the warm flow
            // address the same arc array.
            const int64_t block_first = base.block_arcs[0].first_arc;
            const int64_t target = block_first + base.block_arcs[0].n_arcs / 2;

            lap::FlowProblem perturbed = base;
            perturbed.arcs[static_cast<std::size_t>(target)].cost += delta;

            lap::FlowProblem warm = perturbed;
            warm.warm_flow = cold.flow;
            warm.warm_potential = cold.potential;

            const lap::FlowResult reference = lap::solve_min_cost_flow(perturbed);
            const lap::FlowResult restarted = lap::solve_min_cost_flow(warm);

            INFO("n " << n << " delta " << delta << " seed " << (seed - 1));
            REQUIRE(restarted.status == reference.status);
            REQUIRE(restarted.total_cost == Approx(reference.total_cost).margin(1e-9));
            REQUIRE(restarted.flow == reference.flow);

            // flow_sent counts what had to move from the starting flow, so the
            // warm solve places only what the slackness repair displaced while
            // the cold one places everything.
            REQUIRE(restarted.flow_sent <= reference.flow_sent);
            REQUIRE(restarted.n_augmentations < reference.n_augmentations);
        }
    }
}

TEST_CASE("A warm start on the unchanged problem needs no augmentation",
          "[flow][solve][warm]") {
    const lap::CostMatrix c = random_cost(6, 6, 31337u);
    lap::SourceOracle<lap::CostMatrix> oracle(c);

    lap::FlowProblem base = make_assignment(6, 6, &oracle, 6);
    const lap::FlowResult cold = lap::solve_min_cost_flow(base);
    REQUIRE(cold.status == "optimal");
    REQUIRE(cold.n_augmentations == 6);

    lap::FlowProblem warm = base;
    warm.warm_flow = cold.flow;
    warm.warm_potential = cold.potential;
    const lap::FlowResult restarted = lap::solve_min_cost_flow(warm);

    REQUIRE(restarted.status == "optimal");
    REQUIRE(restarted.n_augmentations == 0);
    REQUIRE(restarted.flow_required == 0);
    REQUIRE(restarted.total_cost == Approx(cold.total_cost).margin(1e-9));
    REQUIRE(restarted.flow == cold.flow);
}

TEST_CASE("A warm flow outside the arc bounds is rejected", "[flow][solve][warm]") {
    lap::FlowProblem prob;
    prob.n_nodes = 2;
    prob.supply = {1, -1};
    prob.arcs.emplace_back(0, 1, 0, 1, 1.0);
    prob.warm_flow = {7};
    REQUIRE_THROWS_AS(lap::solve_min_cost_flow(prob), lap::DimensionException);
}

// ---------------------------------------------------------------------------
// degenerate shapes
// ---------------------------------------------------------------------------

TEST_CASE("Empty and zero-demand problems solve trivially", "[flow][solve][edge]") {
    SECTION("no nodes at all") {
        lap::FlowProblem prob;
        lap::FlowResult res = lap::solve_min_cost_flow(prob);
        REQUIRE(res.status == "optimal");
        REQUIRE(res.flow.empty());
        REQUIRE(res.potential.empty());
        REQUIRE(res.total_cost == Approx(0.0));
    }

    SECTION("nodes but nothing to move") {
        lap::FlowProblem prob;
        prob.n_nodes = 3;
        prob.supply.assign(3, 0);
        prob.arcs.emplace_back(0, 1, 0, 4, 2.0);
        prob.arcs.emplace_back(1, 2, 0, 4, 3.0);

        lap::FlowResult res = lap::solve_min_cost_flow(prob);
        REQUIRE(res.status == "optimal");
        REQUIRE(res.flow_required == 0);
        REQUIRE(res.flow_sent == 0);
        REQUIRE(res.total_cost == Approx(0.0));
        REQUIRE(res.potential.size() == 3u);
        REQUIRE(res.potential[0] == Approx(0.0));
    }

    SECTION("potentials can be declined") {
        const lap::CostMatrix c = random_cost(4, 4, 8u);
        lap::SourceOracle<lap::CostMatrix> oracle(c);
        lap::FlowProblem prob = make_assignment(4, 4, &oracle, 4);

        lap::FlowOptions opts;
        opts.return_potentials = false;
        lap::FlowResult res = lap::solve_min_cost_flow(prob, opts);

        REQUIRE(res.status == "optimal");
        REQUIRE(res.potential.empty());
    }
}

TEST_CASE("The relaxation epsilon is the caller's to set", "[flow][solve][options]") {
    // Every pair costs the same, so which optimum comes back is decided
    // entirely by the tie-breaking predicate. Both settings have to reach the
    // same optimal value.
    lap::CostMatrix c(4, 4);
    for (int64_t i = 0; i < 4; ++i) {
        for (int64_t j = 0; j < 4; ++j) c.at(i, j) = 7.0;
    }
    lap::SourceOracle<lap::CostMatrix> oracle(c);

    lap::FlowOptions tight;
    tight.relax_eps = 1e-18;
    lap::FlowOptions loose;
    loose.relax_eps = lap::TOL;

    lap::FlowProblem a = make_assignment(4, 4, &oracle, 4);
    lap::FlowProblem b = make_assignment(4, 4, &oracle, 4);
    const lap::FlowResult ra = lap::solve_min_cost_flow(a, tight);
    const lap::FlowResult rb = lap::solve_min_cost_flow(b, loose);

    REQUIRE(ra.status == "optimal");
    REQUIRE(rb.status == "optimal");
    REQUIRE(ra.total_cost == Approx(28.0));
    REQUIRE(rb.total_cost == Approx(28.0));
}
