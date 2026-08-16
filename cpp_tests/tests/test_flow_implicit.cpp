// Test suite for the edge-generation loop: the restricted master, the pricing
// round that grows it, the feasibility phase that repairs it, and the
// certificate the three of them assemble for the complete implicit problem.

#include <catch2/catch_test_macros.hpp>
#include <catch2/catch_approx.hpp>

#include "core/lap_certify.h"
#include "core/lap_error.h"
#include "core/lap_types.h"
#include "flow/flow_candidates.h"
#include "flow/flow_compile.h"
#include "flow/flow_feasibility.h"
#include "flow/flow_implicit.h"
#include "flow/flow_oracle.h"
#include "flow/flow_problem.h"
#include "flow/flow_solve.h"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <random>
#include <vector>

using Catch::Approx;

namespace {

using Pair = lap::CandidateSet::Pair;

lap::CostMatrix random_cost(int64_t nr, int64_t nc, std::mt19937& rng) {
    std::uniform_real_distribution<double> unif(-5.0, 20.0);
    lap::CostMatrix c(nr, nc);
    for (int64_t i = 0; i < nr; ++i) {
        for (int64_t j = 0; j < nc; ++j) c.at(i, j) = unif(rng);
    }
    return c;
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

// The same design compiled and expanded in full: every admissible pair is an
// arc before the first augmentation. This is the answer the loop has to reach.
struct DenseSolve {
    bool    row_perfect = false;
    double  total_cost  = 0.0;
};

DenseSolve solve_dense(const lap::CostOracle& oracle) {
    lap::CompiledDesign design = lap::compile_one_to_one(oracle, {});
    const lap::FlowResult res = lap::solve_min_cost_flow(design.problem);
    DenseSolve out;
    out.row_perfect = (res.flow_sent == res.flow_required);
    out.total_cost = res.total_cost;
    return out;
}

int64_t summed_pairs_added(const lap::ImplicitResult& res) {
    int64_t total = 0;
    for (const lap::ImplicitRound& r : res.rounds) total += r.pairs_added;
    return total;
}

int64_t summed_evaluated(const lap::ImplicitResult& res) {
    int64_t total = 0;
    for (const lap::ImplicitRound& r : res.rounds) total += r.n_evaluated;
    return total;
}

std::vector<Pair> every_pair(int64_t nr, int64_t nc) {
    std::vector<Pair> pairs;
    for (int32_t i = 0; i < static_cast<int32_t>(nr); ++i) {
        for (int32_t j = 0; j < static_cast<int32_t>(nc); ++j) pairs.emplace_back(i, j);
    }
    return pairs;
}

}  // namespace

// ---------------------------------------------------------------------------
// condition 1: the loop's answer is the dense solve's answer
// ---------------------------------------------------------------------------

TEST_CASE("Edge generation reaches the dense optimum from an empty candidate set",
          "[flow][implicit]") {
    std::mt19937 rng(20260816u);

    for (int trial = 0; trial < 40; ++trial) {
        const int64_t nr = 1 + static_cast<int64_t>(rng() % 9u);
        const int64_t nc = nr + static_cast<int64_t>(rng() % 8u);

        lap::CostMatrix c = random_cost(nr, nc, rng);
        forbid_random(c, rng, static_cast<int64_t>(rng() % static_cast<unsigned>(nr * nc)));
        lap::SourceOracle<lap::CostMatrix> oracle(c);

        const DenseSolve dense = solve_dense(oracle);

        lap::CompiledDesign design = lap::compile_one_to_one(oracle, {});
        lap::CandidateSet cand(nr, nc);
        lap::ImplicitOptions opts;
        opts.keep_per_row = 2;
        opts.width = 2;
        const lap::ImplicitResult res =
            lap::solve_implicit_assignment(c, design.problem, cand, opts);

        if (!dense.row_perfect) {
            // No arc set over this source matches every row, and the loop says
            // so with a witness rather than by running out of rounds.
            REQUIRE(res.status == "infeasible");
            REQUIRE(res.witness_certified);
            REQUIRE(res.witness.rows.size() > res.witness.cols.size());
            continue;
        }

        REQUIRE(res.status == "optimal");
        REQUIRE(res.total_cost == Approx(dense.total_cost).margin(1e-12));
        REQUIRE(res.certified);
        REQUIRE(res.certificate.certified_optimal);
        REQUIRE(res.certificate.n_matched == nr);
        // The certificate re-derives the cost from the source, so a loop that
        // solves correctly and reports the total wrongly fails here.
        REQUIRE(res.certificate.primal_objective == Approx(res.total_cost).margin(1e-9));
    }
}

TEST_CASE("The seed changes the rounds and not the answer", "[flow][implicit]") {
    std::mt19937 rng(4242u);
    const int64_t nr = 12;
    const int64_t nc = 30;

    const lap::CostMatrix c = random_cost(nr, nc, rng);
    lap::SourceOracle<lap::CostMatrix> oracle(c);
    const DenseSolve dense = solve_dense(oracle);
    REQUIRE(dense.row_perfect);

    // Empty, a diagonal seed, and the whole grid: three different first
    // masters, and one answer.
    std::vector<std::vector<Pair>> seeds;
    seeds.push_back({});
    std::vector<Pair> diagonal;
    for (int32_t i = 0; i < static_cast<int32_t>(nr); ++i) {
        diagonal.emplace_back(i, i);
        diagonal.emplace_back(i, static_cast<int32_t>((i + 7) % nc));
    }
    seeds.push_back(diagonal);
    seeds.push_back(every_pair(nr, nc));

    for (const std::vector<Pair>& seed : seeds) {
        lap::CompiledDesign design = lap::compile_one_to_one(oracle, {});
        lap::CandidateSet cand(nr, nc);
        cand.add_pairs(seed);

        const lap::ImplicitResult res =
            lap::solve_implicit_assignment(c, design.problem, cand);

        CAPTURE(seed.size(), res.rounds.size(),
                res.certificate.min_reduced_cost,
                res.certificate.max_matched_slack);
        REQUIRE(res.status == "optimal");
        REQUIRE(res.total_cost == Approx(dense.total_cost).margin(1e-12));
        REQUIRE(res.certified);
    }

    // The master that already holds every pair has nothing to price in, so it
    // is one round and no pair is added.
    lap::CompiledDesign complete = lap::compile_one_to_one(oracle, {});
    lap::CandidateSet full(nr, nc);
    full.add_pairs(every_pair(nr, nc));
    const lap::ImplicitResult one_round =
        lap::solve_implicit_assignment(c, complete.problem, full);
    REQUIRE(one_round.rounds.size() == 1u);
    REQUIRE(one_round.rounds[0].pairs_added == 0);
    REQUIRE(one_round.candidate_edges == nr * nc);
}

TEST_CASE("A master that omits most of its pairs still certifies the whole problem",
          "[flow][implicit]") {
    std::mt19937 rng(90210u);
    const int64_t nr = 40;
    const int64_t nc = 400;

    const lap::CostMatrix c = random_cost(nr, nc, rng);
    lap::SourceOracle<lap::CostMatrix> oracle(c);
    const DenseSolve dense = solve_dense(oracle);
    REQUIRE(dense.row_perfect);

    lap::CompiledDesign design = lap::compile_one_to_one(oracle, {});
    lap::CandidateSet cand(nr, nc);
    const lap::ImplicitResult res =
        lap::solve_implicit_assignment(c, design.problem, cand);

    REQUIRE(res.status == "optimal");
    CAPTURE(res.certificate.min_reduced_cost, res.certificate.max_v,
            res.certificate.max_matched_slack, res.certificate.max_v_unmatched,
            res.certificate.duality_gap, res.certificate.primal_objective,
            res.certificate.dual_objective, res.certificate.n_matched);
    REQUIRE(res.certificate.primal_feasible);
    REQUIRE(res.certificate.dual_feasible);
    REQUIRE(res.certificate.cs_matched_tight);
    REQUIRE(res.certificate.cs_unmatched_free);
    REQUIRE(res.certified);
    REQUIRE(res.total_cost == Approx(dense.total_cost).margin(1e-12));
    REQUIRE(res.possible_edges == nr * nc);
    REQUIRE(res.candidate_edges < res.possible_edges);
}

// ---------------------------------------------------------------------------
// the round record
// ---------------------------------------------------------------------------

TEST_CASE("The round record accounts for the candidate set and the evaluations",
          "[flow][implicit]") {
    std::mt19937 rng(13579u);
    const int64_t nr = 15;
    const int64_t nc = 40;

    const lap::CostMatrix c = random_cost(nr, nc, rng);
    lap::SourceOracle<lap::CostMatrix> oracle(c);

    lap::CompiledDesign design = lap::compile_one_to_one(oracle, {});
    lap::CandidateSet cand(nr, nc);
    lap::ImplicitOptions opts;
    opts.keep_per_row = 1;
    opts.width = 1;
    const lap::ImplicitResult res =
        lap::solve_implicit_assignment(c, design.problem, cand, opts);

    REQUIRE(res.status == "optimal");
    REQUIRE(res.rounds.size() > 1u);

    // Started empty, so every candidate pair arrived through a round.
    REQUIRE(summed_pairs_added(res) == res.candidate_edges);
    REQUIRE(summed_evaluated(res) == res.edges_evaluated);

    bool priced = false;
    bool reseeded = false;
    for (std::size_t k = 0; k < res.rounds.size(); ++k) {
        const lap::ImplicitRound& r = res.rounds[k];
        REQUIRE(r.round == static_cast<int64_t>(k) + 1);
        // A candidate the source forbids is a candidate with no arc, so the
        // arcs a round adds never outnumber the pairs it added.
        REQUIRE(r.arcs_added <= r.pairs_added);
        REQUIRE(r.block_arcs <= r.candidate_pairs);
        REQUIRE(r.master_seconds >= 0.0);
        REQUIRE(r.pricing_seconds >= 0.0);
        if (r.kind == lap::ImplicitRound::Kind::priced) priced = true;
        if (r.kind == lap::ImplicitRound::Kind::reseeded) reseeded = true;
    }
    REQUIRE(reseeded);   // an empty candidate set starts every row deficient
    REQUIRE(priced);

    const lap::ImplicitRound& last = res.rounds.back();
    REQUIRE(last.kind == lap::ImplicitRound::Kind::priced);
    REQUIRE(last.n_violators == 0);
    REQUIRE(last.min_reduced_cost >= -opts.tol);
    REQUIRE(last.pairs_added == 0);
}

TEST_CASE("The round cap stops the loop and says so", "[flow][implicit]") {
    std::mt19937 rng(2468u);
    const int64_t nr = 12;
    const int64_t nc = 30;

    const lap::CostMatrix c = random_cost(nr, nc, rng);
    lap::SourceOracle<lap::CostMatrix> oracle(c);

    lap::CompiledDesign design = lap::compile_one_to_one(oracle, {});
    lap::CandidateSet cand(nr, nc);
    lap::ImplicitOptions opts;
    opts.keep_per_row = 1;
    opts.width = 1;
    opts.max_rounds = 1;

    const lap::ImplicitResult res =
        lap::solve_implicit_assignment(c, design.problem, cand, opts);
    REQUIRE(res.status == "iteration_limit");
    REQUIRE(res.rounds.size() == 1u);
    REQUIRE_FALSE(res.certified);
    REQUIRE(res.match.empty());
}

TEST_CASE("A deficiency that survives a round is attacked wider", "[flow][implicit]") {
    // Both rows want column 0 and nothing else is close, so the first round
    // seeds them both onto it and leaves one of them unmatched.
    lap::CostMatrix c(2, 3);
    c.at(0, 0) = 0.0; c.at(0, 1) = 5.0; c.at(0, 2) = 6.0;
    c.at(1, 0) = 1.0; c.at(1, 1) = 7.0; c.at(1, 2) = 8.0;
    lap::SourceOracle<lap::CostMatrix> oracle(c);

    lap::CompiledDesign design = lap::compile_one_to_one(oracle, {});
    lap::CandidateSet cand(2, 3);
    lap::ImplicitOptions opts;
    opts.width = 1;
    const lap::ImplicitResult res =
        lap::solve_implicit_assignment(c, design.problem, cand, opts);

    REQUIRE(res.rounds.size() >= 2u);
    REQUIRE(res.rounds[0].kind == lap::ImplicitRound::Kind::reseeded);
    REQUIRE(res.rounds[0].pairs_added == 2);   // one column each, at width 1
    REQUIRE(res.rounds[1].kind == lap::ImplicitRound::Kind::reseeded);
    // Both rows are still deficient and N(S) is column 0, so the round takes
    // the two cheapest columns outside it for each of them: the width doubled.
    REQUIRE(res.rounds[1].pairs_added == 4);

    REQUIRE(res.status == "optimal");
    REQUIRE(res.certified);
    REQUIRE(res.total_cost == Approx(6.0).margin(1e-12));
}

// ---------------------------------------------------------------------------
// the duals a warm-started master hands back
// ---------------------------------------------------------------------------

TEST_CASE("A warm-started master prices a matched pair below zero and a cold one does not",
          "[flow][implicit][duals]") {
    std::mt19937 rng(90210u);
    const int64_t nr = 40;
    const int64_t nc = 400;

    const lap::CostMatrix c = random_cost(nr, nc, rng);
    lap::SourceOracle<lap::CostMatrix> oracle(c);
    const lap::ImplicitOptions opts;

    // Started empty, so every master after the first resumes from the one
    // before it. A min-cost flow is optimal with an arc at its upper bound
    // priced below zero, and the warm start reaches exactly that: the slackness
    // repair saturates a newly added arc and no later augmentation touches it.
    lap::CompiledDesign warm = lap::compile_one_to_one(oracle, {});
    lap::CandidateSet cand(nr, nc);
    const lap::ImplicitResult grown =
        lap::solve_implicit_assignment(c, warm.problem, cand, opts);
    REQUIRE(grown.status == "optimal");

    double worst = 0.0;
    for (const lap::ImplicitRound& r : grown.rounds) {
        worst = std::max(worst, r.matched_slack);
    }
    REQUIRE(worst > opts.tol);
    // And the projection onto the tight face is what carries it to a
    // certificate the LP accepts.
    REQUIRE(grown.certified);

    // Solved in one round over every pair, the question does not arise: each
    // matched arc entered the flow on a shortest path at a reduced cost of zero.
    lap::CompiledDesign cold = lap::compile_one_to_one(oracle, {});
    lap::CandidateSet full(nr, nc);
    full.add_pairs(every_pair(nr, nc));
    const lap::ImplicitResult once =
        lap::solve_implicit_assignment(c, cold.problem, full, opts);
    REQUIRE(once.rounds.size() == 1u);
    REQUIRE(once.rounds[0].matched_slack <= opts.tol);
    REQUIRE(once.certified);
    REQUIRE(once.total_cost == Approx(grown.total_cost).margin(1e-12));
}

// ---------------------------------------------------------------------------
// infeasibility, with a certificate rather than a wider ladder
// ---------------------------------------------------------------------------

TEST_CASE("A row no column admits is infeasible with a certified witness",
          "[flow][implicit]") {
    std::mt19937 rng(864209u);
    const int64_t nr = 6;
    const int64_t nc = 14;

    lap::CostMatrix c = random_cost(nr, nc, rng);
    for (int64_t j = 0; j < nc; ++j) c.forbid(3, j);
    lap::SourceOracle<lap::CostMatrix> oracle(c);

    lap::CompiledDesign design = lap::compile_one_to_one(oracle, {});
    lap::CandidateSet cand(nr, nc);
    const lap::ImplicitResult res =
        lap::solve_implicit_assignment(c, design.problem, cand);

    REQUIRE(res.status == "infeasible");
    REQUIRE(res.witness_certified);
    REQUIRE(res.witness.rows == std::vector<int64_t>{3});
    REQUIRE(res.witness.cols.empty());
    REQUIRE(res.match.empty());
    REQUIRE_FALSE(res.certified);
}

TEST_CASE("More rows than the columns can carry is infeasible", "[flow][implicit]") {
    std::mt19937 rng(777u);
    const lap::CostMatrix c = random_cost(5, 3, rng);
    lap::SourceOracle<lap::CostMatrix> oracle(c);

    lap::CompiledDesign design = lap::compile_one_to_one(oracle, {});
    lap::CandidateSet cand(5, 3);
    const lap::ImplicitResult res =
        lap::solve_implicit_assignment(c, design.problem, cand);

    REQUIRE(res.status == "infeasible");
    REQUIRE(res.witness_certified);
    REQUIRE(res.witness.rows.size() > res.witness.cols.size());
}

// ---------------------------------------------------------------------------
// the two scans the certificate is assembled from
// ---------------------------------------------------------------------------

TEST_CASE("A scan over the restricted graph is a scan over the same graph written densely",
          "[flow][implicit][scan]") {
    std::mt19937 rng(31415u);

    for (int trial = 0; trial < 25; ++trial) {
        const int64_t nr = 1 + static_cast<int64_t>(rng() % 8u);
        const int64_t nc = 1 + static_cast<int64_t>(rng() % 12u);

        lap::CostMatrix c = random_cost(nr, nc, rng);
        forbid_random(c, rng, static_cast<int64_t>(rng() % static_cast<unsigned>(nr * nc)));

        lap::CandidateSet cand(nr, nc);
        std::vector<Pair> seed;
        for (int32_t i = 0; i < static_cast<int32_t>(nr); ++i) {
            for (int32_t j = 0; j < static_cast<int32_t>(nc); ++j) {
                if (rng() % 3u == 0u) seed.emplace_back(i, j);
            }
        }
        cand.add_pairs(seed);

        // The candidate graph written densely: every pair the set omits is
        // forbidden outright, so the grid scan sees the restricted problem.
        lap::CostMatrix densified = c;
        for (int64_t i = 0; i < nr; ++i) {
            for (int64_t j = 0; j < nc; ++j) {
                if (!cand.contains(i, j)) densified.forbid(i, j);
            }
        }

        std::uniform_real_distribution<double> dual(-3.0, 3.0);
        std::vector<double> u(static_cast<std::size_t>(nr));
        std::vector<double> v(static_cast<std::size_t>(nc));
        for (double& ui : u) ui = dual(rng);
        for (double& vj : v) vj = dual(rng);

        const lap::ReducedCostScan restricted = lap::scan_reduced_costs(
            lap::CandidateGraph<lap::CostMatrix>(c, cand), u, v, 1e-9);
        const lap::ReducedCostScan grid = lap::scan_reduced_costs(densified, u, v, 1e-9);

        REQUIRE(restricted.n_admissible == grid.n_admissible);
        REQUIRE(restricted.n_violations == grid.n_violations);
        REQUIRE(restricted.arg_i == grid.arg_i);
        REQUIRE(restricted.arg_j == grid.arg_j);
        if (grid.n_admissible > 0) {
            REQUIRE(restricted.min_reduced_cost == grid.min_reduced_cost);
        }
    }
}

TEST_CASE("A certificate over a supplied scan is the one it would have taken",
          "[flow][implicit][scan]") {
    std::mt19937 rng(271828u);

    for (int trial = 0; trial < 25; ++trial) {
        const int64_t nr = 1 + static_cast<int64_t>(rng() % 7u);
        const int64_t nc = nr + static_cast<int64_t>(rng() % 6u);

        lap::CostMatrix c = random_cost(nr, nc, rng);
        forbid_random(c, rng, static_cast<int64_t>(rng() % static_cast<unsigned>(nr)));

        // A restricted master's answer, so the duals are the ones the loop
        // certifies with rather than an arbitrary pair of vectors.
        lap::SourceOracle<lap::CostMatrix> oracle(c);
        lap::CompiledDesign design = lap::compile_one_to_one(oracle, {});
        lap::CandidateSet cand(nr, nc);
        const lap::ImplicitResult res =
            lap::solve_implicit_assignment(c, design.problem, cand);
        if (res.status != "optimal") continue;

        const lap::ReducedCostScan scan = lap::scan_reduced_costs(c, res.u, res.v, 1e-9);
        const lap::CertificateReport supplied =
            lap::certify_assignment(c, res.match, res.u, res.v, 1e-9, scan);
        const lap::CertificateReport taken =
            lap::certify_assignment(c, res.match, res.u, res.v, 1e-9);

        REQUIRE(supplied.certified_optimal == taken.certified_optimal);
        REQUIRE(supplied.dual_feasible == taken.dual_feasible);
        REQUIRE(supplied.min_reduced_cost == taken.min_reduced_cost);
        REQUIRE(supplied.worst_i == taken.worst_i);
        REQUIRE(supplied.worst_j == taken.worst_j);
        REQUIRE(supplied.duality_gap == taken.duality_gap);

        // The loop assembled the same conclusion from two halves it already had.
        REQUIRE(res.certificate.certified_optimal == taken.certified_optimal);
        REQUIRE(res.certificate.min_reduced_cost == Approx(taken.min_reduced_cost).margin(1e-12));
    }
}

TEST_CASE("Two scans over disjoint pairs read as one scan over both",
          "[flow][implicit][scan]") {
    lap::ReducedCostScan a;
    a.min_reduced_cost = 0.5;
    a.arg_i = 2;
    a.arg_j = 7;
    a.n_admissible = 10;
    a.n_violations = 0;

    lap::ReducedCostScan b;
    b.min_reduced_cost = -0.25;
    b.arg_i = 1;
    b.arg_j = 3;
    b.n_admissible = 4;
    b.n_violations = 1;

    const lap::ReducedCostScan merged = lap::merge_scans(a, b);
    REQUIRE(merged.min_reduced_cost == -0.25);
    REQUIRE(merged.arg_i == 1);
    REQUIRE(merged.arg_j == 3);
    REQUIRE(merged.n_admissible == 14);
    REQUIRE(merged.n_violations == 1);

    // An empty scan carries no argmin to take.
    const lap::ReducedCostScan with_empty = lap::merge_scans(a, lap::ReducedCostScan());
    REQUIRE(with_empty.min_reduced_cost == 0.5);
    REQUIRE(with_empty.arg_i == 2);
    REQUIRE(with_empty.arg_j == 7);
    REQUIRE(with_empty.n_admissible == 10);

    // A tie keeps the pair an ascending scan of both would have named first.
    lap::ReducedCostScan tied = b;
    tied.arg_i = 5;
    tied.arg_j = 0;
    lap::ReducedCostScan early = b;
    early.arg_i = 1;
    early.arg_j = 3;
    REQUIRE(lap::merge_scans(tied, early).arg_i == 1);
    REQUIRE(lap::merge_scans(early, tied).arg_i == 1);
}

// ---------------------------------------------------------------------------
// the shape the loop takes
// ---------------------------------------------------------------------------

TEST_CASE("A problem the loop cannot price is refused rather than solved",
          "[flow][implicit]") {
    std::mt19937 rng(5150u);
    const lap::CostMatrix c = random_cost(4, 9, rng);
    lap::SourceOracle<lap::CostMatrix> oracle(c);

    SECTION("a candidate set of another shape") {
        lap::CompiledDesign design = lap::compile_one_to_one(oracle, {});
        lap::CandidateSet cand(4, 8);
        REQUIRE_THROWS_AS(lap::solve_implicit_assignment(c, design.problem, cand),
                          lap::DimensionException);
    }

    SECTION("a problem whose arcs are already its own") {
        lap::CompiledDesign design = lap::compile_one_to_one(oracle, {});
        lap::expand_blocks(design.problem);
        lap::CandidateSet cand(4, 9);
        REQUIRE_THROWS_AS(lap::solve_implicit_assignment(c, design.problem, cand),
                          lap::DimensionException);
    }

    SECTION("a design that does not match every row") {
        lap::CompiledDesign design = lap::compile_k_cardinality(oracle, 2, {});
        lap::CandidateSet cand(4, 9);
        REQUIRE_THROWS_AS(lap::solve_implicit_assignment(c, design.problem, cand),
                          lap::DimensionException);
    }

    SECTION("options that cannot grow the candidate set") {
        lap::CandidateSet cand(4, 9);
        lap::ImplicitOptions opts;

        lap::CompiledDesign a = lap::compile_one_to_one(oracle, {});
        opts.keep_per_row = 0;
        REQUIRE_THROWS_AS(lap::solve_implicit_assignment(c, a.problem, cand, opts),
                          lap::DimensionException);

        lap::CompiledDesign b = lap::compile_one_to_one(oracle, {});
        opts.keep_per_row = 5;
        opts.width = 0;
        REQUIRE_THROWS_AS(lap::solve_implicit_assignment(c, b.problem, cand, opts),
                          lap::DimensionException);

        lap::CompiledDesign d = lap::compile_one_to_one(oracle, {});
        opts.width = 5;
        opts.max_rounds = 0;
        REQUIRE_THROWS_AS(lap::solve_implicit_assignment(c, d.problem, cand, opts),
                          lap::DimensionException);
    }
}
