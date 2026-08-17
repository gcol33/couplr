// Test suite for the min-cost-flow optimality certificate and the assignment
// duals read off a solved flow.

#include <catch2/catch_test_macros.hpp>
#include <catch2/catch_approx.hpp>

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <limits>
#include <random>
#include <utility>
#include <vector>

#include "core/lap_types.h"
#include "core/lap_certify.h"
#include "flow/flow_problem.h"
#include "flow/flow_oracle.h"
#include "flow/flow_compile.h"
#include "flow/flow_solve.h"
#include "flow/flow_certify.h"
#include "solvers/solve_jv_duals.h"

using Catch::Approx;

namespace {

constexpr double TOL = 1e-9;

lap::FlowProblem make_problem(int32_t n_nodes,
                              std::vector<int64_t> supply,
                              std::vector<lap::FlowArc> arcs) {
    lap::FlowProblem prob;
    prob.n_nodes = n_nodes;
    prob.supply = std::move(supply);
    prob.arcs = std::move(arcs);
    prob.expanded = true;
    return prob;
}

// Two sources feeding two sinks over four arcs. Supplies 2 and 3 against demands
// 3 and 2, capacities well above anything the supplies can send, so the optimum
// is decided by cost alone: the cheap arc 0 -> 2 carries all of node 0's supply
// and node 1 covers the remainder of both demands, for 2*1 + 1*4 + 2*2 = 10.
//
// The three arcs carrying flow sit strictly inside their bounds, so each needs
// cbar = 0, and that fixes the potentials up to the normalization pi(0) = 0:
// pi(2) = 1 from arc 0, pi(1) = -3 from arc 2, pi(3) = -1 from arc 3. The idle
// arc 0 -> 3 then prices at 3 - (-1) = 4, comfortably feasible.
lap::FlowProblem transport_problem() {
    return make_problem(4, {2, 3, -3, -2},
                        {lap::FlowArc(0, 2, 0, 5, 1.0),
                         lap::FlowArc(0, 3, 0, 5, 3.0),
                         lap::FlowArc(1, 2, 0, 5, 4.0),
                         lap::FlowArc(1, 3, 0, 5, 2.0)});
}

const std::vector<int64_t> transport_flow      = {2, 0, 1, 2};
const std::vector<double>  transport_potential = {0.0, -3.0, 1.0, -1.0};

// Six units across two parallel arcs of capacity five. The cheap arc saturates
// and the expensive one carries the remaining unit strictly inside its bounds,
// which pins pi(1) = 3 and leaves the saturated arc pricing at 1 - 3 = -2.
lap::FlowProblem parallel_problem() {
    return make_problem(2, {6, -6},
                        {lap::FlowArc(0, 1, 0, 5, 1.0),
                         lap::FlowArc(0, 1, 0, 5, 3.0)});
}

const std::vector<int64_t> parallel_flow      = {5, 1};
const std::vector<double>  parallel_potential = {0.0, 3.0};

// Two independent source-sink pairs whose arcs cost the same. Swapping the two
// flow values leaves the objective untouched and destroys conservation at all
// four nodes.
lap::FlowProblem twin_problem() {
    return make_problem(4, {1, 2, -1, -2},
                        {lap::FlowArc(0, 2, 0, 2, 3.0),
                         lap::FlowArc(1, 3, 0, 2, 3.0)});
}

// ---------------------------------------------------------------------------
// Compiled assignments, solved by the flow solver, so the potentials handed to
// the certificate are the ones a solve actually produces rather than numbers
// chosen to pass.
// ---------------------------------------------------------------------------

struct SolvedDesign {
    lap::CompiledDesign   design;
    lap::FlowResult       result;
    lap::AssignmentLayout layout;
};

SolvedDesign solve_one_to_one(const lap::CostOracle& costs) {
    SolvedDesign out;
    out.design = lap::compile_one_to_one(costs, {});
    out.result = lap::solve_min_cost_flow(out.design.problem);
    out.layout = lap::layout_of(out.design.problem);
    return out;
}

SolvedDesign solve_k_cardinality(const lap::CostOracle& costs, int64_t k) {
    SolvedDesign out;
    out.design = lap::compile_k_cardinality(costs, k, {});
    out.result = lap::solve_min_cost_flow(out.design.problem);
    out.layout = lap::layout_of(out.design.problem);
    return out;
}

lap::CostMatrix transpose(const lap::CostMatrix& cost) {
    lap::CostMatrix out(cost.ncol, cost.nrow);
    for (int64_t i = 0; i < cost.nrow; ++i) {
        for (int64_t j = 0; j < cost.ncol; ++j) {
            out.at(j, i) = cost.at(i, j);
            if (!cost.allowed(i, j)) out.forbid(j, i);
        }
    }
    return out;
}

lap::CostMatrix random_costs(int64_t nr, int64_t nc, unsigned seed, double lo, double hi) {
    lap::CostMatrix cost(nr, nc);
    std::mt19937 rng(seed);
    std::uniform_real_distribution<double> draw(lo, hi);
    for (int64_t i = 0; i < nr; ++i) {
        for (int64_t j = 0; j < nc; ++j) cost.at(i, j) = draw(rng);
    }
    return cost;
}

// The optimal cost from an independent solver, in whichever orientation
// solve_jv_duals accepts.
double reference_cost(const lap::CostMatrix& cost) {
    if (cost.nrow <= cost.ncol) {
        return lap::solve_jv_duals(cost, false).solution.total_cost;
    }
    const lap::CostMatrix flipped = transpose(cost);
    return lap::solve_jv_duals(flipped, false).solution.total_cost;
}

}  // namespace

TEST_CASE("Flow certificate - hand-computed transportation optimum",
          "[flow_certify][basic]") {
    const lap::FlowProblem prob = transport_problem();
    const lap::FlowCertificate rep =
        lap::certify_flow(prob, transport_flow, transport_potential, TOL);

    REQUIRE(rep.primal_feasible);
    REQUIRE(rep.n_capacity_violations == 0);
    REQUIRE(rep.n_conservation_violations == 0);
    REQUIRE(rep.max_conservation_error == Approx(0.0));
    REQUIRE(rep.primal_objective == Approx(10.0));
    REQUIRE(rep.dual_objective == Approx(10.0));
    REQUIRE(rep.duality_gap == Approx(0.0).margin(1e-12));
    REQUIRE(rep.dual_feasible);
    REQUIRE(rep.complementary_slackness);
    REQUIRE(rep.n_cs_violations == 0);
    REQUIRE(rep.min_residual_reduced_cost == Approx(0.0).margin(1e-12));
    REQUIRE(rep.certified_optimal);
    REQUIRE(rep.tolerance == TOL);
}

TEST_CASE("Flow certificate - a saturated arc may price below zero",
          "[flow_certify][basic]") {
    const lap::FlowProblem prob = parallel_problem();
    const lap::FlowCertificate rep =
        lap::certify_flow(prob, parallel_flow, parallel_potential, TOL);

    // cbar on the cheap arc is 1 - 3 = -2, which is what an arc held back by its
    // capacity looks like. The dual pays upper * cbar for it, and the objectives
    // still agree.
    REQUIRE(rep.primal_objective == Approx(8.0));
    REQUIRE(rep.dual_objective == Approx(8.0));
    REQUIRE(rep.dual_feasible);
    REQUIRE(rep.complementary_slackness);
    REQUIRE(rep.certified_optimal);
}

TEST_CASE("Flow certificate - primal violations", "[flow_certify][primal]") {
    const lap::FlowProblem prob = transport_problem();

    SECTION("a perturbed flow value breaks conservation at both its endpoints") {
        std::vector<int64_t> flow = transport_flow;
        flow[0] -= 1;

        const lap::FlowCertificate rep =
            lap::certify_flow(prob, flow, transport_potential, TOL);

        REQUIRE(rep.n_conservation_violations == 2);
        REQUIRE(rep.max_conservation_error == Approx(1.0));
        REQUIRE(rep.n_capacity_violations == 0);
        REQUIRE_FALSE(rep.primal_feasible);
        REQUIRE(std::isnan(rep.primal_objective));
        REQUIRE_FALSE(rep.certified_optimal);
    }

    SECTION("a flow above its arc's capacity is counted as a capacity violation") {
        lap::FlowProblem tight = transport_problem();
        tight.arcs[0].upper = 1;

        const lap::FlowCertificate rep =
            lap::certify_flow(tight, transport_flow, transport_potential, TOL);

        REQUIRE(rep.n_capacity_violations == 1);
        REQUIRE(rep.n_conservation_violations == 0);
        REQUIRE_FALSE(rep.primal_feasible);
        REQUIRE_FALSE(rep.certified_optimal);
    }

    SECTION("a flow below its arc's lower bound is counted as well") {
        lap::FlowProblem bounded = transport_problem();
        bounded.arcs[1].lower = 1;

        const lap::FlowCertificate rep =
            lap::certify_flow(bounded, transport_flow, transport_potential, TOL);

        REQUIRE(rep.n_capacity_violations == 1);
        REQUIRE_FALSE(rep.primal_feasible);
        REQUIRE_FALSE(rep.certified_optimal);
    }

    SECTION("swapping two equal-cost flows leaves the cost and breaks the flow") {
        const lap::FlowProblem prob_twin = twin_problem();
        const std::vector<double> potential = {0.0, 0.0, 3.0, 3.0};

        const lap::FlowCertificate ok =
            lap::certify_flow(prob_twin, {1, 2}, potential, TOL);
        REQUIRE(ok.primal_objective == Approx(9.0));
        REQUIRE(ok.certified_optimal);

        // 2 * 3 + 1 * 3 is the same 9, and no node balances.
        const lap::FlowCertificate swapped =
            lap::certify_flow(prob_twin, {2, 1}, potential, TOL);
        REQUIRE(swapped.n_conservation_violations == 4);
        REQUIRE_FALSE(swapped.primal_feasible);
        REQUIRE(std::isnan(swapped.primal_objective));
        REQUIRE_FALSE(swapped.certified_optimal);
    }
}

TEST_CASE("Flow certificate - dual violations", "[flow_certify][dual]") {
    SECTION("a potential that prices an unsaturated arc below zero fails dual feasibility") {
        const lap::FlowProblem prob = transport_problem();
        std::vector<double> potential = transport_potential;
        potential[3] += 0.5;  // arc 3 carries 2 of 5 units; its cbar drops to -0.5

        const lap::FlowCertificate rep =
            lap::certify_flow(prob, transport_flow, potential, TOL);

        REQUIRE(rep.primal_feasible);
        REQUIRE_FALSE(rep.dual_feasible);
        REQUIRE(rep.min_residual_reduced_cost == Approx(-0.5));
        REQUIRE(rep.worst_arc == 3);
        REQUIRE_FALSE(rep.certified_optimal);
    }

    SECTION("a potential that prices a flow-carrying arc above zero fails slackness") {
        const lap::FlowProblem prob = transport_problem();
        std::vector<double> potential = transport_potential;
        potential[3] -= 0.5;  // arc 3's cbar rises to +0.5 while it carries flow

        const lap::FlowCertificate rep =
            lap::certify_flow(prob, transport_flow, potential, TOL);

        REQUIRE(rep.primal_feasible);
        REQUIRE(rep.dual_feasible);
        REQUIRE_FALSE(rep.complementary_slackness);
        REQUIRE(rep.n_cs_violations == 1);
        REQUIRE(rep.min_residual_reduced_cost == Approx(-0.5));
        REQUIRE(rep.worst_arc == 3);
        REQUIRE_FALSE(rep.certified_optimal);
    }

    SECTION("an arc strictly inside its bounds needs |cbar| <= tol, in both directions") {
        const lap::FlowProblem prob = parallel_problem();
        // Arc 1 carries 1 of a possible 5, strictly between its bounds.
        REQUIRE(parallel_flow[1] > prob.arcs[1].lower);
        REQUIRE(parallel_flow[1] < prob.arcs[1].upper);

        const lap::FlowCertificate above =
            lap::certify_flow(prob, parallel_flow, {0.0, 2.0}, TOL);
        REQUIRE(above.primal_feasible);
        REQUIRE(above.dual_feasible);
        REQUIRE_FALSE(above.complementary_slackness);
        REQUIRE(above.n_cs_violations == 1);
        REQUIRE(above.duality_gap == Approx(1.0));
        REQUIRE_FALSE(above.certified_optimal);

        const lap::FlowCertificate below =
            lap::certify_flow(prob, parallel_flow, {0.0, 4.0}, TOL);
        REQUIRE(below.primal_feasible);
        REQUIRE_FALSE(below.dual_feasible);
        REQUIRE(below.min_residual_reduced_cost == Approx(-1.0));
        REQUIRE(below.worst_arc == 1);
        REQUIRE(below.duality_gap == Approx(4.0));
        REQUIRE_FALSE(below.certified_optimal);
    }
}

TEST_CASE("Flow certificate - a feasible flow of the wrong cost opens the gap",
          "[flow_certify][dual]") {
    // The 2x2 assignment [[1, 2], [4, 3]] compiled and solved, then perturbed by
    // exchanging the two rows' columns. Conservation survives the exchange, so
    // nothing in the primal complains; the cost rises from 4 to 6 and the whole
    // difference lands in complementary slackness and the gap.
    lap::CostMatrix cost(2, 2);
    cost.at(0, 0) = 1.0; cost.at(0, 1) = 2.0;
    cost.at(1, 0) = 4.0; cost.at(1, 1) = 3.0;
    const lap::SourceOracle<lap::CostMatrix> oracle(cost);

    SolvedDesign solved = solve_one_to_one(oracle);
    REQUIRE(solved.result.status == "optimal");
    REQUIRE(solved.result.total_cost == Approx(4.0));
    REQUIRE(lap::certify_flow(solved.design.problem, solved.result.flow,
                              solved.result.potential, TOL).certified_optimal);

    std::vector<int64_t> swapped = solved.result.flow;
    const lap::BlockArcRange& blk = solved.design.problem.block_arcs[0];
    for (int64_t k = 0; k < blk.n_arcs; ++k) {
        const int32_t i = blk.rc[static_cast<std::size_t>(k)].first;
        const int32_t j = blk.rc[static_cast<std::size_t>(k)].second;
        swapped[static_cast<std::size_t>(blk.first_arc + k)] = (i == j) ? 0 : 1;
    }

    const lap::FlowCertificate rep = lap::certify_flow(
        solved.design.problem, swapped, solved.result.potential, TOL);
    REQUIRE(rep.primal_feasible);
    REQUIRE(rep.n_conservation_violations == 0);
    REQUIRE(rep.primal_objective == Approx(6.0));
    REQUIRE(rep.dual_objective == Approx(4.0));
    REQUIRE_FALSE(rep.complementary_slackness);
    REQUIRE(rep.n_cs_violations >= 1);
    REQUIRE(rep.duality_gap == Approx(2.0));
    REQUIRE_FALSE(rep.certified_optimal);
}

TEST_CASE("Flow certificate - structural failures report primal_feasible false",
          "[flow_certify][structural]") {
    SECTION("an unexpanded problem") {
        lap::FlowProblem prob = transport_problem();
        prob.expanded = false;

        const lap::FlowCertificate rep =
            lap::certify_flow(prob, transport_flow, transport_potential, TOL);
        REQUIRE_FALSE(rep.primal_feasible);
        REQUIRE_FALSE(rep.certified_optimal);
        REQUIRE(std::isnan(rep.primal_objective));
        REQUIRE(std::isnan(rep.dual_objective));
    }

    SECTION("a flow vector of the wrong length") {
        const lap::FlowProblem prob = transport_problem();
        const lap::FlowCertificate rep =
            lap::certify_flow(prob, {2, 0, 1}, transport_potential, TOL);
        REQUIRE_FALSE(rep.primal_feasible);
        REQUIRE_FALSE(rep.certified_optimal);
    }

    SECTION("a potential vector of the wrong length") {
        const lap::FlowProblem prob = transport_problem();
        const lap::FlowCertificate rep =
            lap::certify_flow(prob, transport_flow, {0.0, -3.0, 1.0}, TOL);
        REQUIRE_FALSE(rep.primal_feasible);
        REQUIRE_FALSE(rep.certified_optimal);
    }

    SECTION("an arc endpoint outside the node range") {
        lap::FlowProblem prob = transport_problem();
        prob.arcs[2].head = 9;
        const lap::FlowCertificate rep =
            lap::certify_flow(prob, transport_flow, transport_potential, TOL);
        REQUIRE_FALSE(rep.primal_feasible);
        REQUIRE_FALSE(rep.certified_optimal);
    }

    SECTION("a supply vector of the wrong length") {
        lap::FlowProblem prob = transport_problem();
        prob.supply.pop_back();
        const lap::FlowCertificate rep =
            lap::certify_flow(prob, transport_flow, transport_potential, TOL);
        REQUIRE_FALSE(rep.primal_feasible);
        REQUIRE_FALSE(rep.certified_optimal);
    }

    SECTION("a non-finite potential is a violation, not a silent pass") {
        const lap::FlowProblem prob = transport_problem();
        std::vector<double> potential = transport_potential;
        potential[2] = std::numeric_limits<double>::quiet_NaN();

        const lap::FlowCertificate rep =
            lap::certify_flow(prob, transport_flow, potential, TOL);
        REQUIRE_FALSE(rep.dual_feasible);
        REQUIRE_FALSE(rep.certified_optimal);
    }
}

TEST_CASE("Assignment duals from a flow - one row, two columns",
          "[flow_certify][assignment]") {
    lap::CostMatrix cost(1, 2);
    cost.at(0, 0) = 5.0;
    cost.at(0, 1) = 9.0;
    const lap::SourceOracle<lap::CostMatrix> oracle(cost);

    SolvedDesign solved = solve_one_to_one(oracle);
    REQUIRE(solved.result.total_cost == Approx(5.0));
    REQUIRE(lap::certify_flow(solved.design.problem, solved.result.flow,
                              solved.result.potential, TOL).certified_optimal);

    const lap::AssignmentDuals duals = lap::map_assignment_duals(
        solved.design.problem, solved.layout, solved.result.flow,
        solved.result.potential, lap::AssignmentEquality::Rows);
    REQUIRE(duals.ok());
    REQUIRE(duals.match.size() == 1);
    REQUIRE(duals.match[0] == 0);
    REQUIRE(duals.u[0] + duals.v[0] == Approx(5.0));
    REQUIRE(duals.v[1] == Approx(0.0));

    const lap::CertificateReport rep =
        lap::certify_assignment(cost, duals.match, duals.u, duals.v, TOL);
    REQUIRE(rep.certified_optimal);
    REQUIRE(rep.primal_objective == Approx(5.0));
}

TEST_CASE("Assignment duals from a flow - forbidden pairs",
          "[flow_certify][assignment]") {
    // Row 1 can only reach column 2 and row 0 only columns 0 and 2, so the
    // optimum pays 4 for row 0 rather than taking the column row 1 needs, and
    // column 1 is reachable from nothing at all.
    lap::CostMatrix cost(2, 3);
    cost.at(0, 0) = 4.0; cost.at(0, 1) = 1.0; cost.at(0, 2) = 2.0;
    cost.at(1, 0) = 7.0; cost.at(1, 1) = 6.0; cost.at(1, 2) = 3.0;
    cost.forbid(0, 1);
    cost.forbid(1, 0);
    cost.forbid(1, 1);
    const lap::SourceOracle<lap::CostMatrix> oracle(cost);

    SolvedDesign solved = solve_one_to_one(oracle);
    REQUIRE(solved.result.status == "optimal");
    REQUIRE(solved.result.total_cost == Approx(7.0));
    REQUIRE(lap::certify_flow(solved.design.problem, solved.result.flow,
                              solved.result.potential, TOL).certified_optimal);

    const lap::AssignmentDuals duals = lap::map_assignment_duals(
        solved.design.problem, solved.layout, solved.result.flow,
        solved.result.potential, lap::AssignmentEquality::Rows);
    REQUIRE(duals.ok());
    REQUIRE(duals.match[0] == 0);
    REQUIRE(duals.match[1] == 2);

    REQUIRE(duals.v[1] == Approx(0.0));

    const lap::CertificateReport rep =
        lap::certify_assignment(cost, duals.match, duals.u, duals.v, TOL);
    REQUIRE(rep.certified_optimal);
    REQUIRE(rep.primal_objective == Approx(7.0));

    // Nothing enters column 1 and its sink arc carries no flow, so pricing it
    // above the sink leaves the flow certificate intact and is a dual the
    // mapping has to be able to read. Its multiplier is zero there while the
    // potential difference is not, which is the case v_j = min(b_j, 0) exists
    // for, and reading the difference instead would put v_1 = 1 into a dual the
    // assignment LP bounds above by zero.
    std::vector<double> raised = solved.result.potential;
    const std::size_t col1 =
        static_cast<std::size_t>(solved.layout.col_base + 1);
    const std::size_t sink =
        static_cast<std::size_t>(solved.layout.sink_node);
    raised[col1] = raised[sink] + 1.0;
    REQUIRE(raised[col1] - raised[sink] > TOL);
    REQUIRE(lap::certify_flow(solved.design.problem, solved.result.flow,
                              raised, TOL).certified_optimal);

    const lap::AssignmentDuals raised_duals = lap::map_assignment_duals(
        solved.design.problem, solved.layout, solved.result.flow,
        raised, lap::AssignmentEquality::Rows);
    REQUIRE(raised_duals.ok());
    REQUIRE(raised_duals.match[0] == 0);
    REQUIRE(raised_duals.match[1] == 2);
    REQUIRE(raised_duals.v[1] == Approx(0.0));

    const lap::CertificateReport raised_rep = lap::certify_assignment(
        cost, raised_duals.match, raised_duals.u, raised_duals.v, TOL);
    REQUIRE(raised_rep.certified_optimal);
    REQUIRE(raised_rep.primal_objective == Approx(7.0));
}

TEST_CASE("Flow certificate - an arc out of an idle node still has to price",
          "[flow_certify][dual]") {
    // Node 2 carries no supply and no arc enters it, so nothing in the primal
    // ever touches it. Its arc to the sink has residual capacity all the same,
    // and a potential that leaves node 2 behind while the sink's rises prices
    // that arc at -1, which is a dual bound of 0 against a primal of 1.
    const lap::FlowProblem prob =
        make_problem(3, {1, -1, 0},
                     {lap::FlowArc(0, 1, 0, 1, 1.0),
                      lap::FlowArc(2, 1, 0, 1, 0.0)});

    const lap::FlowCertificate stale = lap::certify_flow(prob, {1, 0}, {0.0, 1.0, 0.0}, TOL);
    REQUIRE(stale.primal_feasible);
    REQUIRE(stale.primal_objective == Approx(1.0));
    REQUIRE_FALSE(stale.dual_feasible);
    REQUIRE(stale.min_residual_reduced_cost == Approx(-1.0));
    REQUIRE(stale.worst_arc == 1);
    REQUIRE(stale.dual_objective == Approx(0.0));
    REQUIRE(stale.duality_gap == Approx(1.0));
    REQUIRE_FALSE(stale.certified_optimal);

    const lap::FlowCertificate carried = lap::certify_flow(prob, {1, 0}, {0.0, 1.0, 1.0}, TOL);
    REQUIRE(carried.dual_feasible);
    REQUIRE(carried.dual_objective == Approx(1.0));
    REQUIRE(carried.certified_optimal);
}

TEST_CASE("Assignment duals from a flow - randomized square and wide",
          "[flow_certify][assignment]") {
    struct Shape { int64_t nr; int64_t nc; };
    const std::vector<Shape> shapes = {
        {1, 1}, {3, 3}, {5, 5}, {8, 8},
        {2, 7}, {4, 9}, {5, 12}, {3, 11}, {1, 6}
    };

    int64_t n_free_priced_above_anchor = 0;
    int64_t n_unclamped_rejected = 0;

    for (double lo : {0.0, -5.0}) {
        for (unsigned seed = 1; seed <= 6; ++seed) {
            for (const Shape& shape : shapes) {
                const unsigned key = seed * 1000u +
                    static_cast<unsigned>(shape.nr * 31 + shape.nc);
                const lap::CostMatrix cost =
                    random_costs(shape.nr, shape.nc, key, lo, lo + 10.0);
                const lap::SourceOracle<lap::CostMatrix> oracle(cost);

                INFO("shape " << shape.nr << "x" << shape.nc
                     << " seed " << seed << " lo " << lo);

                SolvedDesign solved = solve_one_to_one(oracle);
                REQUIRE(solved.result.status == "optimal");
                REQUIRE(solved.result.total_cost ==
                        Approx(reference_cost(cost)).margin(1e-8));
                REQUIRE(lap::certify_flow(solved.design.problem, solved.result.flow,
                                          solved.result.potential, TOL).certified_optimal);

                const lap::AssignmentDuals duals = lap::map_assignment_duals(
                    solved.design.problem, solved.layout, solved.result.flow,
                    solved.result.potential, lap::AssignmentEquality::Rows);
                REQUIRE(duals.ok());

                const lap::CertificateReport rep = lap::certify_assignment(
                    cost, duals.match, duals.u, duals.v, TOL);
                REQUIRE(rep.primal_feasible);
                REQUIRE(rep.dual_feasible);
                REQUIRE(rep.cs_matched_tight);
                REQUIRE(rep.cs_unmatched_free);
                REQUIRE(rep.n_matched == shape.nr);
                REQUIRE(rep.primal_objective ==
                        Approx(solved.result.total_cost).margin(1e-8));
                REQUIRE(rep.certified_optimal);

                // The raw potential difference on a free column, which is what
                // v_j would be without the multiplier's clamp at zero.
                const double anchor = solved.result.potential[
                    static_cast<std::size_t>(solved.layout.sink_node)];
                std::vector<double> raw = duals.v;
                bool any_positive = false;
                for (int64_t j = 0; j < shape.nc; ++j) {
                    raw[static_cast<std::size_t>(j)] = solved.result.potential[
                        static_cast<std::size_t>(solved.layout.col_base + j)] - anchor;
                    if (raw[static_cast<std::size_t>(j)] > TOL) {
                        any_positive = true;
                        ++n_free_priced_above_anchor;
                    }
                }
                if (any_positive) {
                    const lap::CertificateReport unclamped = lap::certify_assignment(
                        cost, duals.match, duals.u, raw, TOL);
                    REQUIRE_FALSE(unclamped.certified_optimal);
                    ++n_unclamped_rejected;
                }
            }
        }
    }

    // The clamp is load-bearing rather than decorative: the solver really does
    // price free columns above the sink, and handing those raw differences to
    // the assignment certificate is rejected every time it happens.
    REQUIRE(n_free_priced_above_anchor > 0);
    REQUIRE(n_unclamped_rejected > 0);
}

TEST_CASE("Assignment duals from a flow - randomized tall, both routes",
          "[flow_certify][assignment]") {
    // The assignment LP puts the equality on the rows and so needs
    // n_rows <= n_cols. A tall instance reaches it two ways: compiled from the
    // transposed cost source, which is the orientation R/lap_certify.R
    // normalizes to, or compiled as a k-cardinality design over the original
    // orientation with k = n_cols, where every column is matched and the rows
    // carry the slack. Both are certified against the same transposed matrix.
    struct Shape { int64_t nr; int64_t nc; };
    const std::vector<Shape> shapes = {{7, 2}, {9, 4}, {12, 5}, {11, 3}, {6, 1}, {4, 4}};

    for (double lo : {0.0, -5.0}) {
        for (unsigned seed = 1; seed <= 6; ++seed) {
            for (const Shape& shape : shapes) {
                const unsigned key = seed * 977u +
                    static_cast<unsigned>(shape.nr * 17 + shape.nc);
                const lap::CostMatrix cost =
                    random_costs(shape.nr, shape.nc, key, lo, lo + 10.0);
                const lap::CostMatrix flipped = transpose(cost);
                const double optimum = reference_cost(cost);

                INFO("shape " << shape.nr << "x" << shape.nc
                     << " seed " << seed << " lo " << lo);

                {
                    const lap::SourceOracle<lap::CostMatrix> oracle(flipped);
                    SolvedDesign solved = solve_one_to_one(oracle);
                    REQUIRE(solved.result.status == "optimal");
                    REQUIRE(solved.result.total_cost == Approx(optimum).margin(1e-8));

                    const lap::AssignmentDuals duals = lap::map_assignment_duals(
                        solved.design.problem, solved.layout, solved.result.flow,
                        solved.result.potential, lap::AssignmentEquality::Rows);
                    REQUIRE(duals.ok());

                    const lap::CertificateReport rep = lap::certify_assignment(
                        flipped, duals.match, duals.u, duals.v, TOL);
                    REQUIRE(rep.certified_optimal);
                    REQUIRE(rep.primal_objective == Approx(optimum).margin(1e-8));
                }

                {
                    const lap::SourceOracle<lap::CostMatrix> oracle(cost);
                    SolvedDesign solved = solve_k_cardinality(oracle, shape.nc);
                    REQUIRE(solved.result.status == "optimal");
                    REQUIRE(solved.result.total_cost == Approx(optimum).margin(1e-8));
                    REQUIRE(lap::certify_flow(solved.design.problem, solved.result.flow,
                                              solved.result.potential, TOL).certified_optimal);

                    const lap::AssignmentDuals duals = lap::map_assignment_duals(
                        solved.design.problem, solved.layout, solved.result.flow,
                        solved.result.potential, lap::AssignmentEquality::Columns);
                    REQUIRE(duals.ok());
                    REQUIRE(static_cast<int64_t>(duals.match.size()) == shape.nc);
                    REQUIRE(static_cast<int64_t>(duals.u.size()) == shape.nc);
                    REQUIRE(static_cast<int64_t>(duals.v.size()) == shape.nr);

                    const lap::CertificateReport rep = lap::certify_assignment(
                        flipped, duals.match, duals.u, duals.v, TOL);
                    REQUIRE(rep.primal_feasible);
                    REQUIRE(rep.dual_feasible);
                    REQUIRE(rep.cs_matched_tight);
                    REQUIRE(rep.cs_unmatched_free);
                    REQUIRE(rep.n_matched == shape.nc);
                    REQUIRE(rep.primal_objective == Approx(optimum).margin(1e-8));
                    REQUIRE(rep.certified_optimal);
                }
            }
        }
    }
}

TEST_CASE("Assignment duals from a flow - a flow that is not an assignment is refused",
          "[flow_certify][assignment]") {
    const lap::CostMatrix cost = random_costs(4, 6, 17u, 0.0, 10.0);
    const lap::SourceOracle<lap::CostMatrix> oracle(cost);
    SolvedDesign solved = solve_one_to_one(oracle);

    SECTION("two block arcs out of the same row") {
        std::vector<int64_t> flow = solved.result.flow;
        const lap::BlockArcRange& blk = solved.design.problem.block_arcs[0];
        int64_t first_of_row0 = -1;
        int64_t second_of_row0 = -1;
        for (int64_t k = 0; k < blk.n_arcs; ++k) {
            if (blk.rc[static_cast<std::size_t>(k)].first != 0) continue;
            if (first_of_row0 < 0) first_of_row0 = k;
            else if (second_of_row0 < 0) second_of_row0 = k;
        }
        REQUIRE(second_of_row0 >= 0);
        flow[static_cast<std::size_t>(blk.first_arc + first_of_row0)] = 1;
        flow[static_cast<std::size_t>(blk.first_arc + second_of_row0)] = 1;

        const lap::AssignmentDuals duals = lap::map_assignment_duals(
            solved.design.problem, solved.layout, flow, solved.result.potential,
            lap::AssignmentEquality::Rows);
        REQUIRE(duals.status == lap::AssignmentMapStatus::NotAnAssignment);
        REQUIRE(duals.match.size() == 4);
        REQUIRE(duals.u.size() == 4);
        REQUIRE(duals.v.size() == 6);
    }

    SECTION("a block arc carrying more than one unit") {
        std::vector<int64_t> flow = solved.result.flow;
        const lap::BlockArcRange& blk = solved.design.problem.block_arcs[0];
        flow[static_cast<std::size_t>(blk.first_arc)] = 2;

        const lap::AssignmentDuals duals = lap::map_assignment_duals(
            solved.design.problem, solved.layout, flow, solved.result.potential,
            lap::AssignmentEquality::Rows);
        REQUIRE(duals.status == lap::AssignmentMapStatus::NotAnAssignment);
        REQUIRE(duals.match.size() == 4);
        REQUIRE(duals.u.size() == 4);
        REQUIRE(duals.v.size() == 6);
    }

    SECTION("a structural mismatch") {
        const lap::AssignmentDuals duals = lap::map_assignment_duals(
            solved.design.problem, solved.layout, solved.result.flow, {0.0, 0.0},
            lap::AssignmentEquality::Rows);
        REQUIRE(duals.status == lap::AssignmentMapStatus::StructuralMismatch);
        REQUIRE(duals.match.empty());
        REQUIRE(duals.u.empty());
        REQUIRE(duals.v.empty());
    }
}
