// Test suite for the design compilers and the lowering predicate.

#include <catch2/catch_test_macros.hpp>
#include <catch2/catch_approx.hpp>

#include "core/lap_error.h"
#include "core/lap_types.h"
#include "flow/flow_compile.h"
#include "flow/flow_oracle.h"
#include "flow/flow_problem.h"
#include "flow/flow_solve.h"

#include <vector>

using Catch::Approx;
using lap::CategoryConstraint;

static lap::CostMatrix make_cost(std::initializer_list<std::initializer_list<double>> rows) {
    std::vector<std::vector<double>> data;
    for (const auto& row : rows) {
        data.push_back(std::vector<double>(row));
    }
    return lap::CostMatrix(data);
}

static const std::vector<CategoryConstraint> NO_CATEGORIES;

// lap::validate() states the structural contract; the conditions are repeated
// here one at a time so a failure names the invariant that broke rather than
// only the problem that broke it.
static bool ranges_overlap(int64_t a0, int64_t a_len, int64_t b0, int64_t b_len) {
    return a0 < b0 + b_len && b0 < a0 + a_len;
}

static void require_well_formed(const lap::FlowProblem& prob) {
    REQUIRE_NOTHROW(lap::validate(prob));
    REQUIRE(prob.supply.size() == static_cast<std::size_t>(prob.n_nodes));

    int64_t total_supply = 0;
    for (int64_t s : prob.supply) total_supply += s;
    REQUIRE(total_supply == 0);

    for (const lap::FlowArc& arc : prob.arcs) {
        REQUIRE(arc.tail >= 0);
        REQUIRE(arc.tail < prob.n_nodes);
        REQUIRE(arc.head >= 0);
        REQUIRE(arc.head < prob.n_nodes);
        REQUIRE(arc.lower >= 0);
        REQUIRE(arc.lower <= arc.upper);
    }

    for (std::size_t p = 0; p < prob.blocks.size(); ++p) {
        const lap::BipartiteBlock& bp = prob.blocks[p];
        REQUIRE(bp.costs != nullptr);
        REQUIRE(bp.lower >= 0);
        REQUIRE(bp.lower <= bp.upper);
        REQUIRE(bp.row_base >= 0);
        REQUIRE(bp.row_base + bp.costs->nrow() <= prob.n_nodes);
        REQUIRE(bp.col_base >= 0);
        REQUIRE(bp.col_base + bp.costs->ncol() <= prob.n_nodes);

        for (std::size_t q = p + 1; q < prob.blocks.size(); ++q) {
            const lap::BipartiteBlock& bq = prob.blocks[q];
            REQUIRE_FALSE(ranges_overlap(bp.row_base, bp.costs->nrow(),
                                         bq.row_base, bq.costs->nrow()));
            REQUIRE_FALSE(ranges_overlap(bp.col_base, bp.costs->ncol(),
                                         bq.col_base, bq.costs->ncol()));
        }
    }
}

static int64_t count_arcs_into_sink(const lap::FlowProblem& prob) {
    int64_t n = 0;
    for (const lap::FlowArc& arc : prob.arcs) {
        if (arc.head == lap::FLOW_SINK) ++n;
    }
    return n;
}

static int64_t count_arcs_out_of_source(const lap::FlowProblem& prob) {
    int64_t n = 0;
    for (const lap::FlowArc& arc : prob.arcs) {
        if (arc.tail == lap::FLOW_SOURCE) ++n;
    }
    return n;
}

// ---------------------------------------------------------------------------
// 1:1 matching
// ---------------------------------------------------------------------------

TEST_CASE("compile_one_to_one - unit capacity bipartite", "[flow][compile][one_to_one]") {
    auto cost = make_cost({
        {1, 2, 3, 4},
        {5, 6, 7, 8},
        {9, 10, 11, 12}
    });
    lap::SourceOracle<lap::CostMatrix> oracle(cost);

    lap::CompiledDesign design = lap::compile_one_to_one(oracle, NO_CATEGORIES);
    const lap::FlowProblem& prob = design.problem;

    require_well_formed(prob);

    SECTION("node layout") {
        REQUIRE(prob.n_nodes == 2 + 3 + 4);
        REQUIRE(design.row_base == lap::FLOW_FIRST_ROW);
        REQUIRE(design.n_rows == 3);
        REQUIRE(design.col_base == lap::FLOW_FIRST_ROW + 3);
        REQUIRE(design.n_cols == 4);
    }

    SECTION("supplies") {
        REQUIRE(prob.supply[lap::FLOW_SOURCE] == 0);
        REQUIRE(prob.supply[lap::FLOW_SINK] == -3);
        for (int i = 0; i < 3; ++i) REQUIRE(prob.supply[design.row_base + i] == 1);
        for (int j = 0; j < 4; ++j) REQUIRE(prob.supply[design.col_base + j] == 0);
        REQUIRE(design.flow_required == 3);
    }

    SECTION("arcs and capacities") {
        REQUIRE(prob.arcs.size() == 4);
        REQUIRE(count_arcs_out_of_source(prob) == 0);
        REQUIRE(count_arcs_into_sink(prob) == 4);
        for (std::size_t j = 0; j < prob.arcs.size(); ++j) {
            REQUIRE(prob.arcs[j].tail == design.col_base + static_cast<int32_t>(j));
            REQUIRE(prob.arcs[j].head == lap::FLOW_SINK);
            REQUIRE(prob.arcs[j].lower == 0);
            REQUIRE(prob.arcs[j].upper == 1);
            REQUIRE(prob.arcs[j].cost == 0.0);
        }
    }

    SECTION("one unit-capacity block") {
        REQUIRE(prob.blocks.size() == 1);
        REQUIRE(prob.blocks[0].lower == 0);
        REQUIRE(prob.blocks[0].upper == 1);
        REQUIRE(prob.blocks[0].row_base == design.row_base);
        REQUIRE(prob.blocks[0].col_base == design.col_base);
        REQUIRE(prob.blocks[0].costs->nrow() == 3);
        REQUIRE(prob.blocks[0].costs->ncol() == 4);
    }

    SECTION("index maps are the identity") {
        REQUIRE(design.row_unit == std::vector<int32_t>{0, 1, 2});
        REQUIRE(design.col_unit == std::vector<int32_t>{0, 1, 2, 3});
    }
}

TEST_CASE("compile_one_to_one - tall instance keeps the row-matched LP",
          "[flow][compile][one_to_one]") {
    auto cost = make_cost({{1, 2}, {3, 4}, {5, 6}});
    lap::SourceOracle<lap::CostMatrix> oracle(cost);

    lap::CompiledDesign design = lap::compile_one_to_one(oracle, NO_CATEGORIES);
    require_well_formed(design.problem);

    // Three rows demand three pairs, two columns can carry two: a maximum flow
    // falls short and the design is a partial one, which is a solver status and
    // not a malformed problem.
    REQUIRE(design.flow_required == 3);
    REQUIRE(design.problem.supply[lap::FLOW_SINK] == -3);
    REQUIRE(count_arcs_into_sink(design.problem) == 2);
    REQUIRE(lap::is_unit_capacity_assignment(design.problem));
}

// ---------------------------------------------------------------------------
// k:1 fixed ratio
// ---------------------------------------------------------------------------

TEST_CASE("compile_fixed_ratio - replicated rows", "[flow][compile][ratio]") {
    auto cost = make_cost({
        {1, 2, 3, 4},
        {5, 6, 7, 8},
        {9, 10, 11, 12}
    });
    lap::SourceOracle<lap::CostMatrix> oracle(cost);

    lap::CompiledDesign design = lap::compile_fixed_ratio(oracle, 2, NO_CATEGORIES);
    const lap::FlowProblem& prob = design.problem;

    require_well_formed(prob);

    SECTION("node counts follow the replication") {
        REQUIRE(design.n_rows == 6);
        REQUIRE(design.n_cols == 4);
        REQUIRE(prob.n_nodes == 2 + 6 + 4);
        REQUIRE(design.col_base == lap::FLOW_FIRST_ROW + 6);
    }

    SECTION("every replica must be matched") {
        for (int e = 0; e < 6; ++e) REQUIRE(prob.supply[design.row_base + e] == 1);
        REQUIRE(prob.supply[lap::FLOW_SINK] == -6);
        REQUIRE(prob.supply[lap::FLOW_SOURCE] == 0);
        REQUIRE(design.flow_required == 6);
    }

    SECTION("columns stay unit capacity") {
        REQUIRE(prob.arcs.size() == 4);
        for (const lap::FlowArc& arc : prob.arcs) REQUIRE(arc.upper == 1);
    }

    SECTION("row map carries the replication") {
        REQUIRE(design.row_unit == std::vector<int32_t>{0, 0, 1, 1, 2, 2});
    }

    SECTION("the block reads the replicated matrix R builds") {
        const lap::CostOracle* src = prob.blocks[0].costs;
        REQUIRE(src->nrow() == 6);
        REQUIRE(src->ncol() == 4);
        for (int64_t e = 0; e < 6; ++e) {
            for (int64_t j = 0; j < 4; ++j) {
                REQUIRE(src->at(e, j) == Approx(cost.at(e / 2, j)));
                REQUIRE(src->allowed(e, j) == cost.allowed(e / 2, j));
            }
        }
    }
}

TEST_CASE("compile_fixed_ratio - ratio 1 is the 1:1 design", "[flow][compile][ratio]") {
    auto cost = make_cost({{1, 2, 3}, {4, 5, 6}});
    lap::SourceOracle<lap::CostMatrix> oracle(cost);

    lap::CompiledDesign one = lap::compile_one_to_one(oracle, NO_CATEGORIES);
    lap::CompiledDesign k1  = lap::compile_fixed_ratio(oracle, 1, NO_CATEGORIES);

    REQUIRE(k1.problem.n_nodes == one.problem.n_nodes);
    REQUIRE(k1.problem.arcs.size() == one.problem.arcs.size());
    REQUIRE(k1.problem.supply == one.problem.supply);
    REQUIRE(k1.row_unit == one.row_unit);
    REQUIRE(k1.flow_required == one.flow_required);
}

TEST_CASE("compile_fixed_ratio - ratio below one is rejected", "[flow][compile][ratio]") {
    auto cost = make_cost({{1, 2}, {3, 4}});
    lap::SourceOracle<lap::CostMatrix> oracle(cost);
    REQUIRE_THROWS_AS(lap::compile_fixed_ratio(oracle, 0, NO_CATEGORIES),
                      lap::DimensionException);
}

// ---------------------------------------------------------------------------
// with replacement
// ---------------------------------------------------------------------------

TEST_CASE("compile_with_replacement - columns carry n_rows", "[flow][compile][replace]") {
    auto cost = make_cost({
        {1, 2, 3, 4},
        {5, 6, 7, 8},
        {9, 10, 11, 12}
    });
    lap::SourceOracle<lap::CostMatrix> oracle(cost);

    lap::CompiledDesign design = lap::compile_with_replacement(oracle, 2, NO_CATEGORIES);
    const lap::FlowProblem& prob = design.problem;

    require_well_formed(prob);

    REQUIRE(prob.n_nodes == 2 + 3 + 4);
    for (int i = 0; i < 3; ++i) REQUIRE(prob.supply[design.row_base + i] == 2);
    REQUIRE(prob.supply[lap::FLOW_SINK] == -6);
    REQUIRE(design.flow_required == 6);

    REQUIRE(prob.arcs.size() == 4);
    for (const lap::FlowArc& arc : prob.arcs) {
        REQUIRE(arc.head == lap::FLOW_SINK);
        REQUIRE(arc.lower == 0);
        REQUIRE(arc.upper == 3);
    }

    REQUIRE(prob.blocks.size() == 1);
    REQUIRE(prob.blocks[0].upper == 1);
}

TEST_CASE("compile_with_replacement - ratio clamps to the column count",
          "[flow][compile][replace]") {
    auto cost = make_cost({{1, 2}, {3, 4}});
    lap::SourceOracle<lap::CostMatrix> oracle(cost);

    lap::CompiledDesign design = lap::compile_with_replacement(oracle, 5, NO_CATEGORIES);
    require_well_formed(design.problem);

    for (int i = 0; i < 2; ++i) REQUIRE(design.problem.supply[design.row_base + i] == 2);
    REQUIRE(design.flow_required == 4);
}

// ---------------------------------------------------------------------------
// variable ratio
// ---------------------------------------------------------------------------

TEST_CASE("compile_variable_ratio - source arcs carry the row range",
          "[flow][compile][variable_ratio]") {
    lap::CostMatrix cost(3, 10);
    lap::SourceOracle<lap::CostMatrix> oracle(cost);

    lap::CompiledDesign design = lap::compile_variable_ratio(oracle, 1, 3, NO_CATEGORIES);
    const lap::FlowProblem& prob = design.problem;

    require_well_formed(prob);

    REQUIRE(prob.n_nodes == 2 + 3 + 10);
    REQUIRE(prob.arcs.size() == 3 + 10);
    REQUIRE(count_arcs_out_of_source(prob) == 3);
    REQUIRE(count_arcs_into_sink(prob) == 10);

    for (int i = 0; i < 3; ++i) {
        const lap::FlowArc& arc = prob.arcs[i];
        REQUIRE(arc.tail == lap::FLOW_SOURCE);
        REQUIRE(arc.head == design.row_base + i);
        REQUIRE(arc.lower == 1);
        REQUIRE(arc.upper == 3);
        REQUIRE(prob.supply[design.row_base + i] == 0);
    }

    // Nine pairs is what three rows at three columns each can hold, and it is
    // below the ten columns available.
    REQUIRE(prob.supply[lap::FLOW_SOURCE] == 9);
    REQUIRE(prob.supply[lap::FLOW_SINK] == -9);
    REQUIRE(design.flow_required == 9);
}

TEST_CASE("compile_variable_ratio - unbounded upper stops at the column count",
          "[flow][compile][variable_ratio]") {
    lap::CostMatrix cost(3, 10);
    lap::SourceOracle<lap::CostMatrix> oracle(cost);

    lap::CompiledDesign design =
        lap::compile_variable_ratio(oracle, 1, lap::FLOW_INF_CAP, NO_CATEGORIES);
    require_well_formed(design.problem);

    REQUIRE(design.problem.arcs[0].upper == 10);
    REQUIRE(design.flow_required == 10);
    REQUIRE(design.problem.supply[lap::FLOW_SOURCE] == 10);
}

TEST_CASE("compile_variable_ratio - rejects bounds no arc can hold",
          "[flow][compile][variable_ratio]") {
    lap::CostMatrix cost(3, 2);
    lap::SourceOracle<lap::CostMatrix> oracle(cost);

    REQUIRE_THROWS_AS(lap::compile_variable_ratio(oracle, 3, 4, NO_CATEGORIES),
                      lap::DimensionException);
    REQUIRE_THROWS_AS(lap::compile_variable_ratio(oracle, 2, 1, NO_CATEGORIES),
                      lap::DimensionException);
}

// ---------------------------------------------------------------------------
// k-cardinality subset
// ---------------------------------------------------------------------------

TEST_CASE("compile_k_cardinality - the source meters the pair count",
          "[flow][compile][cardinality]") {
    lap::CostMatrix cost(3, 4);
    lap::SourceOracle<lap::CostMatrix> oracle(cost);

    lap::CompiledDesign design = lap::compile_k_cardinality(oracle, 2, NO_CATEGORIES);
    const lap::FlowProblem& prob = design.problem;

    require_well_formed(prob);

    REQUIRE(prob.n_nodes == 2 + 3 + 4);
    REQUIRE(prob.arcs.size() == 3 + 4);
    REQUIRE(count_arcs_out_of_source(prob) == 3);
    for (int i = 0; i < 3; ++i) {
        REQUIRE(prob.arcs[i].lower == 0);
        REQUIRE(prob.arcs[i].upper == 1);
    }
    REQUIRE(prob.supply[lap::FLOW_SOURCE] == 2);
    REQUIRE(prob.supply[lap::FLOW_SINK] == -2);
    for (int i = 0; i < 3; ++i) REQUIRE(prob.supply[design.row_base + i] == 0);
    REQUIRE(design.flow_required == 2);
}

TEST_CASE("compile_k_cardinality - k above the pair ceiling is rejected",
          "[flow][compile][cardinality]") {
    lap::CostMatrix cost(3, 4);
    lap::SourceOracle<lap::CostMatrix> oracle(cost);

    REQUIRE_THROWS_AS(lap::compile_k_cardinality(oracle, 4, NO_CATEGORIES),
                      lap::DimensionException);
    REQUIRE_THROWS_AS(lap::compile_k_cardinality(oracle, -1, NO_CATEGORIES),
                      lap::DimensionException);
    REQUIRE_NOTHROW(lap::compile_k_cardinality(oracle, 3, NO_CATEGORIES));
}

// ---------------------------------------------------------------------------
// full matching
// ---------------------------------------------------------------------------

TEST_CASE("compile_full_matching - centres are the left side when it is smaller",
          "[flow][compile][full_match]") {
    lap::CostMatrix cost(3, 8);
    for (int64_t i = 0; i < 3; ++i) {
        for (int64_t j = 0; j < 8; ++j) cost.at(i, j) = static_cast<double>(i * 8 + j);
    }
    lap::SourceOracle<lap::CostMatrix> oracle(cost);

    lap::CompiledFullMatch full =
        lap::compile_full_matching(oracle, 1, lap::FLOW_INF_CAP, NO_CATEGORIES);

    REQUIRE(full.bounds_feasible);
    REQUIRE_FALSE(full.transposed);
    REQUIRE(full.n_centres == 3);
    REQUIRE(full.n_units == 8);
    REQUIRE(full.max_capacity == 8);

    const lap::FlowProblem& prob = full.design.problem;
    require_well_formed(prob);

    REQUIRE(prob.n_nodes == 2 + 3 + 8);
    REQUIRE(prob.arcs.size() == 3 + 8);
    for (int i = 0; i < 3; ++i) {
        REQUIRE(prob.arcs[i].tail == lap::FLOW_SOURCE);
        REQUIRE(prob.arcs[i].lower == 1);
        REQUIRE(prob.arcs[i].upper == 8);
    }
    for (int j = 0; j < 8; ++j) {
        const lap::FlowArc& arc = prob.arcs[3 + j];
        REQUIRE(arc.head == lap::FLOW_SINK);
        REQUIRE(arc.lower == 0);
        REQUIRE(arc.upper == 1);
    }
    REQUIRE(prob.supply[lap::FLOW_SOURCE] == 8);
    REQUIRE(prob.supply[lap::FLOW_SINK] == -8);
    REQUIRE(full.design.flow_required == 8);

    REQUIRE(prob.blocks[0].costs->nrow() == 3);
    REQUIRE(prob.blocks[0].costs->ncol() == 8);
    REQUIRE(prob.blocks[0].costs->at(1, 5) == Approx(cost.at(1, 5)));
}

TEST_CASE("compile_full_matching - orientation transposes when left is larger",
          "[flow][compile][full_match]") {
    lap::CostMatrix cost(8, 3);
    for (int64_t i = 0; i < 8; ++i) {
        for (int64_t j = 0; j < 3; ++j) cost.at(i, j) = static_cast<double>(i * 3 + j);
    }
    lap::SourceOracle<lap::CostMatrix> oracle(cost);

    lap::CompiledFullMatch full = lap::compile_full_matching(oracle, 1, 5, NO_CATEGORIES);

    REQUIRE(full.bounds_feasible);
    REQUIRE(full.transposed);
    REQUIRE(full.n_centres == 3);
    REQUIRE(full.n_units == 8);
    REQUIRE(full.max_capacity == 5);

    const lap::FlowProblem& prob = full.design.problem;
    require_well_formed(prob);

    REQUIRE(prob.n_nodes == 2 + 3 + 8);
    REQUIRE(full.design.n_rows == 3);
    REQUIRE(full.design.n_cols == 8);

    // Row nodes carry right units, column nodes carry left units, so the block
    // reads the transpose of the caller's matrix.
    const lap::CostOracle* src = prob.blocks[0].costs;
    REQUIRE(src->nrow() == 3);
    REQUIRE(src->ncol() == 8);
    for (int64_t i = 0; i < 3; ++i) {
        for (int64_t j = 0; j < 8; ++j) {
            REQUIRE(src->at(i, j) == Approx(cost.at(j, i)));
            REQUIRE(src->allowed(i, j) == cost.allowed(j, i));
        }
    }

    for (int i = 0; i < 3; ++i) REQUIRE(prob.arcs[i].upper == 5);
}

TEST_CASE("compile_full_matching - capacity clamps to the unit count",
          "[flow][compile][full_match]") {
    lap::CostMatrix cost(2, 3);
    lap::SourceOracle<lap::CostMatrix> oracle(cost);

    lap::CompiledFullMatch full = lap::compile_full_matching(oracle, 1, 99, NO_CATEGORIES);

    REQUIRE(full.bounds_feasible);
    REQUIRE(full.max_capacity == 3);
    REQUIRE(full.design.problem.arcs[0].upper == 3);
}

TEST_CASE("compile_full_matching - bounds decided before any arc exists",
          "[flow][compile][full_match]") {
    SECTION("an empty side") {
        lap::CostMatrix cost(0, 5);
        lap::SourceOracle<lap::CostMatrix> oracle(cost);
        lap::CompiledFullMatch full = lap::compile_full_matching(oracle, 1, 5, NO_CATEGORIES);
        REQUIRE_FALSE(full.bounds_feasible);
        REQUIRE_FALSE(full.reason.empty());
        REQUIRE(full.design.problem.n_nodes == 0);
    }

    SECTION("max_controls below min_controls once clamped") {
        lap::CostMatrix cost(3, 8);
        lap::SourceOracle<lap::CostMatrix> oracle(cost);
        lap::CompiledFullMatch full = lap::compile_full_matching(oracle, 3, 2, NO_CATEGORIES);
        REQUIRE_FALSE(full.bounds_feasible);
        REQUIRE(full.design.problem.n_nodes == 0);
    }

    SECTION("fewer units than min_controls per centre") {
        lap::CostMatrix cost(3, 8);
        lap::SourceOracle<lap::CostMatrix> oracle(cost);
        lap::CompiledFullMatch full = lap::compile_full_matching(oracle, 3, 8, NO_CATEGORIES);
        REQUIRE_FALSE(full.bounds_feasible);
        REQUIRE(full.design.problem.n_nodes == 0);

        // Nine mandatory placements against eight units; one fewer centre and
        // the same bounds fit.
        lap::CostMatrix smaller(2, 8);
        lap::SourceOracle<lap::CostMatrix> smaller_oracle(smaller);
        lap::CompiledFullMatch ok =
            lap::compile_full_matching(smaller_oracle, 3, 8, NO_CATEGORIES);
        REQUIRE(ok.bounds_feasible);
        REQUIRE(ok.design.problem.arcs[0].lower == 3);
    }

    SECTION("min_controls below one is a caller error") {
        lap::CostMatrix cost(3, 8);
        lap::SourceOracle<lap::CostMatrix> oracle(cost);
        REQUIRE_THROWS_AS(lap::compile_full_matching(oracle, 0, 8, NO_CATEGORIES),
                          lap::DimensionException);
    }
}

TEST_CASE("compile_full_matching - min_controls above one raises the arc lower bound",
          "[flow][compile][full_match]") {
    lap::CostMatrix cost(3, 8);
    lap::SourceOracle<lap::CostMatrix> oracle(cost);

    lap::CompiledFullMatch full = lap::compile_full_matching(oracle, 2, 8, NO_CATEGORIES);
    require_well_formed(full.design.problem);

    REQUIRE(full.bounds_feasible);
    for (int i = 0; i < 3; ++i) {
        REQUIRE(full.design.problem.arcs[i].lower == 2);
        REQUIRE(full.design.problem.arcs[i].upper == 8);
    }
}

// ---------------------------------------------------------------------------
// exact / blocked
// ---------------------------------------------------------------------------

TEST_CASE("compile_blocked - one network per stratum in one problem",
          "[flow][compile][blocked]") {
    auto first  = make_cost({{1, 2, 3}, {4, 5, 6}});
    auto second = make_cost({{7, 8}, {9, 10}, {11, 12}});
    lap::SourceOracle<lap::CostMatrix> first_oracle(first);
    lap::SourceOracle<lap::CostMatrix> second_oracle(second);

    std::vector<lap::Stratum> strata(2);
    strata[0].costs    = &first_oracle;
    strata[0].row_unit = {0, 2};
    strata[0].col_unit = {1, 3, 4};
    strata[1].costs    = &second_oracle;
    strata[1].row_unit = {1, 3, 4};
    strata[1].col_unit = {0, 2};

    lap::CompiledBlocked blocked = lap::compile_blocked(strata, NO_CATEGORIES);
    const lap::FlowProblem& prob = blocked.design.problem;

    require_well_formed(prob);

    SECTION("rows are contiguous, then columns") {
        REQUIRE(prob.n_nodes == 2 + 5 + 5);
        REQUIRE(blocked.design.row_base == lap::FLOW_FIRST_ROW);
        REQUIRE(blocked.design.n_rows == 5);
        REQUIRE(blocked.design.col_base == lap::FLOW_FIRST_ROW + 5);
        REQUIRE(blocked.design.n_cols == 5);

        REQUIRE(blocked.strata[0].row_base == 2);
        REQUIRE(blocked.strata[0].n_rows == 2);
        REQUIRE(blocked.strata[1].row_base == 4);
        REQUIRE(blocked.strata[1].n_rows == 3);
        REQUIRE(blocked.strata[0].col_base == 7);
        REQUIRE(blocked.strata[0].n_cols == 3);
        REQUIRE(blocked.strata[1].col_base == 10);
        REQUIRE(blocked.strata[1].n_cols == 2);
    }

    SECTION("every stratum is unit capacity on both sides") {
        REQUIRE(prob.blocks.size() == 2);
        for (const lap::BipartiteBlock& blk : prob.blocks) {
            REQUIRE(blk.lower == 0);
            REQUIRE(blk.upper == 1);
        }
        REQUIRE(prob.arcs.size() == 5);
        for (const lap::FlowArc& arc : prob.arcs) {
            REQUIRE(arc.head == lap::FLOW_SINK);
            REQUIRE(arc.upper == 1);
        }
        for (int r = 0; r < 5; ++r) REQUIRE(prob.supply[2 + r] == 1);
        REQUIRE(prob.supply[lap::FLOW_SINK] == -5);
        REQUIRE(blocked.design.flow_required == 5);
    }

    SECTION("index maps concatenate the strata") {
        REQUIRE(blocked.design.row_unit == std::vector<int32_t>{0, 2, 1, 3, 4});
        REQUIRE(blocked.design.col_unit == std::vector<int32_t>{1, 3, 4, 0, 2});
    }
}

TEST_CASE("compile_blocked - index maps must match their cost source",
          "[flow][compile][blocked]") {
    auto only = make_cost({{1, 2}, {3, 4}});
    lap::SourceOracle<lap::CostMatrix> oracle(only);

    std::vector<lap::Stratum> strata(1);
    strata[0].costs    = &oracle;
    strata[0].row_unit = {0};
    strata[0].col_unit = {0, 1};

    REQUIRE_THROWS_AS(lap::compile_blocked(strata, NO_CATEGORIES), lap::DimensionException);
}

TEST_CASE("compile_blocked - no stratum is an empty problem", "[flow][compile][blocked]") {
    std::vector<lap::Stratum> strata;
    lap::CompiledBlocked blocked = lap::compile_blocked(strata, NO_CATEGORIES);

    require_well_formed(blocked.design.problem);
    REQUIRE(blocked.design.problem.n_nodes == 2);
    REQUIRE(blocked.design.problem.arcs.empty());
    REQUIRE(blocked.design.problem.blocks.empty());
    REQUIRE(blocked.design.flow_required == 0);
}

// ---------------------------------------------------------------------------
// category nodes are a reserved hook, not a feature
// ---------------------------------------------------------------------------

TEST_CASE("category constraints are refused by every compiler", "[flow][compile][category]") {
    lap::CostMatrix cost(3, 4);
    lap::SourceOracle<lap::CostMatrix> oracle(cost);

    std::vector<CategoryConstraint> categories(1);
    categories[0].side    = CategoryConstraint::Side::Column;
    categories[0].members = {0, 1};
    categories[0].lower   = 1;
    categories[0].upper   = 2;

    REQUIRE_THROWS_AS(lap::compile_one_to_one(oracle, categories), lap::LapException);
    REQUIRE_THROWS_AS(lap::compile_fixed_ratio(oracle, 2, categories), lap::LapException);
    REQUIRE_THROWS_AS(lap::compile_with_replacement(oracle, 2, categories), lap::LapException);
    REQUIRE_THROWS_AS(lap::compile_variable_ratio(oracle, 1, 2, categories), lap::LapException);
    REQUIRE_THROWS_AS(lap::compile_k_cardinality(oracle, 2, categories), lap::LapException);
    REQUIRE_THROWS_AS(lap::compile_full_matching(oracle, 1, 4, categories), lap::LapException);
    REQUIRE_THROWS_AS(lap::compile_blocked(std::vector<lap::Stratum>(), categories),
                      lap::LapException);
}

// ---------------------------------------------------------------------------
// the lowering predicate
// ---------------------------------------------------------------------------

TEST_CASE("is_unit_capacity_assignment - true exactly on the assignment designs",
          "[flow][compile][lowering]") {
    auto cost = make_cost({
        {1, 2, 3, 4},
        {5, 6, 7, 8},
        {9, 10, 11, 12}
    });
    lap::SourceOracle<lap::CostMatrix> oracle(cost);

    SECTION("1:1 lowers") {
        lap::CompiledDesign design = lap::compile_one_to_one(oracle, NO_CATEGORIES);
        REQUIRE(lap::is_unit_capacity_assignment(design.problem));
    }

    SECTION("k:1 lowers, as the replicated assignment") {
        lap::CompiledDesign design = lap::compile_fixed_ratio(oracle, 2, NO_CATEGORIES);
        REQUIRE(lap::is_unit_capacity_assignment(design.problem));
    }

    SECTION("replacement does not lower: its columns are not unit capacity") {
        lap::CompiledDesign design = lap::compile_with_replacement(oracle, 1, NO_CATEGORIES);
        REQUIRE(design.problem.supply[design.row_base] == 1);
        REQUIRE(design.problem.blocks[0].upper == 1);
        REQUIRE(design.problem.arcs[0].upper == 3);
        REQUIRE_FALSE(lap::is_unit_capacity_assignment(design.problem));
    }

    SECTION("variable ratio does not lower") {
        lap::CompiledDesign design = lap::compile_variable_ratio(oracle, 1, 2, NO_CATEGORIES);
        REQUIRE_FALSE(lap::is_unit_capacity_assignment(design.problem));
    }

    SECTION("k-cardinality does not lower") {
        lap::CompiledDesign design = lap::compile_k_cardinality(oracle, 2, NO_CATEGORIES);
        REQUIRE_FALSE(lap::is_unit_capacity_assignment(design.problem));
    }

    SECTION("full matching does not lower, at either lower bound") {
        lap::CompiledFullMatch one =
            lap::compile_full_matching(oracle, 1, lap::FLOW_INF_CAP, NO_CATEGORIES);
        REQUIRE_FALSE(lap::is_unit_capacity_assignment(one.design.problem));

        lap::CostMatrix wider(3, 8);
        lap::SourceOracle<lap::CostMatrix> wider_oracle(wider);
        lap::CompiledFullMatch two = lap::compile_full_matching(wider_oracle, 2, 4, NO_CATEGORIES);
        REQUIRE(two.bounds_feasible);
        REQUIRE(two.design.problem.arcs[0].lower == 2);
        REQUIRE_FALSE(lap::is_unit_capacity_assignment(two.design.problem));
    }

    SECTION("a blocked problem does not lower: it holds more than one block") {
        std::vector<lap::Stratum> strata(2);
        strata[0].costs    = &oracle;
        strata[0].row_unit = {0, 1, 2};
        strata[0].col_unit = {0, 1, 2, 3};
        strata[1].costs    = &oracle;
        strata[1].row_unit = {3, 4, 5};
        strata[1].col_unit = {4, 5, 6, 7};

        lap::CompiledBlocked blocked = lap::compile_blocked(strata, NO_CATEGORIES);
        REQUIRE(blocked.design.problem.blocks.size() == 2);
        REQUIRE_FALSE(lap::is_unit_capacity_assignment(blocked.design.problem));
    }

    SECTION("a single stratum lowers like the 1:1 design it is") {
        std::vector<lap::Stratum> strata(1);
        strata[0].costs    = &oracle;
        strata[0].row_unit = {0, 1, 2};
        strata[0].col_unit = {0, 1, 2, 3};

        lap::CompiledBlocked blocked = lap::compile_blocked(strata, NO_CATEGORIES);
        REQUIRE(lap::is_unit_capacity_assignment(blocked.design.problem));
    }

    SECTION("an expanded problem does not lower") {
        lap::CompiledDesign design = lap::compile_one_to_one(oracle, NO_CATEGORIES);
        design.problem.expanded = true;
        REQUIRE_FALSE(lap::is_unit_capacity_assignment(design.problem));
    }

    SECTION("a category node above the column block blocks lowering") {
        lap::CompiledDesign design = lap::compile_one_to_one(oracle, NO_CATEGORIES);
        design.problem.n_nodes += 1;
        design.problem.supply.push_back(0);
        REQUIRE_FALSE(lap::is_unit_capacity_assignment(design.problem));
    }
}

TEST_CASE("lower_to_assignment - carries the cost source R would solve",
          "[flow][compile][lowering]") {
    lap::CostMatrix cost(3, 4);
    for (int64_t i = 0; i < 3; ++i) {
        for (int64_t j = 0; j < 4; ++j) cost.at(i, j) = 1.0 + static_cast<double>(i) * 4.0 + j;
    }
    cost.forbid(1, 2);
    cost.forbid(2, 0);
    lap::SourceOracle<lap::CostMatrix> oracle(cost);

    SECTION("1:1 lowers to the caller's own matrix") {
        lap::CompiledDesign design = lap::compile_one_to_one(oracle, NO_CATEGORIES);
        lap::LoweredAssignment low = lap::lower_to_assignment(design);

        REQUIRE(low.valid);
        REQUIRE(low.n_rows == 3);
        REQUIRE(low.n_cols == 4);
        REQUIRE(low.row_base == lap::FLOW_FIRST_ROW);
        REQUIRE(low.col_base == lap::FLOW_FIRST_ROW + 3);
        REQUIRE(low.row_unit == std::vector<int32_t>{0, 1, 2});
        REQUIRE(low.col_unit == std::vector<int32_t>{0, 1, 2, 3});

        for (int64_t i = 0; i < 3; ++i) {
            for (int64_t j = 0; j < 4; ++j) {
                REQUIRE(low.costs->allowed(i, j) == cost.allowed(i, j));
                if (cost.allowed(i, j)) {
                    REQUIRE(low.costs->at(i, j) == Approx(cost.at(i, j)));
                }
            }
        }
    }

    SECTION("k:1 lowers to the replicated matrix, with the map back") {
        lap::CompiledDesign design = lap::compile_fixed_ratio(oracle, 3, NO_CATEGORIES);
        lap::LoweredAssignment low = lap::lower_to_assignment(design);

        REQUIRE(low.valid);
        REQUIRE(low.n_rows == 9);
        REQUIRE(low.n_cols == 4);
        REQUIRE(low.row_unit ==
                std::vector<int32_t>{0, 0, 0, 1, 1, 1, 2, 2, 2});

        for (int64_t e = 0; e < 9; ++e) {
            for (int64_t j = 0; j < 4; ++j) {
                REQUIRE(low.costs->allowed(e, j) == cost.allowed(e / 3, j));
                if (cost.allowed(e / 3, j)) {
                    REQUIRE(low.costs->at(e, j) == Approx(cost.at(e / 3, j)));
                }
            }
        }
    }

    SECTION("a design that does not lower returns an invalid form") {
        lap::CompiledDesign design = lap::compile_k_cardinality(oracle, 2, NO_CATEGORIES);
        lap::LoweredAssignment low = lap::lower_to_assignment(design);

        REQUIRE_FALSE(low.valid);
        REQUIRE(low.costs == nullptr);
        REQUIRE(low.n_rows == 0);
    }
}

TEST_CASE("lower_to_assignment - the bare problem knows only its block order",
          "[flow][compile][lowering]") {
    lap::CostMatrix cost(2, 3);
    lap::SourceOracle<lap::CostMatrix> oracle(cost);

    lap::CompiledDesign design = lap::compile_fixed_ratio(oracle, 2, NO_CATEGORIES);
    lap::LoweredAssignment low = lap::lower_to_assignment(design.problem);

    REQUIRE(low.valid);
    REQUIRE(low.n_rows == 4);
    REQUIRE(low.row_unit == std::vector<int32_t>{0, 1, 2, 3});
}

// ---------------------------------------------------------------------------
// what each compiled design optimizes
// ---------------------------------------------------------------------------

TEST_CASE("compiled 1:1 solves to the assignment optimum", "[flow][compile][solve]") {
    auto cost = make_cost({
        {10, 1, 20, 30, 40},
        {50, 60, 2, 70, 80},
        {90, 100, 110, 3, 120}
    });
    lap::SourceOracle<lap::CostMatrix> oracle(cost);

    lap::CompiledDesign design = lap::compile_one_to_one(oracle, NO_CATEGORIES);
    lap::FlowResult res = lap::solve_min_cost_flow(design.problem);

    REQUIRE(res.status == "optimal");
    REQUIRE(res.flow_required == design.flow_required);
    REQUIRE(res.flow_sent == 3);
    REQUIRE(res.total_cost == Approx(6.0));
}

TEST_CASE("compiled k:1 solves to the replicated optimum", "[flow][compile][solve]") {
    // Each row takes two columns; row 0's two cheapest are 1 and 2, row 1's are
    // 3 and 4, and no column is shared.
    auto cost = make_cost({
        {1, 2, 50, 60},
        {70, 80, 3, 4}
    });
    lap::SourceOracle<lap::CostMatrix> oracle(cost);

    lap::CompiledDesign design = lap::compile_fixed_ratio(oracle, 2, NO_CATEGORIES);
    lap::FlowResult res = lap::solve_min_cost_flow(design.problem);

    REQUIRE(res.status == "optimal");
    REQUIRE(res.flow_sent == 4);
    REQUIRE(res.total_cost == Approx(10.0));
}

TEST_CASE("compiled replacement lets both rows take the same column",
          "[flow][compile][solve]") {
    // The assignment optimum here is 2 + 1 = 3, because one of the rows has to
    // give up the cheap column. Replacement is 1 + 1 = 2, which is what
    // R/matching_core.R:313-339 returns and what a lowered problem would lose.
    auto cost = make_cost({
        {1, 2},
        {1, 3}
    });
    lap::SourceOracle<lap::CostMatrix> oracle(cost);

    lap::CompiledDesign replaced = lap::compile_with_replacement(oracle, 1, NO_CATEGORIES);
    lap::FlowResult res = lap::solve_min_cost_flow(replaced.problem);

    REQUIRE(res.status == "optimal");
    REQUIRE(res.flow_sent == 2);
    REQUIRE(res.total_cost == Approx(2.0));

    lap::CompiledDesign assigned = lap::compile_one_to_one(oracle, NO_CATEGORIES);
    lap::FlowResult assigned_res = lap::solve_min_cost_flow(assigned.problem);
    REQUIRE(assigned_res.total_cost == Approx(3.0));
}

TEST_CASE("compiled k-cardinality stops at k pairs", "[flow][compile][solve]") {
    auto cost = make_cost({
        {1, 9, 9, 9},
        {9, 1, 9, 9},
        {9, 9, 9, 9}
    });
    lap::SourceOracle<lap::CostMatrix> oracle(cost);

    lap::CompiledDesign design = lap::compile_k_cardinality(oracle, 2, NO_CATEGORIES);
    lap::FlowResult res = lap::solve_min_cost_flow(design.problem);

    REQUIRE(res.status == "optimal");
    REQUIRE(res.flow_sent == 2);
    REQUIRE(res.total_cost == Approx(2.0));
}

TEST_CASE("compiled variable ratio fills to its bounds", "[flow][compile][solve]") {
    auto cost = make_cost({
        {0, 0, 5, 5},
        {5, 5, 0, 0}
    });
    lap::SourceOracle<lap::CostMatrix> oracle(cost);

    lap::CompiledDesign design = lap::compile_variable_ratio(oracle, 1, 2, NO_CATEGORIES);
    REQUIRE(design.flow_required == 4);

    lap::FlowResult res = lap::solve_min_cost_flow(design.problem);
    REQUIRE(res.status == "optimal");
    REQUIRE(res.flow_sent == 4);
    REQUIRE(res.total_cost == Approx(0.0));
}

TEST_CASE("compiled full matching places every unit", "[flow][compile][solve]") {
    SECTION("two centres absorbing two units each") {
        auto cost = make_cost({
            {0, 0, 10, 10},
            {10, 10, 0, 0}
        });
        lap::SourceOracle<lap::CostMatrix> oracle(cost);

        lap::CompiledFullMatch full =
            lap::compile_full_matching(oracle, 1, lap::FLOW_INF_CAP, NO_CATEGORIES);
        REQUIRE(full.bounds_feasible);

        lap::FlowResult res = lap::solve_min_cost_flow(full.design.problem);
        REQUIRE(res.status == "optimal");
        REQUIRE(res.flow_sent == 4);
        REQUIRE(res.total_cost == Approx(0.0));
    }

    SECTION("the transposed orientation reaches the same optimum") {
        auto upright = make_cost({
            {0, 0, 10, 10},
            {10, 10, 0, 0}
        });
        auto turned = make_cost({
            {0, 10},
            {0, 10},
            {10, 0},
            {10, 0}
        });
        lap::SourceOracle<lap::CostMatrix> upright_oracle(upright);
        lap::SourceOracle<lap::CostMatrix> turned_oracle(turned);

        lap::CompiledFullMatch a =
            lap::compile_full_matching(upright_oracle, 1, lap::FLOW_INF_CAP, NO_CATEGORIES);
        lap::CompiledFullMatch b =
            lap::compile_full_matching(turned_oracle, 1, lap::FLOW_INF_CAP, NO_CATEGORIES);
        REQUIRE_FALSE(a.transposed);
        REQUIRE(b.transposed);

        lap::FlowResult res_a = lap::solve_min_cost_flow(a.design.problem);
        lap::FlowResult res_b = lap::solve_min_cost_flow(b.design.problem);
        REQUIRE(res_a.status == "optimal");
        REQUIRE(res_b.status == "optimal");
        REQUIRE(res_b.total_cost == Approx(res_a.total_cost));
        REQUIRE(res_b.flow_sent == res_a.flow_sent);
    }

    SECTION("a lower bound above one forces the larger groups") {
        // Every centre must take two units, so the cheap pairing of one unit
        // each is not available and the second-cheapest unit joins each group.
        auto cost = make_cost({
            {0, 1, 100, 100},
            {100, 100, 0, 1}
        });
        lap::SourceOracle<lap::CostMatrix> oracle(cost);

        lap::CompiledFullMatch full =
            lap::compile_full_matching(oracle, 2, lap::FLOW_INF_CAP, NO_CATEGORIES);
        REQUIRE(full.bounds_feasible);
        REQUIRE(full.design.problem.arcs[0].lower == 2);

        lap::FlowResult res = lap::solve_min_cost_flow(full.design.problem);
        REQUIRE(res.status == "optimal");
        REQUIRE(res.flow_sent == 4);
        REQUIRE(res.total_cost == Approx(2.0));
    }
}

TEST_CASE("compiled strata solve independently in one problem",
          "[flow][compile][solve]") {
    auto first  = make_cost({{1, 8}, {8, 2}});
    auto second = make_cost({{3, 9}, {9, 4}});
    lap::SourceOracle<lap::CostMatrix> first_oracle(first);
    lap::SourceOracle<lap::CostMatrix> second_oracle(second);

    std::vector<lap::Stratum> strata(2);
    strata[0].costs    = &first_oracle;
    strata[0].row_unit = {0, 1};
    strata[0].col_unit = {0, 1};
    strata[1].costs    = &second_oracle;
    strata[1].row_unit = {2, 3};
    strata[1].col_unit = {2, 3};

    lap::CompiledBlocked blocked = lap::compile_blocked(strata, NO_CATEGORIES);
    lap::FlowResult res = lap::solve_min_cost_flow(blocked.design.problem);

    REQUIRE(res.status == "optimal");
    REQUIRE(res.flow_sent == 4);
    REQUIRE(res.total_cost == Approx(1.0 + 2.0 + 3.0 + 4.0));
}

TEST_CASE("is_row_separable - true exactly where the columns cannot bind",
          "[flow][compile][separable]") {
    auto cost = make_cost({
        {1, 2, 3, 4},
        {5, 6, 7, 8},
        {9, 10, 11, 12}
    });
    lap::SourceOracle<lap::CostMatrix> oracle(cost);

    SECTION("replacement separates, at every ratio it admits") {
        for (int64_t ratio : {int64_t{1}, int64_t{2}, int64_t{4}}) {
            lap::CompiledDesign design =
                lap::compile_with_replacement(oracle, ratio, NO_CATEGORIES);
            REQUIRE(design.problem.arcs[0].upper == 3);
            REQUIRE(lap::is_row_separable(design.problem));
        }
    }

    SECTION("1:1 does not separate: three rows compete for one column each") {
        lap::CompiledDesign design = lap::compile_one_to_one(oracle, NO_CATEGORIES);
        REQUIRE_FALSE(lap::is_row_separable(design.problem));
    }

    SECTION("k:1 does not separate") {
        lap::CompiledDesign design = lap::compile_fixed_ratio(oracle, 2, NO_CATEGORIES);
        REQUIRE_FALSE(lap::is_row_separable(design.problem));
    }

    SECTION("a design metering its flow at the source does not separate") {
        lap::CompiledDesign variable =
            lap::compile_variable_ratio(oracle, 1, 2, NO_CATEGORIES);
        REQUIRE_FALSE(lap::is_row_separable(variable.problem));

        lap::CompiledDesign subset = lap::compile_k_cardinality(oracle, 2, NO_CATEGORIES);
        REQUIRE_FALSE(lap::is_row_separable(subset.problem));

        lap::CompiledFullMatch full =
            lap::compile_full_matching(oracle, 1, lap::FLOW_INF_CAP, NO_CATEGORIES);
        REQUIRE_FALSE(lap::is_row_separable(full.design.problem));
    }

    SECTION("one column short of every row is one column that can bind") {
        lap::CompiledDesign design =
            lap::compile_with_replacement(oracle, 2, NO_CATEGORIES);
        design.problem.arcs[1].upper = 2;
        REQUIRE_FALSE(lap::is_row_separable(design.problem));
    }

    SECTION("an expanded problem does not separate") {
        lap::CompiledDesign design =
            lap::compile_with_replacement(oracle, 2, NO_CATEGORIES);
        design.problem.expanded = true;
        REQUIRE_FALSE(lap::is_row_separable(design.problem));
    }

    SECTION("a single row separates under either design, having nothing to "
            "compete with") {
        lap::CostMatrix one_row(1, 4);
        lap::SourceOracle<lap::CostMatrix> one_row_oracle(one_row);
        lap::CompiledDesign design =
            lap::compile_one_to_one(one_row_oracle, NO_CATEGORIES);
        REQUIRE(lap::is_row_separable(design.problem));
        REQUIRE(lap::is_unit_capacity_assignment(design.problem));
    }
}

TEST_CASE("the replacement optimum is each row's own cheapest columns",
          "[flow][compile][separable][solve]") {
    // Both rows want the same two columns, and both get them: a column carries
    // one unit from every row, so the second row pays no more than the first.
    auto cost = make_cost({
        {1, 2, 50},
        {3, 4, 60}
    });
    lap::SourceOracle<lap::CostMatrix> oracle(cost);

    lap::CompiledDesign design = lap::compile_with_replacement(oracle, 2, NO_CATEGORIES);
    REQUIRE(lap::is_row_separable(design.problem));
    REQUIRE(design.flow_required == 4);

    lap::FlowResult res = lap::solve_min_cost_flow(design.problem);
    REQUIRE(res.status == "optimal");
    REQUIRE(res.flow_sent == 4);
    REQUIRE(res.total_cost == Approx(1.0 + 2.0 + 3.0 + 4.0));
}

TEST_CASE("ShapeOracle - compiles the network the costs would have",
          "[flow][compile][shape]") {
    lap::CostMatrix cost(3, 4);
    lap::SourceOracle<lap::CostMatrix> priced(cost);
    lap::ShapeOracle shape(3, 4);

    SECTION("reading a cell throws rather than answering") {
        REQUIRE_THROWS_AS(shape.at(0, 0), lap::LapException);
        REQUIRE_THROWS_AS(shape.allowed(0, 0), lap::LapException);
        REQUIRE(shape.nrow() == 3);
        REQUIRE(shape.ncol() == 4);
    }

    SECTION("every couples design compiles to the same network either way") {
        lap::CompiledDesign a = lap::compile_one_to_one(priced, NO_CATEGORIES);
        lap::CompiledDesign b = lap::compile_one_to_one(shape, NO_CATEGORIES);
        REQUIRE(b.problem.n_nodes == a.problem.n_nodes);
        REQUIRE(b.problem.supply == a.problem.supply);
        REQUIRE(b.problem.arcs.size() == a.problem.arcs.size());
        REQUIRE(b.row_unit == a.row_unit);
        REQUIRE(b.col_unit == a.col_unit);
        REQUIRE(b.flow_required == a.flow_required);
        REQUIRE(lap::is_unit_capacity_assignment(b.problem));

        lap::CompiledDesign ka = lap::compile_fixed_ratio(priced, 2, NO_CATEGORIES);
        lap::CompiledDesign kb = lap::compile_fixed_ratio(shape, 2, NO_CATEGORIES);
        REQUIRE(kb.problem.n_nodes == ka.problem.n_nodes);
        REQUIRE(kb.problem.supply == ka.problem.supply);
        REQUIRE(kb.row_unit == ka.row_unit);
        REQUIRE(kb.flow_required == ka.flow_required);
        REQUIRE(lap::is_unit_capacity_assignment(kb.problem));

        lap::CompiledDesign ra = lap::compile_with_replacement(priced, 2, NO_CATEGORIES);
        lap::CompiledDesign rb = lap::compile_with_replacement(shape, 2, NO_CATEGORIES);
        REQUIRE(rb.problem.n_nodes == ra.problem.n_nodes);
        REQUIRE(rb.problem.supply == ra.problem.supply);
        REQUIRE(rb.flow_required == ra.flow_required);
        REQUIRE(lap::is_row_separable(rb.problem));
    }

    SECTION("the k:1 replica map is the shape's, not the costs'") {
        lap::CompiledDesign design = lap::compile_fixed_ratio(shape, 3, NO_CATEGORIES);
        REQUIRE(design.row_unit.size() == 9);
        REQUIRE(design.row_unit == std::vector<int32_t>({0, 0, 0, 1, 1, 1, 2, 2, 2}));
        REQUIRE(design.col_unit == std::vector<int32_t>({0, 1, 2, 3}));
    }

    SECTION("a shape cannot be expanded") {
        lap::CompiledDesign design = lap::compile_one_to_one(shape, NO_CATEGORIES);
        REQUIRE_THROWS_AS(lap::expand_blocks(design.problem), lap::LapException);
    }
}
