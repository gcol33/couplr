// src/flow/flow_compile.cpp
// The design compilers. Every one of them allocates the same node layout,
// attaches one bipartite block per stratum, and then chooses three things: what
// the rows may carry, what a column may carry, and how much flow the problem
// asks for. Nothing else separates the designs, so nothing else is written more
// than once here.

#include "flow_compile.h"

#include "../core/lap_error.h"

#include <algorithm>
#include <limits>
#include <utility>

namespace lap {

namespace {

// Node ids are int32, so a cost source wider than that cannot be laid out.
constexpr int64_t FLOW_MAX_NODES =
    static_cast<int64_t>(std::numeric_limits<int32_t>::max());

// Product of two non-negative counts, clamped at FLOW_INF_CAP. Used where the
// result bounds a capacity, so saturating is the right answer: a bound above
// every reachable flow value bounds nothing.
int64_t capped_mul(int64_t a, int64_t b) {
    if (a <= 0 || b <= 0) return 0;
    if (a > FLOW_INF_CAP / b) return FLOW_INF_CAP;
    return a * b;
}

// Product of two non-negative counts, exact. Used for flow values and supplies,
// where a clamped result would break the supplies-sum-to-zero invariant instead
// of merely being loose.
int64_t exact_mul(int64_t a, int64_t b) {
    if (a <= 0 || b <= 0) return 0;
    if (a > std::numeric_limits<int64_t>::max() / b) {
        LAP_THROW_DIMENSION("flow value exceeds the representable range");
    }
    return a * b;
}

int32_t node_count(int64_t n) {
    if (n < 0 || n > FLOW_MAX_NODES) {
        LAP_THROW_DIMENSION("cost source dimension does not fit a node index");
    }
    return static_cast<int32_t>(n);
}

std::vector<int32_t> identity_map(int32_t n) {
    std::vector<int32_t> map(static_cast<std::size_t>(n));
    for (int32_t i = 0; i < n; ++i) map[static_cast<std::size_t>(i)] = i;
    return map;
}

void reject_categories(const std::vector<CategoryConstraint>& categories) {
    if (!categories.empty()) {
        LAP_THROW("category constraints are not compiled; pass an empty vector");
    }
}

// Node allocation and the three arc sets. Rows are allocated before columns and
// columns before anything else, which is the layout flow_problem.h fixes; the
// builder refuses any other order rather than producing a problem whose blocks
// happen to sit somewhere a dual mapping does not expect.
class Builder {
public:
    Builder() : supply_(2, 0) {}

    int32_t add_rows(int32_t n) {
        if (columns_open_) {
            LAP_THROW_DIMENSION("row nodes must be allocated before column nodes");
        }
        return allocate(n);
    }

    int32_t add_cols(int32_t n) {
        columns_open_ = true;
        return allocate(n);
    }

    void add_source_arcs(int32_t base, int32_t n, int64_t lower, int64_t upper) {
        for (int32_t i = 0; i < n; ++i) {
            arcs_.emplace_back(FLOW_SOURCE, base + i, lower, upper, 0.0);
        }
    }

    void add_sink_arcs(int32_t base, int32_t n, int64_t lower, int64_t upper) {
        for (int32_t j = 0; j < n; ++j) {
            arcs_.emplace_back(base + j, FLOW_SINK, lower, upper, 0.0);
        }
    }

    void add_block(int32_t row_base, int32_t col_base, const CostOracle& costs,
                   int64_t lower, int64_t upper) {
        BipartiteBlock blk;
        blk.row_base = row_base;
        blk.col_base = col_base;
        blk.lower    = lower;
        blk.upper    = upper;
        blk.costs    = &costs;
        blocks_.push_back(blk);
    }

    // Flow the auxiliary source injects, absorbed at the auxiliary sink.
    void inject_at_source(int64_t flow) {
        supply_[static_cast<std::size_t>(FLOW_SOURCE)] += flow;
        supply_[static_cast<std::size_t>(FLOW_SINK)]   -= flow;
    }

    // Flow each row node injects itself, absorbed at the auxiliary sink. A
    // design whose rows carry a fixed amount states it here and leaves the
    // source out of the network entirely.
    void inject_at_rows(int32_t base, int32_t n, int64_t per_row) {
        for (int32_t i = 0; i < n; ++i) {
            supply_[static_cast<std::size_t>(base + i)] += per_row;
        }
        supply_[static_cast<std::size_t>(FLOW_SINK)] -= exact_mul(n, per_row);
    }

    FlowProblem finish() {
        FlowProblem prob;
        prob.n_nodes = static_cast<int32_t>(supply_.size());
        prob.supply  = std::move(supply_);
        prob.arcs    = std::move(arcs_);
        prob.blocks  = std::move(blocks_);
        return prob;
    }

private:
    int32_t allocate(int32_t n) {
        if (n < 0) LAP_THROW_DIMENSION("node count must not be negative");
        const int64_t base = static_cast<int64_t>(supply_.size());
        if (base + n > FLOW_MAX_NODES) {
            LAP_THROW_DIMENSION("problem needs more nodes than a node index holds");
        }
        supply_.resize(static_cast<std::size_t>(base + n), 0);
        return static_cast<int32_t>(base);
    }

    std::vector<int64_t>        supply_;
    std::vector<FlowArc>        arcs_;
    std::vector<BipartiteBlock> blocks_;
    bool                        columns_open_ = false;
};

// One bipartite layer: row nodes, column nodes, and the block of unit-capacity
// pair arcs between them. Each design finishes it by saying where the flow
// comes from and what a column may carry.
struct Skeleton {
    Builder b;
    int32_t row_base = FLOW_FIRST_ROW;
    int32_t n_rows   = 0;
    int32_t col_base = FLOW_FIRST_ROW;
    int32_t n_cols   = 0;
};

Skeleton open_bipartite(const CostOracle& costs) {
    Skeleton s;
    s.n_rows   = node_count(costs.nrow());
    s.n_cols   = node_count(costs.ncol());
    s.row_base = s.b.add_rows(s.n_rows);
    s.col_base = s.b.add_cols(s.n_cols);
    s.b.add_block(s.row_base, s.col_base, costs, 0, 1);
    return s;
}

CompiledDesign close_bipartite(Skeleton& s, int64_t flow_required) {
    CompiledDesign out;
    out.problem       = s.b.finish();
    out.row_base      = s.row_base;
    out.n_rows        = s.n_rows;
    out.col_base      = s.col_base;
    out.n_cols        = s.n_cols;
    out.row_unit      = identity_map(s.n_rows);
    out.col_unit      = identity_map(s.n_cols);
    out.flow_required = flow_required;
    return out;
}

}  // namespace

CompiledDesign compile_one_to_one(const CostOracle&                      costs,
                                  const std::vector<CategoryConstraint>& categories) {
    reject_categories(categories);

    Skeleton s = open_bipartite(costs);
    s.b.inject_at_rows(s.row_base, s.n_rows, 1);
    s.b.add_sink_arcs(s.col_base, s.n_cols, 0, 1);
    return close_bipartite(s, s.n_rows);
}

CompiledDesign compile_fixed_ratio(const CostOracle&                      costs,
                                   int64_t                                ratio,
                                   const std::vector<CategoryConstraint>& categories) {
    reject_categories(categories);
    if (ratio < 1) LAP_THROW_DIMENSION("ratio must be at least 1");

    // The k replicas of a row sit on consecutive nodes, so replica e belongs to
    // row e / ratio and the design is the assignment R already solves.
    auto replicated = std::unique_ptr<CostOracle>(new RowReplicatedOracle(costs, ratio));

    Skeleton s = open_bipartite(*replicated);
    s.b.inject_at_rows(s.row_base, s.n_rows, 1);
    s.b.add_sink_arcs(s.col_base, s.n_cols, 0, 1);

    CompiledDesign out = close_bipartite(s, s.n_rows);
    for (int32_t e = 0; e < out.n_rows; ++e) {
        out.row_unit[static_cast<std::size_t>(e)] = static_cast<int32_t>(e / ratio);
    }
    out.owned.push_back(std::move(replicated));
    return out;
}

CompiledDesign compile_with_replacement(const CostOracle&                      costs,
                                        int64_t                                ratio,
                                        const std::vector<CategoryConstraint>& categories) {
    reject_categories(categories);
    if (ratio < 1) LAP_THROW_DIMENSION("ratio must be at least 1");

    Skeleton s = open_bipartite(costs);

    // A row cannot take more distinct columns than exist, and columns are
    // shared, so the column capacity is the number of rows: the most any column
    // could be asked to carry.
    const int64_t per_row = std::min<int64_t>(ratio, s.n_cols);
    s.b.inject_at_rows(s.row_base, s.n_rows, per_row);
    s.b.add_sink_arcs(s.col_base, s.n_cols, 0, s.n_rows);
    return close_bipartite(s, exact_mul(s.n_rows, per_row));
}

CompiledDesign compile_variable_ratio(const CostOracle&                      costs,
                                      int64_t                                r_min,
                                      int64_t                                r_max,
                                      const std::vector<CategoryConstraint>& categories) {
    reject_categories(categories);
    if (r_min < 0)     LAP_THROW_DIMENSION("r_min must not be negative");
    if (r_max < r_min) LAP_THROW_DIMENSION("r_max must be at least r_min");

    Skeleton s = open_bipartite(costs);
    if (r_min > s.n_cols) {
        LAP_THROW_DIMENSION("r_min exceeds the number of columns, so no row bound is expressible");
    }

    // As many pairs as the bounds allow, at minimum cost among matchings of
    // that size: every column is used when the rows can absorb them all, every
    // row reaches r_max when they cannot.
    const int64_t cap   = std::min<int64_t>(r_max, s.n_cols);
    const int64_t total = std::min<int64_t>(capped_mul(s.n_rows, cap), s.n_cols);

    s.b.add_source_arcs(s.row_base, s.n_rows, r_min, cap);
    s.b.add_sink_arcs(s.col_base, s.n_cols, 0, 1);
    s.b.inject_at_source(total);
    return close_bipartite(s, total);
}

CompiledDesign compile_k_cardinality(const CostOracle&                      costs,
                                     int64_t                                k,
                                     const std::vector<CategoryConstraint>& categories) {
    reject_categories(categories);
    if (k < 0) LAP_THROW_DIMENSION("k must not be negative");

    Skeleton s = open_bipartite(costs);
    if (k > std::min<int64_t>(s.n_rows, s.n_cols)) {
        LAP_THROW_DIMENSION("k exceeds the number of pairs the problem can hold");
    }

    s.b.add_source_arcs(s.row_base, s.n_rows, 0, 1);
    s.b.add_sink_arcs(s.col_base, s.n_cols, 0, 1);
    s.b.inject_at_source(k);
    return close_bipartite(s, k);
}

CompiledFullMatch compile_full_matching(const CostOracle&                      costs,
                                        int64_t                                min_controls,
                                        int64_t                                max_controls,
                                        const std::vector<CategoryConstraint>& categories) {
    reject_categories(categories);
    if (min_controls < 1) LAP_THROW_DIMENSION("min_controls must be at least 1");

    CompiledFullMatch out;
    out.min_controls = min_controls;

    const int64_t n_left  = costs.nrow();
    const int64_t n_right = costs.ncol();
    if (n_left == 0 || n_right == 0) {
        out.bounds_feasible = false;
        out.reason = "one side has no units";
        return out;
    }

    // Group centres are the smaller side, so a centre always has units to
    // absorb and the capacity bounds are read the way the caller wrote them.
    out.transposed = (n_left > n_right);
    out.n_centres  = out.transposed ? n_right : n_left;
    out.n_units    = out.transposed ? n_left  : n_right;

    // A centre cannot absorb more units than exist, so the requested upper
    // bound is clamped before it is compared with the lower one.
    out.max_capacity = std::min<int64_t>(max_controls, out.n_units);
    if (out.max_capacity < min_controls) {
        out.bounds_feasible = false;
        out.reason = "max_controls falls below min_controls once clamped to the unit count";
        return out;
    }
    if (out.n_units < capped_mul(out.n_centres, min_controls)) {
        out.bounds_feasible = false;
        out.reason = "fewer units than min_controls for every centre";
        return out;
    }

    std::unique_ptr<CostOracle> transposed_view;
    const CostOracle*           centre_costs = &costs;
    if (out.transposed) {
        transposed_view = std::unique_ptr<CostOracle>(new TransposedOracle(costs));
        centre_costs    = transposed_view.get();
    }

    // Every unit is placed, which is what makes this full matching rather than
    // a subset selection, so the flow the source injects is the unit count.
    Skeleton s = open_bipartite(*centre_costs);
    s.b.add_source_arcs(s.row_base, s.n_rows, min_controls, out.max_capacity);
    s.b.add_sink_arcs(s.col_base, s.n_cols, 0, 1);
    s.b.inject_at_source(out.n_units);

    out.design = close_bipartite(s, out.n_units);
    if (transposed_view) out.design.owned.push_back(std::move(transposed_view));
    return out;
}

CompiledBlocked compile_blocked(const std::vector<Stratum>&            strata,
                                const std::vector<CategoryConstraint>& categories) {
    reject_categories(categories);

    CompiledBlocked out;
    out.strata.resize(strata.size());

    Builder b;

    for (std::size_t s = 0; s < strata.size(); ++s) {
        if (strata[s].costs == nullptr) {
            LAP_THROW_DIMENSION("stratum has no cost source");
        }
        const int32_t nr = node_count(strata[s].costs->nrow());
        if (strata[s].row_unit.size() != static_cast<std::size_t>(nr)) {
            LAP_THROW_DIMENSION("stratum row_unit length does not match its cost source");
        }
        out.strata[s].n_rows   = nr;
        out.strata[s].row_base = b.add_rows(nr);
    }

    for (std::size_t s = 0; s < strata.size(); ++s) {
        const int32_t nc = node_count(strata[s].costs->ncol());
        if (strata[s].col_unit.size() != static_cast<std::size_t>(nc)) {
            LAP_THROW_DIMENSION("stratum col_unit length does not match its cost source");
        }
        out.strata[s].n_cols   = nc;
        out.strata[s].col_base = b.add_cols(nc);
    }

    int64_t              flow_required = 0;
    std::vector<int32_t> row_unit;
    std::vector<int32_t> col_unit;

    for (std::size_t s = 0; s < strata.size(); ++s) {
        StratumRange& range = out.strata[s];
        range.block = static_cast<int64_t>(s);
        b.add_block(range.row_base, range.col_base, *strata[s].costs, 0, 1);
        b.inject_at_rows(range.row_base, range.n_rows, 1);
        b.add_sink_arcs(range.col_base, range.n_cols, 0, 1);
        flow_required += range.n_rows;
        row_unit.insert(row_unit.end(), strata[s].row_unit.begin(), strata[s].row_unit.end());
        col_unit.insert(col_unit.end(), strata[s].col_unit.begin(), strata[s].col_unit.end());
    }

    out.design.problem       = b.finish();
    out.design.row_base      = FLOW_FIRST_ROW;
    out.design.n_rows        = static_cast<int32_t>(row_unit.size());
    out.design.col_base      = FLOW_FIRST_ROW + out.design.n_rows;
    out.design.n_cols        = static_cast<int32_t>(col_unit.size());
    out.design.row_unit      = std::move(row_unit);
    out.design.col_unit      = std::move(col_unit);
    out.design.flow_required = flow_required;
    return out;
}

bool is_unit_capacity_assignment(const FlowProblem& prob) {
    // After expansion the block arcs are in `arcs` and the arc test below can no
    // longer separate a column's sink arc from a pair arc. Lowering is decided
    // before the arc list is paid for, which is the point of it.
    if (prob.expanded) return false;
    if (prob.blocks.size() != 1) return false;

    const BipartiteBlock& blk = prob.blocks.front();
    if (blk.costs == nullptr) return false;
    if (blk.lower != 0 || blk.upper != 1) return false;

    const int64_t nr = blk.costs->nrow();
    const int64_t nc = blk.costs->ncol();
    if (nr < 0 || nc < 0) return false;
    if (blk.row_base != FLOW_FIRST_ROW) return false;
    if (static_cast<int64_t>(blk.col_base) != FLOW_FIRST_ROW + nr) return false;

    // Nothing above the column block: a category or stratum node would carry a
    // constraint the cost matrix cannot express.
    if (static_cast<int64_t>(prob.n_nodes) != FLOW_FIRST_ROW + nr + nc) return false;
    if (static_cast<int64_t>(prob.supply.size()) != prob.n_nodes) return false;

    // Supply 1 on every row is what says every row must be matched, and an
    // empty source is what says nothing else meters the flow.
    if (prob.supply[static_cast<std::size_t>(FLOW_SOURCE)] != 0) return false;
    if (prob.supply[static_cast<std::size_t>(FLOW_SINK)] != -nr) return false;
    for (int64_t i = 0; i < nr; ++i) {
        if (prob.supply[static_cast<std::size_t>(blk.row_base) +
                        static_cast<std::size_t>(i)] != 1) return false;
    }
    for (int64_t j = 0; j < nc; ++j) {
        if (prob.supply[static_cast<std::size_t>(blk.col_base) +
                        static_cast<std::size_t>(j)] != 0) return false;
    }

    // Exactly one unit-capacity arc per column and no other explicit arc. Unit
    // capacity here is what forbids a column being reused, which the pair arcs'
    // own capacity does not say.
    if (static_cast<int64_t>(prob.arcs.size()) != nc) return false;
    std::vector<bool> reaches_sink(static_cast<std::size_t>(nc), false);
    for (const FlowArc& arc : prob.arcs) {
        if (arc.head != FLOW_SINK) return false;
        const int64_t j = static_cast<int64_t>(arc.tail) - static_cast<int64_t>(blk.col_base);
        if (j < 0 || j >= nc) return false;
        if (reaches_sink[static_cast<std::size_t>(j)]) return false;
        reaches_sink[static_cast<std::size_t>(j)] = true;
        if (arc.lower != 0 || arc.upper != 1) return false;
        if (arc.cost != 0.0) return false;
    }

    return true;
}

LoweredAssignment lower_to_assignment(const FlowProblem& prob) {
    LoweredAssignment out;
    if (!is_unit_capacity_assignment(prob)) return out;

    const BipartiteBlock& blk = prob.blocks.front();
    out.valid    = true;
    out.costs    = blk.costs;
    out.row_base = blk.row_base;
    out.col_base = blk.col_base;
    out.n_rows   = node_count(blk.costs->nrow());
    out.n_cols   = node_count(blk.costs->ncol());
    out.row_unit = identity_map(out.n_rows);
    out.col_unit = identity_map(out.n_cols);
    return out;
}

LoweredAssignment lower_to_assignment(const CompiledDesign& design) {
    LoweredAssignment out = lower_to_assignment(design.problem);
    if (!out.valid) return out;

    if (design.row_unit.size() != static_cast<std::size_t>(out.n_rows) ||
        design.col_unit.size() != static_cast<std::size_t>(out.n_cols)) {
        LAP_THROW_DIMENSION("compiled design index maps do not match its block");
    }
    out.row_unit = design.row_unit;
    out.col_unit = design.col_unit;
    return out;
}

}  // namespace lap
