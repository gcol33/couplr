// src/flow/flow_expand.cpp
// Structural validation and block expansion for the flow representation.
// Pure C++ - NO Rcpp dependencies, same rule as lap_types.h.
//
// These are the two things that happen to a FlowProblem before any arithmetic:
// validate() rejects a problem that is not a flow problem at all, and
// expand_blocks() turns the implicit bipartite blocks into the explicit arc
// array the solver walks. The split matters because the two failures are of
// different kinds. A malformed problem is a programming error in a compiler and
// throws; a problem with no feasible flow is an answer, and the solver returns
// it with a status.

#include "flow_problem.h"
#include "../core/lap_error.h"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <string>
#include <utility>
#include <vector>

namespace lap {

namespace {

// Half-open range of node ids a block claims. Two blocks sharing a node id
// would silently alias two different (i, j) grids onto one node, which the
// solver cannot detect and the mapping back to row/column indices cannot undo.
struct NodeSpan {
    int64_t first = 0;
    int64_t last  = 0;  // exclusive
    int64_t block = 0;  // which block it came from, for the message

    bool empty() const { return last <= first; }
};

std::string arc_label(std::size_t a) {
    return "arc " + std::to_string(a);
}

}  // namespace

void validate(const FlowProblem& prob) {
    const int64_t n_nodes = static_cast<int64_t>(prob.n_nodes);
    if (n_nodes < 0) {
        LAP_THROW_DIMENSION("FlowProblem: n_nodes is negative");
    }
    if (static_cast<int64_t>(prob.supply.size()) != n_nodes) {
        LAP_THROW_DIMENSION("FlowProblem: supply has " +
                            std::to_string(prob.supply.size()) +
                            " entries for " + std::to_string(n_nodes) + " nodes");
    }

    // Every unit that leaves a node enters another one, so the surpluses and
    // the deficits are the same total counted from both ends and the supplies
    // sum to zero. A problem violating this has no feasible flow for a reason
    // that is arithmetic rather than structural, so it is caught here instead
    // of being handed to the solver as an unsatisfiable demand.
    //
    // Accumulated in unsigned arithmetic, whose wraparound is defined, because
    // a signed overflow is undefined behaviour on exactly the oversized input
    // this check exists to reject. A nonzero total can only read as zero if it
    // is a multiple of 2^64, which no supply bounded by FLOW_INF_CAP reaches.
    unsigned long long balance = 0;
    for (const int64_t b : prob.supply) {
        balance += static_cast<unsigned long long>(b);
    }
    if (balance != 0ull) {
        LAP_THROW_DIMENSION("FlowProblem: supplies do not sum to zero");
    }

    for (std::size_t a = 0; a < prob.arcs.size(); ++a) {
        const FlowArc& arc = prob.arcs[a];
        if (arc.tail < 0 || static_cast<int64_t>(arc.tail) >= n_nodes) {
            LAP_THROW_DIMENSION("FlowProblem: " + arc_label(a) + " has tail " +
                                std::to_string(arc.tail) + " outside [0, " +
                                std::to_string(n_nodes) + ")");
        }
        if (arc.head < 0 || static_cast<int64_t>(arc.head) >= n_nodes) {
            LAP_THROW_DIMENSION("FlowProblem: " + arc_label(a) + " has head " +
                                std::to_string(arc.head) + " outside [0, " +
                                std::to_string(n_nodes) + ")");
        }
        if (arc.lower < 0) {
            LAP_THROW_DIMENSION("FlowProblem: " + arc_label(a) +
                                " has a negative lower bound");
        }
        if (arc.upper < arc.lower) {
            LAP_THROW_DIMENSION("FlowProblem: " + arc_label(a) + " has lower " +
                                std::to_string(arc.lower) + " above upper " +
                                std::to_string(arc.upper));
        }
        // A non-finite cost is not a forbidden arc. Forbidden is expressed by
        // omitting the arc, or by allowed(i, j) inside a block; an Inf or NaN
        // that reaches the residual search poisons a shortest-path distance
        // and then every potential derived from it.
        if (!std::isfinite(arc.cost)) {
            LAP_THROW_DIMENSION("FlowProblem: " + arc_label(a) +
                                " has a non-finite cost");
        }
    }

    std::vector<NodeSpan> spans;
    spans.reserve(prob.blocks.size() * 2u);
    for (std::size_t b = 0; b < prob.blocks.size(); ++b) {
        const BipartiteBlock& blk = prob.blocks[b];
        if (blk.costs == nullptr) {
            LAP_THROW_DIMENSION("FlowProblem: block " + std::to_string(b) +
                                " has no cost source");
        }
        const int64_t nr = blk.costs->nrow();
        const int64_t nc = blk.costs->ncol();
        if (nr < 0 || nc < 0) {
            LAP_THROW_DIMENSION("FlowProblem: block " + std::to_string(b) +
                                " has negative dimensions");
        }
        if (blk.lower < 0 || blk.upper < blk.lower) {
            LAP_THROW_DIMENSION("FlowProblem: block " + std::to_string(b) +
                                " has lower " + std::to_string(blk.lower) +
                                " above upper " + std::to_string(blk.upper));
        }
        if (blk.row_base < 0 || static_cast<int64_t>(blk.row_base) + nr > n_nodes) {
            LAP_THROW_DIMENSION("FlowProblem: block " + std::to_string(b) +
                                " row nodes reach outside [0, " +
                                std::to_string(n_nodes) + ")");
        }
        if (blk.col_base < 0 || static_cast<int64_t>(blk.col_base) + nc > n_nodes) {
            LAP_THROW_DIMENSION("FlowProblem: block " + std::to_string(b) +
                                " column nodes reach outside [0, " +
                                std::to_string(n_nodes) + ")");
        }
        spans.push_back(NodeSpan{blk.row_base, blk.row_base + nr,
                                 static_cast<int64_t>(b)});
        spans.push_back(NodeSpan{blk.col_base, blk.col_base + nc,
                                 static_cast<int64_t>(b)});
    }

    // Sorted by start, an overlap can only be between neighbours, so the check
    // is O(B log B) rather than the quadratic sweep a blocked design with one
    // block per stratum would make expensive.
    spans.erase(std::remove_if(spans.begin(), spans.end(),
                               [](const NodeSpan& s) { return s.empty(); }),
                spans.end());
    std::sort(spans.begin(), spans.end(),
              [](const NodeSpan& x, const NodeSpan& y) {
                  return x.first < y.first;
              });
    for (std::size_t k = 1; k < spans.size(); ++k) {
        if (spans[k].first < spans[k - 1].last) {
            LAP_THROW_DIMENSION("FlowProblem: blocks " +
                                std::to_string(spans[k - 1].block) + " and " +
                                std::to_string(spans[k].block) +
                                " claim overlapping node ranges");
        }
    }

    if (!prob.warm_potential.empty() &&
        static_cast<int64_t>(prob.warm_potential.size()) != n_nodes) {
        LAP_THROW_DIMENSION("FlowProblem: warm_potential has " +
                            std::to_string(prob.warm_potential.size()) +
                            " entries for " + std::to_string(n_nodes) + " nodes");
    }

    // warm_flow is aligned to the arc array as it stands after expansion, so it
    // is only checkable once the arc array is final: either the blocks are
    // already expanded, or there were none to expand. Otherwise the solver
    // rechecks it after expand_blocks().
    if (!prob.warm_flow.empty() && (prob.expanded || prob.blocks.empty()) &&
        prob.warm_flow.size() != prob.arcs.size()) {
        LAP_THROW_DIMENSION("FlowProblem: warm_flow has " +
                            std::to_string(prob.warm_flow.size()) +
                            " entries for " + std::to_string(prob.arcs.size()) +
                            " arcs");
    }
}

void expand_blocks(FlowProblem& prob) {
    if (prob.expanded) return;

    // The whole expansion is counted before a single arc is appended. Emitting
    // first and discovering the size later means the failure mode is a machine
    // in swap rather than an error a caller can act on, and the count that
    // matters is the dense one: a source may admit every pair.
    int64_t budget = static_cast<int64_t>(prob.arcs.size());
    for (std::size_t b = 0; b < prob.blocks.size(); ++b) {
        const BipartiteBlock& blk = prob.blocks[b];
        if (blk.costs == nullptr) {
            LAP_THROW_DIMENSION("FlowProblem: block " + std::to_string(b) +
                                " has no cost source");
        }
        const int64_t nr = blk.costs->nrow();
        const int64_t nc = blk.costs->ncol();
        if (nr < 0 || nc < 0) {
            LAP_THROW_DIMENSION("FlowProblem: block " + std::to_string(b) +
                                " has negative dimensions");
        }
        if (nr > 0 && nc > (FLOW_MAX_EXPANDED_ARCS - budget) / nr) {
            LAP_THROW_DIMENSION("FlowProblem: expanding the blocks needs more "
                                "than " + std::to_string(FLOW_MAX_EXPANDED_ARCS) +
                                " arcs");
        }
        budget += nr * nc;
    }

    prob.block_arcs.clear();
    prob.block_arcs.reserve(prob.blocks.size());

    for (const BipartiteBlock& blk : prob.blocks) {
        const CostOracle& src = *blk.costs;
        const int64_t nr = src.nrow();
        const int64_t nc = src.ncol();

        BlockArcRange range;
        range.first_arc = static_cast<int64_t>(prob.arcs.size());

        for (int64_t i = 0; i < nr; ++i) {
            const int32_t tail = static_cast<int32_t>(blk.row_base + i);
            for (int64_t j = 0; j < nc; ++j) {
                // Two separate gates. allowed(i, j) is the source's own
                // admissibility mask, and forbidden cells read as lap::BIG
                // rather than Inf, so a cell can be admissible and still carry
                // a cost the residual search cannot use.
                if (!src.allowed(i, j)) continue;
                const double c = src.at(i, j);
                if (!std::isfinite(c)) continue;

                prob.arcs.emplace_back(tail,
                                       static_cast<int32_t>(blk.col_base + j),
                                       blk.lower, blk.upper, c);
                range.rc.emplace_back(static_cast<int32_t>(i),
                                      static_cast<int32_t>(j));
            }
        }

        range.n_arcs = static_cast<int64_t>(prob.arcs.size()) - range.first_arc;
        prob.block_arcs.push_back(std::move(range));
    }

    prob.expanded = true;
}

}  // namespace lap
