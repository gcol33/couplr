// src/flow/flow_exact_read.h
// A solve of the balance network read exactly.
// Pure C++ - NO Rcpp dependencies, same rule as lap_types.h.
//
// The solver stops at reduced costs within its own tolerance of zero, so its
// flow can miss the exact optimum by a margin of that order and its potentials
// speak for its own arithmetic. What speaks for the network is weak duality:
// for any potentials pi, the dual objective D(pi) of flow_certify.h, taken over
// the costs as stored, is at most the optimum, and it needs no optimality from
// anything, only exact arithmetic.
//
// The read closes the margin before reading anything. It recovers the flow's
// exact potentials from its residual graph, as the certificate does, and while
// the recovery finds a negative cycle instead, it cancels that cycle and
// recovers again. Each cancellation lowers the exact cost strictly and the
// flows are finitely many, so the loop ends at a flow that is exactly optimal
// over the arcs held, and D(pi) at its potentials is its exact value. The flow
// returned is that one, which may differ from the solver's.
//
// A generating search holds some of the pair arcs. The pairs it omits sit at
// their lower bound of zero, so each adds min(cbar, 0) to D(pi); pricing them
// against the same potentials and finding none exactly negative makes that sum
// exactly zero, and D over the arcs held is D over every pair. The potentials
// the pricer needs are the pair potentials returned here: the reduced cost of
// pair (i, j) is its arc cost less U_i and V_j.
#pragma once

#include "../core/lap_exact.h"
#include "../core/lap_exact_potentials.h"
#include "flow_certify.h"
#include "flow_problem.h"

#include <cstddef>
#include <cstdint>
#include <vector>

namespace lap {

// The network's pair arcs: which arc each is, its two units, and the nodes
// those units occupy. Units and nodes are 0-based.
struct PairArcs {
    std::vector<int64_t> arc;
    std::vector<int32_t> left;
    std::vector<int32_t> right;
    std::vector<int32_t> left_node;    // per left unit
    std::vector<int32_t> right_node;   // per right unit
};

// Every arc's cost as the network stores it, which is a double and therefore
// its own exact value.
class StoredArcCosts {
public:
    explicit StoredArcCosts(const FlowProblem& prob) : prob_(prob) {}
    exact::Approximation approximation(std::size_t a) const {
        return exact::Approximation{prob_.arcs[a].cost, 0.0};
    }
    exact::Expansion exact(std::size_t a) const {
        const double c = prob_.arcs[a].cost;
        return c == 0.0 ? exact::Expansion() : exact::Expansion{c};
    }

private:
    const FlowProblem& prob_;
};

struct ExactRead {
    // The flow, after any negative cycle the exact costs showed was cancelled,
    // and how many were.
    std::vector<int64_t> flow;
    int64_t n_cancelled = 0;
    // The flow is optimal over the arcs held, exactly, and `pi` is its
    // recovered potentials; otherwise `pi` is the solver's.
    bool recovered = false;
    std::vector<exact::Expansion> pi;
    // D(pi) over the arcs held, exactly.
    exact::Expansion bound;
    // Per unit, so that a pair's reduced cost is its arc cost less U_i and V_j.
    std::vector<exact::Expansion> pair_u;
    std::vector<exact::Expansion> pair_v;
};

// `flow` and `potential` are the solver's answer to the network.
inline ExactRead exact_flow_read(const FlowProblem& prob, const std::vector<int64_t>& flow,
                                 const std::vector<double>& potential, const PairArcs& pairs) {
    ExactRead out;
    const StoredArcCosts cost(prob);
    out.flow = flow;
    exact::ShortestPaths paths;
    for (;;) {
        paths = recover_flow_potentials(prob, out.flow, potential, true);
        if (paths.ok || paths.cycle.empty()) break;
        cancel_residual_cycle(prob, out.flow, paths.cycle);
        ++out.n_cancelled;
    }
    out.recovered = paths.ok;
    if (paths.ok) {
        out.pi = paths.dist;
    } else {
        out.pi.assign(potential.size(), exact::Expansion());
        for (std::size_t v = 0; v < potential.size(); ++v) {
            if (potential[v] != 0.0) out.pi[v].push_back(potential[v]);
        }
    }
    out.bound = exact_dual_objective(prob, out.pi, cost);

    // A pair arc left_node -> right_node reduces to cost + pi(left) - pi(right),
    // which is cost - U_i - V_j for U_i = -pi(left) and V_j = pi(right).
    out.pair_u.resize(pairs.left_node.size());
    for (std::size_t i = 0; i < pairs.left_node.size(); ++i) {
        out.pair_u[i] = exact::negated(out.pi[static_cast<std::size_t>(pairs.left_node[i])]);
    }
    out.pair_v.resize(pairs.right_node.size());
    for (std::size_t j = 0; j < pairs.right_node.size(); ++j) {
        out.pair_v[j] = out.pi[static_cast<std::size_t>(pairs.right_node[j])];
    }
    return out;
}

}  // namespace lap
