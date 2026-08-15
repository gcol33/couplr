// src/flow/flow_solve.cpp
// Successive shortest paths with Johnson potentials, over a residual graph.
// Pure C++ - NO Rcpp dependencies, same rule as lap_types.h.
//
// ---------------------------------------------------------------------------
// Lower bounds are the solver's job
// ---------------------------------------------------------------------------
//
// The problem as posed is a bounded-variable transshipment:
//
//     min  sum_a cost(a) * f(a)
//     s.t. sum_{a out of v} f(a) - sum_{a into v} f(a) = supply(v)
//          lower(a) <= f(a) <= upper(a)
//
// Substituting f(a) = lower(a) + g(a) moves the lower bounds off the variables
// and into the node balances. The capacity constraint becomes
// 0 <= g(a) <= upper(a) - lower(a), and conservation at v becomes
//
//     sum_out g - sum_in g = supply(v) - sum_out lower + sum_in lower =: d(v)
//
// so the mandatory flow every lower bound forces is charged to the two nodes it
// runs between: the tail loses that much of its ability to supply, the head
// gains that much of an obligation to dispose. The objective shifts by the
// constant sum_a cost(a) * lower(a), which is why the total cost is summed from
// f = lower + g at the end rather than accumulated per augmentation.
//
// The residual d(v) is a b-flow, not a max-flow, so one more step turns it into
// something successive shortest paths can run on. Add an auxiliary source SS
// and an auxiliary sink TT outside the problem's own node range:
//
//     SS -> v   capacity  d(v), cost 0, for every v with d(v) > 0
//     v  -> TT  capacity -d(v), cost 0, for every v with d(v) < 0
//
// Because sum_v supply(v) = 0 and every lower bound contributes -lower at its
// tail and +lower at its head, sum_v d(v) = 0, so the capacity out of SS equals
// the capacity into TT. Call it flow_required. A feasible g exists exactly when
// a maximum flow from SS to TT saturates it: saturating means every node's
// balance is met, and any shortfall means no assignment of g meets them all.
// So "a lower bound cannot be met" is not a separate condition to test; it is
// the same shortfall that leaves flow_sent below flow_required, and the status
// distinguishes the two by how much was placed.
//
// ---------------------------------------------------------------------------
// Reduced costs and the search
// ---------------------------------------------------------------------------
//
//     cbar(a) = cost(a) + pi[tail(a)] - pi[head(a)]
//
// Successive shortest paths keeps the invariant cbar(a) >= 0 on every residual
// arc. Augmenting along a shortest path and then adding the distance labels to
// the potentials restores it, and the arcs on that path come out at cbar == 0.
//
// A cold start has pi == 0, so the invariant asks that no directed cycle of the
// arc set has negative total cost. Every design compiler builds a layered
// source -> rows -> columns -> sink network, which is acyclic, so this holds by
// construction; the augmenting-path walk counts its steps and reports a cycle
// rather than looping forever if it ever does not.
//
// The search itself is Dijkstra with reinsertion: a node whose label improves
// after it was already popped goes back on the queue. That is what makes the
// first search correct when the arc costs straddle zero, which they do whenever
// a cost matrix has negative entries. Reinsertion is also what makes the search
// depend on cbar >= 0 for termination rather than only for its answer, so the
// two places that invariant can be lost are handled where they arise: rounding
// is read as rounding once the invariant is supposed to hold, and a queue that
// outlives the bound on how often labels can improve reports the cycle it is
// circling.
#include "flow_solve.h"

#include "../core/lap_certify.h"
#include "../core/lap_error.h"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <functional>
#include <limits>
#include <queue>
#include <string>
#include <utility>
#include <vector>

namespace lap {

namespace {

// One direction of a residual arc. `rev` is the index of the opposite direction
// inside the adjacency list of `to`, so pushing flow is two array writes.
struct ResArc {
    int32_t to   = -1;
    int32_t rev  = -1;
    int64_t cap  = 0;
    double  cost = 0.0;
};

using Residual = std::vector<std::vector<ResArc>>;

// int64 addition that reports overflow instead of wrapping, because the node
// balances are sums of caller-supplied capacities and a wrapped balance turns
// an impossible demand into a plausible small one.
bool add_checked(int64_t a, int64_t b, int64_t& out) {
    if (b > 0 && a > std::numeric_limits<int64_t>::max() - b) return false;
    if (b < 0 && a < std::numeric_limits<int64_t>::min() - b) return false;
    out = a + b;
    return true;
}

int64_t add_or_throw(int64_t a, int64_t b, const char* what) {
    int64_t out = 0;
    if (!add_checked(a, b, out)) {
        LAP_THROW_DIMENSION(std::string("FlowProblem: ") + what + " overflows int64");
    }
    return out;
}

// "optimal" when the b-flow was met, and every weaker outcome names what stopped
// the solver rather than what the caller hoped for. A shortfall means no
// feasible flow exists at all; the split between "partial" and "infeasible" is
// whether a maximum-cardinality-then-minimum-cost answer was produced or
// nothing moved.
std::string derive_status(int64_t flow_sent, int64_t flow_required, bool no_path) {
    if (flow_sent == flow_required) return "optimal";
    if (!no_path)                   return "iteration_limit";
    if (flow_sent == 0)             return "infeasible";
    return "partial";
}

}  // namespace

FlowResult solve_min_cost_flow(FlowProblem& prob, const FlowOptions& opts) {
    validate(prob);
    expand_blocks(prob);

    const int64_t n_arcs  = static_cast<int64_t>(prob.arcs.size());
    const int32_t n_nodes = prob.n_nodes;

    if (!prob.warm_flow.empty() &&
        static_cast<int64_t>(prob.warm_flow.size()) != n_arcs) {
        LAP_THROW_DIMENSION("FlowProblem: warm_flow has " +
                            std::to_string(prob.warm_flow.size()) +
                            " entries for " + std::to_string(n_arcs) +
                            " expanded arcs");
    }

    FlowResult out;
    out.flow.assign(static_cast<std::size_t>(n_arcs), 0);
    if (n_nodes <= 0) {
        out.status = derive_status(0, 0, true);
        return out;
    }

    const int32_t SS = n_nodes;
    const int32_t TT = n_nodes + 1;
    const int32_t N  = n_nodes + 2;

    // ---- starting point: potentials, then a flow consistent with them ----

    std::vector<double> pi(static_cast<std::size_t>(N), 0.0);

    // Whether cbar >= 0 is already supposed to hold on every residual arc. A
    // cold start does not have it: the arc set may price edges below zero, and
    // the reinserting search below is what makes that case correct. Every
    // later search does have it, and so does a warm start that passed the
    // slackness repair, which is what lets the search read a reduced cost a few
    // ulps below zero as the rounding it is.
    bool residual_nonneg = false;

    bool warm = !prob.warm_potential.empty() || !prob.warm_flow.empty();
    if (!prob.warm_potential.empty()) {
        for (int32_t v = 0; v < n_nodes; ++v) {
            pi[static_cast<std::size_t>(v)] =
                prob.warm_potential[static_cast<std::size_t>(v)];
        }
    }

    std::vector<int64_t> f(static_cast<std::size_t>(n_arcs), 0);
    for (int64_t a = 0; a < n_arcs; ++a) {
        const FlowArc& arc = prob.arcs[static_cast<std::size_t>(a)];
        const int64_t v = prob.warm_flow.empty()
                              ? arc.lower
                              : prob.warm_flow[static_cast<std::size_t>(a)];
        if (v < arc.lower || v > arc.upper) {
            LAP_THROW_DIMENSION("FlowProblem: warm_flow on arc " +
                                std::to_string(a) + " is outside its bounds");
        }
        f[static_cast<std::size_t>(a)] = v;
    }

    if (warm) {
        // A warm flow that was optimal for other costs, or optimal potentials
        // paired with any flow at all, need not satisfy complementary
        // slackness, and a residual arc with cbar < 0 is a negative cycle
        // waiting to happen. Restoring slackness costs one pass: an arc the
        // potentials price above zero belongs at its lower bound, one priced
        // below zero belongs at its upper bound, and one priced at zero may sit
        // anywhere. What that breaks is conservation, and repairing
        // conservation is exactly what the augmentation loop below does.
        //
        // An arc with cbar < 0 and no finite upper bound has no bound to be
        // pushed to, so the supplied potentials cannot be made slack-consistent
        // with any flow and the whole warm start is discarded rather than half
        // applied.
        bool repairable = true;
        for (int64_t a = 0; a < n_arcs && repairable; ++a) {
            const FlowArc& arc = prob.arcs[static_cast<std::size_t>(a)];
            const double cbar = arc.cost +
                                pi[static_cast<std::size_t>(arc.tail)] -
                                pi[static_cast<std::size_t>(arc.head)];
            if (cbar < -opts.tol && arc.upper >= FLOW_INF_CAP) repairable = false;
        }
        if (repairable) {
            for (int64_t a = 0; a < n_arcs; ++a) {
                const FlowArc& arc = prob.arcs[static_cast<std::size_t>(a)];
                const double cbar = arc.cost +
                                    pi[static_cast<std::size_t>(arc.tail)] -
                                    pi[static_cast<std::size_t>(arc.head)];
                if (cbar > opts.tol) {
                    f[static_cast<std::size_t>(a)] = arc.lower;
                } else if (cbar < -opts.tol) {
                    f[static_cast<std::size_t>(a)] = arc.upper;
                }
            }
            residual_nonneg = true;
        } else {
            std::fill(pi.begin(), pi.end(), 0.0);
            for (int64_t a = 0; a < n_arcs; ++a) {
                f[static_cast<std::size_t>(a)] =
                    prob.arcs[static_cast<std::size_t>(a)].lower;
            }
        }
    }

    // ---- node balances the starting flow leaves unmet ----
    //
    // Written against f rather than against lower alone, so the cold case is
    // the warm case with f = lower and there is one formula instead of two.

    std::vector<int64_t> d(static_cast<std::size_t>(n_nodes), 0);
    for (int32_t v = 0; v < n_nodes; ++v) {
        d[static_cast<std::size_t>(v)] = prob.supply[static_cast<std::size_t>(v)];
    }
    for (int64_t a = 0; a < n_arcs; ++a) {
        const FlowArc& arc = prob.arcs[static_cast<std::size_t>(a)];
        const int64_t fa = f[static_cast<std::size_t>(a)];
        int64_t& dt = d[static_cast<std::size_t>(arc.tail)];
        int64_t& dh = d[static_cast<std::size_t>(arc.head)];
        dt = add_or_throw(dt, -fa, "a node balance");
        dh = add_or_throw(dh, fa, "a node balance");
    }

    for (int32_t v = 0; v < n_nodes; ++v) {
        const int64_t dv = d[static_cast<std::size_t>(v)];
        if (dv > 0) {
            out.flow_required = add_or_throw(out.flow_required, dv,
                                             "the required flow");
        }
    }

    // ---- residual graph ----

    Residual g(static_cast<std::size_t>(N));
    auto add_arc = [&g](int32_t u, int32_t v, int64_t cap_fwd, int64_t cap_rev,
                        double cost) -> int32_t {
        const std::size_t su = static_cast<std::size_t>(u);
        const std::size_t sv = static_cast<std::size_t>(v);
        const int32_t iu = static_cast<int32_t>(g[su].size());
        g[su].push_back(ResArc{v, -1, cap_fwd, cost});
        const int32_t iv = static_cast<int32_t>(g[sv].size());
        g[sv].push_back(ResArc{u, iu, cap_rev, -cost});
        g[su][static_cast<std::size_t>(iu)].rev = iv;
        return iu;
    };

    // Auxiliary arcs first and in node order, then the problem's own arcs in the
    // order the compiler emitted them. Arcs are scanned in insertion order
    // inside each adjacency list, so this ordering is what decides which of
    // several equally-cheap shortest paths the search finds.
    for (int32_t v = 0; v < n_nodes; ++v) {
        const int64_t dv = d[static_cast<std::size_t>(v)];
        if (dv > 0) {
            add_arc(SS, v, dv, 0, 0.0);
        } else if (dv < 0) {
            add_arc(v, TT, -dv, 0, 0.0);
        }
    }

    std::vector<int32_t> arc_slot(static_cast<std::size_t>(n_arcs), -1);
    for (int64_t a = 0; a < n_arcs; ++a) {
        const FlowArc& arc = prob.arcs[static_cast<std::size_t>(a)];
        const int64_t placed = f[static_cast<std::size_t>(a)] - arc.lower;
        arc_slot[static_cast<std::size_t>(a)] =
            add_arc(arc.tail, arc.head,
                    (arc.upper - arc.lower) - placed, placed, arc.cost);
    }

    // The auxiliary nodes sit outside the caller's potential vector, and their
    // arcs are the only ones whose reduced cost the caller cannot control. Price
    // SS above every node it feeds and TT below every node that feeds it, and
    // those arcs enter the search non-negative, which is what the successive
    // shortest path invariant needs before the first search. Cold potentials are
    // all zero and both anchors land on zero with them.
    {
        double hi = -std::numeric_limits<double>::infinity();
        double lo = std::numeric_limits<double>::infinity();
        for (int32_t v = 0; v < n_nodes; ++v) {
            const int64_t dv = d[static_cast<std::size_t>(v)];
            if (dv > 0) hi = std::max(hi, pi[static_cast<std::size_t>(v)]);
            if (dv < 0) lo = std::min(lo, pi[static_cast<std::size_t>(v)]);
        }
        pi[static_cast<std::size_t>(SS)] = std::isfinite(hi) ? hi : 0.0;
        pi[static_cast<std::size_t>(TT)] = std::isfinite(lo) ? lo : 0.0;
    }

    // ---- successive shortest paths ----

    int64_t max_augmentations = opts.max_augmentations;
    if (max_augmentations <= 0) max_augmentations = out.flow_required;

    const double INF = std::numeric_limits<double>::infinity();
    std::vector<double>  dist(static_cast<std::size_t>(N));
    std::vector<int32_t> pv(static_cast<std::size_t>(N));
    std::vector<int32_t> pe(static_cast<std::size_t>(N));
    using Entry = std::pair<double, int32_t>;

    // A node goes back on the queue whenever its label improves, so the number
    // of pops is bounded only by how often that can happen: once per node when
    // every reduced cost is non-negative, and at most once per node per
    // residual arc when the first search has to work with costs that straddle
    // zero. Past that bound the labels are descending around a cycle the
    // residual graph prices below zero, and the search would circle it until it
    // ran out of memory rather than ever emptying the queue.
    int64_t n_res_arcs = 0;
    for (int32_t v = 0; v < N; ++v) {
        n_res_arcs += static_cast<int64_t>(g[static_cast<std::size_t>(v)].size());
    }
    const int64_t max_pops = static_cast<int64_t>(N) * (n_res_arcs + 1);

    bool no_path = false;
    while (out.flow_sent < out.flow_required &&
           out.n_augmentations < max_augmentations) {
        std::fill(dist.begin(), dist.end(), INF);
        std::fill(pv.begin(), pv.end(), -1);
        std::fill(pe.begin(), pe.end(), -1);

        std::priority_queue<Entry, std::vector<Entry>, std::greater<Entry>> pq;
        dist[static_cast<std::size_t>(SS)] = 0.0;
        pq.emplace(0.0, SS);

        int64_t pops = 0;
        while (!pq.empty()) {
            if (++pops > max_pops) {
                LAP_THROW("FlowProblem: the residual graph carries a "
                          "negative-cost cycle, so no shortest path exists");
            }
            const Entry cur = pq.top();
            pq.pop();
            const double dcur = cur.first;
            const int32_t u = cur.second;
            if (dcur != dist[static_cast<std::size_t>(u)]) continue;

            const std::vector<ResArc>& adj = g[static_cast<std::size_t>(u)];
            for (int32_t ei = 0; ei < static_cast<int32_t>(adj.size()); ++ei) {
                const ResArc& e = adj[static_cast<std::size_t>(ei)];
                if (e.cap <= 0) continue;
                double rc = e.cost + pi[static_cast<std::size_t>(u)] -
                            pi[static_cast<std::size_t>(e.to)];
                // The two directions of one arc are priced by expressions that
                // are negatives of each other in exact arithmetic and not in
                // floating point, so both can round a few ulps below zero at
                // once. That pair is a cycle of negative reduced cost, and the
                // search would keep going round it lowering labels. Where the
                // invariant says the price cannot be negative, rounding is the
                // only thing that could have made it so.
                if (residual_nonneg && rc < 0.0) rc = 0.0;
                const double nd = dcur + rc;
                if (nd + opts.relax_eps < dist[static_cast<std::size_t>(e.to)]) {
                    dist[static_cast<std::size_t>(e.to)] = nd;
                    pv[static_cast<std::size_t>(e.to)] = u;
                    pe[static_cast<std::size_t>(e.to)] = ei;
                    pq.emplace(nd, e.to);
                }
            }
        }

        if (!std::isfinite(dist[static_cast<std::size_t>(TT)])) {
            no_path = true;
            break;
        }

        // Reached nodes take their distance label. Unreached ones take the
        // largest label the search produced, which is what keeps cbar >= 0 on
        // every residual arc rather than only on the ones the search could
        // walk.
        //
        // No residual arc leaves a reached node for an unreached one, or the
        // head would have been labelled. Residual arcs in the other direction
        // do exist, and on those cbar becomes cbar + shift[tail] - dist[head];
        // leaving an unreached tail alone drives that below zero as soon as the
        // head's label is positive, which is exactly a column no admissible
        // pair can reach still holding a residual arc into the sink. Shifting
        // every unreached node by the maximum label makes shift[tail] >=
        // dist[head] for every such arc, so none of them can go negative, and
        // it leaves every reached node's potential untouched, so the flow the
        // next search finds is the same one.
        double shift = 0.0;
        for (int32_t v = 0; v < N; ++v) {
            const double dv = dist[static_cast<std::size_t>(v)];
            if (std::isfinite(dv)) shift = std::max(shift, dv);
        }
        for (int32_t v = 0; v < N; ++v) {
            const double dv = dist[static_cast<std::size_t>(v)];
            pi[static_cast<std::size_t>(v)] += std::isfinite(dv) ? dv : shift;
        }

        int64_t aug = out.flow_required - out.flow_sent;
        int32_t steps = 0;
        for (int32_t v = TT; v != SS; ) {
            const int32_t u = pv[static_cast<std::size_t>(v)];
            const int32_t ei = pe[static_cast<std::size_t>(v)];
            if (u < 0) {
                LAP_THROW("FlowProblem: the augmenting path from the auxiliary "
                          "sink has no predecessor at node " + std::to_string(v));
            }
            // A predecessor chain longer than the node count has closed a
            // cycle, which a shortest-path tree cannot contain unless the
            // residual graph prices some cycle below zero.
            if (++steps > N) {
                LAP_THROW("FlowProblem: the residual graph carries a "
                          "negative-cost cycle, so no shortest path exists");
            }
            aug = std::min(aug, g[static_cast<std::size_t>(u)]
                                 [static_cast<std::size_t>(ei)].cap);
            v = u;
        }

        for (int32_t v = TT; v != SS; ) {
            const int32_t u = pv[static_cast<std::size_t>(v)];
            const int32_t ei = pe[static_cast<std::size_t>(v)];
            ResArc& e = g[static_cast<std::size_t>(u)][static_cast<std::size_t>(ei)];
            e.cap -= aug;
            g[static_cast<std::size_t>(e.to)][static_cast<std::size_t>(e.rev)].cap += aug;
            v = u;
        }

        out.flow_sent += aug;
        ++out.n_augmentations;
        residual_nonneg = true;
    }

    // ---- read the answer back ----

    // The reverse direction of an arc carries exactly what has been pushed
    // through the forward one, which is g(a); undoing the substitution gives
    // f(a) = lower(a) + g(a).
    detail::CompensatedSum total;
    for (int64_t a = 0; a < n_arcs; ++a) {
        const FlowArc& arc = prob.arcs[static_cast<std::size_t>(a)];
        const ResArc& fwd = g[static_cast<std::size_t>(arc.tail)]
                             [static_cast<std::size_t>(arc_slot[static_cast<std::size_t>(a)])];
        const int64_t placed = g[static_cast<std::size_t>(fwd.to)]
                                [static_cast<std::size_t>(fwd.rev)].cap;
        const int64_t fa = arc.lower + placed;
        out.flow[static_cast<std::size_t>(a)] = fa;
        total.add(arc.cost * static_cast<double>(fa));
    }
    out.total_cost = total.value();

    if (opts.return_potentials) {
        out.potential.assign(static_cast<std::size_t>(n_nodes), 0.0);
        const double gauge = pi[0];
        for (int32_t v = 0; v < n_nodes; ++v) {
            out.potential[static_cast<std::size_t>(v)] =
                pi[static_cast<std::size_t>(v)] - gauge;
        }
    }

    out.status = derive_status(out.flow_sent, out.flow_required, no_path);
    return out;
}

}  // namespace lap
