// src/core/lap_exact_potentials.h
// Exact optimal potentials recovered from an optimal primal, over expansions.
// Pure C++, no Rcpp, same rule as lap_types.h.
//
// A certificate read from the solver's own potentials is exact only when those
// potentials are exactly optimal, and a double often cannot hold an exactly
// optimal potential: the duals of a matching are sums and differences of cost
// entries, u_i = c_{i,mu(i)} - v_{mu(i)} rounds, and the matched arc then misses
// exact tightness by one unit in the last place. On computed distances that is
// the usual case, so the exact conclusion was out of reach on the inputs
// matching is most often run on.
//
// The potentials do not have to come from the solver. Given an optimal primal,
// optimal potentials are the shortest-path distances of its residual graph, and
// those distances are sums of cost entries, which an expansion (lap_exact.h)
// holds exactly. So they are computed here, from the primal alone, in exact
// arithmetic, and the solver's doubles serve only to order the search.
//
// The search is a label-correcting shortest-path computation from a root joined
// to every node at weight zero. Labels are expansions and only ever decrease,
// each by a strict improvement. It returns in one of two states:
//
//   - every arc satisfies d(head) <= d(tail) + w exactly. The labels are then
//     shortest-path distances, and the potentials read off them are exactly
//     feasible, with every arc of the primal's own shortest-path tree tight;
//   - some label was reached by a walk with more edges than there are nodes.
//     That walk repeats a node, and the stretch between the two visits improved
//     the node's label strictly, so its total weight is negative: a negative
//     cycle, which in a residual graph is an improving cycle, and the primal is
//     not optimal. This test holds whatever order the labels are corrected in,
//     so the order is free to follow the doubles.
//
// The order follows them because it is what makes the search cheap. Keyed on
// d(j) - hint(j), with the hints the solver's own potentials, the reduced
// weights are non-negative up to rounding, and the search is Dijkstra's: each
// node settles about once, and a relaxation is decided in doubles unless the two
// sides are within their rounding of each other, which is the tight arcs and
// little else. Without hints it is still correct, and slower.
#pragma once

#include "lap_exact.h"
#include "lap_neighbours.h"
#include "lap_types.h"

#include <cfloat>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <limits>
#include <queue>
#include <utility>
#include <vector>

namespace lap {
namespace exact {

struct ShortestPaths {
    // False when a negative cycle was found; the distances are then partial and
    // mean nothing.
    bool ok = false;
    std::vector<Expansion> dist;
};

// Shortest paths from a root joined to every node at weight zero, so every
// distance is at most zero. `out_arcs(k, emit)` calls `emit(j, a, b)` once per
// arc k -> j of weight a - b, both doubles; a single-double weight passes b = 0.
// `hint` is optional (empty for none) and orders the search only.
template <class OutArcs>
ShortestPaths shortest_paths(int64_t n_nodes, const std::vector<double>& hint,
                             OutArcs&& out_arcs) {
    ShortestPaths out;
    const std::size_t n = static_cast<std::size_t>(n_nodes > 0 ? n_nodes : 0);
    out.dist.assign(n, Expansion());
    std::vector<Approximation> approx(n);
    // Edges on the walk each label was reached by, the root's own edge
    // included. A walk from the root through n distinct nodes has n edges, so a
    // longer one has repeated a node.
    std::vector<int64_t> walk(n, 1);
    const bool hinted = hint.size() == n;
    const auto key = [&](std::size_t j) {
        return approx[j].value - (hinted ? hint[j] : 0.0);
    };

    using Entry = std::pair<double, std::size_t>;
    std::priority_queue<Entry, std::vector<Entry>, std::greater<Entry>> queue;
    std::vector<char> queued(n, 1);
    for (std::size_t j = 0; j < n; ++j) queue.emplace(key(j), j);

    bool negative_cycle = false;
    while (!queue.empty() && !negative_cycle) {
        const std::size_t k = queue.top().second;
        const double k_key = queue.top().first;
        queue.pop();
        // A node re-pushed after an improvement leaves its older entries in the
        // queue; only the one carrying its current key is acted on.
        if (!queued[k] || k_key != key(k)) continue;
        queued[k] = 0;

        const Expansion dk = out.dist[k];
        const Approximation ak = approx[k];
        const int64_t walk_k = walk[k];

        out_arcs(static_cast<int64_t>(k), [&](int64_t head, double a, double b) {
            if (negative_cycle) return;
            const std::size_t j = static_cast<std::size_t>(head);
            // Is d(k) + a - b < d(j)? Decided in doubles when the two sides are
            // clear of each other's rounding, exactly otherwise.
            const Approximation& aj = approx[j];
            const double diff = ((ak.value + a) - b) - aj.value;
            const double magnitude = std::fabs(ak.value) + std::fabs(a) +
                                     std::fabs(b) + std::fabs(aj.value);
            const double bound = 4.0 * DBL_EPSILON * magnitude + ak.error + aj.error;
            if (diff > -bound) {
                if (diff > bound) return;
                const Expansion candidate = add_difference(dk, a, b);
                const Expansion gap = expansion_sum(candidate, negated(out.dist[j]));
                if (sign(gap) >= 0) return;
                out.dist[j] = candidate;
            } else {
                out.dist[j] = add_difference(dk, a, b);
            }
            approx[j] = approximate(out.dist[j]);
            walk[j] = walk_k + 1;
            if (walk[j] > n_nodes) {
                negative_cycle = true;
                return;
            }
            queued[j] = 1;
            queue.emplace(key(j), j);
        });
    }

    out.ok = !negative_cycle;
    return out;
}

// Exact potentials for the rectangular assignment, in the orientation
// lap_certify.h checks: rows <= columns, every row matched.
struct AssignmentPotentials {
    bool ok = false;
    std::vector<Expansion> u;   // one per row
    std::vector<Expansion> v;   // one per column
    // Pairs whose cost was read, which a source that computes its costs pays
    // for one evaluation each.
    int64_t n_evaluated = 0;
};

// Recover exact optimal duals for `match` over `src`, or report that none exist
// because `match` is not optimal.
//
// Substituting the tight u_i = c_{i,mu(i)} - v_{mu(i)} into u_i + v_j <= c_ij
// leaves v_j <= v_{mu(i)} + (c_ij - c_{i,mu(i)}), a difference constraint per
// admissible pair with an arc mu(i) -> j of that weight, plus v_j <= 0 as an arc
// from the root. The largest v meeting them all is the shortest-path distance
// from the root, and the matching is optimal exactly when
//
//   - the arcs carry no negative cycle, which would be an alternating cycle
//     through the matching that lowers its cost, and
//   - every column the matching leaves free sits at distance zero, since the
//     dual objective charges v_j on a free column and complementary slackness
//     asks it to be zero; a negative distance there is an alternating path
//     from that column that lowers the cost.
//
// The root arcs encode the sign condition v_j <= 0, which the LP imposes only
// with more columns than rows. On a square problem every column is matched, so
// shifting every v down and every u up by the same amount keeps both
// conditions, and the root arcs cost nothing.
//
// `u_hint` and `v_hint` are the solver's potentials when there are any, and
// order the search only.
template <class Source>
AssignmentPotentials recover_assignment_potentials(const Source& src,
                                                   const std::vector<int>& match,
                                                   const std::vector<double>& v_hint) {
    AssignmentPotentials out;
    const int64_t nrow = src.nrow;
    const int64_t ncol = src.ncol;
    if (nrow <= 0 || ncol < nrow) return out;
    if (static_cast<int64_t>(match.size()) != nrow) return out;

    std::vector<int64_t> row_of(static_cast<std::size_t>(ncol), -1);
    std::vector<double> matched_cost(static_cast<std::size_t>(nrow), 0.0);
    for (int64_t i = 0; i < nrow; ++i) {
        const int64_t j = match[static_cast<std::size_t>(i)];
        if (j < 0 || j >= ncol) return out;
        if (row_of[static_cast<std::size_t>(j)] >= 0) return out;
        double c = 0.0;
        ++out.n_evaluated;
        if (!cost_if_allowed(src, i, j, c)) return out;
        row_of[static_cast<std::size_t>(j)] = i;
        matched_cost[static_cast<std::size_t>(i)] = c;
    }

    const ShortestPaths paths = shortest_paths(
        ncol, v_hint, [&](int64_t k, auto&& emit) {
            const int64_t i = row_of[static_cast<std::size_t>(k)];
            if (i < 0) return;
            const double ck = matched_cost[static_cast<std::size_t>(i)];
            for_each_admissible(src, i, [&](int64_t j, double c) {
                ++out.n_evaluated;
                if (j != k) emit(j, c, ck);
                return true;
            });
        });
    if (!paths.ok) return out;

    for (int64_t j = 0; j < ncol; ++j) {
        if (row_of[static_cast<std::size_t>(j)] < 0 &&
            sign(paths.dist[static_cast<std::size_t>(j)]) != 0) {
            return out;
        }
    }

    out.v = paths.dist;
    out.u.resize(static_cast<std::size_t>(nrow));
    for (int64_t i = 0; i < nrow; ++i) {
        const int64_t k = match[static_cast<std::size_t>(i)];
        Expansion ci;
        const double c = matched_cost[static_cast<std::size_t>(i)];
        if (c != 0.0) ci.push_back(c);
        out.u[static_cast<std::size_t>(i)] =
            expansion_sum(ci, negated(out.v[static_cast<std::size_t>(k)]));
    }
    out.ok = true;
    return out;
}

// The exact conditions of lap_certify.h, asked of expansion potentials.
struct ExpansionCheck {
    bool    checked = false;
    int64_t n_violations = 0;   // admissible pairs with c - U - V < 0
    int64_t n_untight = 0;      // matched pairs with c - U - V != 0
    bool    sign_ok = false;    // V <= 0 wherever the sign condition applies
    bool    unmatched_free = false;
    int64_t n_evaluated = 0;    // admissible pairs whose cost was read
    bool    holds() const {
        return checked && n_violations == 0 && n_untight == 0 && sign_ok &&
               unmatched_free;
    }
};

// Dual feasibility over every admissible pair of `src`, tightness on the
// matched pairs, the sign condition, and zero on every free column. `match` is
// assumed structurally valid; the caller has already said whether it is.
template <class Source>
ExpansionCheck check_expansion_duals(const Source& src, const std::vector<int>& match,
                                     const std::vector<Expansion>& u,
                                     const std::vector<Expansion>& v) {
    ExpansionCheck out;
    const int64_t nrow = src.nrow;
    const int64_t ncol = src.ncol;
    if (static_cast<int64_t>(u.size()) != nrow || static_cast<int64_t>(v.size()) != ncol ||
        static_cast<int64_t>(match.size()) != nrow) {
        return out;
    }

    std::vector<Approximation> ua(u.size());
    std::vector<Approximation> va(v.size());
    for (std::size_t i = 0; i < u.size(); ++i) ua[i] = approximate(u[i]);
    for (std::size_t j = 0; j < v.size(); ++j) va[j] = approximate(v[j]);

    std::vector<char> used(static_cast<std::size_t>(ncol), 0);
    for (int64_t i = 0; i < nrow; ++i) {
        const std::size_t si = static_cast<std::size_t>(i);
        const int64_t mi = match[si];
        if (mi >= 0 && mi < ncol) used[static_cast<std::size_t>(mi)] = 1;
        for_each_admissible(src, i, [&](int64_t j, double c) {
            ++out.n_evaluated;
            const std::size_t sj = static_cast<std::size_t>(j);
            const int s = sign_reduced_cost(c, u[si], ua[si], v[sj], va[sj]);
            if (s < 0) ++out.n_violations;
            if (j == mi && s != 0) ++out.n_untight;
            return true;
        });
        if (mi >= 0 && mi < ncol && !src.allowed(i, mi)) ++out.n_untight;
    }

    const bool sign_condition_applies = ncol > nrow;
    out.sign_ok = true;
    out.unmatched_free = true;
    for (int64_t j = 0; j < ncol; ++j) {
        const int s = sign(v[static_cast<std::size_t>(j)]);
        if (sign_condition_applies && s > 0) out.sign_ok = false;
        if (!used[static_cast<std::size_t>(j)] && s != 0) out.unmatched_free = false;
    }
    out.checked = true;
    return out;
}

}  // namespace exact
}  // namespace lap
