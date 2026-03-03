// src/solvers/solve_full_matching.cpp
// Min-cost max-flow solver for optimal full matching.
// Uses the standard lower bound transformation to enforce minimum group sizes.
// Reuses the MCMF (Dijkstra + Johnson potentials) pattern from csflow.

#include "solve_full_matching.h"
#include <vector>
#include <queue>
#include <limits>
#include <cmath>
#include <algorithm>
#include <climits>

namespace {

// ---------- Min-cost max-flow core (Dijkstra + Johnson potentials) ----------
// Self-contained copy from csflow to avoid cross-file coupling.

struct FMEdge {
    int to, rev;
    int cap;
    double cost;
    FMEdge(int to, int rev, int cap, double cost)
        : to(to), rev(rev), cap(cap), cost(cost) {}
};

struct FMMcmf {
    int N;
    std::vector<std::vector<FMEdge>> G;
    FMMcmf(int n) : N(n), G(n) {}

    void add_edge(int u, int v, int cap, double cost) {
        G[u].emplace_back(v, static_cast<int>(G[v].size()), cap, cost);
        G[v].emplace_back(u, static_cast<int>(G[u].size()) - 1, 0, -cost);
    }

    // Returns (flow_sent, total_cost)
    std::pair<int, double> min_cost_max_flow(int s, int t, int maxf) {
        const double INF = std::numeric_limits<double>::infinity();
        std::vector<double> pi(N, 0.0);
        std::vector<double> dist(N);
        std::vector<int> pv(N), pe(N);

        int flow = 0;
        double cost = 0.0;

        while (flow < maxf) {
            std::fill(dist.begin(), dist.end(), INF);
            std::fill(pv.begin(), pv.end(), -1);
            std::fill(pe.begin(), pe.end(), -1);

            using Q = std::pair<double, int>;
            std::priority_queue<Q, std::vector<Q>, std::greater<Q>> pq;
            dist[s] = 0.0;
            pq.emplace(0.0, s);

            while (!pq.empty()) {
                auto [d, u] = pq.top();
                pq.pop();
                if (d != dist[u]) continue;
                for (int ei = 0; ei < static_cast<int>(G[u].size()); ++ei) {
                    const FMEdge& e = G[u][ei];
                    if (e.cap <= 0) continue;
                    double rc = e.cost + pi[u] - pi[e.to];
                    double nd = d + rc;
                    if (nd + 1e-18 < dist[e.to]) {
                        dist[e.to] = nd;
                        pv[e.to] = u;
                        pe[e.to] = ei;
                        pq.emplace(nd, e.to);
                    }
                }
            }

            if (!std::isfinite(dist[t])) break;

            for (int v = 0; v < N; ++v) {
                if (std::isfinite(dist[v])) pi[v] += dist[v];
            }

            int aug = maxf - flow;
            int v = t;
            while (v != s) {
                int u = pv[v];
                int ei = pe[v];
                aug = std::min(aug, G[u][ei].cap);
                v = u;
            }
            v = t;
            while (v != s) {
                int u = pv[v];
                int ei = pe[v];
                FMEdge& e = G[u][ei];
                FMEdge& er = G[v][e.rev];
                e.cap -= aug;
                er.cap += aug;
                v = u;
            }

            flow += aug;
            cost += aug * pi[t];
        }
        return {flow, cost};
    }
};

}  // anonymous namespace


namespace lap {

FullMatchResult solve_full_matching(
    const std::vector<double>& cost,
    int n_left,
    int n_right,
    int min_controls,
    int max_controls_val)
{
    FullMatchResult result;
    result.right_to_group.assign(n_right, -1);
    result.left_to_group.assign(n_left, -1);
    result.total_cost = 0.0;
    result.status = "optimal";
    result.n_groups = 0;

    if (n_left == 0 || n_right == 0) {
        result.status = "infeasible";
        return result;
    }

    // Determine orientation: group centers are the smaller side.
    bool transposed = (n_left > n_right);
    int n_center = transposed ? n_right : n_left;
    int n_unit = transposed ? n_left : n_right;

    // Effective capacity per center: [min_controls, max_cap]
    int max_cap = (max_controls_val > n_unit) ? n_unit : max_controls_val;
    if (max_cap < min_controls) {
        result.status = "infeasible";
        return result;
    }

    // Feasibility: need n_unit >= n_center * min_controls
    int mandatory_total = n_center * min_controls;
    if (n_unit < mandatory_total) {
        // Not enough units to give each center min_controls
        result.status = "infeasible";
        return result;
    }

    // Feasibility: total max capacity must accommodate all units
    long long total_max_cap = static_cast<long long>(n_center) * max_cap;

    // ---- Build flow network (standard lower bound transformation) ----
    //
    // Natural network (before transformation):
    //   S --[min_controls, max_cap, 0]--> Center_i --[0, 1, d(i,j)]--> Unit_j --[0, 1, 0]--> T
    //   S has supply n_unit, T has demand n_unit.
    //
    // Lower bound transformation for arcs with lower bound > 0:
    //   For each S -> Center_i arc with bounds [min_controls, max_cap]:
    //     1. Subtract lower bound from capacity: [0, max_cap - min_controls]
    //     2. Adjust node supplies:
    //          b(S)        -= min_controls   (per center)
    //          b(Center_i) += min_controls   (per center)
    //
    // After transformation, node supplies become:
    //   b(S)        = n_unit - mandatory_total   (residual supply)
    //   b(Center_i) = +min_controls              (induced supply per center)
    //   b(T)        = -n_unit                    (demand, unchanged)
    //   b(Unit_j)   = 0                          (transit, unchanged)
    //
    // Convert the b-flow problem to min-cost max-flow with auxiliary nodes:
    //   S' -> v  for each v with b(v) > 0,  cap = b(v),  cost = 0
    //   v  -> T' for each v with b(v) < 0,  cap = -b(v), cost = 0
    //
    // Solve min-cost max-flow from S' to T'.
    // Target flow = n_unit (sum of all positive supplies).
    // If achieved: all lower bounds satisfied and all units matched.

    const int AUX_S = 0;                       // S': auxiliary source
    const int AUX_T = 1;                       // T': auxiliary sink
    const int S = 2;                           // Original source
    const int CENTER_BASE = 3;
    const int UNIT_BASE = CENTER_BASE + n_center;
    const int T = UNIT_BASE + n_unit;
    const int NN = T + 1;

    FMMcmf mf(NN);

    // --- Auxiliary arcs (from lower bound transformation) ---

    // S' -> S: residual supply at S = n_unit - mandatory_total
    int optional_flow = n_unit - mandatory_total;
    if (optional_flow > 0) {
        mf.add_edge(AUX_S, S, optional_flow, 0.0);
    }

    // S' -> Center_i: induced supply = min_controls per center
    for (int i = 0; i < n_center; ++i) {
        mf.add_edge(AUX_S, CENTER_BASE + i, min_controls, 0.0);
    }

    // T -> T': demand absorption = n_unit
    mf.add_edge(T, AUX_T, n_unit, 0.0);

    // --- Transformed arcs (original network with lower bounds subtracted) ---

    // S -> Center_i: residual capacity = max_cap - min_controls
    int extra_cap = max_cap - min_controls;
    for (int i = 0; i < n_center; ++i) {
        if (extra_cap > 0) {
            mf.add_edge(S, CENTER_BASE + i, extra_cap, 0.0);
        }
    }

    // Center_i -> Unit_j: capacity 1, cost = distance (unchanged, lower bound = 0)
    for (int i = 0; i < n_center; ++i) {
        for (int j = 0; j < n_unit; ++j) {
            double c;
            if (!transposed) {
                c = cost[i * n_right + j];
            } else {
                c = cost[j * n_right + i];
            }
            if (!std::isfinite(c)) continue;
            mf.add_edge(CENTER_BASE + i, UNIT_BASE + j, 1, c);
        }
    }

    // Unit_j -> T: capacity 1, cost 0 (unchanged, lower bound = 0)
    for (int j = 0; j < n_unit; ++j) {
        mf.add_edge(UNIT_BASE + j, T, 1, 0.0);
    }

    // Solve: min-cost max-flow from S' to T'
    // Target = min(n_unit, total_max_cap)
    int target_flow = (total_max_cap >= n_unit) ? n_unit
                                                : static_cast<int>(total_max_cap);
    auto [flow_sent, flow_cost] = mf.min_cost_max_flow(AUX_S, AUX_T, target_flow);

    // ---- Extract assignments ----
    std::vector<std::vector<int>> center_units(n_center);

    for (int i = 0; i < n_center; ++i) {
        const auto& adj = mf.G[CENTER_BASE + i];
        for (int ei = 0; ei < static_cast<int>(adj.size()); ++ei) {
            const FMEdge& e = adj[ei];
            if (e.to >= UNIT_BASE && e.to < UNIT_BASE + n_unit) {
                const FMEdge& back = mf.G[e.to][e.rev];
                if (back.cap > 0) {
                    int j = e.to - UNIT_BASE;
                    center_units[i].push_back(j);
                }
            }
        }
    }

    // ---- Post-process: enforce min_controls (dissolve undersized groups) ----
    std::vector<bool> center_matched(n_center, false);
    int group_count = 0;

    for (int i = 0; i < n_center; ++i) {
        if (static_cast<int>(center_units[i].size()) >= min_controls) {
            center_matched[i] = true;
            group_count++;
        }
    }

    // ---- Compute total cost from actual assignments ----
    double total_cost = 0.0;
    for (int i = 0; i < n_center; ++i) {
        if (!center_matched[i]) continue;
        for (int j : center_units[i]) {
            double c;
            if (!transposed) {
                c = cost[i * n_right + j];
            } else {
                c = cost[j * n_right + i];
            }
            total_cost += c;
        }
    }

    // ---- Map back to left/right orientation ----
    result.n_groups = group_count;
    result.total_cost = total_cost;

    if (!transposed) {
        int gid = 0;
        for (int i = 0; i < n_center; ++i) {
            if (!center_matched[i]) continue;
            result.left_to_group[i] = gid;
            for (int j : center_units[i]) {
                result.right_to_group[j] = gid;
            }
            gid++;
        }
    } else {
        int gid = 0;
        for (int i = 0; i < n_center; ++i) {
            if (!center_matched[i]) continue;
            result.right_to_group[i] = gid;
            for (int j : center_units[i]) {
                result.left_to_group[j] = gid;
            }
            gid++;
        }
    }

    if (flow_sent < target_flow && group_count == 0) {
        result.status = "infeasible";
    }

    return result;
}

}  // namespace lap
