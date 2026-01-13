// src/solvers/solve_csflow.cpp
// Pure C++ Cost-Scaling Flow LAP solver - NO Rcpp dependencies

#include "solve_csflow.h"
#include "../core/lap_error.h"
#include "../core/lap_utils.h"
#include <vector>
#include <queue>
#include <limits>
#include <cmath>

namespace lap {

// ---------- Min-cost max-flow core (Dijkstra + Johnson potentials) ----------
struct Edge {
    int to, rev;
    int cap;
    double cost;
    Edge(int to, int rev, int cap, double cost)
        : to(to), rev(rev), cap(cap), cost(cost) {}
};

struct MCMF {
    int N;
    std::vector<std::vector<Edge>> G;

    MCMF(int n) : N(n), G(n) {}

    void add_edge(int u, int v, int cap, double cost) {
        G[u].emplace_back(v, static_cast<int>(G[v].size()), cap, cost);
        G[v].emplace_back(u, static_cast<int>(G[u].size()) - 1, 0, -cost);
    }

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
                auto cur = pq.top();
                pq.pop();
                double d = cur.first;
                int u = cur.second;
                if (d != dist[u]) continue;

                for (int ei = 0; ei < static_cast<int>(G[u].size()); ++ei) {
                    const Edge& e = G[u][ei];
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
                if (std::isfinite(dist[v])) {
                    pi[v] += dist[v];
                }
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
                Edge& e = G[u][ei];
                Edge& er = G[v][e.rev];
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

// ---------- Assignment wrapper ----------
LapResult solve_csflow(const CostMatrix& cost, bool maximize) {
    const int n = cost.nrow;
    const int m = cost.ncol;

    // Handle empty case
    if (n == 0) {
        return LapResult({}, 0.0, "optimal");
    }

    // Dimension check
    if (n > m) {
        LAP_THROW_DIMENSION("Infeasible: number of rows greater than number of columns");
    }

    // Prepare working costs (negated if maximize, BIG for forbidden)
    CostMatrix work = prepare_for_solve(cost, maximize);

    // Check feasibility
    ensure_each_row_has_option(work.mask, n, m);

    // Build flow network
    const int S = 0;
    const int R0 = 1;
    const int C0 = R0 + n;
    const int T = C0 + m;
    const int NN = T + 1;

    MCMF mf(NN);

    // Source to rows
    for (int i = 0; i < n; ++i) {
        mf.add_edge(S, R0 + i, 1, 0.0);
    }

    // Rows to columns (assignment edges)
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            if (!work.allowed(i, j)) continue;
            double c = work.at(i, j);
            if (!std::isfinite(c)) continue;
            mf.add_edge(R0 + i, C0 + j, 1, c);
        }
    }

    // Columns to sink
    for (int j = 0; j < m; ++j) {
        mf.add_edge(C0 + j, T, 1, 0.0);
    }

    // Solve max-flow
    auto result = mf.min_cost_max_flow(S, T, n);
    int pushed = result.first;

    if (pushed < n) {
        LAP_THROW_INFEASIBLE(std::string("Only matched ") + std::to_string(pushed) +
                             " out of " + std::to_string(n) + " rows");
    }

    // Recover matching: examine edges from row nodes to column nodes
    std::vector<int> assignment(n, -1);
    for (int i = 0; i < n; ++i) {
        const auto& adj = mf.G[R0 + i];
        for (int ei = 0; ei < static_cast<int>(adj.size()); ++ei) {
            const Edge& e = adj[ei];
            if (e.to >= C0 && e.to < C0 + m) {
                // Check if flow was pushed through this edge (residual back-edge has positive cap)
                const Edge& back = mf.G[e.to][e.rev];
                if (back.cap > 0) {
                    int j = e.to - C0;
                    assignment[i] = j;  // 0-based
                    break;
                }
            }
        }
    }

    // Verify matching and compute total cost using ORIGINAL costs
    double total = 0.0;
    for (int i = 0; i < n; ++i) {
        int j = assignment[i];
        if (j < 0) {
            LAP_THROW_INFEASIBLE("Could not find full matching");
        }
        if (!cost.allowed(i, j)) {
            LAP_THROW_INFEASIBLE("Chosen forbidden edge");
        }
        double c = cost.at(i, j);
        if (!std::isfinite(c)) {
            LAP_THROW_INFEASIBLE("Chosen edge has non-finite cost");
        }
        total += c;
    }

    return LapResult(std::move(assignment), total, "optimal");
}

}  // namespace lap
