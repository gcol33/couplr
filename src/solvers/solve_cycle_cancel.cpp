// src/solvers/solve_cycle_cancel.cpp
// Pure C++ Cycle Canceling LAP solver - NO Rcpp dependencies

#include "solve_cycle_cancel.h"
#include "../core/lap_error.h"
#include "../core/lap_utils.h"
#include <vector>
#include <queue>
#include <algorithm>
#include <limits>
#include <cmath>

namespace lap {

namespace {

constexpr double INF_DBL = std::numeric_limits<double>::infinity();

struct Edge {
    int to;
    int cap;
    double cost;
    int rev;
};

void add_edge(std::vector<std::vector<Edge>>& adj, int u, int v, int cap, double cost) {
    int fwd_idx = adj[u].size();
    int rev_idx = adj[v].size();
    adj[u].push_back({v, cap, cost, rev_idx});
    adj[v].push_back({u, 0, -cost, fwd_idx});
}

bool ssp_feasible(std::vector<std::vector<Edge>>& adj, int s, int t,
                  int need_flow, double& total_cost) {
    const int N = adj.size();
    std::vector<double> pi(N, 0.0);
    int flow = 0;

    while (flow < need_flow) {
        std::vector<double> dist(N, INF_DBL);
        std::vector<int> parent(N, -1);
        std::vector<int> pedge(N, -1);

        dist[s] = 0.0;
        std::priority_queue<std::pair<double,int>,
                           std::vector<std::pair<double,int>>,
                           std::greater<>> pq;
        pq.push({0.0, s});

        while (!pq.empty()) {
            auto [d, u] = pq.top();
            pq.pop();

            if (d != dist[u]) continue;

            for (int i = 0; i < (int)adj[u].size(); ++i) {
                Edge& e = adj[u][i];
                if (e.cap <= 0) continue;

                int v = e.to;
                double rc = e.cost + pi[u] - pi[v];
                double nd = d + rc;

                if (nd < dist[v]) {
                    dist[v] = nd;
                    parent[v] = u;
                    pedge[v] = i;
                    pq.push({nd, v});
                }
            }
        }

        if (dist[t] == INF_DBL) {
            return false;
        }

        for (int v = 0; v < N; ++v) {
            if (dist[v] < INF_DBL) {
                pi[v] += dist[v];
            }
        }

        int v = t;
        while (v != s) {
            int u = parent[v];
            int ei = pedge[v];
            adj[u][ei].cap -= 1;
            int rev_idx = adj[u][ei].rev;
            adj[v][rev_idx].cap += 1;
            v = u;
        }

        flow += 1;
        total_cost += pi[t] - pi[s];
    }

    return true;
}

bool karp_min_mean_cycle(const std::vector<std::vector<Edge>>& adj,
                         double& mean, std::vector<int>& cycle_nodes) {
    const int N = adj.size();

    std::vector<std::vector<std::pair<int,double>>> incoming(N);

    for (int u = 0; u < N; ++u) {
        for (const Edge& e : adj[u]) {
            if (e.cap > 0) {
                incoming[e.to].push_back({u, e.cost});
            }
        }
    }

    std::vector<std::vector<double>> dp(N + 1, std::vector<double>(N, INF_DBL));
    std::vector<std::vector<int>> par(N + 1, std::vector<int>(N, -1));

    for (int v = 0; v < N; ++v) {
        dp[0][v] = 0.0;
    }

    for (int k = 1; k <= N; ++k) {
        for (int v = 0; v < N; ++v) {
            double best = dp[k][v];
            int best_u = -1;

            for (const auto& [u, w] : incoming[v]) {
                if (dp[k-1][u] < INF_DBL) {
                    double cand = dp[k-1][u] + w;
                    if (cand < best) {
                        best = cand;
                        best_u = u;
                    }
                }
            }

            dp[k][v] = best;
            par[k][v] = best_u;
        }
    }

    double mu = INF_DBL;
    int arg_v = -1;

    for (int v = 0; v < N; ++v) {
        if (dp[N][v] == INF_DBL) continue;

        double max_ratio = -INF_DBL;

        for (int k = 0; k < N; ++k) {
            if (dp[k][v] == INF_DBL) continue;
            int denom = N - k;
            if (denom <= 0) continue;

            double ratio = (dp[N][v] - dp[k][v]) / denom;
            if (ratio > max_ratio) {
                max_ratio = ratio;
            }
        }

        if (max_ratio < mu) {
            mu = max_ratio;
            arg_v = v;
        }
    }

    if (!(mu < -1e-12) || arg_v == -1) {
        return false;
    }

    int x = arg_v;
    for (int i = 0; i < N; ++i) {
        x = par[N][x];
        if (x == -1) break;
    }

    if (x == -1) return false;

    std::vector<bool> seen(N, false);
    std::vector<int> path;
    int cur = x;

    while (!seen[cur]) {
        seen[cur] = true;
        path.push_back(cur);
        cur = par[N][cur];
        if (cur == -1) return false;
    }

    auto it = std::find(path.begin(), path.end(), cur);
    if (it == path.end()) return false;

    cycle_nodes.clear();
    cycle_nodes.insert(cycle_nodes.end(), it, path.end());
    cycle_nodes.push_back(cur);

    mean = mu;
    return true;
}

}  // anonymous namespace

LapResult solve_cycle_cancel(const CostMatrix& cost, bool maximize) {
    const int n0 = cost.nrow;
    const int m0 = cost.ncol;

    // Handle empty case
    if (n0 == 0 || m0 == 0) {
        LAP_THROW_DIMENSION("Cost matrix cannot be empty");
    }

    // Handle transposition for rectangular matrices (n > m)
    bool transposed = false;
    CostMatrix C = cost;
    int n = n0, m = m0;

    if (n0 > m0) {
        // Transpose the matrix
        C = CostMatrix(m0, n0);
        for (int i = 0; i < n0; ++i) {
            for (int j = 0; j < m0; ++j) {
                C.at(j, i) = cost.at(i, j);
                C.mask[j * n0 + i] = cost.mask[i * m0 + j];
            }
        }
        n = m0;
        m = n0;
        transposed = true;
    }

    // Find maximum cost for transformation if maximizing
    double cmax = 0.0;
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            double v = C.at(i, j);
            if (C.allowed(i, j) && std::isfinite(v) && v > cmax) {
                cmax = v;
            }
        }
    }

    // Build flow network
    // Nodes: 0..n-1 (left), n..n+m-1 (right), s=n+m, t=n+m+1
    const int s = n + m;
    const int t = n + m + 1;
    const int N = n + m + 2;
    std::vector<std::vector<Edge>> adj(N);

    // Source to left nodes
    for (int i = 0; i < n; ++i) {
        add_edge(adj, s, i, 1, 0.0);
    }

    // Left to right edges (cost edges)
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            double v = C.at(i, j);
            if (!C.allowed(i, j) || !std::isfinite(v)) continue;

            double w = maximize ? (cmax - v) : v;
            add_edge(adj, i, n + j, 1, w);
        }
    }

    // Right nodes to sink
    for (int j = 0; j < m; ++j) {
        add_edge(adj, n + j, t, 1, 0.0);
    }

    // Find initial feasible flow using successive shortest path
    double total_cost = 0.0;
    bool ok = ssp_feasible(adj, s, t, n, total_cost);

    if (!ok) {
        LAP_THROW_INFEASIBLE("Infeasible: forbidden edges block perfect matching");
    }

    // Iteratively cancel negative cost cycles using Karp's algorithm
    int max_iters = n * m * 10;
    int iters = 0;

    while (iters < max_iters) {
        ++iters;

        double mu;
        std::vector<int> nodes;

        bool found = karp_min_mean_cycle(adj, mu, nodes);

        if (!found) break;

        // Find cycle edges and minimum capacity
        std::vector<Edge*> cyc_edges;
        int theta = 1000000;

        for (size_t k = 0; k < nodes.size() - 1; ++k) {
            int a = nodes[k];
            int b = nodes[k + 1];

            Edge* found_edge = nullptr;
            for (Edge& e : adj[a]) {
                if (e.cap > 0 && e.to == b) {
                    found_edge = &e;
                    break;
                }
            }

            if (found_edge) {
                cyc_edges.push_back(found_edge);
                if (found_edge->cap < theta) theta = found_edge->cap;
            }
        }

        if (theta <= 0 || cyc_edges.empty()) break;

        // Cancel the cycle
        for (Edge* e : cyc_edges) {
            e->cap -= theta;
            int rev_idx = e->rev;
            adj[e->to][rev_idx].cap += theta;
        }
    }

    // Extract assignment from flow
    std::vector<int> assignment(n0, -1);

    if (!transposed) {
        // Normal orientation
        for (int i = 0; i < n; ++i) {
            for (const Edge& e : adj[i]) {
                int j_node = e.to;
                if (j_node >= n && j_node < n + m) {
                    int j = j_node - n;
                    // Flow was sent if capacity is now 0 (started at 1)
                    if (e.cap == 0) {
                        assignment[i] = j;
                        break;
                    }
                }
            }
        }
    } else {
        // Transposed: original rows are now columns
        for (int i = 0; i < n; ++i) {
            for (const Edge& e : adj[i]) {
                int j_node = e.to;
                if (j_node >= n && j_node < n + m) {
                    int j = j_node - n;
                    if (e.cap == 0) {
                        assignment[j] = i;
                        break;
                    }
                }
            }
        }
    }

    // Verify matching and compute total cost using ORIGINAL costs
    double total = 0.0;
    for (int i = 0; i < n0; ++i) {
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
