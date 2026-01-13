// src/solvers/solve_ssp.cpp
// Pure C++ Successive Shortest Path LAP solver - NO Rcpp dependencies

#include "solve_ssp.h"
#include "../core/lap_error.h"
#include "../core/lap_utils.h"
#include <vector>
#include <queue>
#include <limits>
#include <algorithm>
#include <cmath>

namespace lap {

LapResult solve_ssp(const CostMatrix& cost, bool maximize) {
    const int n0 = cost.nrow;
    const int m0 = cost.ncol;

    // Handle empty case
    if (n0 == 0) {
        return LapResult({}, 0.0, "optimal");
    }

    // Auto-transpose if n > m
    bool transposed = false;
    CostMatrix work = cost;
    int n = n0, m = m0;

    if (n0 > m0) {
        transposed = true;
        // Transpose the matrix
        work = CostMatrix(m0, n0);
        for (int i = 0; i < n0; ++i) {
            for (int j = 0; j < m0; ++j) {
                work.at(j, i) = cost.at(i, j);
                work.mask[j * n0 + i] = cost.mask[i * m0 + j];
            }
        }
        n = work.nrow;  // now m0
        m = work.ncol;  // now n0
    }

    // Prepare working costs (negated if maximize)
    CostMatrix work_costs = prepare_for_solve(work, maximize);

    // Check feasibility
    ensure_each_row_has_option(work_costs.mask, n, m);

    // Build flow network
    // Nodes: S=0, rows 1..n, cols n+1..n+m, T=n+m+1
    const int S = 0;
    const int T = 1 + n + m;
    const int N = T + 1;

    struct Edge {
        int to;
        int rev;
        int cap;
        double cost;
    };

    std::vector<std::vector<Edge>> G(N);

    auto addEdge = [&](int u, int v, int cap, double cost) {
        Edge forward{v, static_cast<int>(G[v].size()), cap, cost};
        Edge backward{u, static_cast<int>(G[u].size()), 0, -cost};
        G[u].push_back(forward);
        G[v].push_back(backward);
    };

    // s -> rows (capacity 1, cost 0)
    for (int i = 0; i < n; ++i) {
        addEdge(S, 1 + i, 1, 0.0);
    }

    // rows -> cols for allowed edges only
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            if (!work_costs.allowed(i, j)) continue;  // forbidden
            double cij = work_costs.at(i, j);
            if (!std::isfinite(cij)) continue;  // skip +Inf just in case
            addEdge(1 + i, 1 + n + j, 1, cij);
        }
    }

    // cols -> t (capacity 1, cost 0)
    for (int j = 0; j < m; ++j) {
        addEdge(1 + n + j, T, 1, 0.0);
    }

    // Potentials for reduced costs
    std::vector<double> pi(N, 0.0);

    // Dijkstra function to find shortest path from S to T
    auto dijkstra = [&](std::vector<int>& pv_v, std::vector<int>& pv_e) -> bool {
        const double INF = std::numeric_limits<double>::infinity();
        std::vector<double> dist(N, INF);
        pv_v.assign(N, -1);
        pv_e.assign(N, -1);

        using P = std::pair<double, int>;
        std::priority_queue<P, std::vector<P>, std::greater<P>> pq;
        dist[S] = 0.0;
        pq.push({0.0, S});

        while (!pq.empty()) {
            auto [d, u] = pq.top();
            pq.pop();
            if (d != dist[u]) continue;

            for (int ei = 0; ei < static_cast<int>(G[u].size()); ++ei) {
                const Edge& e = G[u][ei];
                if (e.cap <= 0) continue;
                double rc = e.cost + pi[u] - pi[e.to];  // reduced cost
                double nd = d + rc;
                if (nd < dist[e.to]) {
                    dist[e.to] = nd;
                    pv_v[e.to] = u;
                    pv_e[e.to] = ei;
                    pq.push({nd, e.to});
                }
            }
        }

        if (!std::isfinite(dist[T])) return false;

        // Update potentials
        for (int v = 0; v < N; ++v) {
            if (std::isfinite(dist[v])) {
                pi[v] += dist[v];
            }
        }

        return true;
    };

    // Send n units of flow via successive shortest paths
    int flow = 0;
    std::vector<int> pv_v, pv_e;
    while (flow < n) {
        if (!dijkstra(pv_v, pv_e)) {
            LAP_THROW_INFEASIBLE("Could not send full flow");
        }

        // Augment along path S -> ... -> T
        int v = T;
        while (v != S) {
            int u = pv_v[v];
            int ei = pv_e[v];
            Edge& e = G[u][ei];
            Edge& er = G[v][e.rev];
            e.cap -= 1;
            er.cap += 1;
            v = u;
        }
        ++flow;
    }

    // Extract matching from saturated row->col edges (in work orientation)
    std::vector<int> match_work(n, -1);
    for (int i = 0; i < n; ++i) {
        int u = 1 + i;
        for (const Edge& e : G[u]) {
            // Original forward arc had cap 1; if cap==0 now, it's used
            if (e.to >= 1 + n && e.to < 1 + n + m && e.cap == 0) {
                int j = e.to - (1 + n);
                match_work[i] = j;
                break;
            }
        }
        if (match_work[i] < 0) {
            LAP_THROW_INFEASIBLE("Incomplete assignment");
        }
    }

    // Verify matching and compute total cost using ORIGINAL costs (in work orientation)
    double total = 0.0;
    for (int i = 0; i < n; ++i) {
        int j = match_work[i];
        if (j < 0) {
            LAP_THROW_INFEASIBLE("Could not find full matching");
        }
        if (!work.allowed(i, j)) {
            LAP_THROW_INFEASIBLE("Chosen forbidden edge");
        }
        double c = work.at(i, j);
        if (!std::isfinite(c)) {
            LAP_THROW_INFEASIBLE("Chosen edge has non-finite cost");
        }
        total += c;
    }

    // Map back to original orientation if we transposed
    std::vector<int> assignment;
    if (!transposed) {
        // No transpose: direct mapping (0-based)
        assignment = match_work;
    } else {
        // Transposed: work is m0 x n0; match_work length m0
        // match_work[i] (orig col i) -> matched to j (orig row j)
        assignment.assign(n0, -1);
        for (int i = 0; i < m0; ++i) {
            int j = match_work[i];
            if (j >= 0) {
                assignment[j] = i;  // original row j -> original col i
            }
        }
    }

    return LapResult(std::move(assignment), total, "optimal");
}

}  // namespace lap
