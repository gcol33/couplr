// src/solvers/solve_push_relabel.cpp
// Pure C++ Push-Relabel LAP solver - NO Rcpp dependencies

#include "solve_push_relabel.h"
#include "../core/lap_error.h"
#include "../core/lap_utils.h"
#include <vector>
#include <queue>
#include <limits>
#include <algorithm>
#include <cmath>

namespace lap {

LapResult solve_push_relabel(const CostMatrix& cost, bool maximize) {
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

    // Check feasibility: each row must have at least one allowed column
    ensure_each_row_has_option(work.mask, n, m);

    // Network structure for min-cost max-flow
    // Nodes: source (0), rows 1..n, cols n+1..n+m, sink n+m+1
    const int SOURCE = 0;
    const int SINK = n + m + 1;
    const int NUM_NODES = SINK + 1;

    // Edge structure for residual graph
    struct Edge {
        int to;
        int cap;      // residual capacity
        double cost;  // reduced cost
        int rev;      // index of reverse edge
    };

    std::vector<std::vector<Edge>> graph(NUM_NODES);

    auto add_edge = [&](int from, int to, int cap, double cost) {
        int rev_from = static_cast<int>(graph[to].size());
        int rev_to = static_cast<int>(graph[from].size());
        graph[from].push_back({to, cap, cost, rev_from});
        graph[to].push_back({from, 0, -cost, rev_to});
    };

    // Add source -> row edges (cap 1, cost 0)
    for (int i = 0; i < n; ++i) {
        add_edge(SOURCE, 1 + i, 1, 0.0);
    }

    // Add row -> col edges (cap 1, cost work[i][j])
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            if (work.allowed(i, j)) {
                double c = work.at(i, j);
                if (!std::isfinite(c)) c = BIG;
                add_edge(1 + i, n + 1 + j, 1, c);
            }
        }
    }

    // Add col -> sink edges (cap 1, cost 0)
    for (int j = 0; j < m; ++j) {
        add_edge(n + 1 + j, SINK, 1, 0.0);
    }

    // Push-Relabel with cost optimization
    // Uses successive shortest paths (Dijkstra with potentials)
    std::vector<double> potential(NUM_NODES, 0.0);
    std::vector<double> dist(NUM_NODES);
    std::vector<int> prev_node(NUM_NODES);
    std::vector<int> prev_edge(NUM_NODES);

    int total_flow = 0;

    // Find n augmenting paths (one per row)
    while (total_flow < n) {
        // Dijkstra with potentials (reduced costs)
        const double INF = std::numeric_limits<double>::infinity();
        std::fill(dist.begin(), dist.end(), INF);
        std::fill(prev_node.begin(), prev_node.end(), -1);
        std::fill(prev_edge.begin(), prev_edge.end(), -1);

        dist[SOURCE] = 0.0;
        std::priority_queue<std::pair<double, int>,
                            std::vector<std::pair<double, int>>,
                            std::greater<std::pair<double, int>>> pq;
        pq.push({0.0, SOURCE});

        while (!pq.empty()) {
            auto p = pq.top();
            double d = p.first;
            int u = p.second;
            pq.pop();

            if (d > dist[u] + TOL) continue;

            for (int ei = 0; ei < static_cast<int>(graph[u].size()); ++ei) {
                const Edge& e = graph[u][ei];
                if (e.cap <= 0) continue;

                // Reduced cost
                double rc = e.cost + potential[u] - potential[e.to];
                double nd = dist[u] + rc;

                if (nd + TOL < dist[e.to]) {
                    dist[e.to] = nd;
                    prev_node[e.to] = u;
                    prev_edge[e.to] = ei;
                    pq.push({nd, e.to});
                }
            }
        }

        if (!std::isfinite(dist[SINK])) {
            LAP_THROW_INFEASIBLE("Infeasible: could not find augmenting path");
        }

        // Update potentials
        for (int v = 0; v < NUM_NODES; ++v) {
            if (std::isfinite(dist[v])) {
                potential[v] += dist[v];
            }
        }

        // Augment along path (capacity = 1 for assignment)
        int aug = 1;
        int v = SINK;
        while (v != SOURCE) {
            int u = prev_node[v];
            int ei = prev_edge[v];
            Edge& e = graph[u][ei];
            Edge& rev = graph[v][e.rev];

            e.cap -= aug;
            rev.cap += aug;

            v = u;
        }

        total_flow += aug;
    }

    // Extract matching from residual graph
    std::vector<int> assignment(n, -1);

    for (int i = 0; i < n; ++i) {
        int row_node = 1 + i;
        for (const Edge& e : graph[row_node]) {
            // Check if this is an edge to a column (used = no residual capacity)
            if (e.to >= n + 1 && e.to <= n + m && e.cap == 0) {
                int j = e.to - (n + 1);
                assignment[i] = j;
                break;
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
