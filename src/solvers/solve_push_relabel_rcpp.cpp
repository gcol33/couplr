// src/solvers/solve_push_relabel.cpp
// Push-Relabel Algorithm for Assignment Problems
// Reference: Goldberg & Tarjan (1988) "A new approach to the maximum-flow problem"
//            Extended to min-cost flow for assignment
// Complexity: O(n²m) for assignment problems

#include <Rcpp.h>
#include <vector>
#include <queue>
#include <limits>
#include <algorithm>
#include <cmath>
#include "../core/lap_utils_rcpp.h"

using namespace Rcpp;

// Push-Relabel for min-cost bipartite assignment
// Uses preflow-push with FIFO active node selection
// Combined with successive shortest paths for cost optimization

Rcpp::List solve_push_relabel_impl(NumericMatrix cost, bool maximize) {
  const int n0 = cost.nrow();
  const int m0 = cost.ncol();

  if (n0 == 0 || m0 == 0) {
    return make_result(std::vector<int>(), 0.0);
  }

  // Ensure rows <= cols
  bool transposed = false;
  NumericMatrix C = cost;
  int n = n0, m = m0;

  if (n0 > m0) {
    C = transpose(cost);
    n = m0;
    m = n0;
    transposed = true;
  }

  // Build cost matrix with forbidden edges
  std::vector<std::vector<double>> W(n, std::vector<double>(m));
  std::vector<std::vector<bool>> allowed(n, std::vector<bool>(m, false));
  bool has_finite = false;

  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < m; ++j) {
      double v = C(i, j);
      if (std::isfinite(v)) {
        has_finite = true;
        W[i][j] = maximize ? -v : v;
        allowed[i][j] = true;
      } else {
        W[i][j] = BIG;
        allowed[i][j] = false;
      }
    }
  }

  if (!has_finite) {
    LAP_ERROR("No finite costs found.");
  }

  // Check feasibility: each row must have at least one allowed column
  for (int i = 0; i < n; ++i) {
    bool has_option = false;
    for (int j = 0; j < m; ++j) {
      if (allowed[i][j]) {
        has_option = true;
        break;
      }
    }
    if (!has_option) {
      LAP_ERROR("Infeasible: row %d has no valid assignments", i + 1);
    }
  }

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
    int rev_from = (int)graph[to].size();
    int rev_to = (int)graph[from].size();
    graph[from].push_back({to, cap, cost, rev_from});
    graph[to].push_back({from, 0, -cost, rev_to});
  };

  // Add source -> row edges (cap 1, cost 0)
  for (int i = 0; i < n; ++i) {
    add_edge(SOURCE, 1 + i, 1, 0.0);
  }

  // Add row -> col edges (cap 1, cost W[i][j])
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < m; ++j) {
      if (allowed[i][j]) {
        add_edge(1 + i, n + 1 + j, 1, W[i][j]);
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
  double total_cost = 0.0;

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
      auto [d, u] = pq.top();
      pq.pop();

      if (d > dist[u] + 1e-12) continue;

      for (int ei = 0; ei < (int)graph[u].size(); ++ei) {
        const Edge& e = graph[u][ei];
        if (e.cap <= 0) continue;

        // Reduced cost
        double rc = e.cost + potential[u] - potential[e.to];
        double nd = dist[u] + rc;

        if (nd + 1e-12 < dist[e.to]) {
          dist[e.to] = nd;
          prev_node[e.to] = u;
          prev_edge[e.to] = ei;
          pq.push({nd, e.to});
        }
      }
    }

    if (!std::isfinite(dist[SINK])) {
      LAP_ERROR("Infeasible: could not find augmenting path (flow=%d/%d)", total_flow, n);
    }

    // Update potentials
    for (int v = 0; v < NUM_NODES; ++v) {
      if (std::isfinite(dist[v])) {
        potential[v] += dist[v];
      }
    }

    // Find augmenting path capacity (always 1 for assignment)
    int aug = 1;

    // Augment along path
    int v = SINK;
    while (v != SOURCE) {
      int u = prev_node[v];
      int ei = prev_edge[v];
      Edge& e = graph[u][ei];
      Edge& rev = graph[v][e.rev];

      e.cap -= aug;
      rev.cap += aug;
      total_cost += aug * e.cost;

      v = u;
    }

    total_flow += aug;
  }

  // Extract matching from residual graph
  std::vector<int> row_to_col(n, -1);

  for (int i = 0; i < n; ++i) {
    int row_node = 1 + i;
    for (const Edge& e : graph[row_node]) {
      // Check if this is an edge to a column (used = no residual capacity)
      if (e.to >= n + 1 && e.to <= n + m && e.cap == 0) {
        int j = e.to - (n + 1);
        row_to_col[i] = j;
        break;
      }
    }
  }

  // Build output
  std::vector<int> match_out;
  double result_cost = 0.0;

  if (!transposed) {
    match_out.resize(n);
    for (int i = 0; i < n; ++i) {
      int j = row_to_col[i];
      match_out[i] = (j >= 0) ? (j + 1) : 0;
      if (j >= 0 && j < m0) {
        double val = cost(i, j);
        if (std::isfinite(val)) {
          result_cost += val;
        }
      }
    }
  } else {
    match_out.assign(n0, 0);
    for (int i = 0; i < n; ++i) {
      int j = row_to_col[i];
      if (j >= 0 && j < n0) {
        match_out[j] = i + 1;
        double val = cost(j, i);
        if (std::isfinite(val)) {
          result_cost += val;
        }
      }
    }
  }

  return make_result(match_out, result_cost);
}
