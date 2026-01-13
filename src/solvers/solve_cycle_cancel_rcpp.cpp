// src/solve_cycle_cancel.cpp
// [[Rcpp::plugins(cpp17)]]
#include <Rcpp.h>
#include <vector>
#include <queue>
#include <algorithm>
#include <limits>
#include <cmath>
#include "../core/lap_internal.h"
#include "../core/lap_utils_rcpp.h"

using namespace Rcpp;

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

} // namespace

Rcpp::List solve_cycle_cancel_impl(Rcpp::NumericMatrix cost, bool maximize) {
  const int n0 = cost.nrow();
  const int m0 = cost.ncol();
  
  if (n0 == 0 || m0 == 0) {
    LAP_ERROR("Cost matrix cannot be empty");
  }
  
  bool transposed = false;
  Rcpp::NumericMatrix C = cost;
  int n = n0, m = m0;
  
  if (n0 > m0) {
    C = transpose(cost);
    n = m0;
    m = n0;
    transposed = true;
  }
  
  double cmax = 0.0;
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < m; ++j) {
      double v = C(i, j);
      if (!ISNA(v) && std::isfinite(v) && v > cmax) {
        cmax = v;
      }
    }
  }
  
  const int s = n + m;
  const int t = n + m + 1;
  const int N = n + m + 2;
  std::vector<std::vector<Edge>> adj(N);
  
  for (int i = 0; i < n; ++i) {
    add_edge(adj, s, i, 1, 0.0);
  }
  
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < m; ++j) {
      double v = C(i, j);
      if (ISNA(v) || !std::isfinite(v)) continue;
      
      double w = maximize ? (cmax - v) : v;
      add_edge(adj, i, n + j, 1, w);
    }
  }
  
  for (int j = 0; j < m; ++j) {
    add_edge(adj, n + j, t, 1, 0.0);
  }
  
  double total_cost = 0.0;
  bool ok = ssp_feasible(adj, s, t, n, total_cost);
  
  if (!ok) {
    LAP_ERROR("Infeasible: forbidden edges block perfect matching");
  }
  
  int max_iters = n * m * 10;
  int iters = 0;
  
  while (iters < max_iters) {
    ++iters;
    
    double mu;
    std::vector<int> nodes;
    
    bool found = karp_min_mean_cycle(adj, mu, nodes);
    
    if (!found) break;
    
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
    
    for (Edge* e : cyc_edges) {
      e->cap -= theta;
      int rev_idx = e->rev;
      adj[e->to][rev_idx].cap += theta;
    }
  }
  
  std::vector<int> match(n0, 0);
  double total = 0.0;
  
  if (!transposed) {
    for (int i = 0; i < n; ++i) {
      for (const Edge& e : adj[i]) {
        int j_node = e.to;
        if (j_node >= n && j_node < n + m) {
          int j = j_node - n;
          if (e.cap == 0) {
            match[i] = j + 1;
            total += C(i, j);
            break;
          }
        }
      }
    }
  } else {
    for (int i = 0; i < n; ++i) {
      for (const Edge& e : adj[i]) {
        int j_node = e.to;
        if (j_node >= n && j_node < n + m) {
          int j = j_node - n;
          if (e.cap == 0) {
            match[j] = i + 1;
            total += cost(j, i);
            break;
          }
        }
      }
    }
  }
  
  return make_result(match, total);
}
