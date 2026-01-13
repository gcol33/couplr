// src/solve_ssap_bucket.cpp
// Successive shortest path with Dial's bucket queue for integer costs
// [[Rcpp::plugins(cpp17)]]

#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <limits>
#include <cmath>
#include "../core/lap_utils_rcpp.h"

using namespace Rcpp;

namespace {

constexpr double INT_TOL = 1e-9;
constexpr int INF_INT = 1000000000;

inline bool is_int(double x) {
  return std::abs(x - std::round(x)) <= INT_TOL;
}

std::pair<int, bool> find_scale_factor(const std::vector<double>& finite_vals) {
  if (finite_vals.empty()) return {1, false};
  
  // Check if already integers
  bool all_int = true;
  for (double v : finite_vals) {
    if (!is_int(v)) {
      all_int = false;
      break;
    }
  }
  if (all_int) return {1, true};
  
  // Try scaling factors
  for (int scale : {10, 100, 1000}) {
    bool works = true;
    for (double v : finite_vals) {
      if (!is_int(v * scale)) {
        works = false;
        break;
      }
    }
    if (works) return {scale, true};
  }
  
  // Fallback: use 1000 and round
  return {1000, true};
}

// Min-cost flow edge structure
struct MCFEdge {
  int to;
  int rev;
  int cap;
  int cost;
};

struct MinCostFlowBuckets {
  int N;
  std::vector<std::vector<MCFEdge>> g;
  
  explicit MinCostFlowBuckets(int N_) : N(N_), g(N_) {}
  
  void add_edge(int fr, int to, int cap, int cost) {
    MCFEdge fwd{to, (int)g[to].size(), cap,  cost};
    MCFEdge rev{fr, (int)g[fr].size(), 0,   -cost};
    g[fr].push_back(fwd);
    g[to].push_back(rev);
  }
  
  // Successive shortest path with bucket queue
  // Returns (flow, total_cost)
  std::pair<int, long long> min_cost_flow(int s, int t, int maxf) {
    const int INF = INF_INT;
    std::vector<int> h(N, 0);        // node potentials
    std::vector<int> dist(N);
    std::vector<int> prevv(N);       // previous vertex
    std::vector<int> preve(N);       // previous edge index
    
    int flow = 0;
    long long total_cost = 0;
    
    while (flow < maxf) {
      // Dijkstra with Dial's bucket queue for reduced costs
      std::fill(dist.begin(), dist.end(), INF);
      dist[s] = 0;
      
      std::vector<std::vector<int>> buckets(1);
      int maxd = 0;
      
      auto push_bucket = [&](int d, int v) {
        if (d < 0) d = 0;
        if (d >= INF) return;
        while (d >= (int)buckets.size()) {
          buckets.emplace_back();
        }
        buckets[d].push_back(v);
        if (d > maxd) maxd = d;
      };
      
      push_bucket(0, s);
      int dcur = 0;
      
      // CRITICAL: Run FULL shortest path tree computation
      // Do NOT break when reaching t - need all distances for potential update
      while (dcur <= maxd) {
        // Find next non-empty bucket
        while (dcur <= maxd && 
               (dcur >= (int)buckets.size() || buckets[dcur].empty())) {
          ++dcur;
        }
        if (dcur > maxd) break;
        
        int v = buckets[dcur].back();
        buckets[dcur].pop_back();
        
        // Skip if stale
        if (dist[v] != dcur) continue;
        
        // Relax edges from v
        for (int ei = 0; ei < (int)g[v].size(); ++ei) {
          const MCFEdge &e = g[v][ei];
          if (e.cap <= 0) continue;
          
          // Reduced cost (should be >= 0 with correct potentials)
          int rcost = e.cost + h[v] - h[e.to];
          if (rcost < 0) rcost = 0;  // defensive clamp
          
          int nd = dist[v] + rcost;
          if (nd < dist[e.to] && nd < INF) {
            dist[e.to] = nd;
            prevv[e.to] = v;
            preve[e.to] = ei;
            push_bucket(nd, e.to);
          }
        }
      }
      
      // Check if path to t exists
      if (dist[t] == INF) {
        break;  // No augmenting path
      }
      
      // Update potentials using computed distances
      for (int v = 0; v < N; ++v) {
        if (dist[v] < INF) {
          h[v] += dist[v];
        }
      }
      
      // Find bottleneck capacity along path
      int d = maxf - flow;
      int v = t;
      while (v != s) {
        const MCFEdge &e = g[prevv[v]][preve[v]];
        if (e.cap < d) d = e.cap;
        v = prevv[v];
      }
      if (d <= 0) break;
      
      flow += d;
      
      // CRITICAL FIX: Cost of augmenting path is h[t] - h[s]
      // After potential updates, this equals the shortest path cost
      total_cost += (long long)d * (h[t] - h[s]);
      
      // Augment flow along path
      v = t;
      while (v != s) {
        MCFEdge &e = g[prevv[v]][preve[v]];
        e.cap -= d;
        g[v][e.rev].cap += d;
        v = prevv[v];
      }
    }
    
    return {flow, total_cost};
  }
};

} // namespace


Rcpp::List solve_ssap_bucket_impl(Rcpp::NumericMatrix cost, bool maximize) {
  const int n0 = cost.nrow();
  const int m0 = cost.ncol();
  
  // Handle empty matrix
  if (n0 == 0 || m0 == 0) {
    return make_result(std::vector<int>(), 0.0);
  }
  
  // Ensure n <= m by transposing if needed
  bool transposed = false;
  Rcpp::NumericMatrix C = cost;
  int n = n0, m = m0;
  
  if (n0 > m0) {
    C = transpose(cost);
    n = m0;
    m = n0;
    transposed = true;
  }
  
  // Collect finite values for scaling
  std::vector<double> finite_vals;
  finite_vals.reserve(n * m);
  double min_val = 0.0;  // Track minimum for shifting
  bool has_finite = false;
  
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < m; ++j) {
      double v = C(i, j);
      if (R_finite(v)) {
        finite_vals.push_back(v);
        if (!has_finite || v < min_val) {
          min_val = v;
          has_finite = true;
        }
      }
    }
  }
  
  if (finite_vals.empty()) {
    LAP_ERROR("No finite costs found");
  }
  
  // Shift to make all costs non-negative
  double shift = 0.0;
  if (min_val < 0.0) {
    shift = -min_val;  // Add this to make all >= 0
  }
  
  // Find appropriate scaling factor for absolute values
  std::vector<double> abs_vals;
  abs_vals.reserve(finite_vals.size());
  for (double v : finite_vals) {
    abs_vals.push_back(std::abs(v + shift));
  }
  
  auto [scale, success] = find_scale_factor(abs_vals);
  (void)success;
  
  // Build integer cost matrix: shift then scale
  std::vector<std::vector<int>> CI(n, std::vector<int>(m, INF_INT));
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < m; ++j) {
      double v = C(i, j);
      if (R_finite(v)) {
        // Shift to non-negative, then scale to integer
        CI[i][j] = (int)std::llround((v + shift) * scale);
      }
    }
  }
  
  // Transform for maximize
  if (maximize) {
    int cmax = 0;
    for (int i = 0; i < n; ++i) {
      for (int j = 0; j < m; ++j) {
        if (CI[i][j] < INF_INT && CI[i][j] > cmax) {
          cmax = CI[i][j];
        }
      }
    }
    for (int i = 0; i < n; ++i) {
      for (int j = 0; j < m; ++j) {
        if (CI[i][j] < INF_INT) {
          CI[i][j] = cmax - CI[i][j];
        }
      }
    }
  }
  
  // Feasibility check
  for (int i = 0; i < n; ++i) {
    bool has_edge = false;
    for (int j = 0; j < m; ++j) {
      if (CI[i][j] < INF_INT) {
        has_edge = true;
        break;
      }
    }
    if (!has_edge) {
      LAP_ERROR("Infeasible: row %d has no valid edges", i + 1);
    }
  }
  
  // Build min-cost flow network
  // Nodes: 0..n-1 (rows), n..n+m-1 (cols), s=n+m, t=n+m+1
  int N = n + m + 2;
  int s = n + m;
  int t = n + m + 1;
  
  MinCostFlowBuckets mcf(N);
  
  // Source to rows
  for (int i = 0; i < n; ++i) {
    mcf.add_edge(s, i, 1, 0);
  }
  
  // Rows to columns (assignment edges)
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < m; ++j) {
      if (CI[i][j] < INF_INT) {
        mcf.add_edge(i, n + j, 1, CI[i][j]);
      }
    }
  }
  
  // Columns to sink
  for (int j = 0; j < m; ++j) {
    mcf.add_edge(n + j, t, 1, 0);
  }
  
  // Solve min-cost flow
  auto [flow, total_cost_int] = mcf.min_cost_flow(s, t, n);
  (void)total_cost_int;  // We'll compute from original costs
  
  if (flow != n) {
    LAP_ERROR("Infeasible: could not find perfect matching");
  }
  
  // Extract matching from residual graph
  std::vector<int> row_match(n, -1);
  for (int i = 0; i < n; ++i) {
    for (const auto &e : mcf.g[i]) {
      int j = e.to - n;
      if (j >= 0 && j < m) {
        // Check if reverse edge has positive capacity (flow was sent)
        const MCFEdge &rev = mcf.g[e.to][e.rev];
        if (rev.cap > 0) {
          row_match[i] = j;
          break;
        }
      }
    }
  }
  
  // Build output with original cost (NOT shifted/scaled)
  std::vector<int> match(n0);
  double total = 0.0;
  
  if (!transposed) {
    for (int i = 0; i < n; ++i) {
      if (row_match[i] < 0) {
        LAP_ERROR("Internal error: unmatched row");
      }
      match[i] = row_match[i] + 1;  // 1-based
      total += cost(i, row_match[i]);  // Use ORIGINAL cost
    }
  } else {
    // Transposed: map back to original orientation
    std::fill(match.begin(), match.end(), 0);
    for (int i = 0; i < n; ++i) {
      int j = row_match[i];
      if (j < 0) {
        LAP_ERROR("Internal error: unmatched row (transposed)");
      }
      match[j] = i + 1;  // 1-based
      total += cost(j, i);  // Use ORIGINAL cost
    }
  }
  
  return make_result(match, total);
}
