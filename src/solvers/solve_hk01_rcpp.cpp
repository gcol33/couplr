// src/solve_hk01.cpp
// [[Rcpp::plugins(cpp17)]]
#include <Rcpp.h>
#include <vector>
#include <queue>
#include <limits>
#include <cmath>
#include <algorithm>
#include "../core/lap_internal.h"
#include "../core/lap_utils_rcpp.h"

using namespace Rcpp;

// Provided elsewhere
Rcpp::List prepare_cost_matrix_impl(Rcpp::NumericMatrix cost, bool maximize);

// We may fall back to this when 0-cost HK can't reach perfect on {0,1}
Rcpp::List lap_solve_csflow(Rcpp::NumericMatrix cost, bool maximize);

// ---------------- Hopcroft–Karp for bipartite matching ----------------
struct HK {
  int nL, nR;
  std::vector<std::vector<int>> adj;  // 0..nL-1 -> list of right nodes (0..nR-1)

  // matching: pairU[u] = matched v+1 (or 0), pairV[v] = matched u+1 (or 0)
  std::vector<int> pairU, pairV, dist;

  HK(int nL, int nR) : nL(nL), nR(nR), adj(nL), pairU(nL+1,0), pairV(nR+1,0), dist(nL+1,0) {}

  void add_edge(int u, int v) { adj[u].push_back(v); }

  // BFS layers from free U
  bool bfs() {
    std::queue<int> q;
    const int INF = std::numeric_limits<int>::max();
    for (int u = 0; u < nL; ++u) {
      if (pairU[u+1] == 0) { dist[u+1] = 0; q.push(u+1); }
      else dist[u+1] = INF;
    }
    bool reached_free = false;
    while (!q.empty()) {
      int u1 = q.front(); q.pop();
      int u = u1 - 1;
      for (int v : adj[u]) {
        int v1 = v + 1;
        int u1next = pairV[v1]; // matched partner on left (or 0)
        if (u1next == 0) {
          reached_free = true;      // we can reach a free V on this layer
        } else if (dist[u1next] == INF) {
          dist[u1next] = dist[u1] + 1;
          q.push(u1next);
        }
      }
    }
    return reached_free;
  }

  bool dfs(int u1) {
    int u = u1 - 1;
    for (int v : adj[u]) {
      int v1 = v + 1;
      int u1next = pairV[v1];
      if (u1next == 0 || (dist[u1next] == dist[u1] + 1 && dfs(u1next))) {
        pairU[u1] = v1;
        pairV[v1] = u1;
        return true;
      }
    }
    dist[u1] = std::numeric_limits<int>::max();
    return false;
  }

  int max_matching() {
    int matching = 0;
    while (bfs()) {
      for (int u = 0; u < nL; ++u) {
        if (pairU[u+1] == 0 && dfs(u+1)) matching++;
      }
    }
    return matching;
  }
};

// ---------------- Utility: check cost palette ----------------
struct Palette {
  bool all_equal = false;
  bool is_binary01 = false;
  double equal_value = 0.0;
};

static Palette analyze_palette(const NumericVector &W, const IntegerVector &mask) {
  Palette p;
  double a = 0.0, b = 0.0;
  bool has_a = false, has_b = false;
  for (int k = 0; k < W.size(); ++k) {
    if (mask[k]) continue;               // forbidden
    double c = W[k];
    if (!std::isfinite(c)) continue;
    if (!has_a) { a = c; has_a = true; continue; }
    if (std::fabs(c - a) < 1e-18) continue;
    if (!has_b) { b = c; has_b = true; continue; }
    if (std::fabs(c - b) < 1e-18) continue;
    // Found >= 3 distinct values -> not 0/1 or all equal
    p.all_equal = false; p.is_binary01 = false; return p;
  }
  if (!has_a) { p.all_equal = true; p.equal_value = 0.0; return p; } // empty treated as equal
  if (!has_b) { p.all_equal = true; p.equal_value = a; return p; }
  // Two distinct values: check if they are {0,1} up to tolerance
  double x = std::min(a,b), y = std::max(a,b);
  p.is_binary01 = (std::fabs(x) < 1e-12) && (std::fabs(y - 1.0) < 1e-12);
  p.all_equal = false;
  return p;
}

// ---------------- Main solver ----------------
// Non-exported impl; wrapper is in rcpp_interface.cpp
Rcpp::List solve_hk01_impl(NumericMatrix cost, bool maximize) {
  int n0 = cost.nrow(), m0 = cost.ncol();
  if (n0 == 0)   if (n0 == 0) return make_result(IntegerVector(), 0.0);

  // Auto-transpose for n>m
  bool transposed = false;
  NumericMatrix work = cost;
  int n = n0, m = m0;
  if (n0 > m0) {
    work = transpose(cost);
    std::swap(n, m);
    transposed = true;
  }

  // Prepared costs (respect maximize, mask) and original for reporting
  List Wp = prepare_cost_matrix_impl(work, maximize);
  List Op = prepare_cost_matrix_impl(work, false);

  NumericVector Wnv = Wp["cost"];  // length n*m, row-major
  IntegerVector Mnv = Wp["mask"];  // 0 allowed, 1 forbidden

  NumericVector Onv = Op["cost"];
  IntegerVector Monv = Op["mask"];

  // Feasibility quick check: each row must have at least one allowed finite edge
  for (int i = 0; i < n; ++i) {
    bool ok = false;
    for (int j = 0; j < m; ++j) {
      int k = i*m + j;
      if (Mnv[k] == 0 && std::isfinite(Wnv[k])) { ok = true; break; }
    }
    if (!ok) LAP_ERROR("Infeasible: row %d has no allowed edges", i+1);
  }

  // Inspect palette
  Palette pal = analyze_palette(Wnv, Mnv);

  // Build HK graph adjacency according to palette
  HK hk(n, m);

  auto build_edges = [&](int mode) {
    // mode = 0 -> all finite allowed edges
    // mode = 1 -> only zero-cost edges
    for (int i = 0; i < n; ++i) {
      for (int j = 0; j < m; ++j) {
        int k = i*m + j;
        if (Mnv[k]) continue;
        double c = Wnv[k];
        if (!std::isfinite(c)) continue;
        if (mode == 1) {
          if (std::fabs(c) < 1e-12) hk.add_edge(i, j);
        } else {
          hk.add_edge(i, j);
        }
      }
    }
  };

  IntegerVector match_work(n); std::fill(match_work.begin(), match_work.end(), 0);
  double total = 0.0;

  if (pal.all_equal) {
    // Any perfect matching is optimal
    build_edges(0);
    int mm = hk.max_matching();
    if (mm < n) LAP_ERROR("Infeasible: could not find perfect matching");
    // Recover matching (pairU is 1-based)
    for (int i = 0; i < n; ++i) match_work[i] = hk.pairU[i+1]; // already 1-based col
    // Compute total on ORIGINAL (all equal on allowed edges)
    for (int i = 0; i < n; ++i) {
      int j = match_work[i] - 1;
      int k = i*m + j;
      if (Monv[k]) LAP_ERROR("Infeasible: chosen forbidden edge");
      double c = Onv[k];
      if (!std::isfinite(c)) LAP_ERROR("Infeasible");
      total += c;
    }
  } else if (pal.is_binary01) {
    // Try a zero-cost perfect matching first (fast)
    build_edges(1);
    int mm = hk.max_matching();
    if (mm == n) {
      for (int i = 0; i < n; ++i) match_work[i] = hk.pairU[i+1];
      // total on ORIGINAL costs -> all zeros by definition of Wnv, but recompute to be safe
      for (int i = 0; i < n; ++i) {
        int j = match_work[i] - 1;
        int k = i*m + j;
        if (Monv[k]) LAP_ERROR("Infeasible: chosen forbidden edge");
        double c = Onv[k];
        if (!std::isfinite(c)) LAP_ERROR("Infeasible");
        total += c;
      }
    } else {
      // Need to include some 1-edges: fall back to exact weighted solver (fast on sparse)
      List w = lap_solve_csflow(work, maximize);
      IntegerVector best = w["match"];
      double best_total = as<double>(w["total_cost"]);
      match_work = clone(best);
      total = best_total;
    }
  } else {
    LAP_ERROR("hk01: cost matrix is neither all-equal nor binary {0,1} after preparation");
  }

  // Map back if we transposed
  IntegerVector match_out;
  if (!transposed) {
    match_out = match_work;
  } else {
    match_out = IntegerVector(n0); std::fill(match_out.begin(), match_out.end(), 0);
    for (int i = 0; i < m0; ++i) {
      int j = match_work[i] - 1;
      if (j >= 0) match_out[j] = i + 1;
    }
  }

    return make_result(match_out, total);
}
