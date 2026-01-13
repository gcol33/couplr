// src/solve_csflow.cpp
// [[Rcpp::plugins(cpp17)]]
#include <Rcpp.h>
#include <vector>
#include <queue>
#include <limits>
#include <cmath>
#include "../core/lap_internal.h"
#include "../core/lap_utils_rcpp.h"

using namespace Rcpp;

Rcpp::List prepare_cost_matrix_impl(NumericMatrix cost, bool maximize);

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
  MCMF(int n): N(n), G(n) {}

  void add_edge(int u, int v, int cap, double cost) {
    G[u].emplace_back(v, (int)G[v].size(), cap, cost);
    G[v].emplace_back(u, (int)G[u].size()-1, 0, -cost);
  }

  std::pair<int,double> min_cost_max_flow(int s, int t, int maxf) {
    const double INF = std::numeric_limits<double>::infinity();
    std::vector<double> pi(N, 0.0);
    std::vector<double> dist(N);
    std::vector<int> pv(N), pe(N);

    int flow = 0; double cost = 0.0;

    while (flow < maxf) {
      std::fill(dist.begin(), dist.end(), INF);
      std::fill(pv.begin(), pv.end(), -1);
      std::fill(pe.begin(), pe.end(), -1);

      using Q = std::pair<double,int>;
      std::priority_queue<Q, std::vector<Q>, std::greater<Q>> pq;
      dist[s] = 0.0;
      pq.emplace(0.0, s);

      while (!pq.empty()) {
        auto cur = pq.top(); pq.pop();
        double d = cur.first; int u = cur.second;
        if (d != dist[u]) continue;
        for (int ei = 0; ei < (int)G[u].size(); ++ei) {
          const Edge &e = G[u][ei];
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

      for (int v = 0; v < N; ++v) if (std::isfinite(dist[v])) pi[v] += dist[v];

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
        Edge &e = G[u][ei];
        Edge &er = G[v][e.rev];
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

// ---------- Assignment wrapper with internal auto-transpose ----------
Rcpp::List solve_csflow_impl(NumericMatrix cost, bool maximize) {
  int n0 = cost.nrow();
  int m0 = cost.ncol();
  if (n0 == 0)   if (n0 == 0) return make_result(IntegerVector(), 0.0);

  // If n>m, solve on transposed matrix and map back.
  bool transposed = false;
  NumericMatrix work = cost;
  int n = n0, m = m0;
  if (n0 > m0) {
    transposed = true;
    work = transpose(cost);
    n = work.nrow();  // == m0
    m = work.ncol();  // == n0
  }

  // Prepared working costs (respects maximize, NA mask) and original costs (for reporting)
  List Wp = prepare_cost_matrix_impl(work, maximize);
  List Op = prepare_cost_matrix_impl(work, false);

  NumericVector Wnv = Wp["cost"];  // row-major n*m of work
  IntegerVector Mnv = Wp["mask"];  // 0 allowed, 1 forbidden

  NumericVector Onv = Op["cost"];  // original-scale costs for total
  IntegerVector Monv = Op["mask"];

  // Feasibility: each row must have at least one allowed finite edge
  for (int i = 0; i < n; ++i) {
    bool ok = false;
    for (int j = 0; j < m; ++j) {
      int k = i*m + j;
      if (Mnv[k] == 0 && std::isfinite(Wnv[k])) { ok = true; break; }
    }
    if (!ok) LAP_ERROR("Infeasible: row %d has no allowed edges", i+1);
  }

  // Build flow network on work (n <= m always holds here)
  const int S = 0;
  const int R0 = 1;
  const int C0 = R0 + n;
  const int T  = C0 + m;
  const int NN = T + 1;

  MCMF mf(NN);

  for (int i = 0; i < n; ++i) mf.add_edge(S, R0 + i, 1, 0.0);

  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < m; ++j) {
      int k = i*m + j;
      if (Mnv[k]) continue;
      double c = Wnv[k];
      if (!std::isfinite(c)) continue;
      mf.add_edge(R0 + i, C0 + j, 1, c);
    }
  }

  for (int j = 0; j < m; ++j) mf.add_edge(C0 + j, T, 1, 0.0);

  auto result = mf.min_cost_max_flow(S, T, n);
  int pushed = result.first;
  if (pushed < n) LAP_ERROR("Infeasible: only %d/%d units of flow sent", pushed, n);

  // Recover matching in work orientation (length n)
  IntegerVector match_work(n); std::fill(match_work.begin(), match_work.end(), 0);
  for (int i = 0; i < n; ++i) {
    const auto &adj = mf.G[R0 + i];
    for (int ei = 0; ei < (int)adj.size(); ++ei) {
      const Edge &e = adj[ei];
      if (e.to >= C0 && e.to < C0 + m) {
        const Edge &back = mf.G[e.to][ e.rev ];
        if (back.cap > 0) {
          int j = e.to - C0;
          match_work[i] = j + 1; // 1-based
          break;
        }
      }
    }
  }

  // Compute total cost on ORIGINAL scale
  double total = 0.0;
  for (int i = 0; i < n; ++i) {
    int j = match_work[i] - 1;
    int k = i*m + j;
    if (Monv[k]) LAP_ERROR("Infeasible: chosen forbidden edge");
    double c = Onv[k];
    if (!std::isfinite(c)) LAP_ERROR("Infeasible");
    total += c;
  }

  // Map back to original orientation if we transposed: produce length n0 vector
  IntegerVector match_out;
  if (!transposed) {
    match_out = match_work; // rows (n0) -> cols (m0)
  } else {
    // work is m0 x n0; match_work length m0: row i (orig col i) matched to col j (orig row j)
    match_out = IntegerVector(n0); std::fill(match_out.begin(), match_out.end(), 0);
    for (int i = 0; i < m0; ++i) {
      int j = match_work[i] - 1;        // j in [0, n0)
      if (j >= 0) match_out[j] = i + 1; // original row j -> original col i
    }
  }

    return make_result(match_out, total);
}
