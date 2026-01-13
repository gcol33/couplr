#include <Rcpp.h>
#include <vector>
#include <queue>
#include <limits>
#include <algorithm>
#include "../core/lap_internal.h"
#include "../core/lap_utils_rcpp.h"

using namespace Rcpp;

Rcpp::List prepare_cost_matrix_impl(NumericMatrix cost, bool maximize);

// Min-cost flow via Successive Shortest Path with potentials (Johnson).
// Graph: s -> rows (cap 1, cost 0), rows -> cols (cap 1, cost cij), cols -> t (cap 1, cost 0).
// Works for n <= m, NA handled by omitting edges (forbidden).
// Auto-transposes when n > m.
Rcpp::List solve_ssp_impl(NumericMatrix cost, bool maximize) {
  int n0 = cost.nrow(), m0 = cost.ncol();
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

  // Prepared cost for solving (flip if maximize), and original for reporting.
  List Wp = prepare_cost_matrix_impl(work, maximize);
  List Op = prepare_cost_matrix_impl(work, false);
  NumericVector W = Wp["cost"];     // row-major
  IntegerVector MW = Wp["mask"];    // 0 allowed, 1 forbidden
  NumericVector O = Op["cost"];     // row-major
  IntegerVector MO = Op["mask"];

  // Infeasible row check
    ensure_each_row_has_option(MW, n, m);


  struct Edge { int to, rev, cap; double cost; };
  const int S = 0, T = 1 + n + m; // s=0, rows 1..n, cols n+1..n+m, t=T
  const int N = T + 1;
  std::vector<std::vector<Edge>> G(N);

  auto addEdge = [&](int u, int v, int cap, double cost) {
    Edge a{v, (int)G[v].size(), cap, cost};
    Edge b{u, (int)G[u].size(), 0,  -cost};
    G[u].push_back(a); G[v].push_back(b);
  };

  // s -> rows
  for (int i = 0; i < n; ++i) addEdge(S, 1 + i, 1, 0.0);
  // rows -> cols for allowed edges only
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < m; ++j) {
      if (MW[i*m + j]) continue;               // forbidden
      double cij = W[i*m + j];
      if (!R_finite(cij)) continue;            // skip +Inf just in case
      addEdge(1 + i, 1 + n + j, 1, cij);
    }
  }
  // cols -> t
  for (int j = 0; j < m; ++j) addEdge(1 + n + j, T, 1, 0.0);

  // Potentials
  std::vector<double> pi(N, 0.0);

  auto dijkstra = [&](std::vector<int>& pv_v, std::vector<int>& pv_e)->bool {
    const double INF = std::numeric_limits<double>::infinity();
    std::vector<double> dist(N, INF);
    pv_v.assign(N, -1); pv_e.assign(N, -1);

    using P = std::pair<double,int>;
    std::priority_queue<P, std::vector<P>, std::greater<P>> pq;
    dist[S] = 0.0; pq.push({0.0, S});

    while (!pq.empty()) {
      auto [d,u] = pq.top(); pq.pop();
      if (d != dist[u]) continue;
      for (int ei = 0; ei < (int)G[u].size(); ++ei) {
        const Edge& e = G[u][ei];
        if (e.cap <= 0) continue;
        double rc = e.cost + pi[u] - pi[e.to]; // reduced cost
        double nd = d + rc;
        if (nd < dist[e.to]) {
          dist[e.to] = nd;
          pv_v[e.to] = u; pv_e[e.to] = ei;
          pq.push({nd, e.to});
        }
      }
    }
    if (!R_finite(dist[T])) return false;
    // Update potentials
    for (int v = 0; v < N; ++v) if (R_finite(dist[v])) pi[v] += dist[v];
    return true;
  };

  int flow = 0;
  std::vector<int> pv_v, pv_e;
  while (flow < n) {
    if (!dijkstra(pv_v, pv_e)) LAP_ERROR("Infeasible: could not send full flow");
    // Augment along path S->...->T
    int v = T;
    while (v != S) {
      int u = pv_v[v], ei = pv_e[v];
      Edge &e = G[u][ei], &er = G[v][e.rev];
      e.cap -= 1; er.cap += 1;
      v = u;
    }
    ++flow;
  }

  // Extract matching from saturated row->col edges (in work orientation)
  IntegerVector match_work(n); std::fill(match_work.begin(), match_work.end(), 0);
  for (int i = 0; i < n; ++i) {
    int u = 1 + i;
    for (const Edge& e : G[u]) {
      // original forward arc had cap 1; if cap==0 now, it's used
      if (e.to >= 1 + n && e.to < 1 + n + m && e.cap == 0) {
        int j = e.to - (1 + n);
        match_work[i] = j + 1; // 1-based
        break;
      }
    }
    if (match_work[i] == 0) LAP_ERROR("Infeasible: incomplete assignment");
  }

  // Compute total on original costs (in work orientation)
  double total = 0.0;
  for (int i = 0; i < n; ++i) {
    int j = match_work[i] - 1;
    if (MO[i*m + j]) LAP_ERROR("Infeasible: chosen forbidden edge");
    double c = O[i*m + j];
    if (!R_finite(c)) LAP_ERROR("Infeasible");
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
