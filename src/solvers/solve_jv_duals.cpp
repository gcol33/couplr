// src/solvers/solve_jv_duals.cpp
// Jonker-Volgenant solver with dual variable extraction
// Returns both the optimal assignment and the dual potentials (u, v)

#include <Rcpp.h>
#include <vector>
#include <limits>
#include <algorithm>
#include "../core/lap_internal.h"
#include "../core/lap_utils.h"

using namespace Rcpp;

Rcpp::List prepare_cost_matrix_impl(NumericMatrix cost, bool maximize);

// JV solver that returns dual variables (row potentials u, column potentials v)
// For an optimal assignment, the duals satisfy:
//   u[i] + v[j] <= c[i,j] for all (i,j) (minimization)
//   u[i] + v[j] >= c[i,j] for all (i,j) (maximization)
//   u[i] + v[j] = c[i,j] for assigned pairs (complementary slackness)
Rcpp::List solve_jv_duals_impl(NumericMatrix cost, bool maximize) {
  const int n = cost.nrow();
  const int m = cost.ncol();

  if (n == 0) {
    return Rcpp::List::create(
      Rcpp::Named("match") = IntegerVector(),
      Rcpp::Named("total_cost") = 0.0,
      Rcpp::Named("u") = NumericVector(),
      Rcpp::Named("v") = NumericVector()
    );
  }

  if (n > m) stop("Infeasible: number of rows greater than number of columns");

  // For dual variable extraction, we need to handle maximize specially
  // Using -cost directly (not cmax-cost) so duals transform cleanly
  List prep_orig = prepare_cost_matrix_impl(cost, false);
  NumericVector orig_cost_nv = prep_orig["cost"];
  IntegerVector orig_mask_iv = prep_orig["mask"];

  std::vector<double> orig_cost(orig_cost_nv.begin(), orig_cost_nv.end());

  // For maximization, use -cost (not cmax-cost) so duals are simply negated
  std::vector<double> work_cost(n * m);
  std::vector<int> work_mask(orig_mask_iv.begin(), orig_mask_iv.end());

  if (maximize) {
    for (int k = 0; k < n * m; ++k) {
      work_cost[k] = std::isfinite(orig_cost[k]) ? -orig_cost[k] : orig_cost[k];
    }
  } else {
    work_cost = orig_cost;
  }

  ensure_each_row_has_option(work_mask, n, m);

  // Hungarian with potentials
  std::vector<double> u(n + 1, 0.0), v(m + 1, 0.0);
  std::vector<int> p(m + 1, 0), way(m + 1, 0);

  for (int i = 1; i <= n; ++i) {
    p[0] = i;
    int j0 = 0;
    std::vector<double> minv(m + 1, std::numeric_limits<double>::infinity());
    std::vector<char> used(m + 1, 0);
    way[0] = 0;

    while (true) {
      used[j0] = 1;
      int i0 = p[j0];
      double delta = std::numeric_limits<double>::infinity();
      int j1 = 0;

      for (int j = 1; j <= m; ++j) {
        if (used[j]) continue;
        double cij = work_cost[(i0 - 1) * m + (j - 1)];
        if (!std::isfinite(cij)) cij = BIG;
        double cur = cij - u[i0] - v[j];
        if (cur < minv[j]) { minv[j] = cur; way[j] = j0; }
        if (minv[j] < delta) { delta = minv[j]; j1 = j; }
      }

      for (int j = 0; j <= m; ++j) {
        if (used[j]) { u[p[j]] += delta; v[j] -= delta; }
        else         { minv[j] -= delta; }
      }

      j0 = j1;
      if (p[j0] == 0) break;
    }

    while (true) {
      int j1 = way[j0];
      p[j0] = p[j1];
      j0 = j1;
      if (j0 == 0) break;
    }
  }

  // Build match
  std::vector<int> match(n, -1);
  for (int j = 1; j <= m; ++j) {
    if (p[j] != 0 && p[j] <= n) {
      int row = p[j] - 1;
      match[row] = j;
    }
  }

  // Compute total on original costs
  double total = 0.0;
  for (int i = 0; i < n; ++i) {
    if (match[i] < 1) stop("Infeasible: could not find full matching");
    int col = match[i] - 1;
    if (orig_mask_iv[i * m + col]) stop("Infeasible: chosen forbidden edge");
    double c = orig_cost[i * m + col];
    if (!std::isfinite(c)) stop("Infeasible: chosen edge has non-finite original cost");
    total += c;
  }

  IntegerVector out_match(n);
  for (int i = 0; i < n; ++i) out_match[i] = match[i];

  // Extract dual variables (convert from 1-indexed internal to 0-indexed output)
  // For maximization, negate the duals back
  NumericVector out_u(n), out_v(m);

  if (maximize) {
    // When maximizing, we solved min(-c), so duals need negation
    for (int i = 0; i < n; ++i) out_u[i] = -u[i + 1];
    for (int j = 0; j < m; ++j) out_v[j] = -v[j + 1];
  } else {
    for (int i = 0; i < n; ++i) out_u[i] = u[i + 1];
    for (int j = 0; j < m; ++j) out_v[j] = v[j + 1];
  }

  return Rcpp::List::create(
    Rcpp::Named("match") = out_match,
    Rcpp::Named("total_cost") = total,
    Rcpp::Named("u") = out_u,
    Rcpp::Named("v") = out_v
  );
}
