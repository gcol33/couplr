// src/solve_jv.cpp
#include <Rcpp.h>
#include <vector>
#include <limits>
#include <algorithm>
#include "../core/lap_internal.h"
#include "../core/lap_utils.h"

using namespace Rcpp;

// forward decls
Rcpp::List prepare_cost_matrix_impl(NumericMatrix cost, bool maximize);

// Internal JV/Hungarian style solver for n <= m
Rcpp::List solve_jv_impl(NumericMatrix cost, bool maximize) {
  const int n = cost.nrow();
  const int m = cost.ncol();

  if (n == 0) {
        return make_result(IntegerVector(), 0.0);
  }
  if (n > m) LAP_ERROR("Infeasible: number of rows greater than number of columns");

  // Prepared buffers: work (may be flipped if maximize) and original (for reporting total)
  List prep_work = prepare_cost_matrix_impl(cost, maximize);
  List prep_orig = prepare_cost_matrix_impl(cost, false);

  NumericVector work_cost_nv = prep_work["cost"];   // row-major
  IntegerVector work_mask_iv = prep_work["mask"];   // row-major

  NumericVector orig_cost_nv = prep_orig["cost"];   // row-major
  IntegerVector orig_mask_iv = prep_orig["mask"];   // row-major

  // copy for faster indexing where helpful
  std::vector<double> work_cost(work_cost_nv.begin(), work_cost_nv.end());
  std::vector<int>    work_mask(work_mask_iv.begin(), work_mask_iv.end());
  std::vector<double> orig_cost(orig_cost_nv.begin(), orig_cost_nv.end());

  // quick infeasible detection: any row with all forbidden?
    ensure_each_row_has_option(work_mask, n, m);


  // treat forbidden as very large to avoid them
// using BIG from lap_utils.h
  // Hungarian (rectangular n <= m) with potentials
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
      if (p[j0] == 0) break; // found augmenting column
    }

    // augment
    while (true) {
      int j1 = way[j0];
      p[j0] = p[j1];
      j0 = j1;
      if (j0 == 0) break;
    }
  }

  // build row -> column match (1-based)
  std::vector<int> match(n, -1);
  for (int j = 1; j <= m; ++j) {
    if (p[j] != 0 && p[j] <= n) {
      int row = p[j] - 1;
      match[row] = j;
    }
  }

  // verify and compute total on original costs (not flipped)
  double total = 0.0;
  for (int i = 0; i < n; ++i) {
    if (match[i] < 1) LAP_ERROR("Infeasible: could not find full matching");
    int col = match[i] - 1;
    if (orig_mask_iv[i * m + col]) LAP_ERROR("Infeasible: chosen forbidden edge");
    double c = orig_cost[i * m + col];
    if (!std::isfinite(c)) LAP_ERROR("Infeasible: chosen edge has non-finite original cost");
    total += c;
  }

  IntegerVector out_match(n);
  for (int i = 0; i < n; ++i) out_match[i] = match[i];

    return make_result(out_match, total);
}
