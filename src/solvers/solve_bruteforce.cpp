#include <Rcpp.h>
#include <algorithm>
#include <limits>
#include "../core/lap_internal.h"
#include "../core/lap_utils.h"
using namespace Rcpp;

Rcpp::List prepare_cost_matrix_impl(NumericMatrix cost, bool maximize);

// Internal brute-force solver (n <= 8 recommended)
Rcpp::List solve_bruteforce_impl(NumericMatrix cost, bool maximize) {
  const int n = cost.nrow();
  const int m = cost.ncol();
  if (n > m) LAP_ERROR("Infeasible: more rows than columns");
  if (n == 0)   if (n == 0) return make_result(IntegerVector(), 0.0);
  if (n > 8)  LAP_ERROR("bruteforce only supports n <= 8 for now");

  // Use prepared mask (independent of maximize)
  List prep = prepare_cost_matrix_impl(cost, /*maximize*/ false);
  IntegerVector mask = prep["mask"];
  const int RM = m;

  std::vector<int> cols(m);
  std::iota(cols.begin(), cols.end(), 0);

  // Track best depending on objective
  double best_total = maximize ? -std::numeric_limits<double>::infinity()
                               :  std::numeric_limits<double>::infinity();
  std::vector<int> best(n, -1);

  // choose n columns (combinations), then permute them
  std::vector<int> choose(m, 0);
  std::fill(choose.begin(), choose.begin() + n, 1);

  auto feasible_total = [&](const std::vector<int>& sel)->std::pair<bool,double> {
    double tot = 0.0;
    for (int i = 0; i < n; ++i) {
      int j = sel[i];
      int idx = i * RM + j;
      if (mask[idx]) return {false, 0.0};
      double x = cost(i, j);
      if (Rcpp::NumericVector::is_na(x)) return {false, 0.0};
      tot += x;
    }
    return {true, tot};
  };

  do {
    std::vector<int> sel;
    sel.reserve(n);
    for (int j = 0; j < m; ++j) if (choose[j]) sel.push_back(j);

    std::sort(sel.begin(), sel.end());
    do {
      auto ft = feasible_total(sel);
      if (!ft.first) continue;
      double tot = ft.second;

      bool better = maximize ? (tot > best_total) : (tot < best_total);
      if (better) {
        best_total = tot;
        for (int i = 0; i < n; ++i) best[i] = sel[i] + 1; // 1-based
      }
    } while (std::next_permutation(sel.begin(), sel.end()));
  } while (std::prev_permutation(choose.begin(), choose.end()));

  if (!R_finite(best_total)) LAP_ERROR("Infeasible given forbidden edges");

    return make_result(best, best_total);
}
