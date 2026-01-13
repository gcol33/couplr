// src/solve_murty.cpp
// [[Rcpp::plugins(cpp17)]]
#include <Rcpp.h>
#include <vector>
#include <queue>
#include <set>
#include <string>
#include <limits>
#include <algorithm>
#include <cctype>
#include "../core/lap_internal.h"
#include "../core/lap_utils_rcpp.h"

using namespace Rcpp;

namespace {

// Min-heap by total cost
struct Node {
  double total;
  std::vector<int> match;                    // 1-based cols, length n
  std::vector<std::pair<int,int>> forbid;    // pairs (i, j) 0-based
  bool operator<(const Node& other) const { return total > other.total; }
};

inline std::string lower(std::string s) {
  for (char &c : s) c = static_cast<char>(std::tolower(static_cast<unsigned char>(c)));
  return s;
}

// Choose base LAP solver by name (jv | hungarian | ssp/sap)


} // namespace

// Internal Murty: returns List(matches = IntegerMatrix(got, n), totals = NumericVector(got))
Rcpp::List solve_murty_impl(NumericMatrix cost, int k, bool maximize, std::string single_method /*= "jv"*/) {
  const int n = cost.nrow();
  const int m = cost.ncol();

  if (n == 0 || k <= 0) {
    return List::create(_["matches"] = IntegerMatrix(0, n),
                        _["totals"]  = NumericVector(0));
  }
  if (n > m) LAP_ERROR("Infeasible: n > m");

  // Best solution with chosen base solver (throws if infeasible)
  List base = run_base_solver_by_name(cost, maximize, single_method);
  IntegerVector base_match_iv = base["match"];
  double base_total = as<double>(base["total_cost"]);
  std::vector<int> base_match(base_match_iv.begin(), base_match_iv.end());

  std::priority_queue<Node> pq;
  pq.push(Node{base_total, base_match, {}});

  std::set<std::string> seen;
  seen.insert(match_to_key(base_match));

  std::vector<std::vector<int>> out_matches;
  std::vector<double> out_totals;
  out_matches.reserve(k);
  out_totals.reserve(k);

  while (!pq.empty() && (int)out_matches.size() < k) {
    Node cur = pq.top(); pq.pop();

    out_matches.push_back(cur.match);
    out_totals.push_back(cur.total);

    // Branch: exclude each chosen edge (i, cur.match[i]-1)
    for (int i = 0; i < n; ++i) {
      std::vector<std::pair<int,int>> forbid_next = cur.forbid;
      forbid_next.emplace_back(i, cur.match[i] - 1);

      NumericMatrix Mnext = apply_exclusions(cost, forbid_next);

      // Check if a valid matching exists before calling solver
      if (!has_valid_matching(Mnext)) continue;

      // Solve - guaranteed not to throw since we verified feasibility
      List ch = run_base_solver_by_name(Mnext, maximize, single_method);

      IntegerVector miv = ch["match"];
      double tot = as<double>(ch["total_cost"]);
      std::vector<int> mv(miv.begin(), miv.end());

      std::string key = match_to_key(mv);
      if (seen.insert(key).second) {
        pq.push(Node{tot, mv, std::move(forbid_next)});
      }
    }
  }

  const int got = (int)out_matches.size();
  IntegerMatrix Mout(got, n);
  for (int r = 0; r < got; ++r)
    for (int c = 0; c < n; ++c)
      Mout(r, c) = out_matches[r][c];

  NumericVector Tout(out_totals.begin(), out_totals.end());
  return List::create(_["matches"] = Mout, _["totals"] = Tout);
}
