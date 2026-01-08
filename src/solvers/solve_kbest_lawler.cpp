// src/solve_kbest_lawler.cpp
// [[Rcpp::plugins(cpp17)]]
#include <Rcpp.h>
#include <vector>
#include <queue>
#include <limits>
#include <cmath>
#include <string>
#include <unordered_set>
#include <algorithm>
#include <cctype>
#include "../core/lap_internal.h"
#include "../core/lap_utils.h"

using namespace Rcpp;

// ---- Use exported oracles (they handle maximize internally) ----
Rcpp::List lap_solve_jv(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List lap_solve_hungarian(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List lap_solve_ssp(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List lap_solve_auction(Rcpp::NumericMatrix cost, bool maximize, Rcpp::Nullable<double> eps = R_NilValue);
Rcpp::List lap_solve_bruteforce(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List lap_solve_csflow(Rcpp::NumericMatrix cost, bool maximize);

namespace {

inline std::string lower(std::string s) {
  for (char &c : s) c = static_cast<char>(std::tolower(static_cast<unsigned char>(c)));
  return s;
}

// Solve one LAP with auto-transpose (so oracles expecting n<=m are satisfied).
// Returns {match_out (length n0, 1-based; no zeros), cost_for_ordering}.
// cost_for_ordering is negated when maximizing so a min-heap gives k-best.
std::pair<std::vector<int>, double>
solve_one_with_auto_transpose(NumericMatrix M,
                              const std::string& base_in,
                              bool maximize)
{
  const std::string base = lower(base_in);

  int n0 = M.nrow(), m0 = M.ncol();
  bool transposed = false;
  NumericMatrix work = M;
  if (n0 > m0) {
    work = transpose(M);
    transposed = true;
  }

  List ans;
  if (base == "jv")                        ans = lap_solve_jv(work, maximize);
  else if (base == "hungarian")            ans = lap_solve_hungarian(work, maximize);
  else if (base == "sap" || base == "ssp") ans = lap_solve_ssp(work, maximize);
  else if (base == "auction")              ans = lap_solve_auction(work, maximize, R_NilValue);
  else if (base == "csflow")               ans = lap_solve_csflow(work, maximize);
  else if (base == "bruteforce")           ans = lap_solve_bruteforce(work, maximize);
  else stop("Unknown base method in Lawler: '%s'", base_in.c_str());

  IntegerVector match_iv = ans["match"]; // 1-based in 'work' orientation
  std::vector<int> match_work(match_iv.begin(), match_iv.end());

  // Map back to original orientation
  std::vector<int> match_out;
  if (!transposed) {
    match_out = match_work;
  } else {
    match_out.assign(n0, 0);
    for (int i = 0; i < m0; ++i) {
      int j = match_work[i];           // 1-based col in work == original row
      if (j > 0) match_out[j - 1] = i + 1; // original row j -> original col i
    }
  }

  // Recompute true objective on ORIGINAL M
  // Recompute true objective on ORIGINAL M (allow unmatched rows for m<n)
  double cost_final = 0.0;
  for (int i = 0; i < n0; ++i) {
    int j1 = match_out[i];
    if (j1 > 0) cost_final += M(i, j1 - 1);
    // if j1 == 0 (unmatched), skip — valid in rectangular case
  }
  if (maximize) cost_final = -cost_final;


  return {match_out, cost_final};
}

// Node for PQ
struct LawNode {
  std::vector<int> match;  // 1-based, length n
  double cost;             // possibly negated for maximize
  int nexti;               // next branching position (1..n+1)
  LawNode(std::vector<int> m, double c, int nx) : match(std::move(m)), cost(c), nexti(nx) {}
};

// Min-heap on cost
struct NodeGreater {
  bool operator()(const LawNode& a, const LawNode& b) const { return a.cost > b.cost; }
};

} // namespace

// ---- Non-exported implementation ----
Rcpp::List solve_kbest_lawler_impl(NumericMatrix cost,
                                   int k,
                                   std::string method_base,
                                   bool maximize)
{
  if (k < 1) return List::create();

  const int n = cost.nrow(), m = cost.ncol();
  if (n == 0 || m == 0) return List::create();

  // Best solution
  auto best = solve_one_with_auto_transpose(cost, method_base, maximize);
  std::vector<int> pi0 = best.first;   // perfect matching (no zeros)
  double c0 = best.second;

  // Dedup
  std::unordered_set<std::string> seen;
  seen.reserve(1024);
  seen.insert(match_to_key(pi0));

  // Output vector of Lists
  std::vector<Rcpp::List> out;
  out.reserve(k);

  // Negate back for output if maximizing
  out.push_back(Rcpp::List::create(_["match"] = IntegerVector(pi0.begin(), pi0.end()),
                                   _["total_cost"] = maximize ? -c0 : c0));

  // PQ of child subproblems
  std::priority_queue<LawNode, std::vector<LawNode>, NodeGreater> pq;

  // Seed S_1 branches
  for (int i = 1; i <= n; ++i) {
    if (pi0[i - 1] == 0) continue;  // skip unmatched rows
    std::vector<int> force_cols(pi0.begin(), pi0.begin() + (i - 1));
    NumericMatrix Mi = apply_constraints(cost, force_cols, i, pi0[i - 1]);

    // Check if a valid matching exists before calling solver
    if (!has_valid_matching(Mi)) continue;

    // Solve - guaranteed not to throw since we verified feasibility
    std::pair<std::vector<int>, double> child = solve_one_with_auto_transpose(Mi, method_base, maximize);

    std::string key = match_to_key(child.first);
    if (seen.insert(key).second) {
      pq.emplace(child.first, child.second, i + 1);
    }
  }

  // Main enumeration
  while ((int)out.size() < k && !pq.empty()) {
    LawNode node = pq.top(); pq.pop();

    out.push_back(Rcpp::List::create(
      _["match"] = IntegerVector(node.match.begin(), node.match.end()),
      _["total_cost"] = maximize ? -node.cost : node.cost
    ));

    // Branch from node.nexti .. n
    for (int i = node.nexti; i <= n; ++i) {
      if (node.match[i - 1] == 0) continue;  // skip unmatched rows
      std::vector<int> force_cols(node.match.begin(), node.match.begin() + (i - 1));
      NumericMatrix Mi = apply_constraints(cost, force_cols, i, node.match[i - 1]);

      // Check if a valid matching exists before calling solver
      if (!has_valid_matching(Mi)) continue;

      // Solve - guaranteed not to throw since we verified feasibility
      std::pair<std::vector<int>, double> child = solve_one_with_auto_transpose(Mi, method_base, maximize);

      std::string key = match_to_key(child.first);
      if (seen.insert(key).second) {
        pq.emplace(child.first, child.second, i + 1);
      }
    }
  }

  return Rcpp::wrap(out);
}
