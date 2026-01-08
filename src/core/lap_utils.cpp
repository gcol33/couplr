// src/lap_utils.cpp
#include "lap_utils.h"

#include <sstream>
#include <cctype>   // std::tolower

// ------------------------- small utilities -------------------------

std::string match_to_key(const std::vector<int>& match) {
  std::ostringstream os;
  for (size_t i = 0; i < match.size(); ++i) {
    if (i) os << ',';
    os << match[i];
  }
  return os.str();
}

Rcpp::NumericMatrix apply_exclusions(Rcpp::NumericMatrix base,
                                     const std::vector<std::pair<int,int>>& ex) {
  Rcpp::NumericMatrix M = Rcpp::clone(base);
  for (auto &rc : ex) M(rc.first, rc.second) = NA_REAL;
  return M;
}

Rcpp::NumericMatrix apply_constraints(const Rcpp::NumericMatrix& M,
                                      const std::vector<int>& force_cols,
                                      int i_forbid,
                                      int j_forbid) {
  Rcpp::NumericMatrix A = Rcpp::clone(M);
  const int n = A.nrow(), m = A.ncol();

  // Force first r rows where force_cols[i] > 0 (values are 1-based cols)
  for (int i = 0; i < static_cast<int>(force_cols.size()); ++i) {
    const int row  = i;                  // 0-based
    const int col1 = force_cols[i] - 1;  // 0-based; -1 means "no force"
    if (col1 >= 0 && col1 < m && row >= 0 && row < n) {
      for (int j = 0; j < m; ++j) if (j != col1) A(row, j) = NA_REAL;
      for (int r = 0; r < n; ++r) if (r != row) A(r, col1) = NA_REAL;
    }
  }

  // Forbid a single 1-based pair if provided (0 means skip)
  if (i_forbid >= 1 && j_forbid >= 1) {
    const int ri = i_forbid - 1, cj = j_forbid - 1;
    if (ri >= 0 && ri < n && cj >= 0 && cj < m) A(ri, cj) = NA_REAL;
  }
  return A;
}

// Build CSR-like lists of allowed columns (mask: 0 = allowed, 1 = forbidden)
void build_allowed(const std::vector<int>& mask, int n, int m,
                   std::vector<int>& row_ptr, std::vector<int>& cols) {
  row_ptr.assign(n + 1, 0);
  for (int i = 0; i < n; ++i)
    for (int j = 0; j < m; ++j)
      if (!mask[i * m + j]) ++row_ptr[i + 1];

  for (int i = 1; i <= n; ++i) row_ptr[i] += row_ptr[i - 1];

  cols.assign(row_ptr.back(), -1);
  std::vector<int> fill = row_ptr;
  for (int i = 0; i < n; ++i)
    for (int j = 0; j < m; ++j)
      if (!mask[i * m + j]) cols[fill[i]++] = j;
}

// Check that each row has at least one allowed (non-forbidden) edge
void ensure_each_row_has_option(const std::vector<int>& mask, int n, int m) {
  for (int i = 0; i < n; ++i) {
    bool ok = false;
    for (int j = 0; j < m; ++j) {
      if (!mask[i * m + j]) { ok = true; break; }
    }
    if (!ok) {
      Rcpp::stop("Infeasible: row %d has no allowed edges", i + 1);
    }
  }
}

// Check if matrix is feasible (each row has at least one finite value)
// Returns true if feasible, false otherwise (does not throw)
bool is_feasible(const Rcpp::NumericMatrix& M) {
  const int n = M.nrow();
  const int m = M.ncol();
  if (n == 0 || m == 0) return false;
  if (n > m) return false;  // Cannot match all rows if n > m

  for (int i = 0; i < n; ++i) {
    bool has_finite = false;
    for (int j = 0; j < m; ++j) {
      if (R_finite(M(i, j))) {
        has_finite = true;
        break;
      }
    }
    if (!has_finite) return false;
  }
  return true;
}

// Check if a matching result is valid (no forbidden edges chosen)
bool is_valid_matching(const Rcpp::NumericMatrix& cost,
                       const std::vector<int>& match) {
  const int n = cost.nrow();
  const int m = cost.ncol();

  for (int i = 0; i < n && i < static_cast<int>(match.size()); ++i) {
    int j1 = match[i];  // 1-based column
    if (j1 <= 0 || j1 > m) continue;  // Skip unmatched or out of bounds
    int j = j1 - 1;  // 0-based
    if (!R_finite(cost(i, j))) {
      return false;  // Forbidden edge was chosen
    }
  }
  return true;
}

// Helper for has_valid_matching: DFS to find augmenting path
static bool dfs_augment(int u, const std::vector<std::vector<int>>& adj,
                        std::vector<int>& match_v, std::vector<bool>& visited) {
  for (int v : adj[u]) {
    if (visited[v]) continue;
    visited[v] = true;
    if (match_v[v] < 0 || dfs_augment(match_v[v], adj, match_v, visited)) {
      match_v[v] = u;
      return true;
    }
  }
  return false;
}

// Check if a perfect matching exists using Hungarian-style augmenting paths
// This is O(n*m) worst case but fast for typical k-best subproblems
bool has_valid_matching(const Rcpp::NumericMatrix& M) {
  const int n = M.nrow();
  const int m = M.ncol();

  // Quick checks
  if (n == 0) return true;  // Empty is valid
  if (n > m) return false;  // Can't match all rows

  // Build adjacency list (only finite edges)
  std::vector<std::vector<int>> adj(n);
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < m; ++j) {
      if (R_finite(M(i, j))) {
        adj[i].push_back(j);
      }
    }
    if (adj[i].empty()) return false;  // Row has no options
  }

  // Find maximum matching using Hopcroft-Karp style augmentation
  std::vector<int> match_v(m, -1);  // match_v[j] = row matched to col j, or -1
  int matched = 0;

  for (int u = 0; u < n; ++u) {
    std::vector<bool> visited(m, false);
    if (dfs_augment(u, adj, match_v, visited)) {
      ++matched;
    }
  }

  return matched == n;  // Perfect matching exists iff all rows matched
}

// Central cost computation helper - THE SINGLE SOURCE OF TRUTH
// Computes: sum of original_cost[i, assignment[i]-1] over all matched rows
//
// This function defines what "cost" means across the entire package:
//   1. Always uses the ORIGINAL user-supplied cost matrix (no transformations)
//   2. Works for both minimize and maximize (no sign flips)
//   3. Ignores dummy columns (assignment[i] > ncol)
//   4. Ignores unmatched rows (assignment[i] == 0 or NA)
//   5. Only sums real, finite edges
double compute_total_cost(const Rcpp::NumericMatrix& original_cost,
                          const Rcpp::IntegerVector& assignment) {
  const int n = original_cost.nrow();
  const int m = original_cost.ncol();

  if (assignment.size() != n) {
    Rcpp::stop("compute_total_cost: assignment length %d != nrow %d",
               assignment.size(), n);
  }

  double total = 0.0;

  for (int i = 0; i < n; ++i) {
    int col_1based = assignment[i];

    // Skip unmatched rows
    if (col_1based == 0 || col_1based == NA_INTEGER) {
      continue;
    }

    // Skip dummy columns (assignment points beyond real columns)
    if (col_1based > m) {
      continue;
    }

    // Convert to 0-based
    int j = col_1based - 1;

    // Safety check
    if (j < 0 || j >= m) {
      Rcpp::stop("compute_total_cost: invalid assignment[%d] = %d (ncol = %d)",
                 i, col_1based, m);
    }

    double c = original_cost(i, j);

    // Only accumulate finite costs
    if (R_finite(c)) {
      total += c;
    }
  }

  return total;
}

// Package-wide standard result builders
Rcpp::List make_result(const std::vector<int>& match, double total) {
  return Rcpp::List::create(
    Rcpp::Named("match")      = Rcpp::IntegerVector(match.begin(), match.end()),
    Rcpp::Named("total_cost") = total
  );
}

Rcpp::List make_result(const Rcpp::IntegerVector& match, double total) {
  return Rcpp::List::create(
    Rcpp::Named("match")      = match,
    Rcpp::Named("total_cost") = total
  );
}

// ------------------------- router (base methods) -------------------------

// extern exported solvers used by the router
Rcpp::List lap_solve_jv(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List lap_solve_hungarian(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List lap_solve_ssp(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List lap_solve_auction(Rcpp::NumericMatrix cost, bool maximize, Rcpp::Nullable<double> eps = R_NilValue);
Rcpp::List lap_solve_bruteforce(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List lap_solve_csflow(Rcpp::NumericMatrix cost, bool maximize);

static std::string to_lower_router(std::string x) {
  for (char &c : x) c = static_cast<char>(std::tolower(static_cast<unsigned char>(c)));
  return x;
}

Rcpp::List run_base_solver_by_name(const Rcpp::NumericMatrix& cost,
                                   bool maximize,
                                   const std::string& method) {
  const std::string m = to_lower_router(method);
  if (m == "jv")           return lap_solve_jv(cost, maximize);
  if (m == "hungarian")    return lap_solve_hungarian(cost, maximize);
  if (m == "ssp" || m == "sap") return lap_solve_ssp(cost, maximize);
  if (m == "auction")      return lap_solve_auction(cost, maximize, R_NilValue);
  if (m == "csflow")       return lap_solve_csflow(cost, maximize);
  if (m == "bruteforce")   return lap_solve_bruteforce(cost, maximize);
  Rcpp::stop("Unknown base method: '%s'", method.c_str());
}
