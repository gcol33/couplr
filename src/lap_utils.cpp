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
