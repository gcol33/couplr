// src/lap_utils.h
#pragma once

#include <Rcpp.h>
#include <vector>
#include <string>
#include <utility>

// Constants
constexpr double BIG = 1e100;  // Used for forbidden edges
constexpr double TOL = 1e-12;  // Tolerance for zero comparisons

// String key for 1-based match vectors (e.g., "3,1,2")
std::string match_to_key(const std::vector<int>& match);

// Apply NA exclusions (row,col are 0-based)
Rcpp::NumericMatrix apply_exclusions(
    Rcpp::NumericMatrix base,
    const std::vector<std::pair<int,int>>& ex);

// Apply Lawler-style constraints
//  - force_cols: for rows 1..r, force row i to column force_cols[i-1] (1-based, 0 = no force)
//  - forbid (i_forbid, j_forbid): forbid one 1-based pair (0 = no forbid)
Rcpp::NumericMatrix apply_constraints(
    const Rcpp::NumericMatrix& M,
    const std::vector<int>& force_cols,
    int i_forbid,
    int j_forbid);

// Build CSR-style "allowed" structure from a mask of size n*m.
// mask is length n*m, row-major, with nonzero meaning allowed.
// row_ptr size is n + 1; cols collects allowed column indices per row (0-based).
void build_allowed(
    const std::vector<int>& mask,
    int n,
    int m,
    std::vector<int>& row_ptr,
    std::vector<int>& cols);

// Ensure each row has at least one finite option (used by some solvers)
void ensure_each_row_has_option(const std::vector<int>& mask, int n, int m);

// Overload for Rcpp::IntegerVector (converts and calls vector version)
inline void ensure_each_row_has_option(const Rcpp::IntegerVector& mask, int n, int m) {
  std::vector<int> mask_vec(mask.begin(), mask.end());
  ensure_each_row_has_option(mask_vec, n, m);
}

// Standard result builders
Rcpp::List make_result(const std::vector<int>& match, double total);
Rcpp::List make_result(const Rcpp::IntegerVector& match, double total);

// Transpose helper (inline for performance)
inline Rcpp::NumericMatrix transpose(const Rcpp::NumericMatrix& M) {
  const int r = M.nrow(), c = M.ncol();
  Rcpp::NumericMatrix T(c, r);
  for (int i = 0; i < r; ++i)
    for (int j = 0; j < c; ++j)
      T(j, i) = M(i, j);
  return T;
}

// Base solver router (used by Murty, Lawler)
Rcpp::List run_base_solver_by_name(
    const Rcpp::NumericMatrix& cost,
    bool maximize,
    const std::string& method);
