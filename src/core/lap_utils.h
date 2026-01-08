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

// Check if matrix is feasible (each row has at least one finite value)
// Returns true if feasible, false otherwise (does not throw)
bool is_feasible(const Rcpp::NumericMatrix& M);

// Check if a matching result is valid (no forbidden edges chosen)
// Returns true if all matched edges are finite, false otherwise
bool is_valid_matching(const Rcpp::NumericMatrix& cost,
                       const std::vector<int>& match);

// Check if a perfect matching exists using greedy with backtracking
// More thorough than is_feasible() - actually tries to find a matching
// Returns true if a valid matching exists, false otherwise
bool has_valid_matching(const Rcpp::NumericMatrix& M);

// Overload for Rcpp::IntegerVector (converts and calls vector version)
inline void ensure_each_row_has_option(const Rcpp::IntegerVector& mask, int n, int m) {
  std::vector<int> mask_vec(mask.begin(), mask.end());
  ensure_each_row_has_option(mask_vec, n, m);
}

// Standard result builders
Rcpp::List make_result(const std::vector<int>& match, double total);
Rcpp::List make_result(const Rcpp::IntegerVector& match, double total);

// Central cost computation: sum original_cost[i, match[i]] over all matched rows
// This is the SINGLE SOURCE OF TRUTH for what "cost" means across all solvers.
//
// Parameters:
//   original_cost: The original cost matrix as passed by the user (NumericMatrix)
//   assignment: 1-based column indices (IntegerVector), 0 or NA_INTEGER for unmatched
//
// Returns: Total cost = sum of original_cost[i, assignment[i]-1] for all matched i
//
// Invariants:
//   - Works for both minimize and maximize (no negation!)
//   - Ignores dummy columns (assignment[i] > ncol(original_cost))
//   - Ignores unmatched rows (assignment[i] == 0 or NA_INTEGER)
//   - Only sums over real, finite edges
double compute_total_cost(const Rcpp::NumericMatrix& original_cost,
                          const Rcpp::IntegerVector& assignment);

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

// Non-throwing version: returns true on success, false on failure
// On success, result contains the solution; on failure, result is empty
bool try_run_base_solver(
    const Rcpp::NumericMatrix& cost,
    bool maximize,
    const std::string& method,
    Rcpp::List& result);
