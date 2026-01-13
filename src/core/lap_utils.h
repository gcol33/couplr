// src/core/lap_utils.h
// Pure C++ utility functions for LAP solvers - NO Rcpp dependencies
#pragma once

#include "lap_types.h"
#include "lap_error.h"
#include <vector>
#include <string>
#include <utility>

namespace lap {

// String key for match vectors (useful for deduplication in k-best)
// e.g., {2, 0, 1} -> "2,0,1"
std::string match_to_key(const std::vector<int>& match);

// Build CSR-style "allowed" structure from a mask
// mask: 0 = forbidden, nonzero = allowed (opposite of internal convention!)
// row_ptr: size n+1, row_ptr[i] to row_ptr[i+1] gives range in cols
// cols: allowed column indices (0-based) for each row
void build_allowed(const std::vector<int>& mask, int n, int m,
                   std::vector<int>& row_ptr, std::vector<int>& cols);

// Check that each row has at least one allowed edge
// Throws InfeasibleException if any row has no options
void ensure_each_row_has_option(const std::vector<int>& mask, int n, int m);

// Check if cost matrix is feasible (each row has at least one finite value)
// Returns true if feasible, false otherwise (does not throw)
bool is_feasible(const CostMatrix& cost);

// Check if a matching result is valid (no forbidden edges chosen)
// match: 0-based column indices, -1 = unmatched
bool is_valid_matching(const CostMatrix& cost, const std::vector<int>& match);

// Check if a perfect matching exists using augmenting paths
// More thorough than is_feasible() - actually tries to find a matching
bool has_valid_matching(const CostMatrix& cost);

// Compute total cost from a cost matrix and assignment
// match: 0-based column indices, -1 = unmatched
// Returns sum of cost[i, match[i]] for all matched rows
double compute_total_cost(const CostMatrix& cost, const std::vector<int>& match);

// Compute total cost using original cost matrix (for when we have transformed costs)
// original_cost: the untransformed cost matrix
// match: 0-based column indices, -1 = unmatched
double compute_total_cost(const CostMatrix& original_cost,
                          const CostMatrix& work_cost,
                          const std::vector<int>& match);

// Negate costs for maximization (returns new matrix)
CostMatrix negate_costs(const CostMatrix& cost);

// Prepare cost matrix for solving (handles maximization, padding for rectangular)
// Returns: prepared CostMatrix (negated if maximize, padded if needed)
CostMatrix prepare_for_solve(const CostMatrix& cost, bool maximize);

// Convert 0-based match to 1-based (for R interface)
std::vector<int> to_one_based(const std::vector<int>& match);

// Convert 1-based match to 0-based (from R interface)
std::vector<int> to_zero_based(const std::vector<int>& match);

}  // namespace lap
