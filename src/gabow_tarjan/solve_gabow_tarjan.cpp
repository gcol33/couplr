// solve_gabow_tarjan.cpp
// Gabow-Tarjan LAP solver implementation with R interface

#include <Rcpp.h>
#include <cmath>                // for std::abs, std::round, std::llround, std::fabs
#include "utils_gabow_tarjan.h"
#include "../core/lap_utils.h"

using namespace Rcpp;

/**
 * Gabow-Tarjan LAP solver implementation
 * 
 * Solves the linear assignment problem using the Gabow-Tarjan bit-scaling
 * algorithm, which achieves O(n^3 * log(C)) complexity where C is the
 * maximum cost.
 * 
 * @param cost Cost matrix (NumericMatrix from R)
 * @param maximize If true, solve maximum weight matching instead
 * @return R List with assignment, cost, row_duals, col_duals
 */
Rcpp::List solve_gabow_tarjan_impl(Rcpp::NumericMatrix cost, bool maximize) {
    const int n = cost.nrow();
    const int m = cost.ncol();
    
    // Convert R matrix to C++ cost matrix with integer costs
    CostMatrix cost_matrix(n, std::vector<long long>(m));
    
    // Scaling factor for floating-point → integer
    double scale_factor = 1.0;
    
    // Detect whether all finite costs are (near) integers
    bool all_integer = true;
    
    // Find max absolute value for potential scaling
    double max_abs = 0.0;
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            double val = cost(i, j);
            if (!R_finite(val)) continue;
            
            double abs_val = std::abs(val);
            if (abs_val > max_abs) {
                max_abs = abs_val;
            }
            
            // Check if val is almost an integer
            double rounded = std::round(val);
            if (std::fabs(val - rounded) > 1e-9) {
                all_integer = false;
            }
        }
    }
    
    // Only scale when we actually have non-integer costs
    if (!all_integer && max_abs > 0.0 && max_abs < 1e6) {
        // scale so max ≈ 1e6
        scale_factor = 1e6 / max_abs;
    }
    
    // Fill integer cost matrix (with rounding, not truncation)
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            double val = cost(i, j);
            if (R_finite(val)) {
                double scaled = val;
                
                // Only apply scale_factor for non-integer matrices
                if (!all_integer) {
                    scaled *= scale_factor;
                }
                
                // Proper rounding to nearest integer
                long long int_cost = static_cast<long long>(std::llround(scaled));
                
                // Negate for maximization if needed
                if (maximize) {
                    int_cost = -int_cost;
                }
                
                cost_matrix[i][j] = int_cost;
            } else {
                cost_matrix[i][j] = BIG_INT; // forbidden
            }
        }
    }
    
    // State for inner solver
    MatchVec row_match(n, NIL);
    MatchVec col_match(m, NIL);
    DualVec y_u(n, 0);
    DualVec y_v(m, 0);
    
    // Solve using Gabow–Tarjan bit-scaling algorithm
    try {
        solve_gabow_tarjan_inner(cost_matrix, row_match, col_match, y_u, y_v);
    } catch (const std::exception& e) {
        Rcpp::stop(std::string("Gabow-Tarjan solver error: ") + e.what());
    }
    
    // Convert matching to 1-based R vectors
    Rcpp::IntegerVector row_match_R(n);
    Rcpp::IntegerVector col_match_R(m);
    for (int i = 0; i < n; ++i) {
        row_match_R[i] = (row_match[i] != NIL) ? (row_match[i] + 1) : NA_INTEGER;
    }
    for (int j = 0; j < m; ++j) {
        col_match_R[j] = (col_match[j] != NIL) ? (col_match[j] + 1) : NA_INTEGER;
    }

    // Compute total cost using the centralized helper (THE SINGLE SOURCE OF TRUTH)
    // This ensures consistent cost semantics across all solvers:
    //   - Always uses original unmodified cost matrix
    //   - Works for both minimize and maximize
    //   - Ignores dummy columns automatically
    double total_cost = compute_total_cost(cost, row_match_R);

    // Count matched rows (for diagnostics)
    int n_matched = 0;
    for (int i = 0; i < n; ++i) {
        if (row_match[i] != NIL) {
            ++n_matched;
        }
    }

    // Convert duals back to original scale
    Rcpp::NumericVector u_R(n);
    Rcpp::NumericVector v_R(m);
    for (int i = 0; i < n; ++i) {
        double val = static_cast<double>(y_u[i]) / scale_factor;
        u_R[i] = maximize ? -val : val;
    }
    for (int j = 0; j < m; ++j) {
        double val = static_cast<double>(y_v[j]) / scale_factor;
        v_R[j] = maximize ? -val : val;
    }
    
    // Build result list matching the standard lap_solve API
    // Required fields: "match" and "total_cost" (used by assignment() function in R/lap_solve.R:121,134)
    return Rcpp::List::create(
        // Standard API (required by assignment())
        Rcpp::Named("match")      = row_match_R,
        Rcpp::Named("total_cost") = total_cost,

        // Extra fields for diagnostic/debugging
        Rcpp::Named("row_match")  = row_match_R,
        Rcpp::Named("col_match")  = col_match_R,
        Rcpp::Named("row_duals")  = u_R,
        Rcpp::Named("col_duals")  = v_R,
        Rcpp::Named("u")          = u_R,
        Rcpp::Named("v")          = v_R,
        Rcpp::Named("n_matched")  = n_matched,
        Rcpp::Named("method")     = "gabow_tarjan"
    );
}
