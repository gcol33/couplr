// solve_gabow_tarjan.cpp
// Gabow-Tarjan LAP solver implementation with R interface

#include <Rcpp.h>
#include <cmath>                // for std::abs, std::round, std::llround, std::fabs
#include <algorithm>
#include <limits>
#include "utils_gabow_tarjan.h"
#include "../core/lap_utils_rcpp.h"

using namespace Rcpp;

namespace {

void compute_duals_from_matching(const CostMatrix& cost,
                                 const MatchVec& row_match,
                                 DualVec& y_u,
                                 DualVec& y_v) {
    const int n = static_cast<int>(cost.size());
    const int m = n > 0 ? static_cast<int>(cost[0].size()) : 0;
    constexpr long long INF = std::numeric_limits<long long>::max() / 4;

    y_u.assign(n, 0);
    y_v.assign(m, 0);

    if (n == 0 || m == 0) return;

    std::vector<int> matched_col_for_row(n, NIL);
    std::vector<int> row_for_col(m, NIL);
    for (int i = 0; i < n; ++i) {
        int j = row_match[i];
        if (j >= 0 && j < m && cost[i][j] < BIG_INT) {
            matched_col_for_row[i] = j;
            row_for_col[j] = i;
        }
    }

    // Difference constraints for matched columns under 1-feasibility:
    // u[i] - u[k] <= c(i, match[k]) + 1 - c(k, match[k]).
    // Bellman-Ford from a zero super-source gives feasible row potentials.
    std::vector<long long> u(n, 0);
    for (int iter = 0; iter < n - 1; ++iter) {
        bool changed = false;
        for (int k = 0; k < n; ++k) {
            int jk = matched_col_for_row[k];
            if (jk == NIL || cost[k][jk] >= BIG_INT) {
                continue;
            }
            for (int i = 0; i < n; ++i) {
                if (cost[i][jk] >= BIG_INT) {
                    continue;
                }
                long long w = cost[i][jk] + 1 - cost[k][jk];
                if (u[i] > u[k] + w) {
                    u[i] = u[k] + w;
                    changed = true;
                }
            }
        }
        if (!changed) break;
    }

    y_u = u;

    for (int j = 0; j < m; ++j) {
        int i = row_for_col[j];
        if (i != NIL) {
            y_v[j] = cost[i][j] - y_u[i];
        } else {
            long long best = INF;
            for (int r = 0; r < n; ++r) {
                if (cost[r][j] < BIG_INT) {
                    best = std::min(best, cost[r][j] + 1 - y_u[r]);
                }
            }
            y_v[j] = (best < INF) ? best : 0;
        }
    }
}

} // namespace

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
    solve_gabow_tarjan_inner(cost_matrix, row_match, col_match, y_u, y_v);

    // Warm-started scaling preserves the matching across phases. Recompute the
    // final dual certificate for small/certificate-oriented calls. Large
    // assignment() calls discard duals, so avoid adding an extra cubic pass to
    // the performance path.
    if (n <= 100) {
        compute_duals_from_matching(cost_matrix, row_match, y_u, y_v);
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
