#include <Rcpp.h>
#include <vector>
#include <limits>
#include <algorithm>
#include <cmath>
#include "../core/lap_internal.h"
#include "../core/lap_utils.h"
using namespace Rcpp;

Rcpp::List prepare_cost_matrix_impl(NumericMatrix cost, bool maximize);

// Classic Hungarian (Kuhn-Munkres) for n <= m with NA/forbidden support.
// Works on prepared (possibly flipped) costs; computes total on ORIGINAL costs.
Rcpp::List solve_hungarian_impl(NumericMatrix cost, bool maximize) {
  const int n = cost.nrow(), m = cost.ncol();
  if (n == 0)   if (n == 0) return make_result(IntegerVector(), 0.0);
  if (n > m) LAP_ERROR("Infeasible: n=%d > m=%d", n, m);

  // Prepared work (flip if maximize) and original (for reporting)
  List Wp = prepare_cost_matrix_impl(cost, maximize);
  List Op = prepare_cost_matrix_impl(cost, false);
  NumericVector Wnv = Wp["cost"];   // row-major
  IntegerVector Mnv = Wp["mask"];   // 0 allowed, 1 forbidden
  NumericVector Onv = Op["cost"];   // row-major
  IntegerVector Monv = Op["mask"];  // row-major

  // Check feasibility: each row must have at least one allowed edge
    ensure_each_row_has_option(Mnv, n, m);


  const double INF = std::numeric_limits<double>::infinity();
// using TOL from lap_utils.h
  // Working copy: forbidden -> INF (leave as INF so we never consider it a zero)
  std::vector<double> C(Wnv.begin(), Wnv.end());
  for (int k = 0; k < n*m; ++k) if (Mnv[k]) C[k] = INF;

  auto row_min = [&](int i) {
    double mn = INF;
    for (int j = 0; j < m; ++j) if (std::isfinite(C[i*m+j])) mn = std::min(mn, C[i*m+j]);
    return mn;
  };
  auto col_min = [&](int j) {
    double mn = INF;
    for (int i = 0; i < n; ++i) if (std::isfinite(C[i*m+j])) mn = std::min(mn, C[i*m+j]);
    return mn;
  };

  // Preliminary steps: different for n <= m vs n > m (like reference)
  const int minDim = std::min(n, m);
  
  if (n <= m) {
    // Row reduction only (reference lines 106-118)
    for (int i = 0; i < n; ++i) {
      double mn = row_min(i);
      if (!std::isfinite(mn)) {
        LAP_ERROR("Infeasible: row %d has no finite values (all forbidden?). n=%d, m=%d", i, n, m);
      }
      for (int j = 0; j < m; ++j) if (std::isfinite(C[i*m+j])) C[i*m+j] -= mn;
    }
  } else {
    // Column reduction only (reference lines 152-164)
    for (int j = 0; j < m; ++j) {
      double mn = col_min(j);
      if (std::isfinite(mn)) {
        for (int i = 0; i < n; ++i) if (std::isfinite(C[i*m+j])) C[i*m+j] -= mn;
      }
    }
  }

  // Masks and covers
  std::vector<int> star_row_of_col(m, -1);
  std::vector<int> star_col_of_row(n, -1);
  std::vector<int> prime_col_of_row(n, -1);
  std::vector<char> row_cov(n, 0), col_cov(m, 0);

  auto is_zero = [&](int i, int j) {
    double x = C[i*m + j];
    return std::isfinite(x) && std::abs(x) <= TOL;
  };

  // Initial greedy starring (reference lines 121-143 and 167-178)
  if (n <= m) {
    // Star zeros, covering columns only (reference lines 121-130)
    for (int i = 0; i < n; ++i) {
      for (int j = 0; j < m; ++j) {
        if (is_zero(i,j) && col_cov[j] == 0) {
          star_row_of_col[j] = i;
          star_col_of_row[i] = j;
          col_cov[j] = 1;
          break;
        }
      }
    }
  } else {
    // Star zeros, covering both rows and columns (reference lines 167-176)
    for (int j = 0; j < m; ++j) {
      for (int i = 0; i < n; ++i) {
        if (is_zero(i,j) && row_cov[i] == 0) {
          star_row_of_col[j] = i;
          star_col_of_row[i] = j;
          col_cov[j] = 1;
          row_cov[i] = 1;
          break;
        }
      }
    }
    // Uncover all rows (reference lines 177-178)
    for (int i = 0; i < n; ++i) row_cov[i] = 0;
  }

  // CRITICAL: Recompute column covers from stars to ensure clean invariant
  // This is step2a in the reference - we must have col_cov exactly match stars
  std::fill(col_cov.begin(), col_cov.end(), 0);
  for (int j = 0; j < m; ++j) {
    if (star_row_of_col[j] != -1) col_cov[j] = 1;
  }

  auto find_uncovered_zero = [&]() -> std::pair<int,int> {
    for (int i = 0; i < n; ++i) if (!row_cov[i]) {
      for (int j = 0; j < m; ++j) if (!col_cov[j] && is_zero(i,j)) return {i,j};
    }
    return {-1,-1};
  };

  auto smallest_uncovered = [&]() {
    double mn = INF;
    for (int i = 0; i < n; ++i) if (!row_cov[i])
      for (int j = 0; j < m; ++j) if (!col_cov[j] && std::isfinite(C[i*m + j]))
        mn = std::min(mn, C[i*m + j]);
    return mn;
  };

  while (true) {
    // Count stars to check termination
    int num_stars = 0;
    for (int j = 0; j < m; ++j) if (star_row_of_col[j] >= 0) num_stars++;
    
    // Terminate when we have n stars (complete matching)
    if (num_stars >= n) break;

    // Reset covers: uncover all rows, cover only columns with stars
    std::fill(row_cov.begin(), row_cov.end(), 0);
    std::fill(col_cov.begin(), col_cov.end(), 0);
    for (int j = 0; j < m; ++j) {
      if (star_row_of_col[j] != -1) col_cov[j] = 1;
    }
    std::fill(prime_col_of_row.begin(), prime_col_of_row.end(), -1);

    // Step 3: Find uncovered zeros and build augmenting paths
    long long inner_iter = 0;
    const long long max_inner = n * m * 100;
    
    while (true) {
      if (++inner_iter > max_inner) {
        int num_row_cov = 0, num_col_cov = 0;
        for (int i = 0; i < n; ++i) if (row_cov[i]) num_row_cov++;
        for (int j = 0; j < m; ++j) if (col_cov[j]) num_col_cov++;
        LAP_ERROR("Inner loop exceeded %lld iterations. Stars=%d, RowCov=%d/%d, ColCov=%d/%d",
             max_inner, num_stars, num_row_cov, n, num_col_cov, m);
      }
      
      auto z = find_uncovered_zero();
      if (z.first == -1) {
        // No uncovered zero found - adjust matrix (step 5)
        double d = smallest_uncovered();
        if (!std::isfinite(d)) {
          int num_row_cov = 0, num_col_cov = 0;
          for (int i = 0; i < n; ++i) if (row_cov[i]) num_row_cov++;
          for (int j = 0; j < m; ++j) if (col_cov[j]) num_col_cov++;
          
          LAP_ERROR("Infeasible: no uncovered finite values. Stars=%d, RowCov=%d/%d, ColCov=%d/%d, minDim=%d",
               num_stars, num_row_cov, n, num_col_cov, m, minDim);
        }
        // Adjustment: add to covered rows, subtract from uncovered columns
        for (int i = 0; i < n; ++i) {
          for (int j = 0; j < m; ++j) {
            if (row_cov[i] && std::isfinite(C[i*m + j])) C[i*m + j] += d;
            if (!col_cov[j] && std::isfinite(C[i*m + j])) C[i*m + j] -= d;
          }
        }
        // After adjustment, continue to look for zeros again
        continue;
      }

      // Found an uncovered zero at (i, j) - prime it
      int i = z.first, j = z.second;
      prime_col_of_row[i] = j;

      if (star_col_of_row[i] == -1) {
        // No star in this row - we found an augmenting path!
        // Augment along alternating path starting from (i, j)
        int ii = i, jj = j;
        std::vector<std::pair<int,int>> path;
        path.emplace_back(ii, jj);

        while (true) {
          int i_star = star_row_of_col[jj];
          if (i_star == -1) break;
          path.emplace_back(i_star, jj);
          int j_prime = prime_col_of_row[i_star];
          jj = j_prime;
          path.emplace_back(i_star, jj);
        }

        // Flip stars along path
        // CRITICAL: Unstar all the starred entries first to avoid overwriting
        for (size_t k = 0; k < path.size(); ++k) {
          if (k % 2 == 1) { // star → unset
            int r = path[k].first, c = path[k].second;
            star_row_of_col[c] = -1;
            star_col_of_row[r] = -1;
          }
        }
        // Now star all the primed entries
        for (size_t k = 0; k < path.size(); ++k) {
          if (k % 2 == 0) { // prime → star
            int r = path[k].first, c = path[k].second;
            star_row_of_col[c] = r;
            star_col_of_row[r] = c;
          }
        }

        // After augmentation, go back to outer loop to check if done
        break;
      } else {
        // There's a star in this row - cover the row and uncover the star's column
        row_cov[i] = 1;
        col_cov[star_col_of_row[i]] = 0;
        // Continue in the inner loop to find more zeros
      }
    }
  }

  // Build match from stars
  IntegerVector match(n); std::fill(match.begin(), match.end(), 0);
  int num_matched = 0;
  for (int i = 0; i < n; ++i) {
    int j = star_col_of_row[i];
    if (j >= 0) {
      match[i] = j + 1;
      num_matched++;
    }
  }
  
  // Debug: check if we have enough matches
  if (num_matched < n) {
    // Count how many columns are covered and how many stars we have
    int num_stars = 0;
    for (int j = 0; j < m; ++j) {
      if (star_row_of_col[j] >= 0) num_stars++;
    }
    LAP_ERROR("Infeasible: only %d/%d rows matched, %d stars found, minDim=%d",
         num_matched, n, num_stars, minDim);
  }

  // Compute total on ORIGINAL costs
  double total = 0.0;
  for (int i = 0; i < n; ++i) {
    int j = match[i] - 1;
    if (Monv[i*m + j]) LAP_ERROR("Infeasible: chosen forbidden edge");
    double c = Onv[i*m + j];
    if (!std::isfinite(c)) LAP_ERROR("Infeasible");
    total += c;
  }

    return make_result(match, total);
}
