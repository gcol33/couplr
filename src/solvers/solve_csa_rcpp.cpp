// src/solvers/solve_csa.cpp
// Goldberg-Kennedy Cost-Scaling Assignment (CSA) Algorithm
// Reference: Goldberg & Kennedy (1995) "An efficient cost scaling algorithm
//            for the assignment problem"
// Complexity: O(sqrt(n) * m * log(nC)) where m = number of edges, C = max cost

#include <Rcpp.h>
#include <vector>
#include <limits>
#include <algorithm>
#include <cmath>
#include "../core/lap_utils_rcpp.h"

using namespace Rcpp;

// CSA uses epsilon-scaling auction approach
// For MINIMIZATION, we find objects with minimum reduced cost (c_ij - price_j)
// and DECREASE prices to maintain epsilon-complementary slackness

Rcpp::List solve_csa_impl(NumericMatrix cost, bool maximize) {
  const int n0 = cost.nrow();
  const int m0 = cost.ncol();

  if (n0 == 0 || m0 == 0) {
    return make_result(std::vector<int>(), 0.0);
  }

  // Ensure rows <= cols (standard for assignment)
  bool transposed = false;
  NumericMatrix C = cost;
  int n = n0, m = m0;

  if (n0 > m0) {
    C = transpose(cost);
    n = m0;
    m = n0;
    transposed = true;
  }

  // For rectangular problems (n < m), pad to square
  bool needs_padding = (n < m);
  int nn = m;  // effective problem size (square)

  // Build working cost matrix in row-major flat array
  std::vector<double> W(nn * m, BIG);
  std::vector<int> MASK(nn * m, 1);  // 1 = forbidden, 0 = allowed

  double max_abs_cost = 0.0;
  bool has_finite = false;

  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < m; ++j) {
      double v = C(i, j);
      if (std::isfinite(v)) {
        has_finite = true;
        W[i * m + j] = maximize ? -v : v;
        MASK[i * m + j] = 0;  // allowed
        double av = std::abs(W[i * m + j]);
        if (av > max_abs_cost) max_abs_cost = av;
      }
    }
  }

  if (!has_finite) {
    LAP_ERROR("No finite costs found.");
  }

  // For dummy rows (if padded), set very high costs to all columns
  if (needs_padding) {
    double dummy_cost = (max_abs_cost + 1.0) * m * 10.0;
    for (int i = n; i < nn; ++i) {
      for (int j = 0; j < m; ++j) {
        W[i * m + j] = dummy_cost;
        MASK[i * m + j] = 0;  // allowed
      }
    }
  }

  // Build CSR-style allowed lists
  std::vector<int> row_ptr(nn + 1, 0);
  for (int i = 0; i < nn; ++i) {
    for (int j = 0; j < m; ++j) {
      if (!MASK[i * m + j]) ++row_ptr[i + 1];
    }
  }
  for (int i = 1; i <= nn; ++i) row_ptr[i] += row_ptr[i - 1];

  std::vector<int> cols(row_ptr.back());
  {
    std::vector<int> fill = row_ptr;
    for (int i = 0; i < nn; ++i) {
      for (int j = 0; j < m; ++j) {
        if (!MASK[i * m + j]) cols[fill[i]++] = j;
      }
    }
  }

  // Check feasibility
  for (int i = 0; i < nn; ++i) {
    if (row_ptr[i] == row_ptr[i + 1]) {
      LAP_ERROR("Infeasible: row %d has no valid assignments", i + 1);
    }
  }

  // Dual variables (prices for objects)
  std::vector<double> price(m, 0.0);

  // Assignment arrays
  std::vector<int> a_of_i(nn, -1);  // person i -> object
  std::vector<int> i_of_j(m, -1);   // object j -> person

  // Reduced cost for minimization
  auto reduced_cost = [&](int i, int j) -> double {
    return W[i * m + j] - price[j];
  };

  // Find best (min reduced cost) and second-best for person i
  auto find_best = [&](int i, double& best_rc, double& second_rc, int& best_j) {
    const int start = row_ptr[i], end = row_ptr[i + 1];
    best_rc = std::numeric_limits<double>::infinity();
    second_rc = std::numeric_limits<double>::infinity();
    best_j = -1;

    if (end - start == 1) {
      best_j = cols[start];
      best_rc = reduced_cost(i, best_j);
      return;
    }

    for (int k = start; k < end; ++k) {
      int j = cols[k];
      double rc = reduced_cost(i, j);
      if (rc < best_rc) {
        second_rc = best_rc;
        best_rc = rc;
        best_j = j;
      } else if (rc < second_rc) {
        second_rc = rc;
      }
    }
  };

  // Epsilon scaling parameters
  double epsilon = std::max(1.0, max_abs_cost);
  const double alpha = 7.0;  // Scaling factor (match auction_scaled)
  const double eps_final = std::min(1e-6, 1.0 / (static_cast<double>(nn) * nn));

  int phase = 0;
  const long long max_iter = static_cast<long long>(nn) * m * 100;

  // Main epsilon-scaling loop (do-while ensures at least one phase at eps_final)
  do {
    phase++;

    // Reduce epsilon first
    epsilon /= alpha;
    if (epsilon < eps_final) epsilon = eps_final;

    // Discard matching
    std::fill(a_of_i.begin(), a_of_i.end(), -1);
    std::fill(i_of_j.begin(), i_of_j.end(), -1);

    // Rebuild matching for all persons
    std::vector<int> unmatched;
    unmatched.reserve(nn);
    for (int i = 0; i < nn; ++i) unmatched.push_back(i);

    long long iter = 0;
    while (!unmatched.empty()) {
      if (++iter > max_iter) {
        LAP_ERROR("CSA: iteration guard at eps=%g, phase=%d", epsilon, phase);
      }

      int i = unmatched.back();
      unmatched.pop_back();

      double best_rc, second_rc;
      int best_j;
      find_best(i, best_rc, second_rc, best_j);

      if (best_j < 0) {
        LAP_ERROR("Infeasible: person %d has no valid objects", i + 1);
      }

      // Compute gamma (bid increment)
      double gamma = (!std::isfinite(second_rc)) ? 1e6 : (second_rc - best_rc);

      // DECREASE price (makes object more "expensive" in auction sense)
      price[best_j] -= (gamma + epsilon);

      // Handle displacement
      int old = i_of_j[best_j];
      i_of_j[best_j] = i;
      if (old != -1) {
        a_of_i[old] = -1;
        unmatched.push_back(old);
      }
      a_of_i[i] = best_j;
    }
  } while (epsilon > eps_final);

  // Build output (only real rows, not dummies)
  std::vector<int> match_out;
  double total = 0.0;

  if (!transposed) {
    match_out.resize(n);
    for (int i = 0; i < n; ++i) {
      int j = a_of_i[i];
      match_out[i] = (j >= 0) ? (j + 1) : 0;
      if (j >= 0 && j < m0) {
        double val = cost(i, j);
        if (std::isfinite(val)) {
          total += val;
        }
      }
    }
  } else {
    // Transpose back: original was m0 x n0
    match_out.assign(n0, 0);
    for (int i = 0; i < n; ++i) {
      int j = a_of_i[i];
      if (j >= 0 && j < n0) {
        match_out[j] = i + 1;
        double val = cost(j, i);
        if (std::isfinite(val)) {
          total += val;
        }
      }
    }
  }

  return make_result(match_out, total);
}
