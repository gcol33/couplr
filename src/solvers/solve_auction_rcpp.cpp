// src/solve_auction.cpp
#include <Rcpp.h>
#include <vector>
#include <limits>
#include <algorithm>
#include <cmath>

#include "../core/lap_internal.h"
#include "../core/lap_utils_rcpp.h"

using namespace Rcpp;

// Compute cost spread among allowed entries (max - min)
static inline double allowed_spread(const std::vector<double>& W,
                                    const std::vector<int>& MASK,
                                    int n, int m) {
  double cmin = INFINITY, cmax = -INFINITY;
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < m; ++j) {
      if (!MASK[i * m + j]) {
        double v = W[i * m + j];
        if (R_finite(v)) {
          if (v < cmin) cmin = v;
          if (v > cmax) cmax = v;
        }
      }
    }
  }
  if (!R_finite(cmin) || !R_finite(cmax)) return 0.0;
  double s = cmax - cmin;
  if (s < 0.0) s = 0.0;
  return s;
}


// Forward decl (defined elsewhere)
Rcpp::List prepare_cost_matrix_impl(NumericMatrix cost, bool maximize);

/*** ---------------------------------------------------------------------- ***/
/***  Fixed-epsilon Auction (public impl with eps_in), plus legacy wrapper   ***/
/*** ---------------------------------------------------------------------- ***/

// 3-arg main implementation (eps_in: if NaN or <= 0, uses default 1e-9)
Rcpp::List solve_auction_impl(NumericMatrix cost, bool maximize, double eps_in) {
  const int n = cost.nrow(), m = cost.ncol();
  if (n == 0) return make_result(IntegerVector(), 0.0);
  if (n > m) LAP_ERROR("Infeasible: n > m");

  // Work (maybe flipped for maximize) + original (for reporting)
  List Wp = prepare_cost_matrix_impl(cost, maximize);
  List Op = prepare_cost_matrix_impl(cost, false);
  NumericVector Wnv = Wp["cost"];   // row-major
  IntegerVector Mnv = Wp["mask"];   // 0 = allowed, 1 = forbidden
  NumericVector Onv = Op["cost"];
  IntegerVector Monv = Op["mask"];

  // Flat copies
  std::vector<double> W(Wnv.begin(), Wnv.end());
  std::vector<int>    MASK(Mnv.begin(), Mnv.end());
  std::vector<double> O(Onv.begin(), Onv.end());
  std::vector<int>    OMASK(Monv.begin(), Monv.end());

  // Per-row allowed lists
  std::vector<int> row_ptr, cols;
  build_allowed(MASK, n, m, row_ptr, cols);

  // Quick infeasible check: any row with zero allowed neighbors?
  for (int i = 0; i < n; ++i) {
    if (row_ptr[i] == row_ptr[i + 1]) {
      LAP_ERROR("Infeasible: row %d has no valid options", i + 1);
    }
  }

  // Prices and assignments
  std::vector<double> price(m, 0.0);
  std::vector<int>    a_of_i(n, -1), i_of_j(m, -1);
  std::vector<int>    queue; queue.reserve(n);

  // Epsilon
  // Adaptive epsilon: scale with cost spread / n to avoid tiny bids that cause slow convergence, scale-invariant choice: ε = spread / (2n²)
  double eps;
  if (std::isfinite(eps_in) && eps_in > 0.0) {
    eps = eps_in;
  } else {
    const double spread = allowed_spread(W, MASK, n, m);
    const double base = (spread > 0.0) ? spread : 1.0;
    eps = base / (2.0 * (double)n * (double)n);
    // Defensive clamp to prevent denormal or absurd values
    if (eps < 1e-12) eps = 1e-12;
    if (spread > 0.0 && eps > spread) eps = spread;
  }


  auto profit = [&](int i, int j) {
    // For minimization: maximize negative (cost + price)
    double base = -(W[i * m + j] + price[j]);
    unsigned key = (unsigned)((i + 1) * 1315423911u) ^ (unsigned)((j + 1) * 2654435761u);
    double tweak = std::ldexp((double)(key & 0xFFFFu), -80);
    return base + tweak;
  };

  for (int i = 0; i < n; ++i) queue.push_back(i);

  long long reassign_guard = 0;
  while (!queue.empty()) {
    int i = queue.back();
    queue.pop_back();

    // Best & second-best among allowed
    double best = -INFINITY, second = -INFINITY; 
    int jbest = -1;
    const int start = row_ptr[i], end = row_ptr[i + 1];
    for (int k = start; k < end; ++k) {
      int j = cols[k];
      double val = profit(i, j);
      if (val > best) { second = best; best = val; jbest = j; }
      else if (val > second) { second = val; }
    }
    if (jbest < 0) LAP_ERROR("Infeasible");

    double bid = (second == -INFINITY) ? (2.0 * eps) : (best - second + eps);

    price[jbest] += bid;

    int old = i_of_j[jbest];
    i_of_j[jbest] = i;

    if (old != -1) {
      a_of_i[old] = -1;
      queue.push_back(old);
    }
    a_of_i[i] = jbest;

    if (++reassign_guard > 200000000LL) LAP_ERROR("Auction: exceeded iteration guard");
  }

  // Build result & compute true total on original costs
  IntegerVector match(n);
  double total = 0.0;
  for (int i = 0; i < n; ++i) {
    int j = a_of_i[i];
    if (j < 0) LAP_ERROR("Infeasible: incomplete assignment");
    match[i] = j + 1;
    if (OMASK[i * m + j]) LAP_ERROR("Infeasible: chosen forbidden edge");
    double c = O[i * m + j];
    if (!R_finite(c)) LAP_ERROR("Infeasible");
    total += c;
  }
  return make_result(match, total);
}

// 2-arg legacy wrapper kept for compatibility
Rcpp::List solve_auction_impl(NumericMatrix cost, bool maximize) {
  return solve_auction_impl(cost, maximize, std::numeric_limits<double>::quiet_NaN());
}

/*** ---------------------------------------------------------------------- ***/
/***  Scaled-epsilon Auction (C reference implementation)                   ***/
/*** ---------------------------------------------------------------------- ***/

// Scaled-epsilon Auction matching C reference EXACTLY:
// - Uses COST MINIMIZATION (reduced_cost = cost - price)
// - Prices DECREASE (making objects more attractive)
// - Matching is discarded and rebuilt each phase
Rcpp::List solve_auction_scaled_impl(Rcpp::NumericMatrix cost, bool maximize, 
                                     double initial_epsilon_factor = 1.0,
                                     double alpha = 7.0,
                                     double final_epsilon = -1.0) {
  using namespace Rcpp;




  const int n = cost.nrow(), m = cost.ncol();
  if (n == 0) return make_result(IntegerVector(), 0.0);

  // C reference requires balanced graph (n == m). If n < m, pad with dummies.
  if (n > m) LAP_ERROR("Infeasible: n > m");

  NumericMatrix padded_cost;
  bool needs_padding = (n < m);
  int effective_n = n;

  if (needs_padding) {
    // Pad with dummy rows to make it square
    effective_n = m;
    padded_cost = NumericMatrix(m, m);

    // Copy original costs
    for (int i = 0; i < n; ++i)
      for (int j = 0; j < m; ++j)
        padded_cost(i, j) = cost(i, j);

    // Set dummy rows to very high cost (very low profit in maximize case)
    double dummy_cost = 0.0;
    for (int i = 0; i < n; ++i)
      for (int j = 0; j < m; ++j)
        if (R_finite(cost(i, j)))
          dummy_cost = std::max(dummy_cost, std::abs(cost(i, j)));

    dummy_cost = (dummy_cost + 1.0) * m * 10.0;

    for (int i = n; i < m; ++i)
      for (int j = 0; j < m; ++j)
        padded_cost(i, j) = maximize ? -dummy_cost : dummy_cost;
  } else {
    padded_cost = cost;
  }

  // Now work with the balanced matrix
  const int nn = effective_n;  // nn == m

  // Prep matrices (use padded matrix for W)
  List Wp = prepare_cost_matrix_impl(padded_cost, maximize);
  List Op = prepare_cost_matrix_impl(cost, false);  // original for reporting
  NumericVector Wnv = Wp["cost"];
  IntegerVector Mnv = Wp["mask"];
  NumericVector Onv = Op["cost"];
  IntegerVector Monv = Op["mask"];

  std::vector<double> W(Wnv.begin(), Wnv.end());
  std::vector<int>    MASK(Mnv.begin(), Mnv.end());
  std::vector<double> O(Onv.begin(), Onv.end());
  std::vector<int>    OMASK(Monv.begin(), Monv.end());

  // Build CSR for the BALANCED (possibly padded) matrix
  std::vector<int> row_ptr(nn + 1, 0), cols;
  for (int i = 0; i < nn; ++i)
    for (int j = 0; j < m; ++j)
      if (!MASK[i * m + j]) ++row_ptr[i + 1];

  for (int i = 1; i <= nn; ++i) row_ptr[i] += row_ptr[i - 1];
  cols.resize(row_ptr.back());
  {
    std::vector<int> fill = row_ptr;
    for (int i = 0; i < nn; ++i)
      for (int j = 0; j < m; ++j)
        if (!MASK[i * m + j]) cols[fill[i]++] = j;
  }

  // Find max cost for epsilon
  double max_abs_cost = 0.0;
  for (int i = 0; i < nn; ++i) {
    const int start = row_ptr[i], end = row_ptr[i + 1];
    for (int k = start; k < end; ++k) {
      int j = cols[k];
      double c = std::abs(W[i * m + j]);
      if (R_finite(c) && c > max_abs_cost) max_abs_cost = c;
    }
  }

  double epsilon = std::max(1.0, max_abs_cost * initial_epsilon_factor);
  // Use smaller final epsilon for better optimality guarantee
  double eps_final = (final_epsilon > 0.0) ? final_epsilon : std::min(1e-6, 1.0 / (nn * nn));

  // State: prices persist, matching rebuilt each phase
  std::vector<double> price(m, 0.0);
  std::vector<int> a_of_i(nn, -1);  // nn rows (including dummies if padded)
  std::vector<int> i_of_j(m, -1);

  // Price bounds to prevent overflow (scaled relative to problem size)
  const double price_bound = std::max(1e12, max_abs_cost * nn * 1000.0);

  // C reference formulation: MINIMIZE (cost - price)
  auto reduced_cost = [&](int i, int j) {
    return W[i * m + j] - price[j];
  };

  // Find best (MINIMUM reduced cost) and second-best
  auto find_best = [&](int i, double& best_rc, double& second_rc, int& best_j) {
    const int start = row_ptr[i], end = row_ptr[i + 1];
    best_rc = INFINITY;
    second_rc = INFINITY;
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

  int phase = 0;
  const long long max_iter = static_cast<long long>(nn) * m * 100;

  // Epsilon-scaling loop
  do {
    phase++;

    // Reduce epsilon (C reference does this first each phase)
    epsilon /= alpha;
    if (epsilon < eps_final) epsilon = eps_final;

    // Discard matching
    std::fill(a_of_i.begin(), a_of_i.end(), -1);
    std::fill(i_of_j.begin(), i_of_j.end(), -1);

    // Rebuild matching - all nn persons (including dummies if padded)
    std::vector<int> unmatched;
    unmatched.reserve(nn);
    for (int i = 0; i < nn; ++i) unmatched.push_back(i);

    long long iter = 0;
    while (!unmatched.empty()) {
      if (++iter > max_iter) {
        LAP_ERROR("Auction(scaled): iteration guard at eps=%f, phase=%d", epsilon, phase);
      }

      int i = unmatched.back();
      unmatched.pop_back();

      double best_rc, second_rc;
      int best_j;
      find_best(i, best_rc, second_rc, best_j);

      if (best_j < 0) LAP_ERROR("Infeasible: person %d has no valid neighbors", i + 1);

      // Compute gamma - use epsilon-scaled default when only one option
      double gamma;
      if (second_rc == INFINITY || !std::isfinite(second_rc)) {
        // Only one valid option: use small multiple of epsilon
        gamma = epsilon;
      } else {
        gamma = second_rc - best_rc;
        // Clamp gamma to prevent runaway prices
        if (gamma > price_bound) gamma = price_bound;
        if (gamma < 0.0) gamma = 0.0;  // Should not happen, but defensive
      }

      // DECREASE price with overflow protection
      double new_price = price[best_j] - (gamma + epsilon);
      if (new_price < -price_bound) new_price = -price_bound;
      price[best_j] = new_price;

      // Assignment
      int old = i_of_j[best_j];
      i_of_j[best_j] = i;
      if (old != -1) {
        a_of_i[old] = -1;
        unmatched.push_back(old);
      }
      a_of_i[i] = best_j;
    }
  } while (epsilon > eps_final);

  // Build result - only for ORIGINAL n rows (not dummy rows)
  IntegerVector match(n);
  double total = 0.0;
  for (int i = 0; i < n; ++i) {
    const int j = a_of_i[i];
    if (j < 0) LAP_ERROR("Infeasible: incomplete assignment");
    match[i] = j + 1;
    if (OMASK[i * m + j]) LAP_ERROR("Infeasible: chosen forbidden edge");
    const double c = O[i * m + j];
    if (!R_finite(c)) LAP_ERROR("Infeasible");
    total += c;
  }

  return List::create(_["match"] = match,
                      _["total_cost"] = total,
                      _["phases"] = phase);
}

/*** ---------------------------------------------------------------------- ***/
/***  Gauss-Seidel Auction Algorithm                                        ***/
/*** ---------------------------------------------------------------------- ***/

// Gauss-Seidel variant: sequential bidding with immediate price updates
// This version includes auto-transpose for n > m cases.
Rcpp::List solve_auction_gauss_seidel_impl(NumericMatrix cost, bool maximize, double eps_in) {
  int n0 = cost.nrow(), m0 = cost.ncol();
  if (n0 == 0) return make_result(IntegerVector(), 0.0);

  // Auto-transpose if n > m
  bool transposed = false;
  NumericMatrix work = cost;
  int n = n0, m = m0;
  if (n0 > m0) {
    transposed = true;
    work = transpose(cost);
    n = work.nrow();  // == m0
    m = work.ncol();  // == n0
  }

  // Work (maybe flipped for maximize) + original (for reporting)
  List Wp = prepare_cost_matrix_impl(work, maximize);
  List Op = prepare_cost_matrix_impl(work, false);
  NumericVector Wnv = Wp["cost"];   // row-major
  IntegerVector Mnv = Wp["mask"];   // 0 = allowed, 1 = forbidden
  NumericVector Onv = Op["cost"];
  IntegerVector Monv = Op["mask"];

  // Flat copies
  std::vector<double> W(Wnv.begin(), Wnv.end());
  std::vector<int>    MASK(Mnv.begin(), Mnv.end());
  std::vector<double> O(Onv.begin(), Onv.end());
  std::vector<int>    OMASK(Monv.begin(), Monv.end());

  // Per-row allowed lists
  std::vector<int> row_ptr, cols;
  build_allowed(MASK, n, m, row_ptr, cols);

  // Quick infeasible check
  for (int i = 0; i < n; ++i) {
    if (row_ptr[i] == row_ptr[i + 1]) {
      LAP_ERROR("Infeasible: row %d has no valid options", i + 1);
    }
  }

  // Prices and assignments
  std::vector<double> price(m, 0.0);
  std::vector<int>    a_of_i(n, -1), i_of_j(m, -1);

  // Epsilon
  // Adaptive epsilon: scale with cost spread / n² to match regular auction precision
  double eps;
  if (std::isfinite(eps_in) && eps_in > 0.0) {
    eps = eps_in;
  } else {
    const double spread = allowed_spread(W, MASK, n, m);
    const double base = (spread > 0.0) ? spread : 1.0;
    // Use same epsilon formula as regular auction: spread / (2n²)
    eps = base / (2.0 * (double)n * (double)n);
    // Defensive clamp to prevent denormal or absurd values
    if (eps < 1e-12) eps = 1e-12;
    if (spread > 0.0 && eps > spread) eps = spread;
  }

  // Profit function (for minimization, maximize negative cost)
  auto profit = [&](int i, int j) {
    double base = -(W[i * m + j] + price[j]);
    unsigned key = (unsigned)((i + 1) * 1315423911u) ^ (unsigned)((j + 1) * 2654435761u);
    double tweak = std::ldexp((double)(key & 0xFFFFu), -80);
    return base + tweak;
  };

  // Gauss-Seidel: iterate until all persons are matched
  long long total_bids = 0;
  const long long max_bids = 200000000LL;

  bool converged = false;
  while (!converged) {
    converged = true;

    for (int i = 0; i < n; ++i) {
      // Check if person i is still matched to j
      if (a_of_i[i] >= 0) {
        int j = a_of_i[i];
        if (i_of_j[j] == i) continue;
      }

      // Person i needs to bid
      converged = false;

      // Find best and second-best objects
      double best = -INFINITY, second = -INFINITY;
      int jbest = -1;
      const int start = row_ptr[i], end = row_ptr[i + 1];

      for (int k = start; k < end; ++k) {
        int j = cols[k];
        double val = profit(i, j);
        if (val > best) { second = best; best = val; jbest = j; }
        else if (val > second) { second = val; }
      }

      if (jbest < 0) LAP_ERROR("Infeasible: no valid objects for person %d", i + 1);

      // Compute bid increment and update price immediately (GS difference)
      double bid = (second == -INFINITY) ? (2.0 * eps) : (best - second + eps);
      price[jbest] += bid;

      // Update assignment
      int old = i_of_j[jbest];
      i_of_j[jbest] = i;
      a_of_i[i] = jbest;

      if (old != -1 && old != i) {
        a_of_i[old] = -1;  // old person becomes unmatched
      }

      if (++total_bids > max_bids) {
        LAP_ERROR("Auction (Gauss-Seidel): exceeded iteration guard");
      }
    }
  }

  // Extract matching (in work orientation)
  IntegerVector match_work(n);
  double total = 0.0;
  for (int i = 0; i < n; ++i) {
    int j = a_of_i[i];
    if (j < 0) LAP_ERROR("Infeasible: incomplete assignment");
    match_work[i] = j + 1;  // 1-based

    // Compute cost on original scale
    if (OMASK[i * m + j]) LAP_ERROR("Infeasible: chosen forbidden edge");
    double c = O[i * m + j];
    if (!R_finite(c)) LAP_ERROR("Infeasible");
    total += c;
  }

  // Map back to original orientation if transposed
  IntegerVector match_out;
  if (!transposed) {
    match_out = match_work;
  } else {
    // work is m0 x n0; match_work length m0
    match_out = IntegerVector(n0);
    std::fill(match_out.begin(), match_out.end(), 0);
    for (int i = 0; i < m0; ++i) {
      int j = match_work[i] - 1;
      if (j >= 0) match_out[j] = i + 1;
    }
  }

  return List::create(_["match"] = match_out,
                      _["total_cost"] = total,
                      _["bids"] = (int)total_bids);
}

// Backward compatibility
Rcpp::List solve_auction_scaled_impl(Rcpp::NumericMatrix cost, bool maximize, std::string schedule) {
  // C reference uses alpha = 7 by default
  double alpha = 7.0;
  if (schedule == "pow2" || schedule == "halves") {
    alpha = 4.0;
  }
  return solve_auction_scaled_impl(cost, maximize, 1.0, alpha, -1.0);
}
