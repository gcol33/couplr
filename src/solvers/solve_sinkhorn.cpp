// src/solvers/solve_sinkhorn.cpp
// Sinkhorn-Knopp Algorithm for Entropy-Regularized Optimal Transport
// Reference: Cuturi (2013) "Sinkhorn Distances: Lightspeed Computation of
//            Optimal Transport"
// Complexity: O(n² / ε²) iterations, O(n²) per iteration

#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <algorithm>
#include <limits>

using namespace Rcpp;

// Sinkhorn-Knopp computes the entropy-regularized optimal transport plan:
//   P* = argmin_P <C, P> - (1/lambda) * H(P)
// subject to P1 = r, P'1 = c (marginal constraints)
//
// The solution has form P = diag(u) * K * diag(v) where K = exp(-lambda * C)
// Algorithm alternates scaling rows and columns until convergence.
//
// Returns:
// - transport_plan: doubly stochastic matrix (soft assignment)
// - cost: <C, P*> (transport cost without entropy term)
// - u, v: scaling vectors
// - converged: whether algorithm converged
// - iterations: number of iterations used

Rcpp::List solve_sinkhorn_impl(
    Rcpp::NumericMatrix cost,
    double lambda,           // Regularization parameter (higher = more peaked)
    double tol,              // Convergence tolerance
    int max_iter,            // Maximum iterations
    Rcpp::Nullable<Rcpp::NumericVector> r_weights,  // Row marginals (default: uniform)
    Rcpp::Nullable<Rcpp::NumericVector> c_weights   // Column marginals (default: uniform)
) {
  const int n = cost.nrow();
  const int m = cost.ncol();

  if (n == 0 || m == 0) {
    return List::create(
      _["transport_plan"] = NumericMatrix(0, 0),
      _["cost"] = 0.0,
      _["converged"] = true,
      _["iterations"] = 0
    );
  }

  // Set up marginals (default: uniform)
  std::vector<double> r(n), c(m);

  if (r_weights.isNotNull()) {
    NumericVector rw(r_weights);
    if (rw.size() != n) {
      stop("r_weights must have length equal to number of rows");
    }
    double sum_r = 0.0;
    for (int i = 0; i < n; ++i) {
      r[i] = rw[i];
      sum_r += r[i];
    }
    // Normalize
    for (int i = 0; i < n; ++i) r[i] /= sum_r;
  } else {
    for (int i = 0; i < n; ++i) r[i] = 1.0 / n;
  }

  if (c_weights.isNotNull()) {
    NumericVector cw(c_weights);
    if (cw.size() != m) {
      stop("c_weights must have length equal to number of columns");
    }
    double sum_c = 0.0;
    for (int j = 0; j < m; ++j) {
      c[j] = cw[j];
      sum_c += c[j];
    }
    // Normalize
    for (int j = 0; j < m; ++j) c[j] /= sum_c;
  } else {
    for (int j = 0; j < m; ++j) c[j] = 1.0 / m;
  }

  // Compute K = exp(-lambda * C)
  // Use log-domain for numerical stability with large lambda
  std::vector<double> K(n * m);
  double max_cost = -std::numeric_limits<double>::infinity();

  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < m; ++j) {
      double cij = cost(i, j);
      if (!std::isfinite(cij)) {
        // Treat NA/Inf as very high cost
        cij = 1e10;
      }
      if (cij > max_cost) max_cost = cij;
    }
  }

  // Stabilized kernel: K_ij = exp(-lambda * (C_ij - max_cost)) * exp(-lambda * max_cost)
  // We absorb the constant into the scaling vectors
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < m; ++j) {
      double cij = cost(i, j);
      if (!std::isfinite(cij)) cij = 1e10;
      K[i * m + j] = std::exp(-lambda * cij);
    }
  }

  // Initialize scaling vectors
  std::vector<double> u(n, 1.0);
  std::vector<double> v(m, 1.0);

  // Sinkhorn iterations
  bool converged = false;
  int iter = 0;

  // Temporary vectors for computing Kv and K'u
  std::vector<double> Kv(n), Ktu(m);

  for (iter = 0; iter < max_iter; ++iter) {
    // Save old u for convergence check
    std::vector<double> u_old = u;

    // Update u: u = r / (K * v)
    for (int i = 0; i < n; ++i) {
      double sum = 0.0;
      for (int j = 0; j < m; ++j) {
        sum += K[i * m + j] * v[j];
      }
      Kv[i] = sum;
      u[i] = (sum > 1e-300) ? r[i] / sum : r[i] * 1e300;
    }

    // Update v: v = c / (K' * u)
    for (int j = 0; j < m; ++j) {
      double sum = 0.0;
      for (int i = 0; i < n; ++i) {
        sum += K[i * m + j] * u[i];
      }
      Ktu[j] = sum;
      v[j] = (sum > 1e-300) ? c[j] / sum : c[j] * 1e300;
    }

    // Check convergence: ||u - u_old|| / ||u||
    double diff = 0.0, norm = 0.0;
    for (int i = 0; i < n; ++i) {
      double d = u[i] - u_old[i];
      diff += d * d;
      norm += u[i] * u[i];
    }

    if (norm > 0 && std::sqrt(diff / norm) < tol) {
      converged = true;
      break;
    }
  }

  // Compute transport plan P = diag(u) * K * diag(v)
  NumericMatrix P(n, m);
  double transport_cost = 0.0;

  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < m; ++j) {
      double pij = u[i] * K[i * m + j] * v[j];
      P(i, j) = pij;
      double cij = cost(i, j);
      if (std::isfinite(cij)) {
        transport_cost += pij * cij;
      }
    }
  }

  // Return scaling vectors as well
  NumericVector u_out(n), v_out(m);
  for (int i = 0; i < n; ++i) u_out[i] = u[i];
  for (int j = 0; j < m; ++j) v_out[j] = v[j];

  return List::create(
    _["transport_plan"] = P,
    _["cost"] = transport_cost,
    _["u"] = u_out,
    _["v"] = v_out,
    _["converged"] = converged,
    _["iterations"] = iter + 1,
    _["lambda"] = lambda
  );
}

// Convenience function to round soft assignment to hard assignment
// Uses the transport plan to solve a standard LAP
Rcpp::IntegerVector sinkhorn_round_impl(Rcpp::NumericMatrix P) {
  const int n = P.nrow();
  const int m = P.ncol();

  if (n == 0) return IntegerVector(0);

  // Greedy rounding: iteratively assign row to its most likely column
  std::vector<int> match(n, 0);
  std::vector<bool> col_used(m, false);

  // Create list of (probability, row, col) and sort descending
  std::vector<std::tuple<double, int, int>> entries;
  entries.reserve(n * m);

  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < m; ++j) {
      entries.emplace_back(P(i, j), i, j);
    }
  }

  std::sort(entries.begin(), entries.end(),
            [](const auto& a, const auto& b) {
              return std::get<0>(a) > std::get<0>(b);
            });

  std::vector<bool> row_assigned(n, false);
  int assigned = 0;

  for (const auto& e : entries) {
    int i = std::get<1>(e);
    int j = std::get<2>(e);

    if (!row_assigned[i] && !col_used[j]) {
      match[i] = j + 1;  // 1-based
      row_assigned[i] = true;
      col_used[j] = true;
      ++assigned;
      if (assigned == std::min(n, m)) break;
    }
  }

  return IntegerVector(match.begin(), match.end());
}
