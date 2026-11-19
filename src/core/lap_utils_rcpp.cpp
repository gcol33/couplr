// src/core/lap_utils_rcpp.cpp - Rcpp utility functions
#include "lap_utils_rcpp.h"

#include "lap_cost_source.h"

#include <sstream>
#include <cctype>   // std::tolower

// ------------------------- adapter conversions -------------------------

static std::vector<lap::CaliperSpec> calipers_from_r(Rcpp::List calipers,
                                                     int64_t n_left, int64_t n_right);

lap::CostMatrix rcpp_to_cost_matrix(const Rcpp::NumericMatrix& cost) {
  const int64_t n = cost.nrow();
  const int64_t m = cost.ncol();

  lap::CostMatrix cm(n, m);

  for (int64_t i = 0; i < n; ++i) {
    for (int64_t j = 0; j < m; ++j) {
      double v = cost(i, j);
      cm.at(i, j) = v;
      cm.mask[static_cast<size_t>(lap::flat_index(i, j, m))] = (R_finite(v)) ? 1 : 0;
    }
  }

  return cm;
}

// Maps the same metric name spellings accepted by R's compute_distance_matrix()
// (R/matching_distance.R) -- single source of truth for the string set stays
// in R; this just needs to recognize what R already validated and normalized
// to lowercase before calling down here.
static lap::DistanceMetric metric_from_string(const std::string& metric) {
  if (metric == "euclidean" || metric == "l2") return lap::DistanceMetric::Euclidean;
  if (metric == "manhattan" || metric == "l1" || metric == "cityblock") return lap::DistanceMetric::Manhattan;
  if (metric == "squared_euclidean" || metric == "sqeuclidean" || metric == "sq") return lap::DistanceMetric::SquaredEuclidean;
  if (metric == "chebyshev" || metric == "chebychev" || metric == "maximum" || metric == "max") return lap::DistanceMetric::Chebyshev;
  if (metric == "mahalanobis" || metric == "maha") return lap::DistanceMetric::Mahalanobis;
  LAP_ERROR("rcpp_to_lazy_cost_matrix: unknown distance metric '%s'", metric.c_str());
  return lap::DistanceMetric::Euclidean;  // unreachable
}

Rcpp::NumericVector lazy_pair_distances_impl(
    const Rcpp::NumericMatrix& left_mat,
    const Rcpp::NumericMatrix& right_mat,
    SEXP metric,
    Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
    const Rcpp::IntegerVector& rows,
    const Rcpp::IntegerVector& cols) {
  if (rows.size() != cols.size()) {
    LAP_ERROR("lazy_pair_distances_impl: rows and cols must have the same length");
  }
  // No calipers and no distance cut: these pairs are the ones a solve already
  // chose, so nothing is being filtered, and maximize is false so the distance
  // comes back in the sign it was computed in.
  const LazySource source = rcpp_lazy_source(
      left_mat, right_mat, metric, inv_cov, R_PosInf, Rcpp::List::create(), false);

  Rcpp::NumericVector out(rows.size());
  std::visit([&](const auto& cm) {
    for (R_xlen_t k = 0; k < rows.size(); ++k) {
      const int64_t i = static_cast<int64_t>(rows[k]) - 1;
      const int64_t j = static_cast<int64_t>(cols[k]) - 1;
      if (i < 0 || i >= cm.nrow || j < 0 || j >= cm.ncol) {
        LAP_ERROR("lazy_pair_distances_impl: pair index out of range");
      }
      double c = 0.0;
      out[k] = cost_if_allowed(cm, i, j, c) ? c : NA_REAL;
    }
  }, source);
  return out;
}

Rcpp::NumericMatrix lazy_dense_matrix_impl(
    const Rcpp::NumericMatrix& left_mat,
    const Rcpp::NumericMatrix& right_mat,
    SEXP metric,
    Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov) {
  const LazySource source = rcpp_lazy_source(
      left_mat, right_mat, metric, inv_cov, R_PosInf, Rcpp::List::create(), false);

  Rcpp::NumericMatrix out(left_mat.nrow(), right_mat.nrow());
  std::visit([&](const auto& cm) {
    for (int64_t j = 0; j < cm.ncol; ++j) {
      for (int64_t i = 0; i < cm.nrow; ++i) {
        double c = 0.0;
        out(i, j) = cost_if_allowed(cm, i, j, c) ? c : NA_REAL;
      }
    }
  }, source);
  return out;
}

LazySource rcpp_lazy_source(const Rcpp::NumericMatrix& left_mat,
                            const Rcpp::NumericMatrix& right_mat,
                            SEXP distance,
                            Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                            double max_distance,
                            Rcpp::List calipers,
                            bool maximize) {
  if (Rf_isFunction(distance)) {
    return LazySource(std::in_place_type<lap::CallbackCostSource>,
                      Rcpp::Function(distance), left_mat, right_mat, max_distance,
                      calipers_from_r(calipers, left_mat.nrow(), right_mat.nrow()),
                      maximize);
  }
  Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov_arg = R_NilValue;
  if (inv_cov.isNotNull()) {
    Rcpp::NumericMatrix ic(inv_cov.get());
    if (ic.nrow() > 0 && ic.ncol() > 0) inv_cov_arg = inv_cov;
  }
  return LazySource(std::in_place_type<lap::LazyCostMatrix>,
                    rcpp_to_lazy_cost_matrix(left_mat, right_mat,
                                             Rcpp::as<std::string>(distance),
                                             inv_cov_arg, max_distance, calipers, maximize));
}

double lazy_distance_sd_impl(
    const Rcpp::NumericMatrix& left_mat,
    const Rcpp::NumericMatrix& right_mat,
    SEXP metric,
    Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov) {
  const LazySource source = rcpp_lazy_source(
      left_mat, right_mat, metric, inv_cov, R_PosInf, Rcpp::List::create(), false);

  // The pairs are read left unit by left unit, the order finite_sd_impl()
  // reads a matrix in. A distance that is not a finite number is left out.
  lap::plain::RunningSd acc;
  std::visit([&](const auto& cm) {
    for (int64_t i = 0; i < cm.nrow; ++i) {
      if ((i & 63) == 0) Rcpp::checkUserInterrupt();
      for (int64_t j = 0; j < cm.ncol; ++j) {
        double c = 0.0;
        if (!cost_if_allowed(cm, i, j, c)) continue;
        if (!R_finite(c)) continue;
        acc.add(c);
      }
    }
  }, source);
  const double sd = acc.sd();
  return std::isnan(sd) ? NA_REAL : sd;
}

double finite_sd_impl(const Rcpp::NumericMatrix& x) {
  const int64_t n = x.nrow();
  const int64_t m = x.ncol();
  lap::plain::RunningSd acc;
  for (int64_t i = 0; i < n; ++i) {
    if ((i & 63) == 0) Rcpp::checkUserInterrupt();
    for (int64_t j = 0; j < m; ++j) {
      const double c = x(i, j);
      if (R_finite(c)) acc.add(c);
    }
  }
  const double sd = acc.sd();
  return std::isnan(sd) ? NA_REAL : sd;
}

double plain_sum_impl(const Rcpp::NumericVector& x) {
  return lap::plain::sum(x.begin(), static_cast<int64_t>(x.size()));
}

double plain_mean_impl(const Rcpp::NumericVector& x) {
  return lap::plain::mean(x.begin(), static_cast<int64_t>(x.size()));
}

Rcpp::NumericVector plain_row_sums_impl(const Rcpp::NumericMatrix& x) {
  const int64_t n = x.nrow();
  const int64_t m = x.ncol();
  Rcpp::NumericVector out(static_cast<R_xlen_t>(n));
  // Row i of a column-major matrix is read at a stride of the row count.
  for (int64_t i = 0; i < n; ++i) out[i] = lap::plain::sum(x.begin() + i, m, n);
  return out;
}

Rcpp::List weighted_moments_impl(const Rcpp::NumericVector& x,
                                 const Rcpp::NumericVector& w) {
  if (x.size() != w.size()) {
    LAP_ERROR("weighted_moments_impl: x and w must have the same length");
  }
  const lap::plain::WeightedMoments m =
      lap::plain::weighted_moments(x.begin(), w.begin(), static_cast<int64_t>(x.size()));
  return Rcpp::List::create(
      Rcpp::Named("mean") = std::isnan(m.mean) ? NA_REAL : m.mean,
      Rcpp::Named("var") = std::isnan(m.var) ? NA_REAL : m.var);
}

// The rows of an R matrix as one block of a sample. An R matrix is
// column-major, which is the layout a block is read in.
static lap::plain::ColumnBlock block_of(const Rcpp::NumericMatrix& x) {
  return lap::plain::ColumnBlock{x.begin(), static_cast<int64_t>(x.nrow())};
}

Rcpp::NumericMatrix group_covariance_impl(const Rcpp::NumericMatrix& left_mat,
                                          const Rcpp::NumericMatrix& right_mat) {
  const int64_t p = left_mat.ncol();
  if (right_mat.ncol() != p) {
    LAP_ERROR("group_covariance_impl: left_mat and right_mat must have the same number of columns");
  }
  // Two groups of at least two units each have a within-group covariance to
  // pool. A group of one unit has none, and the two sides are then one sample.
  std::vector<lap::plain::Sample> groups;
  if (left_mat.nrow() >= 2 && right_mat.nrow() >= 2) {
    groups.push_back({block_of(left_mat)});
    groups.push_back({block_of(right_mat)});
  } else {
    groups.push_back({block_of(left_mat), block_of(right_mat)});
  }
  std::vector<double> cov;
  lap::plain::within_group_covariance(groups, p, cov);
  Rcpp::NumericMatrix out(static_cast<int>(p), static_cast<int>(p));
  std::copy(cov.begin(), cov.end(), out.begin());
  return out;
}

SEXP precision_matrix_impl(const Rcpp::NumericMatrix& cov_mat) {
  const int64_t p = cov_mat.nrow();
  if (cov_mat.ncol() != p) {
    LAP_ERROR("precision_matrix_impl: the covariance matrix must be square");
  }
  const std::vector<double> a(cov_mat.begin(), cov_mat.end());
  std::vector<double> inv;
  if (!lap::plain::spd_inverse(a, p, inv)) return R_NilValue;
  Rcpp::NumericMatrix out(static_cast<int>(p), static_cast<int>(p));
  std::copy(inv.begin(), inv.end(), out.begin());
  return out;
}

Rcpp::List scaling_statistics_impl(const Rcpp::NumericMatrix& left_mat,
                                   const Rcpp::NumericMatrix& right_mat,
                                   const std::string& method) {
  const int64_t p = left_mat.ncol();
  if (right_mat.ncol() != p) {
    LAP_ERROR("scaling_statistics_impl: left_mat and right_mat must have the same number of columns");
  }
  const lap::plain::Sample sample{block_of(left_mat), block_of(right_mat)};
  Rcpp::NumericVector center(static_cast<R_xlen_t>(p));
  Rcpp::NumericVector scale(static_cast<R_xlen_t>(p));
  if (method == "standardize") {
    for (int64_t k = 0; k < p; ++k) {
      center[k] = lap::plain::column_mean(sample, k);
      scale[k] = lap::plain::column_sd(sample, k, center[k]);
    }
  } else if (method == "range") {
    Rcpp::NumericVector upper(static_cast<R_xlen_t>(p));
    for (int64_t k = 0; k < p; ++k) {
      double lo = 0.0, hi = 0.0;
      lap::plain::column_range(sample, k, lo, hi);
      center[k] = lo;
      upper[k] = hi;
      scale[k] = hi - lo;
    }
    return Rcpp::List::create(Rcpp::Named("center") = center,
                              Rcpp::Named("scale") = scale,
                              Rcpp::Named("upper") = upper);
  } else if (method == "robust") {
    for (int64_t k = 0; k < p; ++k) {
      double median = 0.0, mad = 0.0;
      lap::plain::column_median_mad(sample, k, median, mad);
      center[k] = median;
      scale[k] = mad;
    }
  } else {
    LAP_ERROR("scaling_statistics_impl: unknown method '%s'", method.c_str());
  }
  return Rcpp::List::create(Rcpp::Named("center") = center,
                            Rcpp::Named("scale") = scale);
}

// Each element is list(threshold, left, right): the caliper and the raw values
// of its variable for every left and every right unit, in row order (R's
// lazy_cost_spec_calipers()).
static std::vector<lap::CaliperSpec> calipers_from_r(Rcpp::List calipers,
                                                     int64_t n_left, int64_t n_right) {
  std::vector<lap::CaliperSpec> caliper_specs;
  caliper_specs.reserve(static_cast<std::size_t>(calipers.size()));
  for (R_xlen_t k = 0; k < calipers.size(); ++k) {
    Rcpp::List cal(calipers[k]);
    Rcpp::NumericVector left = cal["left"];
    Rcpp::NumericVector right = cal["right"];
    if (left.size() != n_left || right.size() != n_right) {
      LAP_ERROR("caliper %lld: its values cover %lld left and %lld right units, "
                "the costs %lld and %lld", (long long)(k + 1),
                (long long)left.size(), (long long)right.size(),
                (long long)n_left, (long long)n_right);
    }
    caliper_specs.push_back(lap::CaliperSpec{
        Rcpp::as<double>(cal["threshold"]),
        std::vector<double>(left.begin(), left.end()),
        std::vector<double>(right.begin(), right.end())});
  }
  return caliper_specs;
}

lap::LazyCostMatrix rcpp_to_lazy_cost_matrix(
    const Rcpp::NumericMatrix& left_mat,
    const Rcpp::NumericMatrix& right_mat,
    const std::string& metric,
    Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
    double max_distance,
    Rcpp::List calipers,
    bool maximize) {
  const int64_t n = left_mat.nrow();
  const int64_t m = right_mat.nrow();
  const int64_t p = left_mat.ncol();

  if (right_mat.ncol() != p) {
    LAP_ERROR("rcpp_to_lazy_cost_matrix: left_mat and right_mat must have the same number of columns");
  }

  // Repack column-major R matrices into row-major flat vectors: the solver
  // inner loops fix a row and scan all variables for a fixed unit, which
  // needs that unit's features contiguous for cache efficiency.
  std::vector<double> left_flat(static_cast<size_t>(n * p));
  for (int64_t i = 0; i < n; ++i) {
    for (int64_t k = 0; k < p; ++k) {
      left_flat[static_cast<size_t>(i * p + k)] = left_mat(i, k);
    }
  }
  std::vector<double> right_flat(static_cast<size_t>(m * p));
  for (int64_t j = 0; j < m; ++j) {
    for (int64_t k = 0; k < p; ++k) {
      right_flat[static_cast<size_t>(j * p + k)] = right_mat(j, k);
    }
  }

  std::vector<double> inv_cov_flat;
  if (inv_cov.isNotNull()) {
    Rcpp::NumericMatrix ic(inv_cov);
    if (ic.nrow() != p || ic.ncol() != p) {
      LAP_ERROR("rcpp_to_lazy_cost_matrix: inv_cov must be %lld x %lld",
                (long long)p, (long long)p);
    }
    inv_cov_flat.resize(static_cast<size_t>(p * p));
    for (int64_t a = 0; a < p; ++a) {
      for (int64_t b = 0; b < p; ++b) {
        inv_cov_flat[static_cast<size_t>(a * p + b)] = ic(a, b);
      }
    }
  }

  std::vector<lap::CaliperSpec> caliper_specs = calipers_from_r(calipers, n, m);

  return lap::LazyCostMatrix(std::move(left_flat), std::move(right_flat), p,
                             metric_from_string(metric), std::move(inv_cov_flat),
                             max_distance, std::move(caliper_specs), maximize);
}

Rcpp::List lap_result_to_rcpp(const lap::LapResult& result,
                              const Rcpp::NumericMatrix& original_cost) {
  const int n = static_cast<int>(result.assignment.size());

  // Convert 0-based to 1-based (0 = unmatched)
  Rcpp::IntegerVector match(n);
  for (int i = 0; i < n; ++i) {
    match[i] = (result.assignment[i] >= 0) ? (result.assignment[i] + 1) : 0;
  }

  // Recompute total from original costs for cross-solver consistency
  double total = compute_total_cost(original_cost, match);

  return make_result(match, total);
}

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
void build_allowed(const std::vector<int>& mask, int64_t n, int64_t m,
                   std::vector<int>& row_ptr, std::vector<int>& cols) {
  row_ptr.assign(static_cast<size_t>(n + 1), 0);
  for (int64_t i = 0; i < n; ++i)
    for (int64_t j = 0; j < m; ++j)
      if (!mask[static_cast<size_t>(lap::flat_index(i, j, m))]) ++row_ptr[static_cast<size_t>(i + 1)];

  for (int64_t i = 1; i <= n; ++i) row_ptr[static_cast<size_t>(i)] += row_ptr[static_cast<size_t>(i - 1)];

  cols.assign(static_cast<size_t>(row_ptr.back()), -1);
  std::vector<int> fill = row_ptr;
  for (int64_t i = 0; i < n; ++i)
    for (int64_t j = 0; j < m; ++j)
      if (!mask[static_cast<size_t>(lap::flat_index(i, j, m))]) cols[static_cast<size_t>(fill[static_cast<size_t>(i)]++)] = static_cast<int>(j);
}

// Check that each row has at least one allowed (non-forbidden) edge
void ensure_each_row_has_option(const std::vector<int>& mask, int64_t n, int64_t m) {
  for (int64_t i = 0; i < n; ++i) {
    bool ok = false;
    for (int64_t j = 0; j < m; ++j) {
      if (!mask[static_cast<size_t>(lap::flat_index(i, j, m))]) { ok = true; break; }
    }
    if (!ok) {
      LAP_ERROR("Infeasible: row %lld has no allowed edges", static_cast<long long>(i + 1));
    }
  }
}

// Check if matrix is feasible (each row has at least one finite value)
// Returns true if feasible, false otherwise (does not throw)
bool is_feasible(const Rcpp::NumericMatrix& M) {
  const int n = M.nrow();
  const int m = M.ncol();
  if (n == 0 || m == 0) return false;
  if (n > m) return false;  // Cannot match all rows if n > m

  for (int i = 0; i < n; ++i) {
    bool has_finite = false;
    for (int j = 0; j < m; ++j) {
      if (R_finite(M(i, j))) {
        has_finite = true;
        break;
      }
    }
    if (!has_finite) return false;
  }
  return true;
}

// Check if a matching result is valid (no forbidden edges chosen)
bool is_valid_matching(const Rcpp::NumericMatrix& cost,
                       const std::vector<int>& match) {
  const int n = cost.nrow();
  const int m = cost.ncol();

  for (int i = 0; i < n && i < static_cast<int>(match.size()); ++i) {
    int j1 = match[i];  // 1-based column
    if (j1 <= 0 || j1 > m) continue;  // Skip unmatched or out of bounds
    int j = j1 - 1;  // 0-based
    if (!R_finite(cost(i, j))) {
      return false;  // Forbidden edge was chosen
    }
  }
  return true;
}

// Helper for has_valid_matching: DFS to find augmenting path
static bool dfs_augment(int u, const std::vector<std::vector<int>>& adj,
                        std::vector<int>& match_v, std::vector<bool>& visited) {
  for (int v : adj[u]) {
    if (visited[v]) continue;
    visited[v] = true;
    if (match_v[v] < 0 || dfs_augment(match_v[v], adj, match_v, visited)) {
      match_v[v] = u;
      return true;
    }
  }
  return false;
}

// Check if a perfect matching exists using Hungarian-style augmenting paths
// This is O(n*m) worst case but fast for typical k-best subproblems
bool has_valid_matching(const Rcpp::NumericMatrix& M) {
  const int n = M.nrow();
  const int m = M.ncol();

  // Quick checks
  if (n == 0) return true;  // Empty is valid
  if (n > m) return false;  // Can't match all rows

  // Build adjacency list (only finite edges)
  std::vector<std::vector<int>> adj(n);
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < m; ++j) {
      if (R_finite(M(i, j))) {
        adj[i].push_back(j);
      }
    }
    if (adj[i].empty()) return false;  // Row has no options
  }

  // Find maximum matching using Hopcroft-Karp style augmentation
  std::vector<int> match_v(m, -1);  // match_v[j] = row matched to col j, or -1
  int matched = 0;

  for (int u = 0; u < n; ++u) {
    std::vector<bool> visited(m, false);
    if (dfs_augment(u, adj, match_v, visited)) {
      ++matched;
    }
  }

  return matched == n;  // Perfect matching exists iff all rows matched
}

// Central cost computation helper - THE SINGLE SOURCE OF TRUTH
// Computes: sum of original_cost[i, assignment[i]-1] over all matched rows
//
// This function defines what "cost" means across the entire package:
//   1. Always uses the ORIGINAL user-supplied cost matrix (no transformations)
//   2. Works for both minimize and maximize (no sign flips)
//   3. Ignores dummy columns (assignment[i] > ncol)
//   4. Ignores unmatched rows (assignment[i] == 0 or NA)
//   5. Only sums real, finite edges
double compute_total_cost(const Rcpp::NumericMatrix& original_cost,
                          const Rcpp::IntegerVector& assignment) {
  const int n = original_cost.nrow();
  const int m = original_cost.ncol();

  if (assignment.size() != n) {
    LAP_ERROR("compute_total_cost: assignment length %d != nrow %d",
               (int)assignment.size(), n);
  }

  double total = 0.0;

  for (int i = 0; i < n; ++i) {
    int col_1based = assignment[i];

    // Skip unmatched rows
    if (col_1based == 0 || col_1based == NA_INTEGER) {
      continue;
    }

    // Skip dummy columns (assignment points beyond real columns)
    if (col_1based > m) {
      continue;
    }

    // Convert to 0-based
    int j = col_1based - 1;

    // Safety check
    if (j < 0 || j >= m) {
      LAP_ERROR("compute_total_cost: invalid assignment[%d] = %d (ncol = %d)",
                 i, col_1based, m);
    }

    double c = original_cost(i, j);

    // Only accumulate finite costs
    if (R_finite(c)) {
      total += c;
    }
  }

  return total;
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
  LAP_ERROR("Unknown base method: '%s'", method.c_str());
}
