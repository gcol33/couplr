// src/interface/probe_cost_matrix.cpp
// Single-pass summary of a cost matrix, supplying every data-dependent input
// the "auto" dispatcher in assignment() needs, plus the NaN rejection that runs
// for all methods.
//
// The R-level equivalent was any(is.nan(cost)), range(cost, na.rm, finite) and
// mean(is.na(cost) | is.infinite(cost)). Each of those allocates a temporary
// the size of the matrix, so selecting a solver cost several n*m allocations
// and several passes before any solving began. One pass with no allocation
// replaces them.
//
// Integer matrices are read as integers rather than coerced, since coercion
// would itself allocate the copy this exists to avoid. An integer matrix has no
// NaN and no infinities, so NA_INTEGER is its only non-finite value.

#include <Rcpp.h>

#include <algorithm>
#include <vector>

// Kept in sync with the rule table in R/lap_dispatch.R: a caller reads
// `constant` and `binary` for the Hopcroft-Karp test, `n_distinct` for the
// few-cost-levels test, `n_nonfinite` for the sparsity report, and `has_nan`
// for the input check. `n_distinct` counts distinct finite values up to 64 and
// reports 65 for anything above.
namespace {

// How many distinct finite values the probe tells apart. A count past it is
// reported as one more than it, which says "many" without the probe paying to
// keep counting: the distinct values are held sorted, a value already held is
// found by binary search, and once the set is full nothing new is inserted.
constexpr int kDistinctCap = 64;

struct DistinctCount {
  std::vector<double> held;
  bool overflow = false;

  void offer(double x) {
    if (overflow) return;
    const auto it = std::lower_bound(held.begin(), held.end(), x);
    if (it != held.end() && *it == x) return;
    if (static_cast<int>(held.size()) == kDistinctCap) {
      overflow = true;
      return;
    }
    held.insert(it, x);
  }

  double count() const {
    return overflow ? static_cast<double>(kDistinctCap + 1)
                    : static_cast<double>(held.size());
  }
};

}  // namespace

Rcpp::List probe_cost_matrix_impl(SEXP cost) {
  R_xlen_t n_total = Rf_xlength(cost);
  DistinctCount distinct;
  R_xlen_t n_nonfinite = 0;
  R_xlen_t n_finite = 0;
  bool has_nan = false;
  bool all_in_01 = true;
  double lo = R_PosInf, hi = R_NegInf;

  if (TYPEOF(cost) == INTSXP) {
    const int* p = INTEGER(cost);
    for (R_xlen_t k = 0; k < n_total; ++k) {
      const int x = p[k];
      if (x == NA_INTEGER) { ++n_nonfinite; continue; }
      ++n_finite;
      const double d = static_cast<double>(x);
      if (d < lo) lo = d;
      if (d > hi) hi = d;
      if (all_in_01 && x != 0 && x != 1) all_in_01 = false;
      distinct.offer(d);
    }
  } else {
    const double* p = REAL(cost);
    for (R_xlen_t k = 0; k < n_total; ++k) {
      const double x = p[k];
      if (!R_finite(x)) {
        ++n_nonfinite;
        // ISNAN covers NA and NaN alike; R_IsNA separates the two, and only a
        // true NaN is rejected, matching any(is.nan(cost)).
        if (ISNAN(x) && !R_IsNA(x)) has_nan = true;
        continue;
      }
      ++n_finite;
      if (x < lo) lo = x;
      if (x > hi) hi = x;
      if (all_in_01 && x != 0.0 && x != 1.0) all_in_01 = false;
      distinct.offer(x);
    }
  }

  // range(na.rm = TRUE, finite = TRUE) over an all-non-finite matrix yields
  // c(Inf, -Inf), which failed the is.finite() guard; report it the same way.
  const bool any_finite = (n_finite > 0);
  const bool constant   = any_finite && (lo == hi);
  const bool binary     = any_finite && (lo == 0.0) && (hi == 1.0) && all_in_01;

  return Rcpp::List::create(
    Rcpp::_["has_nan"]     = has_nan,
    Rcpp::_["n_nonfinite"] = static_cast<double>(n_nonfinite),
    Rcpp::_["n_total"]     = static_cast<double>(n_total),
    Rcpp::_["any_finite"]  = any_finite,
    Rcpp::_["constant"]    = constant,
    Rcpp::_["binary"]      = binary,
    Rcpp::_["n_distinct"]  = distinct.count()
  );
}
