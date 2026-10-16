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
#include <cmath>
#include <cstdint>
#include <cstring>
#include <vector>

// Kept in sync with the branch order in assignment(): a caller reads
// `constant` and `binary` for the Hopcroft-Karp test, `n_nonfinite` for the
// sparsity test, and `has_nan` for the input check.
Rcpp::List probe_cost_matrix_impl(SEXP cost) {
  R_xlen_t n_total = Rf_xlength(cost);
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
    Rcpp::_["binary"]      = binary
  );
}

// The matching code asks three more questions of a dense cost matrix: whether
// any pair is admissible, which rows and columns have an admissible pair, and
// how the admissible costs are distributed. Answered in R, each built a
// temporary the size of the matrix (a logical mask, the vector of finite
// costs, a sorted copy for the quantiles), and together they took the peak of
// a dense match to several times the matrix. The answers below read the
// matrix in place and allocate at most the row and column flags and the
// quantile histograms.

namespace {

// A cost stands for an allowed pair when it is finite and below the forbidden
// marker, the test R's .is_valid_cost() applies.
inline bool admissible(double x, double big) {
  return R_finite(x) && x < big;
}

// Calls f(k, x) for every cell in storage order, integer and logical cells
// read as doubles and NA_INTEGER as NA_REAL, without coercing the matrix.
template <typename F>
void visit_costs(SEXP cost, F&& f) {
  const R_xlen_t n = Rf_xlength(cost);
  switch (TYPEOF(cost)) {
  case REALSXP: {
    const double* p = REAL(cost);
    for (R_xlen_t k = 0; k < n; ++k) f(k, p[k]);
    break;
  }
  case INTSXP:
  case LGLSXP: {
    const int* p = TYPEOF(cost) == INTSXP ? INTEGER(cost) : LOGICAL(cost);
    for (R_xlen_t k = 0; k < n; ++k) {
      f(k, p[k] == NA_INTEGER ? NA_REAL : static_cast<double>(p[k]));
    }
    break;
  }
  default:
    Rcpp::stop("the cost matrix must be numeric");
  }
}

// An unsigned key with the order of the doubles: the sign bit is set on
// non-negative values and every bit is flipped on negative ones. -0 and +0
// compare equal, so both take the key of +0.
inline uint64_t order_key(double x) {
  if (x == 0.0) x = 0.0;
  uint64_t b;
  std::memcpy(&b, &x, sizeof b);
  return (b >> 63) ? ~b : (b | (uint64_t(1) << 63));
}

inline double key_value(uint64_t key) {
  const uint64_t b = (key >> 63) ? (key & ~(uint64_t(1) << 63)) : ~key;
  double x;
  std::memcpy(&x, &b, sizeof x);
  return x;
}

constexpr int kDigitBits = 16;
constexpr size_t kBuckets = size_t(1) << kDigitBits;
using Histogram = std::vector<uint64_t>;

// The digit of a histogram holding the 0-based rank `rank`, and the rank
// within that digit's bucket.
inline void descend(const Histogram& h, uint64_t& rank, uint64_t& digit) {
  for (digit = 0; digit < kBuckets; ++digit) {
    if (rank < h[digit]) return;
    rank -= h[digit];
  }
  Rcpp::stop("internal error: rank beyond the histogram");
}

// The admissible costs at the 0-based ranks `ranks`, by most-significant-digit
// radix select on order_key(). `top` is the histogram of the keys' top 16 bits
// over every admissible cost; each further pass histograms the next 16 bits of
// the keys sharing a rank's digits fixed so far. Four digits fix the key, so
// the matrix is read three more times and never copied.
std::vector<double> select_ranks(SEXP cost, double big, const Histogram& top,
                                 const std::vector<uint64_t>& ranks) {
  const size_t t = ranks.size();
  std::vector<uint64_t> prefix(t), rest(ranks);
  for (size_t i = 0; i < t; ++i) descend(top, rest[i], prefix[i]);

  for (int pass = 1; pass < 64 / kDigitBits; ++pass) {
    const int fixed = pass * kDigitBits;
    const int shift = 64 - fixed - kDigitBits;
    std::vector<uint64_t> groups(prefix);
    std::sort(groups.begin(), groups.end());
    groups.erase(std::unique(groups.begin(), groups.end()), groups.end());
    std::vector<Histogram> hist(groups.size(), Histogram(kBuckets, 0));

    visit_costs(cost, [&](R_xlen_t, double x) {
      if (!admissible(x, big)) return;
      const uint64_t key = order_key(x);
      const uint64_t head = key >> (64 - fixed);
      for (size_t g = 0; g < groups.size(); ++g) {
        if (head == groups[g]) {
          ++hist[g][(key >> shift) & (kBuckets - 1)];
          break;
        }
      }
    });

    for (size_t i = 0; i < t; ++i) {
      const size_t g = std::lower_bound(groups.begin(), groups.end(), prefix[i]) -
                       groups.begin();
      uint64_t digit;
      descend(hist[g], rest[i], digit);
      prefix[i] = (prefix[i] << kDigitBits) | digit;
    }
  }

  std::vector<double> out(t);
  for (size_t i = 0; i < t; ++i) out[i] = key_value(prefix[i]);
  return out;
}

} // namespace

bool cost_any_admissible_impl(SEXP cost, double big) {
  const R_xlen_t n = Rf_xlength(cost);
  if (TYPEOF(cost) == REALSXP) {
    const double* p = REAL(cost);
    for (R_xlen_t k = 0; k < n; ++k) {
      if (admissible(p[k], big)) return true;
    }
    return false;
  }
  bool any = false;
  visit_costs(cost, [&](R_xlen_t, double x) { any = any || admissible(x, big); });
  return any;
}

// Row and column flags marking the rows and columns with at least one
// admissible pair.
Rcpp::List cost_margins_impl(SEXP cost, double big) {
  Rcpp::IntegerVector dim = Rf_getAttrib(cost, R_DimSymbol);
  if (dim.size() != 2) Rcpp::stop("the cost must be a matrix");
  const R_xlen_t nr = dim[0];
  Rcpp::LogicalVector row_ok(dim[0], false), col_ok(dim[1], false);
  visit_costs(cost, [&](R_xlen_t k, double x) {
    if (!admissible(x, big)) return;
    row_ok[k % nr] = true;
    col_ok[k / nr] = true;
  });
  return Rcpp::List::create(Rcpp::_["row_ok"] = row_ok,
                            Rcpp::_["col_ok"] = col_ok);
}

// The counts, extremes and type-7 quantiles (R's default) of the admissible
// costs. A quantile needs the order statistics at floor and ceiling of
// 1 + (n - 1) p, interpolated as quantile.default() does, so the values equal
// R's to the last bit.
Rcpp::List cost_summary_impl(SEXP cost, double big, double zero_tol,
                             Rcpp::NumericVector probs) {
  const bool want_q = probs.size() > 0;
  uint64_t n_adm = 0, n_zeros = 0;
  double lo = R_PosInf, hi = R_NegInf;
  Histogram top(want_q ? kBuckets : 0, 0);

  visit_costs(cost, [&](R_xlen_t, double x) {
    if (!admissible(x, big)) return;
    ++n_adm;
    if (std::fabs(x) < zero_tol) ++n_zeros;
    if (x < lo) lo = x;
    if (x > hi) hi = x;
    if (want_q) ++top[order_key(x) >> (64 - kDigitBits)];
  });

  Rcpp::NumericVector q(probs.size(), NA_REAL);
  if (want_q && n_adm > 0) {
    std::vector<uint64_t> ranks;
    std::vector<double> index(probs.size());
    for (R_xlen_t i = 0; i < probs.size(); ++i) {
      index[i] = 1.0 + static_cast<double>(n_adm - 1) * probs[i];
      ranks.push_back(static_cast<uint64_t>(std::floor(index[i])) - 1);
      ranks.push_back(static_cast<uint64_t>(std::ceil(index[i])) - 1);
    }
    const std::vector<double> at = select_ranks(cost, big, top, ranks);
    for (R_xlen_t i = 0; i < probs.size(); ++i) {
      const double x_lo = at[2 * i], x_hi = at[2 * i + 1];
      const double h = index[i] - std::floor(index[i]);
      q[i] = (h > 0 && x_hi != x_lo) ? (1 - h) * x_lo + h * x_hi : x_lo;
    }
  }

  return Rcpp::List::create(
    Rcpp::_["n_total"]      = static_cast<double>(Rf_xlength(cost)),
    Rcpp::_["n_admissible"] = static_cast<double>(n_adm),
    Rcpp::_["n_zeros"]      = static_cast<double>(n_zeros),
    Rcpp::_["min"]          = n_adm > 0 ? lo : NA_REAL,
    Rcpp::_["max"]          = n_adm > 0 ? hi : NA_REAL,
    Rcpp::_["quantiles"]    = q
  );
}
