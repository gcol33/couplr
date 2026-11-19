// src/core/lap_plain_arith.h
// The arithmetic every built-in distance is made of - NO Rcpp dependencies
//
// A distance reaches a solver as a double, and the solver chooses among
// matchings whose totals can tie to the last bit, so two platforms return the
// same matching only when they hold the same bits. IEEE 754 fixes the result
// of +, -, *, / and sqrt on doubles; what it leaves open is everything this
// header closes:
//
//   - the order of a sum. Every sum here runs over its terms in index order.
//   - the width of an accumulator. Every intermediate is a double; no long
//     double, whose width differs between x86_64 and arm64.
//   - the fusing of a*b + c into one rounded operation where the target has
//     the instruction. Every function here is compiled with contraction off.
//   - the library behind a factorization. Means, covariances, the Cholesky
//     factor, the triangular solve and the inverse are written out here in
//     scalar operations, with no BLAS or LAPACK routine behind them.
//
// Means, covariances, scaling statistics, the precision matrix of a
// Mahalanobis distance and the per-pair distance kernels are all defined here
// and nowhere else: the dense matrix, a lazy cost source and the implicit
// loop's metric tree read them from this header.
#pragma once

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <vector>

// Contraction is switched off in the source, per function, because the two
// compilers give it different handles.
//
// clang reads the standard pragma at the start of a compound statement and
// applies it to the operations written in that block. The front end then emits
// separate multiplications and additions, which stay separate wherever the
// function is inlined.
//
// GCC does not implement the pragma. It contracts per compiled function, under
// that function's -ffp-contract setting, so the setting is given by the
// optimize attribute, and the function is kept out of line so that its
// arithmetic is never compiled inside a caller that carries another setting.
// Both are applied only where the target has a fused multiply-add instruction
// (__FP_FAST_FMA), which is the only place GCC has anything to contract.
#if defined(__clang__)
#  define LAP_PLAIN_ARITH_FN
#  define LAP_PLAIN_ARITH _Pragma("STDC FP_CONTRACT OFF")
#elif defined(__GNUC__) && defined(__FP_FAST_FMA)
#  define LAP_PLAIN_ARITH_FN __attribute__((noinline, optimize("fp-contract=off")))
#  define LAP_PLAIN_ARITH
#else
#  define LAP_PLAIN_ARITH_FN
#  define LAP_PLAIN_ARITH
#endif

namespace lap {

enum class DistanceMetric {
    Euclidean,
    Manhattan,
    SquaredEuclidean,
    Chebyshev,
    Mahalanobis
};

namespace plain {

// One block of rows of a sample: `n` rows of a column-major matrix, column k
// starting at data + k * n. A sample is a sequence of blocks read in order, so
// the left and right units of a design are one sample without being copied
// into one matrix.
struct ColumnBlock {
    const double* data = nullptr;
    int64_t n = 0;

    const double* column(int64_t k) const {
        return data + static_cast<std::size_t>(k) * static_cast<std::size_t>(n);
    }
};

using Sample = std::vector<ColumnBlock>;

inline int64_t sample_size(const Sample& sample) {
    int64_t n = 0;
    for (const ColumnBlock& b : sample) n += b.n;
    return n;
}

// The sum of n values read at a stride, in index order.
LAP_PLAIN_ARITH_FN inline double sum(const double* x, int64_t n, int64_t stride = 1) {
    LAP_PLAIN_ARITH
    double s = 0.0;
    for (int64_t i = 0; i < n; ++i) s += x[static_cast<std::size_t>(i * stride)];
    return s;
}

// The product of two doubles, rounded once. A product written inside a sum
// can be fused with the addition that follows it; a product taken here cannot.
LAP_PLAIN_ARITH_FN inline double times(double a, double b) {
    LAP_PLAIN_ARITH
    return a * b;
}

// The weighted mean and the unbiased weighted variance of n values:
// mean = sum(w x) / sum(w), corrected by the weighted mean of the deviations
// from it as column_mean() corrects its sum, and
// var = sum(w) / (sum(w)^2 - sum(w^2)) * sum(w (x - mean)^2),
// which for unit weights are the mean and the n - 1 sample variance. NaN for
// both without weight, and for the variance when sum(w)^2 does not exceed
// sum(w^2), as with one unit.
struct WeightedMoments {
    double mean = std::numeric_limits<double>::quiet_NaN();
    double var = std::numeric_limits<double>::quiet_NaN();
};

LAP_PLAIN_ARITH_FN inline WeightedMoments weighted_moments(const double* x,
                                                           const double* w,
                                                           int64_t n) {
    LAP_PLAIN_ARITH
    WeightedMoments out;
    double sw = 0.0;
    double swx = 0.0;
    double sww = 0.0;
    for (int64_t i = 0; i < n; ++i) {
        sw += w[i];
        swx += w[i] * x[i];
        sww += w[i] * w[i];
    }
    if (n == 0 || sw == 0.0) return out;
    const double m = swx / sw;
    if (std::isfinite(m)) {
        double t = 0.0;
        for (int64_t i = 0; i < n; ++i) t += w[i] * (x[i] - m);
        out.mean = m + t / sw;
    } else {
        out.mean = m;
    }
    const double denom = sw * sw - sww;
    if (!(denom > 0.0)) return out;
    double ss = 0.0;
    for (int64_t i = 0; i < n; ++i) {
        const double d = x[i] - out.mean;
        ss += w[i] * (d * d);
    }
    out.var = sw / denom * ss;
    return out;
}

// The mean of column k, in two passes over the rows in order, block after
// block: the sum over the count, then that value corrected by the mean of the
// deviations from it. The second pass removes the rounding the first sum
// accumulated, and it returns a constant column's value exactly, so that the
// deviations of a constant column are zero and its standard deviation is 0
// rather than a rounding residue. NaN for an empty sample.
LAP_PLAIN_ARITH_FN inline double column_mean(const Sample& sample, int64_t k) {
    LAP_PLAIN_ARITH
    const double n = static_cast<double>(sample_size(sample));
    double s = 0.0;
    for (const ColumnBlock& b : sample) {
        const double* x = b.column(k);
        for (int64_t i = 0; i < b.n; ++i) s += x[i];
    }
    const double m = s / n;
    if (!std::isfinite(m)) return m;
    double t = 0.0;
    for (const ColumnBlock& b : sample) {
        const double* x = b.column(k);
        for (int64_t i = 0; i < b.n; ++i) t += x[i] - m;
    }
    return m + t / n;
}

// The mean of n values by the same two passes.
inline double mean(const double* x, int64_t n) {
    return column_mean(Sample{ColumnBlock{x, n}}, 0);
}

// The sum over the sample of (x_ia - mean_a) * (x_ib - mean_b), in row order.
LAP_PLAIN_ARITH_FN inline double centred_cross_product(const Sample& sample,
                                                       int64_t a, int64_t b,
                                                       double mean_a,
                                                       double mean_b) {
    LAP_PLAIN_ARITH
    double s = 0.0;
    for (const ColumnBlock& blk : sample) {
        const double* xa = blk.column(a);
        const double* xb = blk.column(b);
        for (int64_t i = 0; i < blk.n; ++i) {
            const double da = xa[i] - mean_a;
            const double db = xb[i] - mean_b;
            s += da * db;
        }
    }
    return s;
}

// The within-group covariance of `p` columns: each group's cross-products
// about its own means, added in group order, over the total count less the
// number of groups. One group gives the sample covariance; a treated and a
// control group give the pooled covariance of a two-group design. `out` is
// p * p, symmetric to the bit because each entry is computed once and written
// to both places. The entries are NaN when the count does not exceed the
// number of groups.
LAP_PLAIN_ARITH_FN inline void within_group_covariance(
        const std::vector<Sample>& groups, int64_t p, std::vector<double>& out) {
    LAP_PLAIN_ARITH
    const std::size_t sp = static_cast<std::size_t>(p);
    out.assign(sp * sp, 0.0);
    int64_t n = 0;
    for (const Sample& g : groups) {
        n += sample_size(g);
        std::vector<double> mean(sp);
        for (int64_t k = 0; k < p; ++k) {
            mean[static_cast<std::size_t>(k)] = column_mean(g, k);
        }
        for (int64_t a = 0; a < p; ++a) {
            for (int64_t b = a; b < p; ++b) {
                out[static_cast<std::size_t>(a * p + b)] +=
                    centred_cross_product(g, a, b,
                                          mean[static_cast<std::size_t>(a)],
                                          mean[static_cast<std::size_t>(b)]);
            }
        }
    }
    const double dof = static_cast<double>(n - static_cast<int64_t>(groups.size()));
    for (int64_t a = 0; a < p; ++a) {
        for (int64_t b = a; b < p; ++b) {
            const double v = dof > 0.0
                ? out[static_cast<std::size_t>(a * p + b)] / dof
                : std::numeric_limits<double>::quiet_NaN();
            out[static_cast<std::size_t>(a * p + b)] = v;
            out[static_cast<std::size_t>(b * p + a)] = v;
        }
    }
}

// The sample standard deviation of column k about `mean`, NaN below two rows.
LAP_PLAIN_ARITH_FN inline double column_sd(const Sample& sample, int64_t k,
                                           double mean) {
    LAP_PLAIN_ARITH
    const int64_t n = sample_size(sample);
    if (n < 2) return std::numeric_limits<double>::quiet_NaN();
    return std::sqrt(centred_cross_product(sample, k, k, mean, mean) /
                     static_cast<double>(n - 1));
}

// The smallest and largest value of column k. A NaN anywhere in the column is
// returned as both, as min() and max() report a missing value.
inline void column_range(const Sample& sample, int64_t k, double& lo, double& hi) {
    lo = std::numeric_limits<double>::infinity();
    hi = -std::numeric_limits<double>::infinity();
    for (const ColumnBlock& b : sample) {
        const double* x = b.column(k);
        for (int64_t i = 0; i < b.n; ++i) {
            if (std::isnan(x[i])) {
                lo = hi = x[i];
                return;
            }
            if (x[i] < lo) lo = x[i];
            if (x[i] > hi) hi = x[i];
        }
    }
}

// The median of `v`, which is reordered: the middle order statistic, or half
// the sum of the two middle ones. An order statistic is a value of the input,
// so it does not depend on how the selection is carried out. NaN for an empty
// input or one that holds a NaN, which has no order.
LAP_PLAIN_ARITH_FN inline double median_of(std::vector<double>& v) {
    LAP_PLAIN_ARITH
    if (v.empty()) return std::numeric_limits<double>::quiet_NaN();
    for (double x : v) {
        if (std::isnan(x)) return std::numeric_limits<double>::quiet_NaN();
    }
    const std::size_t n = v.size();
    const std::size_t hi = n / 2;
    std::nth_element(v.begin(), v.begin() + static_cast<std::ptrdiff_t>(hi), v.end());
    const double upper = v[hi];
    if (n % 2 == 1) return upper;
    const double lower =
        *std::max_element(v.begin(), v.begin() + static_cast<std::ptrdiff_t>(hi));
    return (lower + upper) / 2.0;
}

// The median of column k and the median absolute deviation about it, scaled
// by 1.4826 so that it estimates the standard deviation of a normal sample.
LAP_PLAIN_ARITH_FN inline void column_median_mad(const Sample& sample, int64_t k,
                                                 double& median, double& mad) {
    LAP_PLAIN_ARITH
    std::vector<double> v;
    v.reserve(static_cast<std::size_t>(sample_size(sample)));
    for (const ColumnBlock& b : sample) {
        const double* x = b.column(k);
        v.insert(v.end(), x, x + b.n);
    }
    std::vector<double> work = v;
    median = median_of(work);
    for (double& x : v) x = std::fabs(x - median);
    mad = 1.4826 * median_of(v);
}

// Lower-triangular L with A = L L', A the symmetric part of `a` (n * n; the
// symmetric part reads the same in row-major and column-major order). False
// when a pivot is not positive, which is A not being positive definite. The
// symmetric part is what a quadratic form d' A d evaluates for any A at all,
// so a matrix off by a rounding error in its symmetry is the same problem.
// `l` is row-major.
LAP_PLAIN_ARITH_FN inline bool cholesky_lower(const std::vector<double>& a,
                                              int64_t n, std::vector<double>& l) {
    LAP_PLAIN_ARITH
    l.assign(static_cast<std::size_t>(n * n), 0.0);
    for (int64_t i = 0; i < n; ++i) {
        for (int64_t j = 0; j <= i; ++j) {
            double s = 0.5 * (a[static_cast<std::size_t>(i * n + j)] +
                              a[static_cast<std::size_t>(j * n + i)]);
            for (int64_t k = 0; k < j; ++k) {
                s -= l[static_cast<std::size_t>(i * n + k)] *
                     l[static_cast<std::size_t>(j * n + k)];
            }
            if (i == j) {
                if (!(s > 0.0)) return false;
                l[static_cast<std::size_t>(i * n + i)] = std::sqrt(s);
            } else {
                l[static_cast<std::size_t>(i * n + j)] =
                    s / l[static_cast<std::size_t>(j * n + j)];
            }
        }
    }
    return true;
}

// Solve L x = b by forward substitution, L lower triangular and row-major with
// a nonzero diagonal. `x` holds n doubles and may not alias `b`.
LAP_PLAIN_ARITH_FN inline void solve_lower(const std::vector<double>& l, int64_t n,
                                           const double* b, double* x) {
    LAP_PLAIN_ARITH
    for (int64_t i = 0; i < n; ++i) {
        double s = b[i];
        for (int64_t k = 0; k < i; ++k) {
            s -= l[static_cast<std::size_t>(i * n + k)] * x[k];
        }
        x[i] = s / l[static_cast<std::size_t>(i * n + i)];
    }
}

// The inverse of the symmetric part of `a` (n * n) through its Cholesky
// factor: X = L^-1 one column at a time by forward substitution, then
// A^-1 = X' X. False when the matrix is not positive definite to working
// precision: a pivot that is not positive, or a factor whose reciprocal
// condition number 1 / (||L||_inf ||L^-1||_inf), squared to stand for the
// matrix, falls below the machine epsilon. `out` is symmetric to the bit.
LAP_PLAIN_ARITH_FN inline bool spd_inverse(const std::vector<double>& a, int64_t n,
                                           std::vector<double>& out) {
    LAP_PLAIN_ARITH
    const std::size_t sn = static_cast<std::size_t>(n);
    if (a.size() != sn * sn) return false;
    for (double v : a) {
        if (!std::isfinite(v)) return false;
    }
    std::vector<double> l;
    if (!cholesky_lower(a, n, l)) return false;

    // x holds L^-1 row-major; column j is the solution of L x = e_j.
    std::vector<double> x(sn * sn, 0.0);
    std::vector<double> rhs(sn, 0.0);
    std::vector<double> col(sn, 0.0);
    for (int64_t j = 0; j < n; ++j) {
        rhs[static_cast<std::size_t>(j)] = 1.0;
        solve_lower(l, n, rhs.data(), col.data());
        rhs[static_cast<std::size_t>(j)] = 0.0;
        for (int64_t i = 0; i < n; ++i) {
            x[static_cast<std::size_t>(i * n + j)] = col[static_cast<std::size_t>(i)];
        }
    }

    double norm_l = 0.0;
    double norm_x = 0.0;
    for (int64_t i = 0; i < n; ++i) {
        double row_l = 0.0;
        double row_x = 0.0;
        for (int64_t j = 0; j <= i; ++j) {
            row_l += std::fabs(l[static_cast<std::size_t>(i * n + j)]);
            row_x += std::fabs(x[static_cast<std::size_t>(i * n + j)]);
        }
        if (row_l > norm_l) norm_l = row_l;
        if (row_x > norm_x) norm_x = row_x;
    }
    const double rcond = 1.0 / (norm_l * norm_x);
    if (!(rcond * rcond >= std::numeric_limits<double>::epsilon())) return false;

    out.assign(sn * sn, 0.0);
    for (int64_t i = 0; i < n; ++i) {
        for (int64_t j = i; j < n; ++j) {
            // X is lower triangular, so column i and column j of it meet from
            // row j down.
            double s = 0.0;
            for (int64_t k = j; k < n; ++k) {
                s += x[static_cast<std::size_t>(k * n + i)] *
                     x[static_cast<std::size_t>(k * n + j)];
            }
            out[static_cast<std::size_t>(i * n + j)] = s;
            out[static_cast<std::size_t>(j * n + i)] = s;
        }
    }
    return true;
}

// The sum over k of (a_k - b_k)^2, in index order. Every digit the coordinates
// carry is kept, which ||a||^2 + ||b||^2 - 2 a.b would not when the
// coordinates are large next to the distance between them.
LAP_PLAIN_ARITH_FN inline double squared_difference(const double* a, const double* b,
                                                    int64_t n) {
    LAP_PLAIN_ARITH
    double s = 0.0;
    for (int64_t k = 0; k < n; ++k) {
        const double d = a[k] - b[k];
        s += d * d;
    }
    return s;
}

// The distance between two points of `n` coordinates under `metric`.
// `inv_cov` is the n * n row-major precision matrix, read under Mahalanobis
// alone.
//
// Mahalanobis is evaluated as the sum over a of d_a * (sum over b of A_ab d_b),
// the inner sum recomputing its own differences, and the metric tree's
// allowance for this evaluation is counted off exactly these operations.
LAP_PLAIN_ARITH_FN inline double pair_distance(DistanceMetric metric,
                                               const double* li, const double* rj,
                                               int64_t n, const double* inv_cov) {
    LAP_PLAIN_ARITH
    switch (metric) {
        case DistanceMetric::Euclidean: {
            double s = 0.0;
            for (int64_t k = 0; k < n; ++k) {
                const double d = li[k] - rj[k];
                s += d * d;
            }
            return std::sqrt(s);
        }
        case DistanceMetric::Manhattan: {
            double s = 0.0;
            for (int64_t k = 0; k < n; ++k) {
                s += std::abs(li[k] - rj[k]);
            }
            return s;
        }
        case DistanceMetric::SquaredEuclidean: {
            double s = 0.0;
            for (int64_t k = 0; k < n; ++k) {
                const double d = li[k] - rj[k];
                s += d * d;
            }
            return s;
        }
        case DistanceMetric::Chebyshev: {
            double s = 0.0;
            for (int64_t k = 0; k < n; ++k) {
                // std::max drops a NaN operand, so a missing coordinate
                // is returned rather than passed over.
                const double d = std::abs(li[k] - rj[k]);
                if (std::isnan(d)) return d;
                s = std::max(s, d);
            }
            return s;
        }
        case DistanceMetric::Mahalanobis: {
            double s = 0.0;
            for (int64_t a = 0; a < n; ++a) {
                const double diff_a = li[a] - rj[a];
                if (diff_a == 0.0) continue;
                double row_sum = 0.0;
                const double* inv_row = inv_cov + static_cast<std::size_t>(a * n);
                for (int64_t b = 0; b < n; ++b) {
                    row_sum += inv_row[b] * (li[b] - rj[b]);
                }
                s += diff_a * row_sum;
            }
            return std::sqrt(std::max(s, 0.0));
        }
    }
    return 0.0;  // unreachable
}

// The sample standard deviation of a stream of values, read once. Welford's
// update keeps the variance from being the difference of two large sums: the
// running mean moves by delta / count and the sum of squares grows by
// delta * (x - new mean). The values are taken in the order they are added.
class RunningSd {
public:
    LAP_PLAIN_ARITH_FN void add(double x) {
        LAP_PLAIN_ARITH
        count_ += 1.0;
        const double delta = x - mean_;
        mean_ += delta / count_;
        ss_ += delta * (x - mean_);
    }

    // NaN below two values.
    LAP_PLAIN_ARITH_FN double sd() const {
        LAP_PLAIN_ARITH
        if (count_ < 2.0) return std::numeric_limits<double>::quiet_NaN();
        return std::sqrt(ss_ / (count_ - 1.0));
    }

private:
    double count_ = 0.0;
    double mean_ = 0.0;
    double ss_ = 0.0;
};

}  // namespace plain

}  // namespace lap
