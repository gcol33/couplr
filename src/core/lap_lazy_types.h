// src/core/lap_lazy_types.h
// Lazy (on-demand) cost source - NO Rcpp dependencies
//
// Computes C(i,j) on demand from the underlying feature rows instead of
// materializing an n_left x n_right dense matrix. Trades compute for RAM:
// a 100k x 100k dense CostMatrix needs ~80GB, but the underlying 100k x 100
// feature matrices need ~80MB. Exposes the same at()/allowed()/empty()
// interface as CostMatrix, and bakes in "prepared" semantics (forbidden ->
// BIG, negated if maximize) at construction time, matching what
// prepare_for_solve() does for a dense CostMatrix -- so solver bodies that
// only call .at()/.allowed() work unmodified when templated on this type.
#pragma once

#include "lap_types.h"
#include <vector>
#include <cstdint>
#include <cmath>
#include <algorithm>

namespace lap {

enum class DistanceMetric {
    Euclidean,
    Manhattan,
    SquaredEuclidean,
    Chebyshev,
    Mahalanobis
};

// Per-variable caliper: pairs with |left[i,var_index] - right[j,var_index]|
// exceeding threshold are forbidden, independent of the chosen distance
// metric (matches R's apply_calipers(), which checks raw per-variable
// differences regardless of `distance`).
struct CaliperSpec {
    int64_t var_index;
    double threshold;
};

class LazyCostMatrix {
public:
    int64_t nrow = 0;  // public, matching CostMatrix's convention (solvers
    int64_t ncol = 0;  // read work.nrow/work.ncol directly)

    // left_rowmajor: nrow * n_vars, row-major (row i = feature vector for left unit i)
    // right_rowmajor: ncol * n_vars, row-major
    // inv_cov: n_vars * n_vars, row-major; only read when metric == Mahalanobis
    // max_distance: Inf when unconstrained
    LazyCostMatrix(std::vector<double> left_rowmajor,
                   std::vector<double> right_rowmajor,
                   int64_t n_vars,
                   DistanceMetric metric,
                   std::vector<double> inv_cov,
                   double max_distance,
                   std::vector<CaliperSpec> calipers,
                   bool negate)
        : nrow(static_cast<int64_t>(left_rowmajor.size()) / (n_vars > 0 ? n_vars : 1))
        , ncol(static_cast<int64_t>(right_rowmajor.size()) / (n_vars > 0 ? n_vars : 1))
        , left_(std::move(left_rowmajor))
        , right_(std::move(right_rowmajor))
        , n_vars_(n_vars)
        , metric_(metric)
        , inv_cov_(std::move(inv_cov))
        , max_distance_(max_distance)
        , calipers_(std::move(calipers))
        , negate_(negate) {}

    double at(int64_t i, int64_t j) const {
        if (!allowed(i, j)) return BIG;
        double d = raw_distance(i, j);
        return negate_ ? -d : d;
    }

    bool allowed(int64_t i, int64_t j) const {
        const double* li = &left_[static_cast<size_t>(i * n_vars_)];
        const double* rj = &right_[static_cast<size_t>(j * n_vars_)];
        for (const auto& cal : calipers_) {
            double diff = std::abs(li[cal.var_index] - rj[cal.var_index]);
            if (diff > cal.threshold) return false;
        }
        if (std::isfinite(max_distance_)) {
            if (raw_distance(i, j) > max_distance_) return false;
        }
        return true;
    }

    bool empty() const { return nrow == 0 || ncol == 0; }

    // Whether at() reports negated distances (maximize mode). Callers that
    // need the true, unnegated distance for a chosen (allowed) pair -- e.g.
    // to report a "total cost" using original costs, matching the dense
    // CostMatrix convention of reporting from the pre-negation matrix --
    // can undo the sign via this flag rather than needing a second,
    // un-negated copy of the source.
    bool is_negated() const { return negate_; }

private:
    double raw_distance(int64_t i, int64_t j) const {
        const double* li = &left_[static_cast<size_t>(i * n_vars_)];
        const double* rj = &right_[static_cast<size_t>(j * n_vars_)];

        switch (metric_) {
            case DistanceMetric::Euclidean: {
                double s = 0.0;
                for (int64_t k = 0; k < n_vars_; ++k) {
                    double d = li[k] - rj[k];
                    s += d * d;
                }
                return std::sqrt(s);
            }
            case DistanceMetric::Manhattan: {
                double s = 0.0;
                for (int64_t k = 0; k < n_vars_; ++k) {
                    s += std::abs(li[k] - rj[k]);
                }
                return s;
            }
            case DistanceMetric::SquaredEuclidean: {
                double s = 0.0;
                for (int64_t k = 0; k < n_vars_; ++k) {
                    double d = li[k] - rj[k];
                    s += d * d;
                }
                return s;
            }
            case DistanceMetric::Chebyshev: {
                double s = 0.0;
                for (int64_t k = 0; k < n_vars_; ++k) {
                    s = std::max(s, std::abs(li[k] - rj[k]));
                }
                return s;
            }
            case DistanceMetric::Mahalanobis: {
                // diff' * inv_cov * diff, inv_cov row-major n_vars x n_vars
                double s = 0.0;
                for (int64_t a = 0; a < n_vars_; ++a) {
                    double diff_a = li[a] - rj[a];
                    if (diff_a == 0.0) continue;
                    double row_sum = 0.0;
                    const double* inv_row = &inv_cov_[static_cast<size_t>(a * n_vars_)];
                    for (int64_t b = 0; b < n_vars_; ++b) {
                        row_sum += inv_row[b] * (li[b] - rj[b]);
                    }
                    s += diff_a * row_sum;
                }
                return std::sqrt(std::max(s, 0.0));
            }
        }
        return 0.0;  // unreachable
    }

    std::vector<double> left_;
    std::vector<double> right_;
    int64_t n_vars_;
    DistanceMetric metric_;
    std::vector<double> inv_cov_;
    double max_distance_;
    std::vector<CaliperSpec> calipers_;
    bool negate_;
};

}  // namespace lap
