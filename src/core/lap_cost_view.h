// src/core/lap_cost_view.h
// Zero-copy decorator that fakes rectangular-to-square padding over any
// cost source exposing at()/allowed()/nrow/ncol, so a solver's padding step
// doesn't need a dense materialization for a lazy source the way it does
// for CostMatrix. Rows n0..n0_padded-1 are dummy rows: allowed everywhere,
// cost = dummy_cost (already sign-adjusted for maximize by the caller).
#pragma once

#include <cstdint>

namespace lap {

template <typename Base>
class PaddedCostView {
public:
    int64_t nrow;  // padded row count (== ncol, square)
    int64_t ncol;  // == base_.ncol, unchanged

    PaddedCostView(const Base& base, int64_t n0, double dummy_cost)
        : nrow(base.ncol)
        , ncol(base.ncol)
        , base_(base)
        , n0_(n0)
        , dummy_cost_(dummy_cost) {}

    double at(int64_t i, int64_t j) const {
        return (i < n0_) ? base_.at(i, j) : dummy_cost_;
    }

    bool allowed(int64_t i, int64_t j) const {
        return (i < n0_) ? base_.allowed(i, j) : true;
    }

    bool empty() const { return nrow == 0 || ncol == 0; }

private:
    const Base& base_;
    int64_t n0_;
    double dummy_cost_;
};

}  // namespace lap
