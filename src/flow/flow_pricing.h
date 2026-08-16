// src/flow/flow_pricing.h
// Block pricing: which omitted pairs a restricted master would want.
// Pure C++ - NO Rcpp dependencies, same rule as lap_types.h.
//
// A restricted master solves over the candidate set and returns duals. Those
// duals price every pair the master did not hold: cbar_ij = c_ij - u_i - v_j,
// and a pair with cbar_ij < -tol is one the master would have used had it been
// offered. When no omitted pair prices below -tol the duals are feasible for
// the complete implicit problem and the restricted answer is optimal for it.
//
// Two things are asked of one pass, and both are answered here:
//
//   - the minimum reduced cost over the omitted pairs, which is the
//     termination test;
//   - the `keep_per_row` most negative per row, which is what the next round
//     adds to the candidate set.
//
// Only omitted pairs are priced. A candidate pair that became an arc is priced
// at or above zero by the master's own optimality, and a candidate pair the
// source forbids is not admissible at all, so neither can be the violator this
// is looking for.
//
// This is the pricer that works for every cost source. It is O(nrow * ncol)
// arithmetic and makes no assumption about the cost function beyond the source
// concept, which is what makes it both the fallback for metrics no bound can
// prune and the reference every pruned result is asserted against.
//
// It templates on the concrete source rather than reaching through CostOracle.
// The master pays one virtual call per arc at expansion time and can afford it;
// this loop runs over every omitted pair and cannot.
//
// The scan is row-outer and stores nothing: the source is read one pair at a
// time and the reduced cost is consumed on the spot. Column blocking, which
// would read a block of `v` and of the source's column data once per block
// rather than once per row, was written against this order and measured at
// five widths on four shapes by dev_notes/phase3/c4_timing.cpp. It is worth
// nothing on any of them, to the millisecond, because both arrays are walked
// in order and the prefetcher already covers them while the distance
// arithmetic dominates.
#pragma once

#include "../core/lap_cost_source.h"
#include "../core/lap_error.h"
#include "flow_candidates.h"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <string>
#include <utility>
#include <vector>

namespace lap {

// An omitted pair and the reduced cost it priced at.
struct PricedPair {
    int32_t i = -1;
    int32_t j = -1;
    double  cbar = 0.0;
};

struct BlockPricing {
    // The kept violators, ascending by (i, j). At most keep_per_row per row,
    // so this is a subset of the pairs n_violators counts.
    std::vector<PricedPair> violators;

    // Per row, the minimum reduced cost over that row's omitted admissible
    // pairs; infinite where the row has none. A row's entry is what a pruning
    // pricer has to reproduce, and reproducing only the global minimum would
    // hide a bug that drops one row's violators.
    std::vector<double> row_min;

    double  min_reduced_cost = std::numeric_limits<double>::infinity();

    // Omitted admissible pairs with cbar < -tol. Zero is the termination test.
    int64_t n_violators = 0;

    // Omitted pairs considered, admissible or not: the O(nrow * ncol) work.
    int64_t n_scanned = 0;

    // Omitted pairs whose cost was computed. This is what edges_evaluated
    // reports, and it is the quantity a tree pricer drives down.
    int64_t n_evaluated = 0;
};

namespace detail {

// The `keep` most negative reduced costs per row, held as one flat max-heap
// per row inside a single allocation. A vector of heaps would allocate once
// per row, and a pricing round touches every row.
class RowViolators {
public:
    RowViolators(int64_t nrow, int keep)
        : keep_(keep > 0 ? keep : 0)
        , count_(static_cast<std::size_t>(nrow > 0 ? nrow : 0), 0) {
        if (keep_ > 0 && nrow > 0) {
            heap_.resize(static_cast<std::size_t>(nrow) * static_cast<std::size_t>(keep_));
        }
    }

    void offer(int64_t i, double cbar, int32_t j) {
        if (keep_ == 0) return;
        const std::ptrdiff_t off =
            static_cast<std::ptrdiff_t>(i) * static_cast<std::ptrdiff_t>(keep_);
        const auto beg = heap_.begin() + off;
        int32_t& n = count_[static_cast<std::size_t>(i)];
        if (n < keep_) {
            beg[n] = Entry(cbar, j);
            ++n;
            std::push_heap(beg, beg + n);
        } else if (cbar < beg->first) {
            std::pop_heap(beg, beg + n);
            beg[n - 1] = Entry(cbar, j);
            std::push_heap(beg, beg + n);
        }
    }

    // Append every row's kept pairs, rows ascending and columns ascending
    // within a row. Reorders the heaps, so it runs once at the end.
    void emit(std::vector<PricedPair>& out) {
        if (keep_ == 0) return;
        for (std::size_t i = 0; i < count_.size(); ++i) {
            const int32_t n = count_[i];
            if (n == 0) continue;
            const auto beg = heap_.begin() +
                static_cast<std::ptrdiff_t>(i) * static_cast<std::ptrdiff_t>(keep_);
            std::sort(beg, beg + n, [](const Entry& a, const Entry& b) {
                return a.second < b.second;
            });
            for (int32_t t = 0; t < n; ++t) {
                out.push_back(PricedPair{static_cast<int32_t>(i), beg[t].second,
                                         beg[t].first});
            }
        }
    }

private:
    using Entry = std::pair<double, int32_t>;  // max-heap on cbar

    int32_t keep_ = 0;
    std::vector<Entry> heap_;
    std::vector<int32_t> count_;
};

}  // namespace detail

// Price every pair the candidate set omits, against duals `u` (length
// src.nrow) and `v` (length src.ncol).
//
// `keep_per_row` bounds how many violators a row contributes; 0 counts them
// and keeps none, which is the termination test on its own.
//
// A length or dimension mismatch throws rather than returning an empty result.
// An empty result reads as "nothing prices below zero", which is the signal to
// stop pricing and declare the answer optimal, so a mismatch that returned one
// would end the loop on a wrong answer.
template <class Source>
BlockPricing price_block(const Source& src,
                         const std::vector<double>& u,
                         const std::vector<double>& v,
                         CandidateSet& cand,
                         int keep_per_row,
                         double tol) {
    const int64_t nrow = src.nrow;
    const int64_t ncol = src.ncol;

    if (cand.nrow() != nrow || cand.ncol() != ncol) {
        LAP_THROW_DIMENSION("price_block: candidate set is " +
                            std::to_string(cand.nrow()) + " x " +
                            std::to_string(cand.ncol()) + ", source is " +
                            std::to_string(nrow) + " x " + std::to_string(ncol));
    }
    if (static_cast<int64_t>(u.size()) != nrow) {
        LAP_THROW_DIMENSION("price_block: " + std::to_string(u.size()) +
                            " row duals for " + std::to_string(nrow) + " rows");
    }
    if (static_cast<int64_t>(v.size()) != ncol) {
        LAP_THROW_DIMENSION("price_block: " + std::to_string(v.size()) +
                            " column duals for " + std::to_string(ncol) + " columns");
    }

    BlockPricing out;
    out.row_min.assign(static_cast<std::size_t>(nrow > 0 ? nrow : 0),
                       std::numeric_limits<double>::infinity());
    if (nrow <= 0 || ncol <= 0) return out;

    detail::RowViolators keep(nrow, keep_per_row);

    for (int64_t i = 0; i < nrow; ++i) {
        const int32_t* rb = cand.row_begin(i);
        const int64_t  rn = cand.row_size(i);
        const double ui = u[static_cast<std::size_t>(i)];
        double rmin = std::numeric_limits<double>::infinity();

        // The row's candidates are ascending and j advances by one, so the
        // membership test is a single cursor rather than a binary search per
        // pair: once a candidate is passed, the next one is still ahead of j.
        int64_t cur = 0;

        for (int64_t j = 0; j < ncol; ++j) {
            if (cur < rn && rb[cur] == j) {
                ++cur;
                continue;
            }
            ++out.n_scanned;
            // Forbidden arcs carry BIG, not Inf, so a source that reports the
            // pair inadmissible must not have its cost read. Both questions go
            // in one call, which is what keeps a source that computes its costs
            // from evaluating the same pair three times.
            double c = 0.0;
            if (!cost_if_allowed(src, i, j, c)) continue;
            ++out.n_evaluated;
            const double cbar = c - ui - v[static_cast<std::size_t>(j)];
            if (cbar < rmin) rmin = cbar;
            if (cbar < -tol) {
                ++out.n_violators;
                keep.offer(i, cbar, static_cast<int32_t>(j));
            }
        }

        out.row_min[static_cast<std::size_t>(i)] = rmin;
        if (rmin < out.min_reduced_cost) out.min_reduced_cost = rmin;
    }

    keep.emit(out.violators);
    cand.note_evaluated(out.n_evaluated);
    return out;
}

// The violators as the pairs add_pairs() takes, in the order it returns them.
inline std::vector<CandidateSet::Pair>
violator_pairs(const std::vector<PricedPair>& violators) {
    std::vector<CandidateSet::Pair> pairs;
    pairs.reserve(violators.size());
    for (const PricedPair& p : violators) pairs.emplace_back(p.i, p.j);
    return pairs;
}

}  // namespace lap
