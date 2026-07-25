// src/core/lap_types.h
// Pure C++ types for LAP solvers - NO Rcpp dependencies
#pragma once

#include <vector>
#include <string>
#include <limits>
#include <cmath>
#include <cstdint>

namespace lap {

// Constants
constexpr double BIG = 1e100;   // Used for forbidden edges
constexpr double TOL = 1e-12;   // Tolerance for zero comparisons

// Flat row-major index, computed in 64-bit to avoid overflow past
// nrow*ncol > INT_MAX (~46,341 square) which plain `int` arithmetic
// silently wraps (undefined behavior) on matrices the package's own
// vignettes already reach (n=50,000).
inline int64_t flat_index(int64_t i, int64_t j, int64_t ncol) {
    return i * ncol + j;
}

// Cost matrix: row-major flat vector + dimensions
struct CostMatrix {
    std::vector<double> data;   // row-major, size = nrow * ncol
    std::vector<int> mask;      // 1=allowed, 0=forbidden, size = nrow * ncol
    int64_t nrow = 0;
    int64_t ncol = 0;

    CostMatrix() = default;

    CostMatrix(int64_t rows, int64_t cols)
        : data(static_cast<size_t>(rows * cols), 0.0)
        , mask(static_cast<size_t>(rows * cols), 1)
        , nrow(rows)
        , ncol(cols) {}

    CostMatrix(const std::vector<std::vector<double>>& mat) {
        if (mat.empty()) {
            nrow = ncol = 0;
            return;
        }
        nrow = static_cast<int64_t>(mat.size());
        ncol = static_cast<int64_t>(mat[0].size());
        data.resize(static_cast<size_t>(nrow * ncol));
        mask.resize(static_cast<size_t>(nrow * ncol), 1);
        for (int64_t i = 0; i < nrow; ++i) {
            for (int64_t j = 0; j < ncol; ++j) {
                double v = mat[static_cast<size_t>(i)][static_cast<size_t>(j)];
                data[static_cast<size_t>(flat_index(i, j, ncol))] = v;
                mask[static_cast<size_t>(flat_index(i, j, ncol))] = std::isfinite(v) ? 1 : 0;
            }
        }
    }

    // Element access
    double& at(int64_t i, int64_t j) { return data[static_cast<size_t>(flat_index(i, j, ncol))]; }
    double at(int64_t i, int64_t j) const { return data[static_cast<size_t>(flat_index(i, j, ncol))]; }

    // Check if edge is allowed (finite cost)
    bool allowed(int64_t i, int64_t j) const { return mask[static_cast<size_t>(flat_index(i, j, ncol))] != 0; }

    // Set edge as forbidden
    void forbid(int64_t i, int64_t j) {
        mask[static_cast<size_t>(flat_index(i, j, ncol))] = 0;
        data[static_cast<size_t>(flat_index(i, j, ncol))] = BIG;
    }

    // Check if empty
    bool empty() const { return nrow == 0 || ncol == 0; }
};

// Result of a single LAP solution
struct LapResult {
    std::vector<int> assignment;  // 0-based column indices, -1 = unmatched
    double total_cost = 0.0;
    std::string status = "optimal";

    LapResult() = default;

    LapResult(std::vector<int> assign, double cost, std::string stat = "optimal")
        : assignment(std::move(assign))
        , total_cost(cost)
        , status(std::move(stat)) {}

    // Check if row i is matched
    bool is_matched(int i) const {
        return i >= 0 && i < static_cast<int>(assignment.size()) &&
               assignment[i] >= 0;
    }

    // Number of matched rows
    int n_matched() const {
        int count = 0;
        for (int j : assignment) {
            if (j >= 0) ++count;
        }
        return count;
    }
};

// Result for k-best solutions
struct KBestResult {
    std::vector<LapResult> solutions;
    std::string status = "optimal";

    bool empty() const { return solutions.empty(); }
    size_t size() const { return solutions.size(); }
};

// Result with dual variables (for sensitivity analysis)
struct DualResult {
    LapResult solution;
    std::vector<double> u;  // row potentials
    std::vector<double> v;  // column potentials
};

}  // namespace lap
