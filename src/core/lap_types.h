// src/core/lap_types.h
// Pure C++ types for LAP solvers - NO Rcpp dependencies
#pragma once

#include <vector>
#include <string>
#include <limits>
#include <cmath>

namespace lap {

// Constants
constexpr double BIG = 1e100;   // Used for forbidden edges
constexpr double TOL = 1e-12;   // Tolerance for zero comparisons

// Cost matrix: row-major flat vector + dimensions
struct CostMatrix {
    std::vector<double> data;   // row-major, size = nrow * ncol
    std::vector<int> mask;      // 1=allowed, 0=forbidden, size = nrow * ncol
    int nrow = 0;
    int ncol = 0;

    CostMatrix() = default;

    CostMatrix(int rows, int cols)
        : data(rows * cols, 0.0)
        , mask(rows * cols, 1)
        , nrow(rows)
        , ncol(cols) {}

    CostMatrix(const std::vector<std::vector<double>>& mat) {
        if (mat.empty()) {
            nrow = ncol = 0;
            return;
        }
        nrow = static_cast<int>(mat.size());
        ncol = static_cast<int>(mat[0].size());
        data.resize(nrow * ncol);
        mask.resize(nrow * ncol, 1);
        for (int i = 0; i < nrow; ++i) {
            for (int j = 0; j < ncol; ++j) {
                double v = mat[i][j];
                data[i * ncol + j] = v;
                mask[i * ncol + j] = std::isfinite(v) ? 1 : 0;
            }
        }
    }

    // Element access
    double& at(int i, int j) { return data[i * ncol + j]; }
    double at(int i, int j) const { return data[i * ncol + j]; }

    // Check if edge is allowed (finite cost)
    bool allowed(int i, int j) const { return mask[i * ncol + j] != 0; }

    // Set edge as forbidden
    void forbid(int i, int j) {
        mask[i * ncol + j] = 0;
        data[i * ncol + j] = BIG;
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
