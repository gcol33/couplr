// src/solvers/solve_lapmod.cpp
// LAPMOD: Sparse Linear Assignment Problem solver
// Based on: Jonker & Volgenant (1987) "A Shortest Augmenting Path Algorithm"
//
// This implementation uses CSR format to exploit sparsity.
// For matrices with >50% NA/Inf entries and n>100, this is faster than dense JV.

#include <Rcpp.h>
#include <vector>
#include <limits>
#include <algorithm>
#include <cmath>
#include "../core/lap_internal.h"
#include "../core/lap_utils.h"

using namespace Rcpp;

namespace {

constexpr int UNASSIGNED = -1;
constexpr double INF = std::numeric_limits<double>::infinity();

struct SparseMatrix {
    int n, m;
    std::vector<int> row_ptr;
    std::vector<int> col_idx;
    std::vector<double> values;

    int row_size(int i) const { return row_ptr[i + 1] - row_ptr[i]; }
    int col(int i, int k) const { return col_idx[row_ptr[i] + k]; }
    double cost(int i, int k) const { return values[row_ptr[i] + k]; }
};

SparseMatrix build_sparse(const NumericMatrix& cost, bool maximize) {
    SparseMatrix sp;
    sp.n = cost.nrow();
    sp.m = cost.ncol();
    sp.row_ptr.resize(sp.n + 1);

    double cmax = R_NegInf;
    if (maximize) {
        for (int i = 0; i < sp.n; ++i)
            for (int j = 0; j < sp.m; ++j) {
                double c = cost(i, j);
                if (R_finite(c) && c > cmax) cmax = c;
            }
    }

    sp.row_ptr[0] = 0;
    for (int i = 0; i < sp.n; ++i) {
        int count = 0;
        for (int j = 0; j < sp.m; ++j) {
            double c = cost(i, j);
            if (R_finite(c) && !NumericVector::is_na(c)) count++;
        }
        sp.row_ptr[i + 1] = sp.row_ptr[i] + count;
    }

    sp.col_idx.resize(sp.row_ptr[sp.n]);
    sp.values.resize(sp.row_ptr[sp.n]);

    std::vector<int> pos(sp.n, 0);
    for (int i = 0; i < sp.n; ++i) {
        for (int j = 0; j < sp.m; ++j) {
            double c = cost(i, j);
            if (R_finite(c) && !NumericVector::is_na(c)) {
                int idx = sp.row_ptr[i] + pos[i];
                sp.col_idx[idx] = j;
                sp.values[idx] = maximize ? (cmax - c) : c;
                pos[i]++;
            }
        }
    }

    return sp;
}

// Shortest augmenting path from free_row using Dijkstra
bool augment(const SparseMatrix& sp, int free_row,
             std::vector<int>& x, std::vector<int>& y,
             std::vector<double>& u, std::vector<double>& v) {
    const int m = sp.m;

    std::vector<double> d(m, INF);
    std::vector<int> pred(m, UNASSIGNED);
    std::vector<bool> scanned(m, false);

    // Initialize from free_row
    for (int k = 0; k < sp.row_size(free_row); ++k) {
        int j = sp.col(free_row, k);
        double red = sp.cost(free_row, k) - u[free_row] - v[j];
        if (red < d[j]) {
            d[j] = red;
            pred[j] = -2;
        }
    }

    int sink = UNASSIGNED;
    double min_len = INF;

    while (true) {
        int j_min = UNASSIGNED;
        double d_min = INF;
        for (int j = 0; j < m; ++j) {
            if (!scanned[j] && d[j] < d_min) {
                d_min = d[j];
                j_min = j;
            }
        }

        if (j_min == UNASSIGNED || d_min == INF) break;

        scanned[j_min] = true;

        if (y[j_min] == UNASSIGNED) {
            sink = j_min;
            min_len = d_min;
            break;
        }

        int i = y[j_min];
        for (int k = 0; k < sp.row_size(i); ++k) {
            int j = sp.col(i, k);
            if (scanned[j]) continue;
            double new_d = d_min + sp.cost(i, k) - u[i] - v[j];
            if (new_d < d[j]) {
                d[j] = new_d;
                pred[j] = j_min;
            }
        }
    }

    if (sink == UNASSIGNED) return false;

    // Update duals
    u[free_row] += min_len;
    for (int j = 0; j < m; ++j) {
        if (scanned[j] && d[j] < INF) {
            v[j] += d[j] - min_len;
            if (y[j] != UNASSIGNED) {
                u[y[j]] -= d[j] - min_len;
            }
        }
    }

    // Augment path
    int j = sink;
    while (pred[j] != -2) {
        int pj = pred[j];
        int i = y[pj];
        y[j] = i;
        std::swap(x[i], j);
        j = pj;
    }
    y[j] = free_row;
    x[free_row] = j;

    return true;
}

} // namespace

Rcpp::List solve_lapmod_impl(NumericMatrix cost, bool maximize) {
    const int n = cost.nrow();
    const int m = cost.ncol();

    if (n == 0) return make_result(IntegerVector(), 0.0);
    if (n > m) stop("Infeasible: rows (%d) > cols (%d)", n, m);

    SparseMatrix sp = build_sparse(cost, maximize);

    for (int i = 0; i < n; ++i) {
        if (sp.row_size(i) == 0) {
            stop("Infeasible: row %d has no finite costs", i + 1);
        }
    }

    std::vector<int> x(n, UNASSIGNED);
    std::vector<int> y(m, UNASSIGNED);
    std::vector<double> u(n, 0.0);
    std::vector<double> v(m, 0.0);

    // Skip column reduction - let shortest augmenting paths handle everything
    // This is simpler and produces optimal results
    // Column reduction is an optimization that can speed things up but
    // the sequential augmentation approach works correctly without it

    // Augment for each row
    for (int i = 0; i < n; ++i) {
        if (!augment(sp, i, x, y, u, v)) {
            stop("Infeasible: cannot match row %d", i + 1);
        }
    }

    IntegerVector match(n);
    for (int i = 0; i < n; ++i) match[i] = x[i] + 1;

    double total = compute_total_cost(cost, match);
    return make_result(match, total);
}
