// src/solvers/solve_ramshaw_tarjan.cpp
// Ramshaw-Tarjan Algorithm for Rectangular Assignment Problems
// Reference: Ramshaw & Tarjan (2012) "On minimum-cost assignments in
//            unbalanced bipartite graphs"
// Complexity: O(nm + n² log n) for m >> n

#include <Rcpp.h>
#include <vector>
#include <queue>
#include <limits>
#include <algorithm>
#include <cmath>
#include "../core/lap_utils.h"

using namespace Rcpp;

// Ramshaw-Tarjan specializes in rectangular problems (n rows, m cols, n < m)
// Key insight: Instead of padding to square, use a modified Hungarian that
// maintains n row potentials and only tracks "active" column potentials.
//
// The algorithm:
// 1. Initialize row potentials u[i] = min_j c[i][j]
// 2. For each unmatched row, find augmenting path via Dijkstra on reduced costs
// 3. Update potentials along the path
// 4. Augment matching
//
// This avoids O(m) dummy operations when m >> n.

Rcpp::List solve_ramshaw_tarjan_impl(NumericMatrix cost, bool maximize) {
  const int n0 = cost.nrow();
  const int m0 = cost.ncol();

  if (n0 == 0 || m0 == 0) {
    return make_result(std::vector<int>(), 0.0);
  }

  // Ensure rows <= cols
  bool transposed = false;
  NumericMatrix C = cost;
  int n = n0, m = m0;

  if (n0 > m0) {
    C = transpose(cost);
    n = m0;
    m = n0;
    transposed = true;
  }

  // Build cost matrix (negate for maximize)
  std::vector<std::vector<double>> W(n, std::vector<double>(m));
  std::vector<std::vector<bool>> allowed(n, std::vector<bool>(m, false));
  bool has_finite = false;

  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < m; ++j) {
      double v = C(i, j);
      if (std::isfinite(v)) {
        has_finite = true;
        W[i][j] = maximize ? -v : v;
        allowed[i][j] = true;
      } else {
        W[i][j] = BIG;
        allowed[i][j] = false;
      }
    }
  }

  if (!has_finite) {
    stop("No finite costs found.");
  }

  // Check feasibility: each row must have at least one allowed column
  for (int i = 0; i < n; ++i) {
    bool has_option = false;
    for (int j = 0; j < m; ++j) {
      if (allowed[i][j]) {
        has_option = true;
        break;
      }
    }
    if (!has_option) {
      stop("Infeasible: row %d has no valid assignments", i + 1);
    }
  }

  // Dual potentials
  std::vector<double> u(n, 0.0);  // Row potentials
  std::vector<double> v(m, 0.0);  // Column potentials

  // Initialize row potentials: u[i] = min_j W[i][j]
  for (int i = 0; i < n; ++i) {
    double min_cost = BIG;
    for (int j = 0; j < m; ++j) {
      if (allowed[i][j] && W[i][j] < min_cost) {
        min_cost = W[i][j];
      }
    }
    u[i] = min_cost;
  }

  // Matching arrays
  std::vector<int> row_to_col(n, -1);
  std::vector<int> col_to_row(m, -1);

  // Reduced cost
  auto reduced_cost = [&](int i, int j) -> double {
    return W[i][j] - u[i] - v[j];
  };

  // Augment matching one row at a time using Dijkstra
  for (int start_row = 0; start_row < n; ++start_row) {
    // Dijkstra to find shortest augmenting path from start_row
    std::vector<double> dist(m, std::numeric_limits<double>::infinity());
    std::vector<int> prev_col(m, -1);   // Previous column in path
    std::vector<int> prev_row(m, -1);   // Row that led to this column
    std::vector<bool> col_visited(m, false);

    // Priority queue: (distance, column)
    std::priority_queue<std::pair<double, int>,
                        std::vector<std::pair<double, int>>,
                        std::greater<std::pair<double, int>>> pq;

    // Initialize: edges from start_row to all columns
    for (int j = 0; j < m; ++j) {
      if (allowed[start_row][j]) {
        double rc = reduced_cost(start_row, j);
        if (rc < dist[j]) {
          dist[j] = rc;
          prev_row[j] = start_row;
          pq.push({dist[j], j});
        }
      }
    }

    int end_col = -1;  // Unmatched column we find

    while (!pq.empty()) {
      auto [d, j] = pq.top();
      pq.pop();

      if (col_visited[j]) continue;
      col_visited[j] = true;

      // If this column is unmatched, we found augmenting path
      if (col_to_row[j] < 0) {
        end_col = j;
        break;
      }

      // Otherwise, extend through the matched row
      int matched_row = col_to_row[j];
      for (int jj = 0; jj < m; ++jj) {
        if (!col_visited[jj] && allowed[matched_row][jj]) {
          double rc = reduced_cost(matched_row, jj);
          double new_dist = dist[j] + rc;
          if (new_dist < dist[jj]) {
            dist[jj] = new_dist;
            prev_col[jj] = j;
            prev_row[jj] = matched_row;
            pq.push({new_dist, jj});
          }
        }
      }
    }

    if (end_col < 0) {
      stop("Infeasible: could not find augmenting path for row %d", start_row + 1);
    }

    // Update potentials for visited columns
    // This maintains reduced cost non-negativity
    double delta = dist[end_col];
    u[start_row] += delta;

    for (int j = 0; j < m; ++j) {
      if (col_visited[j]) {
        v[j] -= (delta - dist[j]);
        if (col_to_row[j] >= 0) {
          u[col_to_row[j]] += (delta - dist[j]);
        }
      }
    }

    // Augment along the path
    int cur_col = end_col;
    int cur_row = prev_row[cur_col];

    while (cur_col >= 0) {
      int old_col = (cur_row == start_row) ? -1 : prev_col[cur_col];
      int old_match = col_to_row[cur_col];

      // Make assignment
      row_to_col[cur_row] = cur_col;
      col_to_row[cur_col] = cur_row;

      if (old_col < 0) break;

      cur_col = old_col;
      cur_row = prev_row[cur_col];
    }
  }

  // Build output
  std::vector<int> match_out;
  double total = 0.0;

  if (!transposed) {
    match_out.resize(n);
    for (int i = 0; i < n; ++i) {
      int j = row_to_col[i];
      match_out[i] = (j >= 0) ? (j + 1) : 0;
      if (j >= 0 && j < m0) {
        double val = cost(i, j);
        if (std::isfinite(val)) {
          total += val;
        }
      }
    }
  } else {
    match_out.assign(n0, 0);
    for (int i = 0; i < n; ++i) {
      int j = row_to_col[i];
      if (j >= 0 && j < n0) {
        match_out[j] = i + 1;
        double val = cost(j, i);
        if (std::isfinite(val)) {
          total += val;
        }
      }
    }
  }

  return make_result(match_out, total);
}
