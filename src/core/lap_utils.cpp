// src/core/lap_utils.cpp
// Pure C++ utility functions for LAP solvers - NO Rcpp dependencies

#include "lap_utils.h"
#include <sstream>
#include <cmath>
#include <algorithm>

namespace lap {

std::string match_to_key(const std::vector<int>& match) {
    std::ostringstream os;
    for (size_t i = 0; i < match.size(); ++i) {
        if (i) os << ',';
        os << match[i];
    }
    return os.str();
}

void build_allowed(const std::vector<int>& mask, int64_t n, int64_t m,
                   std::vector<int64_t>& row_ptr, std::vector<int>& cols) {
    row_ptr.assign(static_cast<size_t>(n + 1), 0);

    // Count allowed entries per row
    // Note: mask uses 0=forbidden, nonzero=allowed
    for (int64_t i = 0; i < n; ++i) {
        for (int64_t j = 0; j < m; ++j) {
            if (mask[static_cast<size_t>(flat_index(i, j, m))] != 0) ++row_ptr[static_cast<size_t>(i + 1)];
        }
    }

    // Prefix sum
    for (int64_t i = 1; i <= n; ++i) {
        row_ptr[static_cast<size_t>(i)] += row_ptr[static_cast<size_t>(i - 1)];
    }

    // Fill column indices
    cols.assign(static_cast<size_t>(row_ptr.back()), -1);
    std::vector<int64_t> fill = row_ptr;
    for (int64_t i = 0; i < n; ++i) {
        for (int64_t j = 0; j < m; ++j) {
            if (mask[static_cast<size_t>(flat_index(i, j, m))] != 0) {
                cols[static_cast<size_t>(fill[static_cast<size_t>(i)]++)] = static_cast<int>(j);
            }
        }
    }
}

void ensure_each_row_has_option(const std::vector<int>& mask, int64_t n, int64_t m) {
    for (int64_t i = 0; i < n; ++i) {
        bool has_option = false;
        for (int64_t j = 0; j < m; ++j) {
            if (mask[static_cast<size_t>(flat_index(i, j, m))] != 0) {
                has_option = true;
                break;
            }
        }
        if (!has_option) {
            throw InfeasibleException("Infeasible: row " + std::to_string(i + 1) +
                                     " has no allowed edges");
        }
    }
}

bool is_feasible(const CostMatrix& cost) {
    if (cost.empty()) return false;
    if (cost.nrow > cost.ncol) return false;

    for (int64_t i = 0; i < cost.nrow; ++i) {
        bool has_finite = false;
        for (int64_t j = 0; j < cost.ncol; ++j) {
            if (cost.allowed(i, j) && std::isfinite(cost.at(i, j))) {
                has_finite = true;
                break;
            }
        }
        if (!has_finite) return false;
    }
    return true;
}

bool is_valid_matching(const CostMatrix& cost, const std::vector<int>& match) {
    for (int64_t i = 0; i < cost.nrow && i < static_cast<int64_t>(match.size()); ++i) {
        int j = match[static_cast<size_t>(i)];  // 0-based
        if (j < 0 || j >= cost.ncol) continue;  // Skip unmatched
        if (!cost.allowed(i, j) || !std::isfinite(cost.at(i, j))) {
            return false;  // Forbidden edge was chosen
        }
    }
    return true;
}

// Helper for has_valid_matching: DFS to find augmenting path
static bool dfs_augment(int u, const std::vector<std::vector<int>>& adj,
                        std::vector<int>& match_v, std::vector<bool>& visited) {
    for (int v : adj[u]) {
        if (visited[v]) continue;
        visited[v] = true;
        if (match_v[v] < 0 || dfs_augment(match_v[v], adj, match_v, visited)) {
            match_v[v] = u;
            return true;
        }
    }
    return false;
}

bool has_valid_matching(const CostMatrix& cost) {
    // DFS-augmenting-path feasibility check: already O(n*m) with per-row DFS
    // overhead, impractical well before n/m individually approach INT_MAX, so
    // truncation here is intentional (unlike the flat-index sites above, which
    // overflow at a realistic ~46,341-square scale).
    const int n = static_cast<int>(cost.nrow);
    const int m = static_cast<int>(cost.ncol);

    if (n == 0) return true;
    if (n > m) return false;

    // Build adjacency list (only allowed/finite edges)
    std::vector<std::vector<int>> adj(n);
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            if (cost.allowed(i, j) && std::isfinite(cost.at(i, j))) {
                adj[i].push_back(j);
            }
        }
        if (adj[i].empty()) return false;
    }

    // Find maximum matching
    std::vector<int> match_v(m, -1);
    int matched = 0;

    for (int u = 0; u < n; ++u) {
        std::vector<bool> visited(m, false);
        if (dfs_augment(u, adj, match_v, visited)) {
            ++matched;
        }
    }

    return matched == n;
}

double compute_total_cost(const CostMatrix& cost, const std::vector<int>& match) {
    double total = 0.0;

    for (int64_t i = 0; i < static_cast<int64_t>(match.size()) && i < cost.nrow; ++i) {
        int j = match[static_cast<size_t>(i)];  // 0-based
        if (j < 0 || j >= cost.ncol) continue;  // Skip unmatched or dummy

        double c = cost.at(i, j);
        if (std::isfinite(c)) {
            total += c;
        }
    }

    return total;
}

double compute_total_cost(const CostMatrix& original_cost,
                          const CostMatrix& /*work_cost*/,
                          const std::vector<int>& match) {
    // Always use original cost for reporting
    return compute_total_cost(original_cost, match);
}

CostMatrix negate_costs(const CostMatrix& cost) {
    CostMatrix result = cost;
    for (size_t i = 0; i < result.data.size(); ++i) {
        if (std::isfinite(result.data[i])) {
            result.data[i] = -result.data[i];
        }
    }
    return result;
}

CostMatrix prepare_for_solve(const CostMatrix& cost, bool maximize) {
    CostMatrix result = maximize ? negate_costs(cost) : cost;

    // Ensure forbidden entries are BIG
    for (int64_t i = 0; i < result.nrow; ++i) {
        for (int64_t j = 0; j < result.ncol; ++j) {
            if (!result.allowed(i, j)) {
                result.at(i, j) = BIG;
            }
        }
    }

    return result;
}

std::vector<int> to_one_based(const std::vector<int>& match) {
    std::vector<int> result(match.size());
    for (size_t i = 0; i < match.size(); ++i) {
        result[i] = (match[i] >= 0) ? (match[i] + 1) : 0;
    }
    return result;
}

std::vector<int> to_zero_based(const std::vector<int>& match) {
    std::vector<int> result(match.size());
    for (size_t i = 0; i < match.size(); ++i) {
        result[i] = (match[i] > 0) ? (match[i] - 1) : -1;
    }
    return result;
}

}  // namespace lap
