// src/solvers/solve_bottleneck.cpp
// Bottleneck Assignment Problem (BAP) solver
// Minimizes the MAXIMUM edge cost in a perfect matching (minimax objective)
//
// Algorithm:
// 1. Collect all unique finite costs and sort them
// 2. Binary search on threshold T
// 3. For each T, check if perfect matching exists using edges with cost <= T
// 4. Use Hopcroft-Karp as the bipartite matching subroutine
//
// Complexity: O(E√V log(unique costs)) where E = edges, V = vertices

#include <Rcpp.h>
#include <vector>
#include <queue>
#include <algorithm>
#include <limits>
#include <cmath>
#include "../core/lap_internal.h"
#include "../core/lap_utils.h"

using namespace Rcpp;

namespace {

constexpr double INF = std::numeric_limits<double>::infinity();

// Hopcroft-Karp for bipartite matching
// Returns size of maximum matching
class HopcroftKarp {
public:
    int nL, nR;
    std::vector<std::vector<int>> adj;
    std::vector<int> pairL, pairR, dist;

    HopcroftKarp(int left, int right)
        : nL(left), nR(right), adj(left),
          pairL(left, -1), pairR(right, -1), dist(left) {}

    void add_edge(int u, int v) {
        adj[u].push_back(v);
    }

    void clear_edges() {
        for (auto& a : adj) a.clear();
        std::fill(pairL.begin(), pairL.end(), -1);
        std::fill(pairR.begin(), pairR.end(), -1);
    }

    bool bfs() {
        std::queue<int> q;
        for (int u = 0; u < nL; ++u) {
            if (pairL[u] < 0) {
                dist[u] = 0;
                q.push(u);
            } else {
                dist[u] = -1;
            }
        }

        bool found = false;
        while (!q.empty()) {
            int u = q.front();
            q.pop();

            for (int v : adj[u]) {
                int w = pairR[v];
                if (w < 0) {
                    found = true;
                } else if (dist[w] < 0) {
                    dist[w] = dist[u] + 1;
                    q.push(w);
                }
            }
        }
        return found;
    }

    bool dfs(int u) {
        for (int v : adj[u]) {
            int w = pairR[v];
            if (w < 0 || (dist[w] == dist[u] + 1 && dfs(w))) {
                pairL[u] = v;
                pairR[v] = u;
                return true;
            }
        }
        dist[u] = -1;
        return false;
    }

    int max_matching() {
        int matching = 0;
        while (bfs()) {
            for (int u = 0; u < nL; ++u) {
                if (pairL[u] < 0 && dfs(u)) {
                    matching++;
                }
            }
        }
        return matching;
    }

    // Get current matching as vector (left -> right, -1 if unmatched)
    std::vector<int> get_matching() const {
        return pairL;
    }
};

} // namespace

// Main Bottleneck Assignment solver
Rcpp::List solve_bottleneck_impl(NumericMatrix cost, bool maximize) {
    const int n = cost.nrow();
    const int m = cost.ncol();

    if (n == 0) {
        return make_result(IntegerVector(), 0.0);
    }

    if (n > m) {
        stop("Infeasible: rows (%d) > cols (%d)", n, m);
    }

    // Collect all unique finite costs
    std::vector<double> unique_costs;
    unique_costs.reserve(n * m);

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            double c = cost(i, j);
            if (R_finite(c) && !NumericVector::is_na(c)) {
                unique_costs.push_back(c);
            }
        }
    }

    if (unique_costs.empty()) {
        stop("Infeasible: no finite costs in matrix");
    }

    // Sort and deduplicate
    std::sort(unique_costs.begin(), unique_costs.end());
    unique_costs.erase(std::unique(unique_costs.begin(), unique_costs.end()),
                       unique_costs.end());

    // For maximize: we want to maximize the minimum edge cost
    // Transform: negate costs, find minimum bottleneck, negate back
    // Or equivalently: binary search from high to low
    if (maximize) {
        std::reverse(unique_costs.begin(), unique_costs.end());
    }

    // Binary search for minimum threshold that allows perfect matching
    HopcroftKarp hk(n, m);

    auto can_match = [&](double threshold) -> bool {
        hk.clear_edges();

        for (int i = 0; i < n; ++i) {
            for (int j = 0; j < m; ++j) {
                double c = cost(i, j);
                if (!R_finite(c) || NumericVector::is_na(c)) continue;

                if (maximize) {
                    // For maximize: include edges with cost >= threshold
                    if (c >= threshold) {
                        hk.add_edge(i, j);
                    }
                } else {
                    // For minimize: include edges with cost <= threshold
                    if (c <= threshold) {
                        hk.add_edge(i, j);
                    }
                }
            }
        }

        return hk.max_matching() == n;
    };

    // Binary search on unique_costs
    int lo = 0, hi = unique_costs.size() - 1;
    int best = -1;

    // First check if any matching is possible
    if (!can_match(unique_costs[hi])) {
        stop("Infeasible: no perfect matching possible");
    }

    while (lo <= hi) {
        int mid = lo + (hi - lo) / 2;
        if (can_match(unique_costs[mid])) {
            best = mid;
            hi = mid - 1;  // Try to find smaller threshold
        } else {
            lo = mid + 1;
        }
    }

    if (best < 0) {
        stop("Infeasible: no perfect matching found");
    }

    double bottleneck = unique_costs[best];

    // Reconstruct the matching at the optimal threshold
    can_match(bottleneck);  // This sets up hk with the matching
    std::vector<int> matching = hk.get_matching();

    // Convert to 1-based R output
    IntegerVector match(n);
    for (int i = 0; i < n; ++i) {
        match[i] = matching[i] + 1;  // 0-based to 1-based
    }

    // For bottleneck, we return the bottleneck value as total_cost
    // (This is different from LAP where total_cost is the sum)
    return make_result(match, bottleneck);
}
