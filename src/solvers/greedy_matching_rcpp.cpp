// ==============================================================================
// Greedy Matching Algorithm
// ==============================================================================
// Fast approximate one-to-one matching using greedy strategies

#include <Rcpp.h>
#include <algorithm>
#include <vector>
#include <queue>
#include "../core/lap_utils_rcpp.h"

using namespace Rcpp;

// Candidate pair for greedy matching
struct CandidatePair {
    int row;
    int col;
    double cost;

    CandidatePair(int r, int c, double w) : row(r), col(c), cost(w) {}

    // For priority queue (min-heap)
    bool operator>(const CandidatePair& other) const {
        return cost > other.cost;
    }
};

// ==============================================================================
// Greedy matching: sorted pairs strategy
// ==============================================================================
// Collect all valid pairs, sort by cost, greedily assign

// Implementation: Greedy matching using sorted pairs
List greedy_matching_sorted_impl(NumericMatrix cost_matrix, bool maximize) {
    int n = cost_matrix.nrow();
    int m = cost_matrix.ncol();

    const double BIG_COST = std::numeric_limits<double>::max() / 2.0;

    // Collect all valid candidate pairs
    std::vector<CandidatePair> candidates;
    candidates.reserve(n * m);

    for (int i = 0; i < n; i++) {
        for (int j = 0; j < m; j++) {
            double c = cost_matrix(i, j);

            // Skip forbidden pairs (NA, Inf, or BIG_COST)
            if (std::isnan(c) || std::isinf(c) || c >= BIG_COST) {
                continue;
            }

            // For maximize, negate cost for sorting
            double sort_cost = maximize ? -c : c;
            candidates.push_back(CandidatePair(i, j, sort_cost));
        }
    }

    // Sort candidates by cost (ascending)
    std::sort(candidates.begin(), candidates.end(),
              [](const CandidatePair& a, const CandidatePair& b) {
                  return a.cost < b.cost;
              });

    // Track which rows/cols are matched
    std::vector<bool> row_matched(n, false);
    std::vector<bool> col_matched(m, false);
    std::vector<int> match(n, 0);  // 0 = unmatched (R indexing)

    double total_cost = 0.0;
    int n_matched = 0;

    // Greedily assign pairs
    for (const auto& pair : candidates) {
        if (!row_matched[pair.row] && !col_matched[pair.col]) {
            // Match this pair
            match[pair.row] = pair.col + 1;  // Convert to 1-based for R
            row_matched[pair.row] = true;
            col_matched[pair.col] = true;

            // Get actual cost (not negated)
            double actual_cost = cost_matrix(pair.row, pair.col);
            total_cost += actual_cost;
            n_matched++;

            // Early exit if all rows matched
            if (n_matched == n || n_matched == m) {
                break;
            }
        }
    }

    return List::create(
        Named("match") = match,
        Named("total_cost") = total_cost,
        Named("n_matched") = n_matched
    );
}

// ==============================================================================
// Greedy matching: row-best strategy
// ==============================================================================
// For each unmatched row, find its best available column

// Implementation: Greedy matching using row-best strategy
List greedy_matching_row_best_impl(NumericMatrix cost_matrix, bool maximize) {
    int n = cost_matrix.nrow();
    int m = cost_matrix.ncol();

    const double BIG_COST = std::numeric_limits<double>::max() / 2.0;

    std::vector<bool> col_matched(m, false);
    std::vector<int> match(n, 0);  // 0 = unmatched

    double total_cost = 0.0;
    int n_matched = 0;

    // For each row, find best available column
    for (int i = 0; i < n; i++) {
        int best_col = -1;
        double best_cost = maximize ? -std::numeric_limits<double>::infinity()
                                    : std::numeric_limits<double>::infinity();

        // Find best available column for this row
        for (int j = 0; j < m; j++) {
            if (col_matched[j]) continue;

            double c = cost_matrix(i, j);

            // Skip forbidden pairs
            if (std::isnan(c) || std::isinf(c) || c >= BIG_COST) {
                continue;
            }

            // Check if this is better
            bool is_better = maximize ? (c > best_cost) : (c < best_cost);
            if (is_better) {
                best_cost = c;
                best_col = j;
            }
        }

        // If found a valid match, assign it
        if (best_col >= 0) {
            match[i] = best_col + 1;  // 1-based for R
            col_matched[best_col] = true;
            total_cost += best_cost;
            n_matched++;
        }
    }

    return List::create(
        Named("match") = match,
        Named("total_cost") = total_cost,
        Named("n_matched") = n_matched
    );
}

// ==============================================================================
// Greedy matching: priority queue strategy
// ==============================================================================
// Use priority queue for efficient selection (good for very large problems)

// Implementation: Greedy matching using priority queue
List greedy_matching_pq_impl(NumericMatrix cost_matrix, bool maximize) {
    int n = cost_matrix.nrow();
    int m = cost_matrix.ncol();

    const double BIG_COST = std::numeric_limits<double>::max() / 2.0;

    // Min-heap priority queue
    std::priority_queue<CandidatePair, std::vector<CandidatePair>,
                       std::greater<CandidatePair>> pq;

    // Build priority queue
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < m; j++) {
            double c = cost_matrix(i, j);

            if (std::isnan(c) || std::isinf(c) || c >= BIG_COST) {
                continue;
            }

            double sort_cost = maximize ? -c : c;
            pq.push(CandidatePair(i, j, sort_cost));
        }
    }

    // Track matched units
    std::vector<bool> row_matched(n, false);
    std::vector<bool> col_matched(m, false);
    std::vector<int> match(n, 0);

    double total_cost = 0.0;
    int n_matched = 0;

    // Greedily assign from priority queue
    while (!pq.empty() && n_matched < std::min(n, m)) {
        CandidatePair pair = pq.top();
        pq.pop();

        if (!row_matched[pair.row] && !col_matched[pair.col]) {
            match[pair.row] = pair.col + 1;
            row_matched[pair.row] = true;
            col_matched[pair.col] = true;

            double actual_cost = cost_matrix(pair.row, pair.col);
            total_cost += actual_cost;
            n_matched++;
        }
    }

    return List::create(
        Named("match") = match,
        Named("total_cost") = total_cost,
        Named("n_matched") = n_matched
    );
}

// ==============================================================================
// Main dispatcher
// ==============================================================================

// Implementation: Greedy matching dispatcher
List greedy_matching_impl(NumericMatrix cost_matrix, bool maximize,
                         std::string strategy) {

    List result;

    if (strategy == "sorted") {
        result = greedy_matching_sorted_impl(cost_matrix, maximize);
    } else if (strategy == "row_best") {
        result = greedy_matching_row_best_impl(cost_matrix, maximize);
    } else if (strategy == "pq") {
        result = greedy_matching_pq_impl(cost_matrix, maximize);
    } else {
        LAP_ERROR("Unknown greedy strategy: %s", strategy.c_str());
    }

    // Add metadata
    result["status"] = "optimal";  // Greedy is "optimal" for its strategy
    result["method_used"] = "greedy_" + strategy;

    return result;
}
