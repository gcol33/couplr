#ifndef SOLVE_FULL_MATCHING_H
#define SOLVE_FULL_MATCHING_H

#include <vector>
#include <string>

namespace lap {

struct FullMatchResult {
    std::vector<int> right_to_group;  // For each right unit j: which left unit (0-based), -1 if unmatched
    std::vector<int> left_to_group;   // For each left unit i: group ID (0-based), -1 if unmatched
    double total_cost;
    std::string status;               // "optimal" or "infeasible"
    int n_groups;
};

// Solve full matching via min-cost max-flow.
// cost: n_left x n_right distance matrix (Inf = forbidden)
// min_controls: minimum right units per group (>= 1)
// max_controls_val: maximum right units per group (INT_MAX for unlimited)
FullMatchResult solve_full_matching(
    const std::vector<double>& cost,
    int n_left,
    int n_right,
    int min_controls,
    int max_controls_val
);

}  // namespace lap

#endif  // SOLVE_FULL_MATCHING_H
