// utils_gabow_tarjan.cpp
// Module A: Cost-length & 1-feasibility utilities for Gabow-Tarjan algorithm
// PATCHED VERSION with diagnostic error reporting

#include "utils_gabow_tarjan.h"
#include <Rcpp.h>
#include <set>
#include <utility>
#include <queue>
#include <map>
#include <algorithm>
#include <stdexcept>
#include <iostream>  // For std::cerr diagnostic output

// ============================================================================
// Module A: Cost-length & 1-feasibility utilities
// ============================================================================

/**
 * Compute cost-length cl(e) for edge with cost c_ij
 * 
 * @param c_ij Edge cost
 * @param in_matching Whether edge is in the matching
 * @return cl(e) = c(e) if in matching, c(e) + 1 otherwise
 */
long long cost_length(long long c_ij, bool in_matching) {
    return in_matching ? c_ij : (c_ij + 1);
}

/**
 * Check if an edge is eligible (tight in the 1-feasibility constraint)
 * 
 * @param c_ij Edge cost
 * @param in_matching Whether edge is in the matching
 * @param yu Dual variable for row vertex
 * @param yv Dual variable for column vertex
 * @return true if yu + yv == cl(e)
 */
bool is_eligible(long long c_ij, bool in_matching, 
                 long long yu, long long yv) {
    return yu + yv == cost_length(c_ij, in_matching);
}

/**
 * Check 1-feasibility conditions for a matching and duals
 * 
 * Verifies:
 * 1. For all finite edges (i,j): y_u[i] + y_v[j] <= c(i,j) + 1
 * 2. For matched edges (i,j): y_u[i] + y_v[j] >= c(i,j)
 * 
 * @param cost Cost matrix (BIG_INT indicates forbidden edge)
 * @param row_match Matching from rows (row_match[i] = j or NIL)
 * @param col_match Matching from columns (col_match[j] = i or NIL)
 * @param y_u Dual variables for rows
 * @param y_v Dual variables for columns
 * @return true if all 1-feasibility conditions satisfied
 */
bool check_one_feasible(const CostMatrix& cost,
                        const MatchVec& row_match,
                        const MatchVec& col_match,
                        const DualVec& y_u,
                        const DualVec& y_v) {
    int n = static_cast<int>(cost.size());
    if (n == 0) return true;
    
    int m = static_cast<int>(cost[0].size());
    
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            long long c_ij = cost[i][j];
            
            // Skip forbidden edges
            if (c_ij >= BIG_INT) continue;
            
            long long sum_duals = y_u[i] + y_v[j];
            bool matched = (row_match[i] == j && col_match[j] == i);
            
            // Condition 1: y_u[i] + y_v[j] <= c(i,j) + 1
            if (sum_duals > c_ij + 1) {
                return false;
            }
            
            // Condition 2: for matched edges, y_u[i] + y_v[j] >= c(i,j)
            if (matched && sum_duals < c_ij) {
                return false;
            }
        }
    }
    
    return true;
}

// ============================================================================
// Module B: Equality graph construction
// ============================================================================

/**
 * Build equality graph (eligible edges) as adjacency lists
 * 
 * For each row i, returns list of columns j where edge (i,j) is eligible,
 * meaning y_u[i] + y_v[j] == cl(i,j)
 * 
 * @param cost Cost matrix (BIG_INT indicates forbidden edge)
 * @param row_match Matching from rows (row_match[i] = j or NIL)
 * @param y_u Dual variables for rows
 * @param y_v Dual variables for columns
 * @return eq_graph[i] = list of eligible column indices for row i
 */
std::vector<std::vector<int>>
build_equality_graph(const CostMatrix& cost,
                     const MatchVec& row_match,
                     const DualVec& y_u,
                     const DualVec& y_v)
{
    const int n = static_cast<int>(cost.size());
    const int m = n > 0 ? static_cast<int>(cost[0].size()) : 0;

    std::vector<std::vector<int>> eq_graph(n);

    for (int i = 0; i < n; ++i) {
        eq_graph[i].clear();
        for (int j = 0; j < m; ++j) {
            long long c_ij = cost[i][j];
            
            // Skip forbidden edges
            if (c_ij >= BIG_INT) {
                continue;
            }
            
            bool in_matching = (row_match[i] == j);
            if (is_eligible(c_ij, in_matching, y_u[i], y_v[j])) {
                eq_graph[i].push_back(j);
            }
        }
    }
    
    return eq_graph;
}

/**
 * Incrementally update equality graph after dual variable changes
 *
 * After Step 1, only columns on augmenting paths have y_v decreased.
 * This function updates the equality graph by checking only the affected
 * columns against all rows, achieving O(|affected_cols| × n) complexity
 * instead of O(nm).
 *
 * @param eq_graph Existing equality graph to update (modified in place)
 * @param cost Cost matrix
 * @param row_match Current matching
 * @param y_u Dual variables for rows
 * @param y_v Dual variables for columns (updated since last graph build)
 * @param affected_cols List of column indices whose y_v decreased
 */
void update_equality_graph_incremental(std::vector<std::vector<int>>& eq_graph,
                                        const CostMatrix& cost,
                                        const MatchVec& row_match,
                                        const DualVec& y_u,
                                        const DualVec& y_v,
                                        const std::vector<int>& affected_cols)
{
    const int n = static_cast<int>(cost.size());
    const int m = n > 0 ? static_cast<int>(cost[0].size()) : 0;

    // For each affected column, check all rows for new eligible edges
    for (int j : affected_cols) {
        if (j < 0 || j >= m) continue;

        for (int i = 0; i < n; ++i) {
            long long c_ij = cost[i][j];

            // Skip forbidden edges
            if (c_ij >= BIG_INT) {
                continue;
            }

            bool in_matching = (row_match[i] == j);
            bool is_elig = is_eligible(c_ij, in_matching, y_u[i], y_v[j]);

            // Check if (i,j) is already in eq_graph[i]
            auto& adj_list = eq_graph[i];
            auto it = std::find(adj_list.begin(), adj_list.end(), j);
            bool already_present = (it != adj_list.end());

            if (is_elig && !already_present) {
                // Edge became eligible: add it
                adj_list.push_back(j);
            } else if (!is_elig && already_present) {
                // Edge no longer eligible: remove it
                adj_list.erase(it);
            }
        }
    }
}

// ============================================================================
// Module C: Augment matching along a path
// ============================================================================

/**
 * Apply symmetric difference of current matching M with edge set P (path edges)
 * 
 * Given an augmenting path represented as a list of edges, updates the matching
 * by taking M' = M Δ P (symmetric difference). This flips matched/unmatched
 * status of all edges in the path.
 * 
 * @param edges List of (row, col) pairs forming the augmenting path
 * @param row_match Matching from rows (modified in place)
 * @param col_match Matching from columns (modified in place)
 */
void augment_along_path(const std::vector<std::pair<int,int>>& edges,
                        MatchVec& row_match,
                        MatchVec& col_match)
{
    // OPTIMIZED: Direct in-place flip - O(path_length) instead of O(n)
    // Paper (page 6): "Augmenting along P means M ← M ⊕ P"
    // This is just flipping matched/unmatched status along the path

    for (const auto& edge : edges) {
        int i = edge.first;
        int j = edge.second;
        row_match[i] = j;
        col_match[j] = i;
    }
}

// ============================================================================
// Module D: Maximal set of augmenting paths on equality graph
// ============================================================================

struct ParentInfo {
    char prev_side;  // 'r' for row, 'c' for col, or 0 for root
    int  prev_idx;
    int  edge_row;
    int  edge_col;
};

/**
 * Find ONE augmenting path in the equality graph using BFS
 * 
 * Uses BFS on the residual graph (alternating unmatched/matched edges)
 * to find an augmenting path from a free row to a free column.
 * 
 * @param eq_graph Equality graph (adjacency lists of eligible edges)
 * @param row_match Current matching from rows
 * @param col_match Current matching from columns
 * @param banned_row Rows to exclude from search
 * @param banned_col Columns to exclude from search
 * @return List of edges forming augmenting path, or empty if none found
 */
std::vector<std::pair<int,int>>
find_one_augmenting_path_eq(const std::vector<std::vector<int>>& eq_graph,
                            const MatchVec& row_match,
                            const MatchVec& col_match,
                            const std::vector<bool>& banned_row,
                            const std::vector<bool>& banned_col)
{
    const int n = static_cast<int>(eq_graph.size());
    
    // Determine number of columns
    int m = 0;
    for (const auto& nbrs : eq_graph) {
        for (int j : nbrs) {
            if (j + 1 > m) m = j + 1;
        }
    }
    
    std::vector<bool> visited_row(n, false);
    std::vector<bool> visited_col(m, false);
    
    // Parent map keyed by (side, index)
    using Key = std::pair<char, int>;
    std::map<Key, ParentInfo> parent;
    
    std::queue<Key> q;
    
    // Initialize BFS from all free, non-banned rows
    for (int i = 0; i < n; ++i) {
        if (row_match[i] == NIL && !banned_row[i]) {
            visited_row[i] = true;
            parent[{'r', i}] = {0, -1, -1, -1};  // root
            q.push({'r', i});
        }
    }
    
    while (!q.empty()) {
        auto [side, idx] = q.front();
        q.pop();
        
        if (side == 'r') {
            int i = idx;
            for (int j : eq_graph[i]) {
                if (j >= m) continue;  // defensive
                if (banned_col[j] || visited_col[j]) continue;
                
                visited_col[j] = true;
                parent[{'c', j}] = {'r', i, i, j};
                
                // Free column: augmenting path found
                if (col_match[j] == NIL && !banned_col[j]) {
                    std::vector<std::pair<int,int>> edges;
                    Key cur = {'c', j};
                    
                    while (true) {
                        auto it = parent.find(cur);
                        if (it == parent.end()) break;
                        const ParentInfo& p = it->second;
                        if (p.prev_side == 0) {
                            break;  // root
                        }
                        edges.emplace_back(p.edge_row, p.edge_col);
                        cur = {p.prev_side, p.prev_idx};
                    }
                    
                    std::reverse(edges.begin(), edges.end());
                    return edges;
                }
                
                // Otherwise follow matched edge j->i2
                int i2 = col_match[j];
                if (i2 != NIL && !banned_row[i2] && !visited_row[i2]) {
                    visited_row[i2] = true;
                    parent[{'r', i2}] = {'c', j, i2, j};
                    q.push({'r', i2});
                }
            }
        }
        // We never push 'c' nodes directly into the queue
    }
    
    // No path found
    return {};
}

/**
 * Find maximal set of vertex-disjoint augmenting paths
 * 
 * Repeatedly finds augmenting paths and marks vertices as banned
 * to ensure paths are vertex-disjoint.
 * 
 * @param eq_graph Equality graph (adjacency lists of eligible edges)
 * @param row_match Current matching from rows
 * @param col_match Current matching from columns
 * @return List of paths, where each path is a list of edges
 */
std::vector<std::vector<std::pair<int,int>>>
find_maximal_augmenting_paths(const std::vector<std::vector<int>>& eq_graph,
                              const MatchVec& row_match,
                              const MatchVec& col_match)
{
    // OPTIMIZED: Single DFS pass - O(m) instead of O(sqrt(n) * m)
    // Paper approach: Find ALL maximal paths in ONE traversal (like Hopcroft-Karp)

    const int n = static_cast<int>(eq_graph.size());

    // Determine number of columns
    int m = 0;
    for (const auto& nbrs : eq_graph) {
        for (int j : nbrs) {
            if (j + 1 > m) m = j + 1;
        }
    }

    std::vector<bool> visited_row(n, false);
    std::vector<bool> visited_col(m, false);
    std::vector<std::pair<int,int>> current_path;
    std::vector<std::vector<std::pair<int,int>>> all_paths;

    // DFS from a single row to find an augmenting path
    std::function<bool(int)> dfs = [&](int i) -> bool {
        if (visited_row[i]) return false;
        visited_row[i] = true;

        // Try all eligible edges from row i
        for (int j : eq_graph[i]) {
            if (visited_col[j]) continue;

            visited_col[j] = true;
            current_path.emplace_back(i, j);

            // If j is free, we found an augmenting path!
            if (col_match[j] == NIL) {
                all_paths.push_back(current_path);
                // Don't clear path yet - keep it for potential reuse
                return true;
            }

            // Otherwise, follow matched edge to continue DFS
            int i2 = col_match[j];
            if (i2 != NIL && !visited_row[i2]) {
                if (dfs(i2)) {
                    return true;
                }
            }

            // Backtrack
            current_path.pop_back();
            // Don't unmark visited_col[j] - ensures vertex-disjoint paths
        }

        return false;
    };

    // Try DFS from each free row
    for (int i = 0; i < n; ++i) {
        if (row_match[i] == NIL && !visited_row[i]) {
            current_path.clear();
            if (dfs(i)) {
                // Successfully found a path
                // visited arrays ensure next search finds vertex-disjoint path
            }
        }
    }

    return all_paths;
}

// ============================================================================
// Module E: Hungarian-style search on cost-length (Step 2 core)
// ============================================================================

/**
 * Build cost-length matrix from cost matrix and current matching
 * 
 * @param cost Original cost matrix
 * @param row_match Current matching from rows
 * @return Cost-length matrix where cl(i,j) = c(i,j) if (i,j) matched, c(i,j)+1 otherwise
 */
CostMatrix build_cl_matrix(const CostMatrix& cost,
                           const MatchVec& row_match)
{
    const int n = static_cast<int>(cost.size());
    const int m = n > 0 ? static_cast<int>(cost[0].size()) : 0;
    
    CostMatrix C_cl(n, std::vector<long long>(m));
    
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            long long c_ij = cost[i][j];
            
            // Forbidden edges remain forbidden
            if (c_ij >= BIG_INT) {
                C_cl[i][j] = BIG_INT;
            } else {
                bool in_matching = (row_match[i] == j);
                C_cl[i][j] = cost_length(c_ij, in_matching);
            }
        }
    }
    
    return C_cl;
}

/**
 * Hungarian/Dijkstra search on cost-length matrix
 * 
 * Performs a Hungarian search to find an augmenting path, maintaining
 * 1-feasibility. Updates duals and matching if a path is found.
 * 
 * @param C_cl Cost-length matrix
 * @param row_match Current matching from rows (modified in place)
 * @param col_match Current matching from columns (modified in place)
 * @param y_u Dual variables for rows (modified in place)
 * @param y_v Dual variables for columns (modified in place)
 * @return true if augmenting path found and applied, false otherwise
 */
/**
 * Hungarian/Dijkstra search on cost-length matrix
 * 
 * Performs a Hungarian search to find an augmenting path, maintaining
 * 1-feasibility. Updates duals and matching if a path is found.
 * 
 * @param C_cl Cost-length matrix
 * @param row_match Current matching from rows (modified in place)
 * @param col_match Current matching from columns (modified in place)
 * @param y_u Dual variables for rows (modified in place)
 * @param y_v Dual variables for columns (modified in place)
 * @return true if augmenting path found and applied, false otherwise
 */
bool hungarian_search_cl(const CostMatrix& C_cl,
                         MatchVec& row_match,
                         MatchVec& col_match,
                         DualVec& y_u,
                         DualVec& y_v)
{
    const int n = static_cast<int>(C_cl.size());
    const int m = n > 0 ? static_cast<int>(C_cl[0].size()) : 0;
    if (n == 0 || m == 0) return false;

    // ---------------------------------------------------------------------
    // PRE-STEP: make duals feasible w.r.t. C_cl on matched edges
    // For a matched edge (i,j), if y_u[i] + y_v[j] > C_cl[i][j],
    // decrease y_u[i] so that y_u[i] + y_v[j] == C_cl[i][j].
    // This preserves 1-feasibility w.r.t the original cost c and
    // ensures Hungarian starts from a dual-feasible state on C_cl.
    // ---------------------------------------------------------------------
    for (int i = 0; i < n; ++i) {
        int j = row_match[i];
        if (j == NIL) continue;
        if (j < 0 || j >= m) continue;
        long long ccl = C_cl[i][j];
        if (ccl >= BIG_INT) continue;  // should not happen for a matched edge
        long long sum_duals = y_u[i] + y_v[j];
        if (sum_duals > ccl) {
            long long diff = sum_duals - ccl;  // in a 1-feasible state this is 0 or 1
            y_u[i] -= diff;
        }
    }

    // ---------------------------------------------------------------------
    // Standard Hungarian search on C_cl, starting from a free row
    // This finds exactly one augmenting path and updates (row_match, col_match, y_u, y_v).
    // ---------------------------------------------------------------------

    // Find a free row to start from
    int start_row = -1;
    for (int i = 0; i < n; ++i) {
        if (row_match[i] == NIL) {
            start_row = i;
            break;
        }
    }
    if (start_row == -1) {
        // Matching already perfect
        return false;
    }

    std::vector<bool> in_S(n, false);          // rows in alternating tree
    std::vector<bool> in_T(m, false);          // cols in alternating tree
    std::vector<long long> slack(m, BIG_INT);  // slack[j] = min reduced cost
    std::vector<int> slack_row(m, -1);         // row achieving slack[j]
    std::vector<int> parent(n, -1);            // parent[i] = previous row in path to row i

    // Initialize with the root free row in S
    in_S[start_row] = true;
    for (int j = 0; j < m; ++j) {
        long long reduced = C_cl[start_row][j] - y_u[start_row] - y_v[j];
        slack[j] = reduced;
        slack_row[j] = start_row;
    }

    // Main loop: grow alternating tree until augmenting path found
    while (true) {
        // 1) Find minimum slack among columns not in T
        long long delta = BIG_INT;
        for (int j = 0; j < m; ++j) {
            if (!in_T[j] && slack[j] < delta) {
                delta = slack[j];
            }
        }
        if (delta == BIG_INT) {
            // No augmenting path can be found
            return false;
        }

        // 2) Dual adjustment
        if (delta > 0) {
            for (int i = 0; i < n; ++i) {
                if (in_S[i]) {
                    y_u[i] += delta;
                }
            }
            for (int j = 0; j < m; ++j) {
                if (in_T[j]) {
                    y_v[j] -= delta;
                } else {
                    slack[j] -= delta;
                }
            }
        }

        // 3) Scan ALL columns with zero slack and add them to T
        //    This is critical: after dual adjustment, multiple columns may become tight
        //    We must process all of them before the next iteration
        int j_final = -1;
        bool made_progress = false;  // Track if we added anything to T
        for (int j = 0; j < m; ++j) {
            if (in_T[j] || slack[j] != 0) {
                continue;
            }
            
            // Column j is now tight, add it to T
            int i = slack_row[j];
            in_T[j] = true;
            made_progress = true;  // We added a column to T
            
            // If j is free, we found an augmenting path
            if (col_match[j] == NIL) {
                j_final = j;
                break;  // Found augmenting path, stop scanning
            }
            
            // Otherwise, extend tree via the matched edge (j, matched_row)
            int i2 = col_match[j];
            if (!in_S[i2]) {
                in_S[i2] = true;
                parent[i2] = i;  // parent[row] = previous row that reached it
                
                // Update slack using this new row
                for (int jj = 0; jj < m; ++jj) {
                    if (!in_T[jj]) {
                        long long val = C_cl[i2][jj] - y_u[i2] - y_v[jj];
                        if (val < slack[jj]) {
                            slack[jj] = val;
                            slack_row[jj] = i2;
                        }
                    }
                }
            }
        }
        
        // Safety check: if we didn't make progress, we shouldn't continue
        if (!made_progress && j_final == -1) {
            // This shouldn't happen in a correct implementation
            // but prevents infinite loop
            return false;
        }
        
        // 4) If we found a free column, augment along the path and stop
        if (j_final != -1) {
            // Reconstruct and flip the augmenting path
            int j = j_final;
            int i = slack_row[j];
            while (true) {
                int j_prev = row_match[i];
                row_match[i] = j;
                col_match[j] = i;
                if (i == start_row) {
                    break;
                }
                i = parent[i];
                j = j_prev;
            }
            return true;
        }
    }
}
bool hungarian_step_one_feasible(const CostMatrix& cost,
                                 MatchVec& row_match,
                                 MatchVec& col_match,
                                 DualVec& y_u,
                                 DualVec& y_v)
{
    const int n = static_cast<int>(cost.size());
    if (n == 0) return false;
    const int m = static_cast<int>(cost[0].size());
    if (m == 0) return false;
    
    // -------------------------------------------------------------------------
    // CRITICAL: Dual initialization/repair step
    // If current duals are not 1-feasible for the original costs, we must
    // repair them. Otherwise, cost-length reduced costs can be negative,
    // causing the Hungarian search to fail or hang.
    // -------------------------------------------------------------------------
    
    if (!check_one_feasible(cost, row_match, col_match, y_u, y_v)) {
        // =====================================================================
        // ERROR: Module H produced infeasible input!
        // =====================================================================
        std::cerr << "ERROR: hungarian_step_one_feasible received infeasible input!\n";
        std::cerr << "This indicates Module H is not maintaining 1-feasibility across scales.\n";
        
        // Report violation details
        long long worst_unmatched = 0;
        long long worst_matched_lower = 0;
        long long worst_matched_upper = 0;
        
        for (int i = 0; i < n; ++i) {
            for (int j = 0; j < m; ++j) {
                if (cost[i][j] < BIG_INT) {
                    long long sum_duals = y_u[i] + y_v[j];
                    bool matched = (row_match[i] == j && col_match[j] == i);
                    
                    // Check y_u[i] + y_v[j] <= c[i][j] + 1
                    long long violation_upper = sum_duals - (cost[i][j] + 1);
                    if (violation_upper > 0) {
                        worst_unmatched = std::max(worst_unmatched, violation_upper);
                        if (matched) {
                            worst_matched_upper = std::max(worst_matched_upper, violation_upper);
                        }
                    }
                    
                    // For matched edges: check y_u[i] + y_v[j] >= c[i][j]
                    if (matched) {
                        long long violation_lower = cost[i][j] - sum_duals;
                        if (violation_lower > 0) {
                            worst_matched_lower = std::max(worst_matched_lower, violation_lower);
                        }
                    }
                }
            }
        }
        
        std::cerr << "  Worst violation (y_u+y_v > c+1): " << worst_unmatched << "\n";
        std::cerr << "  Worst violation on matched (y_u+y_v < c): " << worst_matched_lower << "\n";
        std::cerr << "  Worst violation on matched (y_u+y_v > c+1): " << worst_matched_upper << "\n";
        std::cerr << "  Attempting repair...\n";
        
        // =====================================================================
        // REPAIR LOGIC (defensive programming - should not be needed!)
        // =====================================================================
        
        bool matching_empty = true;
        for (int i = 0; i < n; ++i) {
            if (row_match[i] != NIL) {
                matching_empty = false;
                break;
            }
        }
        
        if (matching_empty) {
            // Initialize duals to ensure 1-feasibility for empty matching
            // Set all row duals to 0
            for (int i = 0; i < n; ++i) {
                y_u[i] = 0;
            }
            
            // Set each column dual to min over rows of (c(i,j) + 1)
            for (int j = 0; j < m; ++j) {
                long long min_val = BIG_INT;
                for (int i = 0; i < n; ++i) {
                    long long c_ij = cost[i][j];
                    if (c_ij < BIG_INT) {  // finite edge
                        long long val = c_ij + 1;
                        if (val < min_val) {
                            min_val = val;
                        }
                    }
                }
                y_v[j] = (min_val < BIG_INT) ? min_val : 0;
            }
        } else {
            // Non-empty matching with non-feasible duals
            // Strategy: 
            // 1. First enforce all upper bounds y_u + y_v <= c + 1 by decreasing duals
            // 2. Then enforce matched edge lower bounds y_u + y_v >= c by increasing duals
            // 3. If step 2 created new upper bound violations, iterate
            
            const int MAX_REPAIR_ITERS = 20;
            for (int iter = 0; iter < MAX_REPAIR_ITERS; ++iter) {
                bool any_violation = false;
                
                // Step 1: Fix all upper bound violations
                // For each edge, if y_u[i] + y_v[j] > c[i][j] + 1, decrease one dual
                for (int i = 0; i < n; ++i) {
                    for (int j = 0; j < m; ++j) {
                        long long c_ij = cost[i][j];
                        if (c_ij >= BIG_INT) continue;
                        
                        long long sum_duals = y_u[i] + y_v[j];
                        long long upper_bound = c_ij + 1;
                        
                        if (sum_duals > upper_bound) {
                            any_violation = true;
                            long long excess = sum_duals - upper_bound;
                            
                            // Prefer to decrease column dual for matched edges to preserve
                            // lower bound, otherwise decrease row dual
                            bool is_matched = (row_match[i] == j);
                            if (is_matched && y_v[j] >= excess) {
                                y_v[j] -= excess;
                            } else {
                                y_u[i] -= excess;
                            }
                        }
                    }
                }
                
                // Step 2: Fix lower bound violations for matched edges
                // For matched edge (i,j), ensure y_u[i] + y_v[j] >= c[i][j]
                for (int i = 0; i < n; ++i) {
                    int j = row_match[i];
                    if (j != NIL && j >= 0 && j < m) {
                        long long c_ij = cost[i][j];
                        if (c_ij >= BIG_INT) continue;
                        
                        long long sum_duals = y_u[i] + y_v[j];
                        if (sum_duals < c_ij) {
                            any_violation = true;
                            long long deficit = c_ij - sum_duals;
                            
                            // Increase both duals proportionally to minimize impact on other edges
                            long long half_deficit = deficit / 2;
                            long long remainder = deficit - half_deficit;
                            y_u[i] += half_deficit + remainder;
                            y_v[j] += half_deficit;
                        }
                    }
                }
                
                if (!any_violation) {
                    break;  // Converged
                }
            }
        }
    }
    
    // Build cost-length matrix
    CostMatrix C_cl = build_cl_matrix(cost, row_match);
    
    // Perform Hungarian search
    return hungarian_search_cl(C_cl, row_match, col_match, y_u, y_v);
}

// ============================================================================
// Module F: match_gt - Inner Gabow-Tarjan matching algorithm
// ============================================================================

/**
 * Check if matching is perfect (all rows matched)
 * 
 * @param row_match Current matching from rows
 * @return true if all rows are matched
 */
bool is_perfect(const MatchVec& row_match) {
    for (int j : row_match) {
        if (j == NIL) return false;
    }
    return true;
}

/**
 * Apply Step 1 of Gabow-Tarjan algorithm
 *
 * Finds maximal set of vertex-disjoint augmenting paths on eligible edges
 * and updates matching and duals.
 *
 * Steps:
 * 1. Build equality graph of eligible edges (or use provided one for incremental updates)
 * 2. Find maximal set of vertex-disjoint augmenting paths
 * 3. Augment along each path
 * 4. For each column on any path, decrease y_v[j] by 1
 *
 * @param cost Cost matrix
 * @param row_match Current matching from rows (modified in place)
 * @param col_match Current matching from columns (modified in place)
 * @param y_u Dual variables for rows (modified in place)
 * @param y_v Dual variables for columns (modified in place)
 * @param eq_graph Optional pointer to existing equality graph (for incremental updates)
 * @param affected_cols_out Optional pointer to store list of columns whose y_v decreased
 * @return true if augmenting paths were found, false otherwise
 */
bool apply_step1(const CostMatrix& cost,
                MatchVec& row_match,
                MatchVec& col_match,
                DualVec& y_u,
                DualVec& y_v,
                std::vector<std::vector<int>>* eq_graph,
                std::vector<int>* affected_cols_out)
{
    // 1. Build equality graph of eligible edges (or use provided one)
    std::vector<std::vector<int>> local_eq_graph;
    std::vector<std::vector<int>>& graph_ref = eq_graph ? *eq_graph : local_eq_graph;

    if (!eq_graph) {
        // No existing graph provided: build from scratch
        local_eq_graph = build_equality_graph(cost, row_match, y_u, y_v);
    }

    // 2. Find maximal set of vertex-disjoint augmenting paths
    auto paths = find_maximal_augmenting_paths(graph_ref, row_match, col_match);

    if (paths.empty()) {
        return false;
    }

    // 3. Augment paths individually and track columns used
    // OPTIMIZED: Augment each path individually for efficiency
    // Old approach: collected all edges into a set (O(n log n)), then augmented
    // New approach: augment each path directly (O(total_path_length))
    std::vector<bool> col_used(y_v.size(), false);

    for (const auto& path : paths) {
        // Augment this path directly
        augment_along_path(path, row_match, col_match);

        // Mark columns as used
        for (const auto& e : path) {
            int j = e.second;
            if (j >= 0 && j < static_cast<int>(col_used.size())) {
                col_used[j] = true;
            }
        }
    }

    // 4. Decrease y_v[j] for all columns on any path (V1 vertices in paper)
    // Also collect affected columns for incremental updates
    if (affected_cols_out) {
        affected_cols_out->clear();
    }

    for (int j = 0; j < static_cast<int>(col_used.size()); ++j) {
        if (col_used[j]) {
            y_v[j] -= 1;
            if (affected_cols_out) {
                affected_cols_out->push_back(j);
            }
        }
    }

    return true;
}

/**
 * Gabow-Tarjan inner matching algorithm
 * 
 * Finds a 1-optimal perfect matching by alternating between:
 * - Step 1: Maximal augmenting paths on eligible edges
 * - Step 2: Hungarian search to find one augmenting path
 * 
 * Assumes:
 * - Integer costs with c(i,j) >= -1
 * - A perfect matching exists with cost <= an (where a is small constant)
 * 
 * @param cost Cost matrix (BIG_INT for forbidden edges)
 * @param row_match Matching from rows (modified in place, or initialized)
 * @param col_match Matching from columns (modified in place, or initialized)
 * @param y_u Dual variables for rows (modified in place, or initialized)
 * @param y_v Dual variables for columns (modified in place, or initialized)
 * @param max_iters Maximum iterations to prevent infinite loops
 * @param check_feasible If true, check 1-feasibility at each step (debug only)
 * @throws std::runtime_error if no perfect matching exists or max_iters exceeded
 */
void match_gt(const CostMatrix& cost,
             MatchVec& row_match,
             MatchVec& col_match,
             DualVec& y_u,
             DualVec& y_v,
             int max_iters,
             bool check_feasible)
{
    const int n = static_cast<int>(cost.size());
    const int m = n > 0 ? static_cast<int>(cost[0].size()) : 0;
    
    // Initialize vectors if needed
    if (static_cast<int>(row_match.size()) != n) {
        row_match.assign(n, NIL);
    }
    if (static_cast<int>(col_match.size()) != m) {
        col_match.assign(m, NIL);
    }
    if (static_cast<int>(y_u.size()) != n) {
        y_u.assign(n, 0);
    }
    if (static_cast<int>(y_v.size()) != m) {
        y_v.assign(m, 0);
    }
    
    // =========================================================================
    // NORMALIZATION PHASE: Handle inconsistent matchings and non-feasible duals
    // =========================================================================
    
    // Step 1: Make row_match and col_match consistent
    // Treat row_match as authoritative, rebuild col_match from it
    for (int j = 0; j < m; ++j) {
        col_match[j] = NIL;
    }
    
    for (int i = 0; i < n; ++i) {
        int j = row_match[i];
        if (j != NIL && j >= 0 && j < m) {
            // Check for conflicts (multiple rows trying to match same column)
            if (col_match[j] != NIL) {
                // Conflict: column j already claimed by another row
                // Drop this row's match
                row_match[i] = NIL;
            } else {
                // No conflict: establish the match
                col_match[j] = i;
            }
        } else if (j != NIL) {
            // Invalid column index: clear it
            row_match[i] = NIL;
        }
    }
    
    // Step 2: Check 1-feasibility of current state
    bool is_feasible = check_one_feasible(cost, row_match, col_match, y_u, y_v);
    
    if (!is_feasible) {
        // =====================================================================
        // ERROR: match_gt received infeasible input!
        // =====================================================================
        std::cerr << "ERROR: match_gt received infeasible input!\n";
        std::cerr << "This indicates Module H or scale_match is not properly initializing state.\n";
        
        // Report violation details
        long long worst_unmatched = 0;
        long long worst_matched_lower = 0;
        long long worst_matched_upper = 0;
        int n_violations = 0;
        
        for (int i = 0; i < n; ++i) {
            for (int j = 0; j < m; ++j) {
                if (cost[i][j] < BIG_INT) {
                    long long sum_duals = y_u[i] + y_v[j];
                    bool matched = (row_match[i] == j && col_match[j] == i);
                    
                    // Check y_u[i] + y_v[j] <= c[i][j] + 1
                    long long violation_upper = sum_duals - (cost[i][j] + 1);
                    if (violation_upper > 0) {
                        worst_unmatched = std::max(worst_unmatched, violation_upper);
                        n_violations++;
                        if (matched) {
                            worst_matched_upper = std::max(worst_matched_upper, violation_upper);
                        }
                    }
                    
                    // For matched edges: check y_u[i] + y_v[j] >= c[i][j]
                    if (matched) {
                        long long violation_lower = cost[i][j] - sum_duals;
                        if (violation_lower > 0) {
                            worst_matched_lower = std::max(worst_matched_lower, violation_lower);
                            n_violations++;
                        }
                    }
                }
            }
        }
        
        std::cerr << "  Total violations: " << n_violations << "\n";
        std::cerr << "  Worst violation (y_u+y_v > c+1): " << worst_unmatched << "\n";
        std::cerr << "  Worst violation on matched (y_u+y_v < c): " << worst_matched_lower << "\n";
        std::cerr << "  Worst violation on matched (y_u+y_v > c+1): " << worst_matched_upper << "\n";
        std::cerr << "  Resetting to canonical empty matching...\n";
        
        // =====================================================================
        // RESET LOGIC (defensive programming - should not be needed!)
        // =====================================================================
        
        // State is not 1-feasible: discard the matching and duals,
        // restart from canonical empty matching with 1-feasible duals
        
        // Clear matching
        for (int i = 0; i < n; ++i) {
            row_match[i] = NIL;
        }
        for (int j = 0; j < m; ++j) {
            col_match[j] = NIL;
        }
        
        // Initialize duals canonically for empty matching
        // Row duals: all zero
        for (int i = 0; i < n; ++i) {
            y_u[i] = 0;
        }
        
        // Column duals: y_v[j] = min_i(c(i,j) + 1) over finite edges
        for (int j = 0; j < m; ++j) {
            long long min_val = BIG_INT;
            for (int i = 0; i < n; ++i) {
                long long c_ij = cost[i][j];
                if (c_ij < BIG_INT) {
                    long long val = c_ij + 1;
                    if (val < min_val) {
                        min_val = val;
                    }
                }
            }
            y_v[j] = (min_val < BIG_INT) ? min_val : 0;
        }
    }
    
    // At this point:
    // - row_match and col_match are consistent
    // - The state is 1-feasible
    // - Ready to run the main Gabow-Tarjan loop


    // OPTIMIZATION: Incremental equality graph updates
    // Build initial equality graph once, then update incrementally after each Step 1
    std::vector<std::vector<int>> eq_graph = build_equality_graph(cost, row_match, y_u, y_v);
    std::vector<int> affected_cols;

    int it = 0;
    while (!is_perfect(row_match)) {
        ++it;
        if (it > max_iters) {
            throw std::runtime_error("match_gt exceeded max_iters");
        }

        // Optional: Check 1-feasibility before Step 1 (debug only)
        if (check_feasible) {
            if (!check_one_feasible(cost, row_match, col_match, y_u, y_v)) {
                throw std::runtime_error("1-feasibility violated before Step 1");
            }
        }

        // Step 1: Maximal vertex-disjoint augmenting paths on eligible edges
        // Use incremental updates: pass existing eq_graph and get affected columns
        bool found_paths = apply_step1(cost, row_match, col_match, y_u, y_v,
                                       &eq_graph, &affected_cols);

        // If Step 1 found paths, update the equality graph incrementally
        // (only check rows against columns whose y_v decreased)
        if (found_paths && !affected_cols.empty()) {
            update_equality_graph_incremental(eq_graph, cost, row_match, y_u, y_v, affected_cols);
        }

        // Check if matching is now perfect
        if (is_perfect(row_match)) {
            break;
        }
        
        // Step 2: Only run if Step 1 found no paths
        // If Step 1 found paths, loop back to try Step 1 again
        if (!found_paths) {
            // Hungarian search on cost-length to find one augmenting path
            if (!hungarian_step_one_feasible(cost, row_match, col_match, y_u, y_v)) {
                throw std::runtime_error("No augmenting path in Step 2 (no perfect matching)");
            }

            // After Step 2, duals may have changed significantly: rebuild equality graph
            eq_graph = build_equality_graph(cost, row_match, y_u, y_v);
        }

        // Optional: Check 1-feasibility after Step 2 (debug only)
        if (check_feasible) {
            if (!check_one_feasible(cost, row_match, col_match, y_u, y_v)) {
                throw std::runtime_error("1-feasibility violated after Step 2");
            }
        }
    }
}

// ============================================================================
// Module G: scale_match - Wrapper for bit-scaling outer loop
// ============================================================================

/**
 * Gabow-Tarjan scale_match wrapper for bit-scaling outer loop
 * 
 * This function transforms the cost matrix by subtracting global duals,
 * runs match_gt on the transformed costs to obtain a 1-optimal matching
 * with local duals, then updates the global duals and matching.
 * 
 * Supports rectangular matrices:
 * - If n > m (more rows than cols): pads with dummy columns (cost = BIG_INT)
 * - If n <= m: works directly on the matrix
 * 
 * Algorithm:
 * 1. Build c'(i,j) = c(i,j) - y_u[i] - y_v[j]
 * 2. Run match_gt on c' starting from current matching to get local duals y'
 * 3. Update global duals: y_u[i] += y'_u[i], y_v[j] += y'_v[j]
 * 4. Update global matching to the one found by match_gt
 * 
 * @param cost Original cost matrix (BIG_INT indicates forbidden edge)
 *             Can be n×m with n != m
 * @param row_match Current matching from rows; updated in-place
 * @param col_match Current matching from columns; updated in-place
 * @param y_u Global dual variables for rows; updated in-place
 * @param y_v Global dual variables for columns; updated in-place
 */
void scale_match(const CostMatrix& cost,
                MatchVec& row_match,
                MatchVec& col_match,
                DualVec& y_u,
                DualVec& y_v) {
    const int n = static_cast<int>(cost.size());
    const int m = (n > 0 ? static_cast<int>(cost[0].size()) : 0);
    
    // Ensure dual vectors are properly sized
    if (static_cast<int>(y_u.size()) != n) {
        y_u.resize(n, 0);
    }
    if (static_cast<int>(y_v.size()) != m) {
        y_v.resize(m, 0);
    }
    
    // Handle rectangular matrices by padding if n > m
    // For n <= m, work directly on the original matrix
    int n_work = n;
    int m_work = m;
    
    if (n > m) {
        // Need to pad with dummy columns
        m_work = n;  // Make it square
        
        // Resize dual vectors for padded matrix
        y_v.resize(m_work, 0);
        col_match.resize(m_work, NIL);
    }
    
    // 1. Build c'(i,j) = c(i,j) - y_u[i] - y_v[j]
    //    Forbidden edges (cost >= BIG_INT) remain BIG_INT
    //    Padding columns (j >= m) get BIG_INT
    CostMatrix cost_prime(n_work, std::vector<long long>(m_work, BIG_INT));
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            long long c_ij = cost[i][j];
            if (c_ij >= BIG_INT) {
                // Forbidden edge stays forbidden
                cost_prime[i][j] = BIG_INT;
            } else {
                // Subtract global duals
                cost_prime[i][j] = c_ij - y_u[i] - y_v[j];
            }
        }
        // Dummy columns (if any) stay as BIG_INT
        for (int j = m; j < m_work; ++j) {
            cost_prime[i][j] = BIG_INT;
        }
    }
    
    // 2. Local matching and duals for match_gt on cost_prime
    //    Start with current matching state
    MatchVec row_loc = row_match;
    MatchVec col_loc = col_match;
    
    // Ensure row_loc and col_loc are properly sized for working dimensions
    row_loc.resize(n_work, NIL);
    col_loc.resize(m_work, NIL);
    
    // Local duals start at zero
    DualVec y_u_loc(n_work, 0);
    DualVec y_v_loc(m_work, 0);
    
    // Run inner Gabow-Tarjan solver on transformed costs
    match_gt(cost_prime, row_loc, col_loc, y_u_loc, y_v_loc,
             /*max_iters=*/1000,
             /*check_feasible=*/false);
    
    // 3. Update global duals: y <- y + y'
    for (int i = 0; i < n; ++i) {
        y_u[i] += y_u_loc[i];
    }
    // Only update actual columns (not dummy padding)
    for (int j = 0; j < m; ++j) {
        y_v[j] += y_v_loc[j];
    }
    
    // 4. Update global matching to the result from match_gt
    //    For rectangular matrices, only copy back the actual columns
    row_match = row_loc;  // Full row matching (may include dummy columns)
    
    // For col_match, only keep the actual columns
    col_match.resize(m);
    for (int j = 0; j < m; ++j) {
        col_match[j] = col_loc[j];
    }
    
    // Keep y_v at size m (actual columns only)
    y_v.resize(m);
}

// ============================================================================
// Module H: Gabow-Tarjan bit-scaling outer loop
// ============================================================================

/**
 * Find maximum finite cost in the cost matrix
 * 
 * @param cost Cost matrix (BIG_INT for forbidden edges)
 * @return Maximum finite cost value (0 if all edges are forbidden)
 */
long long find_max_cost(const CostMatrix& cost) {
    long long max_cost = 0;
    for (const auto& row : cost) {
        for (long long c : row) {
            if (c < BIG_INT && c > max_cost) {
                max_cost = c;
            }
        }
    }
    return max_cost;
}

/**
 * Gabow-Tarjan bit-scaling algorithm for minimum cost perfect matching - CORRECTED
 * 
 * CRITICAL FIXES APPLIED:
 * 1. Multiply by (n+1) BEFORE bit-scaling (as per paper)
 * 2. Don't reuse matching across scales - start fresh each time
 * 3. Maintain 1-feasibility strictly throughout
 * 
 * Algorithm:
 * 1. Shift costs to non-negative: c'(e) = c(e) - min_cost
 * 2. Scale by (n+1): ĉ(e) = (n+1) * c'(e)
 * 3. Determine number of bits k for ĉ_max
 * 4. Build costs bit-by-bit from MSB to LSB
 * 5. At each scale s:
 *    a. Update costs: c(e) ← 2c(e) + (bit s of ĉ(e))
 *    b. Update duals: y(v) ← 2y(v) - 1
 *    c. Run scale_match with FRESH matching to get 1-optimal solution
 * 6. Adjust duals back for original costs
 * 
 * The (n+1) scaling is ESSENTIAL: it ensures that a 1-optimal matching for
 * the scaled costs is an optimal matching for the original costs.
 * 
 * @param cost Original cost matrix (BIG_INT for forbidden edges)
 * @param row_match Output: optimal matching from rows (0-based)
 * @param col_match Output: optimal matching from columns (0-based)
 * @param y_u Output: optimal dual variables for rows
 * @param y_v Output: optimal dual variables for columns
 */
void solve_gabow_tarjan_inner(const CostMatrix& cost,
                              MatchVec& row_match,
                              MatchVec& col_match,
                              DualVec& y_u,
                              DualVec& y_v) {
    const int n = static_cast<int>(cost.size());
    const int m = (n > 0 ? static_cast<int>(cost[0].size()) : 0);
    
    if (n == 0 || m == 0) {
        row_match.clear();
        col_match.clear();
        y_u.clear();
        y_v.clear();
        return;
    }
    
    // Step 1: Find minimum cost to shift everything to non-negative
    long long min_cost = 0;
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            if (cost[i][j] < BIG_INT && cost[i][j] < min_cost) {
                min_cost = cost[i][j];
            }
        }
    }
    
    // Step 2: CRITICAL - Multiply by (n+1) as per paper
    // Create ĉ(e) = (n+1) * (c(e) - min_cost)
    CostMatrix scaled_cost(n, std::vector<long long>(m));
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            if (cost[i][j] < BIG_INT) {
                scaled_cost[i][j] = static_cast<long long>(n + 1) * (cost[i][j] - min_cost);
            } else {
                scaled_cost[i][j] = BIG_INT;
            }
        }
    }
    
    // Step 3: Find maximum scaled cost to determine number of bits
    long long C_max = 0;
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            if (scaled_cost[i][j] < BIG_INT && scaled_cost[i][j] > C_max) {
                C_max = scaled_cost[i][j];
            }
        }
    }
    
    if (C_max == 0) {
        // All costs are equal after scaling
        row_match.assign(n, NIL);
        col_match.assign(m, NIL);
        y_u.assign(n, min_cost);
        y_v.assign(m, 0);
        
        // Find any perfect matching
        for (int i = 0; i < n && i < m; ++i) {
            if (cost[i][i] < BIG_INT) {
                row_match[i] = i;
                col_match[i] = i;
            }
        }
        return;
    }
    
    // Step 4: Determine number of bits k
    int k = 0;
    long long temp = C_max;
    while (temp > 0) {
        temp >>= 1;
        ++k;
    }
    
    // Step 5: Initialize current costs to 0 (will build bit by bit)
    CostMatrix c_current(n, std::vector<long long>(m, 0));
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            if (scaled_cost[i][j] >= BIG_INT) {
                c_current[i][j] = BIG_INT;
            }
        }
    }
    
    // Initialize duals to 0
    y_u.assign(n, 0);
    y_v.assign(m, 0);
    
    // Step 6: Bit-scaling loop (from MSB to LSB)
    for (int s = k - 1; s >= 0; --s) {
        // Step 1 of paper: Update costs and duals
        
        // Update costs: c(e) ← 2c(e) + (bit s of scaled_cost)
        for (int i = 0; i < n; ++i) {
            for (int j = 0; j < m; ++j) {
                if (scaled_cost[i][j] < BIG_INT) {
                    // Double current cost
                    c_current[i][j] = c_current[i][j] << 1;
                    
                    // Add bit s
                    long long bit_s = (scaled_cost[i][j] >> s) & 1LL;
                    c_current[i][j] += bit_s;
                }
            }
        }
        
        // Update duals: y(v) ← 2y(v) - 1
        for (int i = 0; i < n; ++i) {
            y_u[i] = (y_u[i] << 1) - 1;
        }
        for (int j = 0; j < m; ++j) {
            y_v[j] = (y_v[j] << 1) - 1;
        }
        
        // Step 2: Find 1-optimal matching with scale_match
        // CRITICAL: Start with FRESH matching each time (don't reuse)
        row_match.assign(n, NIL);
        col_match.assign(m, NIL);
        
        scale_match(c_current, row_match, col_match, y_u, y_v);
    }
    
    // Step 7: Adjust duals back for original costs
    // The duals maintain: y_u[i] + y_v[j] = (n+1) * (original[i][j] - min_cost) for matched edges
    // We need to adjust back to original scale and add back min_cost shift
    for (int i = 0; i < n; ++i) {
        y_u[i] = y_u[i] / static_cast<long long>(n + 1) + min_cost;
    }
    for (int j = 0; j < m; ++j) {
        y_v[j] = y_v[j] / static_cast<long long>(n + 1);
    }
}
