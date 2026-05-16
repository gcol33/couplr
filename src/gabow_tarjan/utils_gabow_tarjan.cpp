// utils_gabow_tarjan.cpp
// Module A: Cost-length & 1-feasibility utilities for Gabow-Tarjan algorithm
// PATCHED VERSION with diagnostic error reporting

#include "utils_gabow_tarjan.h"
#include <Rcpp.h>
#include "../core/lap_utils_rcpp.h"
#include <set>
#include <utility>
#include <queue>
#include <map>
#include <algorithm>
#include <limits>
// stdexcept removed - use LAP_ERROR() instead
// #include <iostream>  // Removed for CRAN compliance (no std::cerr allowed)

#ifdef COUPLR_GT_DEBUG
#define DEBUG_ASSERT_FEASIBLE(cost, rm, cm, yu, yv, msg)                       \
    do {                                                                        \
        if (!check_one_feasible((cost), (rm), (cm), (yu), (yv))) {              \
            LAP_ERROR(msg);                                                     \
        }                                                                       \
    } while (0)
#else
#define DEBUG_ASSERT_FEASIBLE(cost, rm, cm, yu, yv, msg) ((void)0)
#endif

// ---------------------------------------------------------------------------
// Optional profiling instrumentation. Enable by building with
//   PKG_CPPFLAGS += -DCOUPLR_GT_PROFILE
// and the package will print a per-solve breakdown to stdout. Release builds
// compile every PROF_* call to a no-op. Counters are process-scoped (not
// thread-local); the R single-threaded execution model makes that fine.
// ---------------------------------------------------------------------------
#ifdef COUPLR_GT_PROFILE
#include <chrono>
namespace {
struct GtProfile {
    long long total_ns           = 0;
    long long step1_ns           = 0;
    long long step2_ns           = 0;
    long long enqueue_ns         = 0;
    long long bucket_loop_ns     = 0;
    long long eq_build_ns        = 0;
    long long eq_update_ns       = 0;
    long long step1_calls        = 0;
    long long step1_paths        = 0;
    long long step2_calls        = 0;
    long long step2_enqueue_edges= 0;
    long long step2_bucket_pops  = 0;
    long long eq_builds          = 0;
    long long eq_updates         = 0;
    long long match_gt_calls     = 0;
    long long scale_match_calls  = 0;

    void reset() { *this = GtProfile{}; }
    void print() const {
        auto ms = [](long long ns) { return ns / 1e6; };
        Rcpp::Rcout << "[GT_PROFILE]"
                    << "  total=" << ms(total_ns) << " ms"
                    << "  step1=" << ms(step1_ns) << " (" << step1_calls
                    << " calls, " << step1_paths << " paths)"
                    << "  step2=" << ms(step2_ns) << " (" << step2_calls
                    << " calls)"
                    << "    enqueue=" << ms(enqueue_ns) << " ("
                    << step2_enqueue_edges << " edges)"
                    << "    bucket=" << ms(bucket_loop_ns) << " ("
                    << step2_bucket_pops << " pops)"
                    << "  eq_build=" << ms(eq_build_ns) << " ("
                    << eq_builds << ")"
                    << "  eq_update=" << ms(eq_update_ns) << " ("
                    << eq_updates << ")"
                    << "  match_gt=" << match_gt_calls
                    << "  scale_match=" << scale_match_calls
                    << "\n";
    }
};
inline GtProfile& gt_prof() {
    static GtProfile p;
    return p;
}
struct PrTimer {
    long long& target;
    std::chrono::steady_clock::time_point t0;
    explicit PrTimer(long long& tgt)
        : target(tgt), t0(std::chrono::steady_clock::now()) {}
    ~PrTimer() {
        auto t1 = std::chrono::steady_clock::now();
        target += std::chrono::duration_cast<std::chrono::nanoseconds>(t1 - t0)
                      .count();
    }
};
}  // namespace
#define PROF_TIMER(field) PrTimer pr_timer_##field(gt_prof().field)
#define PROF_INC(field, by) (gt_prof().field += (by))
#define PROF_RESET() (gt_prof().reset())
#define PROF_PRINT() (gt_prof().print())
#else
#define PROF_TIMER(field) ((void)0)
#define PROF_INC(field, by) ((void)0)
#define PROF_RESET() ((void)0)
#define PROF_PRINT() ((void)0)
#endif

// Phase 5 / Phase 8 historical note. Earlier Step 2 implementations used a
// paper-style bucket array Q[r] indexed by r = O(n) (theoretical) and
// silently dropped finite, not-in-T edges whose r exceeded the paper's
// 6n+2 bound. The drop was load-bearing under that implementation because
// this code only tightened duals on visited rows/cols, so phase-exit
// reduced costs were not held within the paper's [-1, n+1] range. Phase 5
// fuzzed it (~86% drop fire rate, 0 cost mismatches vs JV) and decided the
// drop was structurally safe. Phase 8 replaced the bucket array with a
// JV-style scan-min Dijkstra-Hungarian over cost-length, which has no
// bucket and no drop -- correctness is now structural. dev_notes/phase5_fuzz.R
// stays as a regression check: any future change must produce 0 cost
// mismatches across the 897 square instances it generates.

namespace {

// O(n) check: is every row unmatched?
bool gt_matching_is_empty(const MatchVec& row_match) {
    for (int j : row_match) {
        if (j != NIL) return false;
    }
    return true;
}

// Canonical 1-feasible dual init for an empty matching:
//   y_u[i] = 0
//   y_v[j] = min over rows of (c(i, j) + 1) for finite edges, or 0 if all forbidden
//
// This is the tightest 1-feasible setting when no edge is matched. It is the
// only setup that is robust to costs with c < -1 (where the zero duals fail
// the upper bound y_u + y_v <= c + 1). Called once at the start of match_gt
// / hungarian_step_one_feasible whenever the matching arrives empty; caller-
// supplied duals are overwritten because there is no matched edge whose value
// they would be carrying. Cost: O(n m), runs once per entry, not per Step 2.
void gt_init_empty_duals(const CostMatrix& cost,
                         DualVec& y_u,
                         DualVec& y_v) {
    const int n = static_cast<int>(cost.size());
    const int m = n > 0 ? static_cast<int>(cost[0].size()) : 0;
    for (int i = 0; i < n; ++i) {
        y_u[i] = 0;
    }
    for (int j = 0; j < m; ++j) {
        long long min_val = BIG_INT;
        for (int i = 0; i < n; ++i) {
            long long c_ij = cost[i][j];
            if (c_ij < BIG_INT) {
                long long val = c_ij + 1;
                if (val < min_val) min_val = val;
            }
        }
        y_v[j] = (min_val < BIG_INT) ? min_val : 0;
    }
}

}  // namespace

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
    PROF_TIMER(eq_build_ns);
    PROF_INC(eq_builds, 1);
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
 * Handles two callers:
 *   - Step 1 augment + col-decrement: only y_v on path cols changes. Pass
 *     affected_rows = {} and affected_cols = path cols. Each existing eligible
 *     edge to an affected col is rechecked; no new edges become eligible
 *     because y_v only decreases (and for rows not on the path, cl is
 *     unchanged), so the routine performs deletions only on those columns.
 *   - Step 2 Hungarian search: y_u changes on rows in S, y_v on cols in T,
 *     and row_match changes along the augmenting path (all path rows in S,
 *     all path cols in T). For rows in S, every edge (i, *) may have flipped
 *     eligibility, so eq_graph[i] is rebuilt from scratch over all m cols.
 *     For rows not in S, cl(i, j) is unchanged for every j (row_match[i] is
 *     untouched, and col_match[j] only moves between rows in S, never to a
 *     row outside S), so it suffices to prune edges to cols in T whose
 *     y_v dropped, same as the Step 1 case.
 *
 * @param eq_graph Existing equality graph to update (modified in place)
 * @param cost Cost matrix
 * @param row_match Current matching
 * @param y_u Dual variables for rows
 * @param y_v Dual variables for columns
 * @param affected_rows Row indices whose y_u changed (or whose row_match
 *                      changed). Their adjacency lists are rebuilt entirely.
 * @param affected_cols Column indices whose y_v decreased. For non-affected
 *                      rows, edges to these cols are pruned if no longer
 *                      eligible.
 */
void update_equality_graph_incremental(std::vector<std::vector<int>>& eq_graph,
                                        const CostMatrix& cost,
                                        const MatchVec& row_match,
                                        const DualVec& y_u,
                                        const DualVec& y_v,
                                        const std::vector<int>& affected_rows,
                                        const std::vector<int>& affected_cols)
{
    PROF_TIMER(eq_update_ns);
    PROF_INC(eq_updates, 1);
    const int n = static_cast<int>(cost.size());
    const int m = n > 0 ? static_cast<int>(cost[0].size()) : 0;

    if (affected_rows.empty() && affected_cols.empty()) return;

    std::vector<bool> in_affected_rows(n, false);
    for (int i : affected_rows) {
        if (i >= 0 && i < n) in_affected_rows[i] = true;
    }

    // Full rebuild for rows whose y_u (and possibly row_match) changed.
    for (int i : affected_rows) {
        if (i < 0 || i >= n) continue;
        auto& adj_list = eq_graph[i];
        adj_list.clear();
        for (int j = 0; j < m; ++j) {
            long long c_ij = cost[i][j];
            if (c_ij >= BIG_INT) continue;
            bool in_matching = (row_match[i] == j);
            if (is_eligible(c_ij, in_matching, y_u[i], y_v[j])) {
                adj_list.push_back(j);
            }
        }
    }

    if (affected_cols.empty()) return;

    std::vector<bool> affected_col_mask(m, false);
    for (int j : affected_cols) {
        if (j >= 0 && j < m) affected_col_mask[j] = true;
    }

    // Prune eligible edges to affected cols for rows that were not rebuilt.
    for (int i = 0; i < n; ++i) {
        if (in_affected_rows[i]) continue;
        auto& adj_list = eq_graph[i];
        adj_list.erase(
            std::remove_if(adj_list.begin(), adj_list.end(),
                           [&](int j) {
                               if (j < 0 || j >= m || !affected_col_mask[j]) {
                                   return false;
                               }
                               long long c_ij = cost[i][j];
                               if (c_ij >= BIG_INT) {
                                   return true;
                               }
                               bool in_matching = (row_match[i] == j);
                               return !is_eligible(c_ij, in_matching, y_u[i], y_v[j]);
                           }),
            adj_list.end());
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
    const int m = static_cast<int>(col_match.size());
    
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
 * Find a maximal set of vertex-disjoint shortest augmenting paths in the
 * equality graph (Hopcroft-Karp style).
 *
 * Two-phase Hopcroft-Karp structure:
 *   1. BFS from all free rows assigns levels. Rows at even levels, cols at
 *      odd levels. Cols reach via eligible edges; rows reach via matched
 *      edges (col_match[j] -> next_row at col_level + 1). BFS stops one
 *      layer past the first free col discovered, so every leveled vertex
 *      lies on a shortest augmenting path.
 *   2. DFS from free rows along level-increasing edges only, using
 *      visited_col + next_edge[i] to keep total DFS work O(E) per call.
 *      Each row that fails to find a path has its level demoted (`row_level
 *      = INF`) so later DFS roots don't redescend it.
 *
 * Restricting Step 1 to shortest paths bounds the number of times match_gt
 * loops Step 1 / Step 2 per scaling phase — the paper's O(sqrt(n)) HK round
 * count, modulo dual changes from Step 2 reshaping the eligibility graph.
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
    const int n = static_cast<int>(eq_graph.size());
    const int m = static_cast<int>(col_match.size());

    constexpr int INF_LEVEL = std::numeric_limits<int>::max();
    std::vector<int> row_level(n, INF_LEVEL);
    std::vector<int> col_level(m, INF_LEVEL);

    // ---- Phase 1: BFS to assign levels. ------------------------------------
    std::queue<int> bfs_q;
    for (int i = 0; i < n; ++i) {
        if (row_match[i] == NIL) {
            row_level[i] = 0;
            bfs_q.push(i);
        }
    }

    int free_col_level = INF_LEVEL;  // Shortest augmenting-path length (cols).
    while (!bfs_q.empty()) {
        int i = bfs_q.front();
        bfs_q.pop();

        // Don't expand beyond the shortest free-col layer.
        if (row_level[i] >= free_col_level) continue;

        for (int j : eq_graph[i]) {
            if (j < 0 || j >= m) continue;
            if (col_level[j] != INF_LEVEL) continue;

            col_level[j] = row_level[i] + 1;
            if (col_match[j] == NIL) {
                // Free col reached. Record the layer and keep draining the
                // queue at this level so all shortest paths are discoverable.
                if (free_col_level == INF_LEVEL) {
                    free_col_level = col_level[j];
                }
            } else {
                int next_row = col_match[j];
                if (next_row >= 0 && next_row < n &&
                    row_level[next_row] == INF_LEVEL) {
                    row_level[next_row] = col_level[j] + 1;
                    bfs_q.push(next_row);
                }
            }
        }
    }

    std::vector<std::vector<std::pair<int,int>>> all_paths;
    if (free_col_level == INF_LEVEL) {
        // No augmenting path exists at this point in the eligibility graph.
        return all_paths;
    }

    // ---- Phase 2: DFS along level-increasing edges. -----------------------
    std::vector<bool> visited_col(m, false);
    std::vector<size_t> next_edge(n, 0);

    for (int root = 0; root < n; ++root) {
        if (row_match[root] != NIL || row_level[root] != 0) continue;

        std::vector<int> path_rows;
        std::vector<std::pair<int,int>> path_edges;
        path_rows.push_back(root);

        while (!path_rows.empty()) {
            int i = path_rows.back();
            bool advanced = false;

            while (next_edge[i] < eq_graph[i].size()) {
                int j = eq_graph[i][next_edge[i]++];
                if (j < 0 || j >= m) continue;
                // Level filter: only follow edges into the next layer.
                if (col_level[j] != row_level[i] + 1) continue;
                if (visited_col[j]) continue;

                visited_col[j] = true;
                path_edges.emplace_back(i, j);

                if (col_match[j] == NIL) {
                    all_paths.push_back(path_edges);
                    path_rows.clear();
                    path_edges.clear();
                    advanced = true;
                    break;
                }

                int next_row = col_match[j];
                if (next_row >= 0 && next_row < n &&
                    row_level[next_row] == col_level[j] + 1) {
                    path_rows.push_back(next_row);
                    advanced = true;
                    break;
                }

                // Matched edge doesn't ascend to the right level (target row
                // either past the path-length cap or already exhausted by a
                // prior failed DFS). Back out and try the next edge.
                path_edges.pop_back();
            }

            if (advanced) continue;

            // No further progress from row i. Demote it so later DFS roots
            // don't redescend through here.
            row_level[i] = INF_LEVEL;
            if (path_edges.empty()) break;
            path_edges.pop_back();
            path_rows.pop_back();
        }
    }

    return all_paths;
}

// ============================================================================
// Module E: Hungarian-style search on cost-length (Step 2 core)
// ============================================================================

/**
 * Hungarian/Dijkstra search on cost-length (computed inline from cost + matching).
 *
 * Performs a Hungarian search to find an augmenting path, maintaining
 * 1-feasibility. Updates duals and matching if a path is found.
 *
 * cl(i, j) is derived on the fly:
 *   cl(i, j) = cost[i][j] + 1   if (i, j) is not in the matching
 *   cl(i, j) = cost[i][j]       if (i, j) is in the matching
 * Forbidden edges (cost[i][j] >= BIG_INT) are skipped.
 *
 * @param cost Cost matrix (BIG_INT for forbidden edges)
 * @param row_match Current matching from rows (modified in place)
 * @param col_match Current matching from columns (modified in place)
 * @param y_u Dual variables for rows (modified in place)
 * @param y_v Dual variables for columns (modified in place)
 * @return true if augmenting path found and applied, false otherwise
 */
bool hungarian_search_cl(const CostMatrix& cost,
                         MatchVec& row_match,
                         MatchVec& col_match,
                         DualVec& y_u,
                         DualVec& y_v,
                         std::vector<int>* affected_rows_out,
                         std::vector<int>* affected_cols_out)
{
    PROF_TIMER(step2_ns);
    PROF_INC(step2_calls, 1);
    const int n = static_cast<int>(cost.size());
    const int m = n > 0 ? static_cast<int>(cost[0].size()) : 0;
    if (n == 0 || m == 0) return false;
    if (affected_rows_out) affected_rows_out->clear();
    if (affected_cols_out) affected_cols_out->clear();

    // Inline cost-length helper: cl(i, j) for a known cost[i][j] value.
    // Caller must have already checked c_ij < BIG_INT.
    auto cl_of = [&](int i, int j, long long c_ij) -> long long {
        return (row_match[i] == j) ? c_ij : (c_ij + 1);
    };

    // Invariant: every matched edge entering Step 2 is tight, i.e. y_u[i] +
    // y_v[j] == cost[i][j]. Step 1's augment+col-decrement maintains this; the
    // augment loop below maintains it across Step 2 by decrementing y_v on
    // every newly-matched col, mirroring Step 1. Under COUPLR_GT_DEBUG we
    // assert it; in release we rely on it (the old defensive y_u -= 1 fix-up
    // would have masked stale eq_graph entries on rows that PRE-STEPed but did
    // not enter S, breaking the Phase 2 incremental update).
#ifdef COUPLR_GT_DEBUG
    for (int i = 0; i < n; ++i) {
        int j = row_match[i];
        if (j == NIL || j < 0 || j >= m) continue;
        long long c_ij = cost[i][j];
        if (c_ij >= BIG_INT) continue;
        if (y_u[i] + y_v[j] != c_ij) {
            LAP_ERROR("Matched edge not tight entering hungarian_search_cl "
                      "(y_u[i] + y_v[j] != cost[i][j])");
        }
    }
#endif

    // ---------------------------------------------------------------------
    // Phase 8: Dijkstra-Hungarian on cost-length cl, JV-style scan-min.
    //
    // Per iteration: scan all unsettled cols, pick the one with the smallest
    // stored dist[j]; settle it; if it's free, augment, otherwise add its
    // match's row to S and relax that row's edges. Augment by walking
    // pred_row[col] back to the free root, with y_v[col] -= 1 on every
    // newly-matched col (Phase 2 invariant restoring matched-edge tightness
    // after the cl_matched = cl_unmatched - 1 shift).
    //
    // Eliminates the bucket array, bucket_bound, and the Phase 5 silent
    // drop. No edge is ever dropped.
    //
    // Optimizations layered:
    //   (a) S_list and T_list as explicit vectors; dual updates iterate
    //       these directly instead of scanning all n / m with an in_S /
    //       in_T branch.
    //   (b) Lazy delta on dist[]. JV's per-iter "for j not in T: dist[j] -=
    //       delta" loop is O(m); we replace it by tracking delta_pending
    //       and storing dist[j] in (current_form + delta_pending) form,
    //       i.e. the absolute reduced cost at the moment of update. Scan-min
    //       compares stored values directly (order-preserving since they
    //       all share the same delta_pending offset). Relax shifts new
    //       candidates by + delta_pending before storing. Functionally
    //       equivalent to v1 but eliminates the O(m) subtract loop per iter.
    //
    // We tried an "absolute-form dist[]" variant that compared d_absolute to
    // dist[j]_absolute (skipping the +delta_pending shift), but it broke
    // matched-edge tightness on >1-step augmenting paths -- the JV-style
    // shortest path in CURRENT form (where dist[j] = path cost via current
    // duals) is required for the dual updates to preserve matched-edge
    // tightness, not the absolute shortest path. The lazy-delta version
    // below keeps JV's comparison invariant.
    // ---------------------------------------------------------------------
    constexpr long long INF = std::numeric_limits<long long>::max() / 4;
    std::vector<long long> dist(m, INF);
    std::vector<int> pred_row(m, NIL);
    std::vector<bool> in_T(m, false);
    std::vector<bool> in_S(n, false);
    std::vector<int> S_list;
    std::vector<int> T_list;
    S_list.reserve(static_cast<size_t>(n));
    T_list.reserve(static_cast<size_t>(m));

    // Initial relax from every free row. delta_pending = 0 here, so
    // "stored" dist == "absolute reduced cost" == "current form".
    bool has_free = false;
    for (int i = 0; i < n; ++i) {
        if (row_match[i] != NIL) continue;
        has_free = true;
        in_S[i] = true;
        S_list.push_back(i);
        for (int j = 0; j < m; ++j) {
            long long c_ij = cost[i][j];
            if (c_ij >= BIG_INT) continue;
            long long ccl = cl_of(i, j, c_ij);
            long long d = ccl - y_u[i] - y_v[j];
            if (d < dist[j]) {
                dist[j] = d;
                pred_row[j] = i;
            }
            PROF_INC(step2_enqueue_edges, 1);
        }
    }
    if (!has_free) return false;

    long long delta_pending = 0;
    int sink_col = NIL;
    while (sink_col == NIL) {
        // Scan-min over unsettled cols. Stored values share the same
        // delta_pending offset; order is preserved.
        long long best_stored = INF;
        int j1 = NIL;
        for (int j = 0; j < m; ++j) {
            if (in_T[j]) continue;
            if (dist[j] < best_stored) { best_stored = dist[j]; j1 = j; }
        }
        if (j1 == NIL || best_stored >= INF) return false;
        PROF_INC(step2_bucket_pops, 1);

        // Effective delta (= JV's "delta" in current form) is best minus
        // the accumulated offset; in absolute terms, advance the pending
        // offset to match.
        long long delta = best_stored - delta_pending;
        if (delta != 0) {
            for (int i : S_list) y_u[i] += delta;
            for (int j : T_list) y_v[j] -= delta;
        }
        delta_pending = best_stored;

        in_T[j1] = true;
        T_list.push_back(j1);

        if (col_match[j1] == NIL) {
            sink_col = j1;
            break;
        }

        int next_row = col_match[j1];
        if (next_row < 0 || next_row >= n) return false;
        if (in_S[next_row]) {
            // Alternating cycle: shouldn't happen under 1-feasibility on a
            // simple bipartite graph.
            return false;
        }
        in_S[next_row] = true;
        S_list.push_back(next_row);

        // Relax next_row's edges. next_row was just added; y_u[next_row] is
        // unchanged from entry. y_v[j] for j not in T is also unchanged.
        // So d_abs is computed against entry-time duals (i.e. JV's "current
        // form" for this candidate, since next_row's u contribution is u_init).
        // Convert to stored form by adding delta_pending; compare to dist[j]
        // (also stored form). Both share the same offset; comparison preserved.
        for (int j = 0; j < m; ++j) {
            if (in_T[j]) continue;
            long long c_ij = cost[next_row][j];
            if (c_ij >= BIG_INT) continue;
            long long ccl = cl_of(next_row, j, c_ij);
            long long d_stored = (ccl - y_u[next_row] - y_v[j]) + delta_pending;
            if (d_stored < dist[j]) {
                dist[j] = d_stored;
                pred_row[j] = next_row;
            }
            PROF_INC(step2_enqueue_edges, 1);
        }
    }

    // Augment along the path. Walk pred_row[col] back to a free root.
    // Read row_match[row] BEFORE the reassignment to recover the col `row`
    // entered S through (NIL for roots). y_v[col] -= 1 on every newly-
    // matched col keeps the matched edge tight after augment flips cl by 1.
    int col = sink_col;
    while (col != NIL) {
        int row = pred_row[col];
        if (row < 0 || row >= n) return false;
        int prev_col = row_match[row];
        row_match[row] = col;
        col_match[col] = row;
        y_v[col] -= 1;
        col = prev_col;
    }

    if (affected_rows_out) {
        affected_rows_out->assign(S_list.begin(), S_list.end());
    }
    if (affected_cols_out) {
        affected_cols_out->assign(T_list.begin(), T_list.end());
    }
    return true;
}
bool hungarian_step_one_feasible(const CostMatrix& cost,
                                 MatchVec& row_match,
                                 MatchVec& col_match,
                                 DualVec& y_u,
                                 DualVec& y_v,
                                 std::vector<int>* affected_rows_out,
                                 std::vector<int>* affected_cols_out)
{
    const int n = static_cast<int>(cost.size());
    if (n == 0) return false;
    const int m = static_cast<int>(cost[0].size());
    if (m == 0) return false;

    // Empty-matching entry path: caller's duals carry no matched-edge value;
    // initialize to the canonical 1-feasible y_u = 0, y_v = min_i(c + 1). This
    // is the only init step kept from the old defensive block; it costs O(nm)
    // once and is required for correctness when c contains values < -1. The
    // 20-iter dual-repair loop for non-empty matchings is gone — caller is
    // expected to deliver a 1-feasible state.
    const bool was_empty = gt_matching_is_empty(row_match);
    if (was_empty) {
        gt_init_empty_duals(cost, y_u, y_v);
    }

    DEBUG_ASSERT_FEASIBLE(cost, row_match, col_match, y_u, y_v,
        "1-feasibility violated entering hungarian_step_one_feasible");

    bool ok = hungarian_search_cl(cost, row_match, col_match, y_u, y_v,
                                  affected_rows_out, affected_cols_out);

    // gt_init_empty_duals rewrote every y_v, so the equality graph must be
    // rebuilt for every row regardless of which rows entered S. Signal this
    // to the caller by populating affected_rows_out with all rows.
    if (ok && was_empty && affected_rows_out) {
        affected_rows_out->clear();
        affected_rows_out->reserve(static_cast<size_t>(n));
        for (int i = 0; i < n; ++i) affected_rows_out->push_back(i);
    }

    return ok;
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
    PROF_TIMER(step1_ns);
    PROF_INC(step1_calls, 1);
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
    PROF_INC(step1_paths, static_cast<long long>(paths.size()));

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
    PROF_INC(match_gt_calls, 1);
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
    
    // -------------------------------------------------------------------------
    // NORMALIZATION: rebuild col_match from row_match (O(n+m)).
    // The caller is responsible for delivering a 1-feasible (cost, duals,
    // matching) triple; that invariant is asserted under COUPLR_GT_DEBUG. The
    // previous O(nm) check_one_feasible + discard/restart block has been
    // removed (Phase 1 of gt-speedup.md).
    // -------------------------------------------------------------------------
    for (int j = 0; j < m; ++j) {
        col_match[j] = NIL;
    }

    for (int i = 0; i < n; ++i) {
        int j = row_match[i];
        if (j != NIL && j >= 0 && j < m) {
            if (col_match[j] != NIL) {
                // Conflict: column j already claimed by another row. Drop this match.
                row_match[i] = NIL;
            } else {
                col_match[j] = i;
            }
        } else if (j != NIL) {
            row_match[i] = NIL;
        }
    }

    // Empty-matching entry path: canonical 1-feasible dual init (O(nm) once,
    // required for correctness when c contains values < -1). For non-empty
    // matchings the caller must supply 1-feasible duals.
    if (gt_matching_is_empty(row_match)) {
        gt_init_empty_duals(cost, y_u, y_v);
    }

    DEBUG_ASSERT_FEASIBLE(cost, row_match, col_match, y_u, y_v,
        "1-feasibility violated entering match_gt");

    if (is_perfect(row_match)) {
        return;
    }


    // Build initial equality graph once. From here on it is maintained
    // incrementally: Step 1 mutates only y_v on path cols; Step 2 mutates
    // y_u on rows that entered S and y_v on cols that entered T. The
    // post-Step-2 full O(nm) rebuild has been replaced (Phase 2 of
    // gt-speedup.md).
    std::vector<std::vector<int>> eq_graph = build_equality_graph(cost, row_match, y_u, y_v);
    std::vector<int> affected_rows;
    std::vector<int> affected_cols;
    const std::vector<int> no_affected_rows;

    int it = 0;
    while (!is_perfect(row_match)) {
        ++it;
        if (it > max_iters) {
            LAP_ERROR("match_gt exceeded max_iters");
        }

        // Optional: Check 1-feasibility before Step 1 (debug only)
        if (check_feasible) {
            if (!check_one_feasible(cost, row_match, col_match, y_u, y_v)) {
                LAP_ERROR("1-feasibility violated before Step 1");
            }
        }

        // Step 1: maximal vertex-disjoint augmenting paths on eligible edges.
        bool found_paths = apply_step1(cost, row_match, col_match, y_u, y_v,
                                       &eq_graph, &affected_cols);

        if (found_paths && !affected_cols.empty()) {
            update_equality_graph_incremental(eq_graph, cost, row_match, y_u, y_v,
                                              no_affected_rows, affected_cols);
        }

        if (is_perfect(row_match)) {
            break;
        }

        // Step 2: only when Step 1 found no paths. One Hungarian search to
        // extract an augmenting path on the cost-length graph; emits the
        // forest's S/T as affected_rows/affected_cols for the eq-graph patch.
        if (!found_paths) {
            if (!hungarian_step_one_feasible(cost, row_match, col_match, y_u, y_v,
                                             &affected_rows, &affected_cols)) {
                LAP_ERROR("No augmenting path in Step 2 (no perfect matching)");
            }

            update_equality_graph_incremental(eq_graph, cost, row_match, y_u, y_v,
                                              affected_rows, affected_cols);
        }

        // Optional: Check 1-feasibility after Step 2 (debug only)
        if (check_feasible) {
            if (!check_one_feasible(cost, row_match, col_match, y_u, y_v)) {
                LAP_ERROR("1-feasibility violated after Step 2");
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
 * - If n > m (more rows than cols): pads with zero-cost dummy columns
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
    PROF_INC(scale_match_calls, 1);
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
    //    Padding columns (j >= m) are zero-cost dummy assignments
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
        for (int j = m; j < m_work; ++j) {
            cost_prime[i][j] = 0;
        }
    }
    
    // 2. Local matching and duals for match_gt on cost_prime.
    //
    //    Phase 1 note: match_gt no longer carries an O(nm) discard/restart
    //    block. With y_u_loc = y_v_loc = 0 a non-empty inherited matching is
    //    not generally 1-feasible on cost_prime (matched-edge lower bound
    //    fails when reduced cost > 0), so we start match_gt from an empty
    //    matching here. The previous code path also discarded the matching
    //    every phase via match_gt's check_one_feasible; this just makes that
    //    explicit and cheap. Phase 4 (drop cost_prime) restores a real
    //    bit-scaling warm start.
    MatchVec row_loc(n_work, NIL);
    MatchVec col_loc(m_work, NIL);
    DualVec y_u_loc(n_work, 0);
    DualVec y_v_loc(m_work, 0);

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
 * 2. Reuse matching across scales so each phase warm-starts from the previous one
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
 *    c. Run scale_match from the current matching to get 1-optimal solution
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
    PROF_RESET();
    PROF_TIMER(total_ns);
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
                    // Double current cost (use multiplication to avoid UB on negative values)
                    c_current[i][j] = c_current[i][j] * 2;

                    // Add bit s
                    long long bit_s = (scaled_cost[i][j] >> s) & 1LL;
                    c_current[i][j] += bit_s;
                }
            }
        }
        
        // Update duals: y(v) ← 2y(v) - 1
        // Use multiplication instead of bit shift to avoid UB with negative values
        for (int i = 0; i < n; ++i) {
            y_u[i] = 2 * y_u[i] - 1;
        }
        for (int j = 0; j < m; ++j) {
            y_v[j] = 2 * y_v[j] - 1;
        }
        
        // Step 2: Find 1-optimal matching with scale_match.
        // Carrying the previous phase's matching is the scaling warm start.
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

#ifdef COUPLR_GT_PROFILE
    // PROF_TIMER(total_ns) destructor hasn't run yet; sample the elapsed time
    // for the print, then let the destructor finalize total_ns.
    {
        auto& p = gt_prof();
        // Approximate: print the running totals (total_ns finalized just after).
        // We use a print here so the line appears once per solve in benchmarks.
        long long saved_total = p.total_ns;
        p.total_ns = std::chrono::duration_cast<std::chrono::nanoseconds>(
                         std::chrono::steady_clock::now() -
                         pr_timer_total_ns.t0)
                         .count();
        p.print();
        p.total_ns = saved_total;
    }
#endif
}
