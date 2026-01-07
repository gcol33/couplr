#ifndef COUPLR_NS_INIT_H
#define COUPLR_NS_INIT_H

#include "ns_types.h"
#include "ns_graph.h"
#include <algorithm>

namespace couplr {
namespace ns {

// Initialize with a feasible spanning tree using Big-M artificial arcs
// For assignment: the network is source -> rows -> cols -> sink
// Each node has supply/demand: source = +n, rows = 0 (transshipment),
// cols = 0 (transshipment), sink = -n
//
// Key insight: In Network Simplex, flows MUST satisfy conservation at every node.
// The spanning tree represents the basis; non-tree arcs are at bounds (0 or capacity).
//
// For a feasible start, we use artificial arcs:
//   - Add artificial arc from source directly to sink with huge cost (BIG_M)
//   - Initial tree: all original source->row, row->col, col->sink arcs at lower bound (flow=0)
//   - Artificial arc carries all n units of flow
//   - Pivots will naturally drive out the artificial arc
//
// Simpler approach for assignment: Start with a valid matching and proper tree structure.

inline void initialize_spanning_tree_greedy(NSState& state) {
    int n = state.n_rows;
    int m = state.n_cols;
    int source = state.source_node();
    int sink = state.sink_node();

    // Track which rows and cols are matched
    std::vector<int> row_match(n, -1);  // row i matched to col row_match[i]
    std::vector<int> col_match(m, -1);  // col j matched to row col_match[j]

    // Greedy: for each row, find cheapest unmatched col
    for (int i = 0; i < n; ++i) {
        int best_j = -1;
        double best_cost = BIG_M;
        for (int j = 0; j < m; ++j) {
            if (col_match[j] < 0) {
                int arc = state.row_to_col_arc(i, j);
                if (state.arc_cost[arc] < best_cost) {
                    best_cost = state.arc_cost[arc];
                    best_j = j;
                }
            }
        }
        if (best_j >= 0) {
            row_match[i] = best_j;
            col_match[best_j] = i;
        }
    }

    // Reset all arcs to lower bound (flow = 0)
    for (int a = 0; a < state.num_arcs; ++a) {
        state.arc_state[a] = STATE_LOWER;
        state.arc_flow[a] = 0;
    }

    // Build spanning tree structure:
    // The spanning tree must have exactly (num_nodes - 1) arcs
    // For assignment with n rows and m cols: num_nodes = n + m + 2
    // Tree arcs needed = n + m + 1
    //
    // Tree structure:
    //   - n source->row arcs (all rows connect to source)
    //   - 1 row->col arc per col (connect each col to exactly one row)  [m arcs]
    //   - 1 col->sink arc (sink connects to one col)                    [1 arc]
    // Total: n + m + 1 tree arcs

    // Source is root
    state.parent[source] = NO_NODE;
    state.parent_arc[source] = NO_ARC;
    state.depth[source] = 0;

    // All source->row arcs are in tree with flow = 1
    // (Source supplies 1 unit to each row)
    for (int i = 0; i < n; ++i) {
        int row = state.row_node(i);
        int arc = state.source_to_row_arc(i);

        state.parent[row] = source;
        state.parent_arc[row] = arc;
        state.arc_to_parent_up[row] = false;  // Arc points from parent (source) to child (row)
        state.depth[row] = 1;

        state.arc_state[arc] = STATE_TREE;
        state.arc_flow[arc] = 1;
    }

    // For each col, pick parent row:
    // - If col is matched, parent = matched row, flow = 1
    // - If col is unmatched, parent = row 0, flow = 0
    for (int j = 0; j < m; ++j) {
        int col = state.col_node(j);
        int parent_row_idx;
        int flow;

        if (col_match[j] >= 0) {
            parent_row_idx = col_match[j];
            flow = 1;
        } else {
            parent_row_idx = 0;  // Unmatched cols connect to row 0
            flow = 0;
        }

        int arc = state.row_to_col_arc(parent_row_idx, j);
        state.parent[col] = state.row_node(parent_row_idx);
        state.parent_arc[col] = arc;
        state.arc_to_parent_up[col] = false;
        state.depth[col] = 2;

        state.arc_state[arc] = STATE_TREE;
        state.arc_flow[arc] = flow;
    }

    // Sink connects to ONE col via col->sink arc
    // Choose the first matched col (or col 0 if none matched)
    int sink_col = 0;
    for (int j = 0; j < m; ++j) {
        if (col_match[j] >= 0) {
            sink_col = j;
            break;
        }
    }

    // The col->sink tree arc carries flow = n (all matched units exit through it in tree)
    // But this is wrong for network simplex! The tree only tracks basis structure.
    //
    // CRITICAL FIX: In network simplex for assignment problems:
    // - Tree arcs can have any flow value (they're basis variables)
    // - Non-tree arcs are at their bounds (0 for LOWER, capacity for UPPER)
    //
    // For the TREE structure: sink has one parent col, that arc is in tree
    // For FLOW: each matched col j must have col->sink flow = 1
    //           but only ONE col->sink arc is in the TREE
    //           other col->sink arcs with flow=1 must be STATE_UPPER
    {
        int arc = state.col_to_sink_arc(sink_col);
        state.parent[sink] = state.col_node(sink_col);
        state.parent_arc[sink] = arc;
        state.arc_to_parent_up[sink] = false;
        state.depth[sink] = 3;

        state.arc_state[arc] = STATE_TREE;
        // Flow on this tree arc = 1 if this col is matched, else 0
        state.arc_flow[arc] = (col_match[sink_col] >= 0) ? 1 : 0;
    }

    // Set flows on non-tree col->sink arcs
    // Matched cols (other than sink_col) have flow = 1 -> STATE_UPPER
    // Unmatched cols have flow = 0 -> STATE_LOWER (already set)
    for (int j = 0; j < m; ++j) {
        if (j == sink_col) continue;  // Skip tree arc
        int arc = state.col_to_sink_arc(j);

        if (col_match[j] >= 0) {
            // Matched col: flow = 1 (at upper bound)
            state.arc_flow[arc] = 1;
            state.arc_state[arc] = STATE_UPPER;
        }
        // Unmatched cols stay at STATE_LOWER with flow = 0
    }

    // Build thread order (DFS preorder from source)
    std::vector<int> thread_order;
    std::vector<bool> visited(state.num_nodes, false);
    std::vector<int> stack;
    stack.push_back(source);

    while (!stack.empty()) {
        int curr = stack.back();
        stack.pop_back();
        if (visited[curr]) continue;
        visited[curr] = true;
        thread_order.push_back(curr);

        // Find children (nodes whose parent is curr)
        std::vector<int> children;
        for (int node = 0; node < state.num_nodes; ++node) {
            if (state.parent[node] == curr) {
                children.push_back(node);
            }
        }
        // Push in reverse order for correct DFS order
        for (int i = static_cast<int>(children.size()) - 1; i >= 0; --i) {
            stack.push_back(children[i]);
        }
    }

    // Build thread and rev_thread arrays (circular)
    for (size_t i = 0; i < thread_order.size(); ++i) {
        int curr = thread_order[i];
        int next = thread_order[(i + 1) % thread_order.size()];
        state.thread[curr] = next;
        state.rev_thread[next] = curr;
    }

    // Compute subtree sizes (bottom-up)
    for (int i = 0; i < state.num_nodes; ++i) {
        state.subtree_size[i] = 1;
    }

    for (int i = static_cast<int>(thread_order.size()) - 1; i >= 0; --i) {
        int node = thread_order[i];
        int par = state.parent[node];
        if (par != NO_NODE) {
            state.subtree_size[par] += state.subtree_size[node];
        }
    }
}

// Compute potentials from the spanning tree
// For tree arc e from u to v: reduced_cost = c_uv - pi_u + pi_v = 0
// Therefore: pi_v = pi_u - c_uv
inline void compute_potentials(NSState& state) {
    int source = state.source_node();
    state.potential[source] = 0.0;

    // Process nodes by depth (BFS order)
    std::vector<std::pair<int, int>> depth_node;
    for (int i = 0; i < state.num_nodes; ++i) {
        if (state.depth[i] >= 0) {
            depth_node.emplace_back(state.depth[i], i);
        }
    }
    std::sort(depth_node.begin(), depth_node.end());

    for (const auto& dn : depth_node) {
        int node = dn.second;
        if (node == source) continue;

        int arc = state.parent_arc[node];
        int par = state.parent[node];

        if (arc == NO_ARC || par == NO_NODE) continue;

        // For tree arc from u to v: rc = c - pi_u + pi_v = 0  =>  pi_v = pi_u - c
        // If arc goes parent -> node (u=par, v=node): pi_node = pi_par - cost
        // If arc goes node -> parent (u=node, v=par): pi_par = pi_node - cost  =>  pi_node = pi_par + cost

        if (state.arc_source[arc] == par) {
            // Arc: parent -> node
            state.potential[node] = state.potential[par] - state.arc_cost[arc];
        } else {
            // Arc: node -> parent
            state.potential[node] = state.potential[par] + state.arc_cost[arc];
        }
    }
}

} // namespace ns
} // namespace couplr

#endif // COUPLR_NS_INIT_H
