#ifndef COUPLR_NS_PIVOT_H
#define COUPLR_NS_PIVOT_H

#include "ns_types.h"
#include "ns_graph.h"
#include <algorithm>

namespace couplr {
namespace ns {

// Find entering arc using block search pricing
// Returns arc index with negative reduced cost, or NO_ARC if optimal
inline int find_entering_arc(NSState& state) {
    int num_arcs = state.num_arcs;
    int block_size = state.block_size;

    int best_arc = NO_ARC;
    double best_rc = -EPSILON;  // Must be strictly negative

    // Search all arcs in blocks, starting from next_arc
    int cnt = 0;
    int arc = state.next_arc;

    while (cnt < num_arcs) {
        // Check this arc
        if (state.arc_state[arc] != STATE_TREE) {
            double rc = reduced_cost(state, arc);

            // For lower bound arcs: enter if rc < 0 (increase flow reduces cost)
            // For upper bound arcs: enter if rc > 0 (decrease flow reduces cost)
            if (state.arc_state[arc] == STATE_LOWER && rc < best_rc) {
                best_rc = rc;
                best_arc = arc;
            } else if (state.arc_state[arc] == STATE_UPPER && -rc < best_rc) {
                best_rc = -rc;
                best_arc = arc;
            }
        }

        ++arc;
        if (arc >= num_arcs) arc = 0;
        ++cnt;

        // After each block, if we found something, use it
        if (cnt % block_size == 0 && best_arc != NO_ARC) {
            break;
        }
    }

    // Update next_arc for next call
    state.next_arc = (best_arc != NO_ARC) ? best_arc : arc;

    return best_arc;
}

// Find the join (LCA) of two nodes in the spanning tree
inline int find_join(const NSState& state, int u, int v) {
    // Walk both nodes up to same depth, then walk together
    while (state.depth[u] > state.depth[v]) {
        u = state.parent[u];
    }
    while (state.depth[v] > state.depth[u]) {
        v = state.parent[v];
    }
    while (u != v) {
        u = state.parent[u];
        v = state.parent[v];
    }
    return u;
}

// Find leaving arc and compute delta (flow change)
// When entering arc is added, it forms a cycle with tree arcs
// We trace this cycle and find the arc with minimum residual capacity
inline PivotInfo find_leaving_arc(NSState& state, int entering_arc) {
    PivotInfo info;
    info.entering_arc = entering_arc;
    info.u_in = state.arc_source[entering_arc];
    info.v_in = state.arc_target[entering_arc];

    // Determine direction: are we increasing or decreasing flow on entering arc?
    bool entering_forward = (state.arc_state[entering_arc] == STATE_LOWER);
    // If LOWER: we want to increase flow (forward direction)
    // If UPPER: we want to decrease flow (backward direction)

    // Find join node (LCA)
    info.join_node = find_join(state, info.u_in, info.v_in);

    // For assignment problems, delta is always 0 or 1
    // Initialize to 1 (can always push at least 1 unit in theory)
    info.delta = 1;
    info.leaving_arc = entering_arc;  // Default: entering arc itself
    info.first_side = true;

    // Trace cycle from u_in to join
    // On this path, if entering_forward, arcs pointing toward join are "forward" (residual = cap - flow)
    // and arcs pointing away from join are "backward" (residual = flow)
    int node = info.u_in;
    while (node != info.join_node) {
        int arc = state.parent_arc[node];
        int par = state.parent[node];

        // Determine if this arc is forward or backward in the cycle
        // Cycle direction: u_in -> ... -> join -> ... -> v_in (for forward entering)
        // From u_in to join, we traverse "up" the tree

        // If arc points from node to parent (arc_source = node, arc_target = parent)
        // then in the cycle (going up), this arc is traversed backward
        bool arc_points_to_parent = (state.arc_source[arc] == node);

        int residual;
        if (entering_forward) {
            // Going up from u_in: we push flow "backward" through the tree path
            // If arc points to parent: cycle goes opposite, so we increase flow
            // If arc points from parent: cycle goes with arc, so we decrease flow
            if (arc_points_to_parent) {
                // Arc: node -> parent. Cycle goes parent <- node (backward on arc)
                // To push flow backward, need arc_flow > 0
                residual = state.arc_flow[arc];
            } else {
                // Arc: parent -> node. Cycle goes parent <- node (forward on arc)
                // Residual = 1 - flow (since cap = 1)
                residual = 1 - state.arc_flow[arc];
            }
        } else {
            // entering_forward = false, so we're pushing flow in reverse
            if (arc_points_to_parent) {
                residual = 1 - state.arc_flow[arc];
            } else {
                residual = state.arc_flow[arc];
            }
        }

        if (residual < info.delta) {
            info.delta = residual;
            info.leaving_arc = arc;
            info.first_side = true;
            info.u_out = node;
            info.v_out = par;
        } else if (residual == info.delta && info.leaving_arc != entering_arc) {
            // Tie-breaking: prefer arc closer to join (Cunningham's rule)
            // Already set, keep first found (which is closer to u_in)
        }

        node = par;
    }

    // Trace cycle from v_in to join
    node = info.v_in;
    while (node != info.join_node) {
        int arc = state.parent_arc[node];
        int par = state.parent[node];

        bool arc_points_to_parent = (state.arc_source[arc] == node);

        int residual;
        if (entering_forward) {
            // From v_in to join: we push flow "forward" through this path
            // Opposite of u_in side
            if (arc_points_to_parent) {
                residual = 1 - state.arc_flow[arc];
            } else {
                residual = state.arc_flow[arc];
            }
        } else {
            if (arc_points_to_parent) {
                residual = state.arc_flow[arc];
            } else {
                residual = 1 - state.arc_flow[arc];
            }
        }

        if (residual < info.delta) {
            info.delta = residual;
            info.leaving_arc = arc;
            info.first_side = false;
            info.u_out = node;
            info.v_out = par;
        }

        node = par;
    }

    // If leaving_arc is still entering_arc, it means entering arc itself is the bottleneck
    // This is a degenerate pivot (delta = 0)
    if (info.leaving_arc == entering_arc) {
        info.u_out = info.u_in;
        info.v_out = info.v_in;
    }

    return info;
}

// Update flow along the cycle
inline void augment_flow(NSState& state, const PivotInfo& info) {
    if (info.delta == 0) return;  // Degenerate pivot

    int entering_arc = info.entering_arc;
    bool entering_forward = (state.arc_state[entering_arc] == STATE_LOWER);

    // Update entering arc flow
    if (entering_forward) {
        state.arc_flow[entering_arc] += info.delta;
    } else {
        state.arc_flow[entering_arc] -= info.delta;
    }

    // Update flows on u_in -> join path
    int node = state.arc_source[entering_arc];  // u_in
    while (node != info.join_node) {
        int arc = state.parent_arc[node];
        int par = state.parent[node];

        bool arc_points_to_parent = (state.arc_source[arc] == node);

        if (entering_forward) {
            if (arc_points_to_parent) {
                state.arc_flow[arc] -= info.delta;
            } else {
                state.arc_flow[arc] += info.delta;
            }
        } else {
            if (arc_points_to_parent) {
                state.arc_flow[arc] += info.delta;
            } else {
                state.arc_flow[arc] -= info.delta;
            }
        }

        node = par;
    }

    // Update flows on v_in -> join path
    node = state.arc_target[entering_arc];  // v_in
    while (node != info.join_node) {
        int arc = state.parent_arc[node];
        int par = state.parent[node];

        bool arc_points_to_parent = (state.arc_source[arc] == node);

        if (entering_forward) {
            if (arc_points_to_parent) {
                state.arc_flow[arc] += info.delta;
            } else {
                state.arc_flow[arc] -= info.delta;
            }
        } else {
            if (arc_points_to_parent) {
                state.arc_flow[arc] -= info.delta;
            } else {
                state.arc_flow[arc] += info.delta;
            }
        }

        node = par;
    }
}

// Update tree structure after pivot - complete rebuild approach for correctness
inline void update_tree(NSState& state, const PivotInfo& info) {
    int entering_arc = info.entering_arc;
    int leaving_arc = info.leaving_arc;

    // Update arc states
    state.arc_state[entering_arc] = STATE_TREE;

    // Leaving arc goes to lower or upper bound
    if (state.arc_flow[leaving_arc] == 0) {
        state.arc_state[leaving_arc] = STATE_LOWER;
    } else {
        state.arc_state[leaving_arc] = STATE_UPPER;
    }

    // If entering == leaving (degenerate), no tree structure change
    if (entering_arc == leaving_arc) {
        return;
    }

    // Full tree rebuild from arc states (simple but O(n^2) - acceptable for correctness)
    // This avoids subtle bugs in incremental tree updates

    int source = state.source_node();

    // Reset tree structure
    for (int i = 0; i < state.num_nodes; ++i) {
        state.parent[i] = NO_NODE;
        state.parent_arc[i] = NO_ARC;
        state.depth[i] = -1;
    }

    // BFS from source to rebuild tree using only STATE_TREE arcs
    std::vector<int> queue;
    queue.push_back(source);
    state.depth[source] = 0;

    size_t qi = 0;
    while (qi < queue.size()) {
        int curr = queue[qi++];

        // Find all tree arcs incident to curr
        for (int arc = 0; arc < state.num_arcs; ++arc) {
            if (state.arc_state[arc] != STATE_TREE) continue;

            int u = state.arc_source[arc];
            int v = state.arc_target[arc];

            if (u == curr && state.depth[v] < 0) {
                // Arc curr -> v, v not yet visited
                state.parent[v] = curr;
                state.parent_arc[v] = arc;
                state.arc_to_parent_up[v] = false;  // Arc points from parent to child
                state.depth[v] = state.depth[curr] + 1;
                queue.push_back(v);
            } else if (v == curr && state.depth[u] < 0) {
                // Arc u -> curr, u not yet visited
                state.parent[u] = curr;
                state.parent_arc[u] = arc;
                state.arc_to_parent_up[u] = true;  // Arc points from child to parent
                state.depth[u] = state.depth[curr] + 1;
                queue.push_back(u);
            }
        }
    }

    // Rebuild subtree sizes (bottom-up)
    for (int i = 0; i < state.num_nodes; ++i) {
        state.subtree_size[i] = 1;
    }

    // Process by decreasing depth
    std::vector<std::pair<int, int>> depth_node;
    for (int i = 0; i < state.num_nodes; ++i) {
        if (state.depth[i] >= 0) {
            depth_node.emplace_back(state.depth[i], i);
        }
    }
    std::sort(depth_node.rbegin(), depth_node.rend());

    for (const auto& dn : depth_node) {
        int n = dn.second;
        int par = state.parent[n];
        if (par != NO_NODE) {
            state.subtree_size[par] += state.subtree_size[n];
        }
    }

    // Rebuild thread (DFS preorder)
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

        // Find children
        std::vector<int> children;
        for (int n = 0; n < state.num_nodes; ++n) {
            if (state.parent[n] == curr) {
                children.push_back(n);
            }
        }
        // Push in reverse order
        for (int i = static_cast<int>(children.size()) - 1; i >= 0; --i) {
            stack.push_back(children[i]);
        }
    }

    // Build thread array
    for (size_t i = 0; i < thread_order.size(); ++i) {
        int curr = thread_order[i];
        int next = thread_order[(i + 1) % thread_order.size()];
        state.thread[curr] = next;
        state.rev_thread[next] = curr;
    }
}

// Update potentials after pivot - full recompute for correctness
inline void update_potentials(NSState& state, const PivotInfo& /* info */) {
    // Simple approach: recompute all potentials from scratch
    // This is O(n) and avoids subtle bugs

    int source = state.source_node();
    state.potential[source] = 0.0;

    // Process nodes in BFS order (by depth)
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
            state.potential[node] = state.potential[par] - state.arc_cost[arc];
        } else {
            state.potential[node] = state.potential[par] + state.arc_cost[arc];
        }
    }
}

// Perform complete pivot operation
inline void do_pivot(NSState& state, const PivotInfo& info) {
    augment_flow(state, info);
    update_tree(state, info);
    update_potentials(state, info);
    ++state.pivot_count;
}

// Extract assignment from the flow solution
inline NSResult extract_assignment(const NSState& state) {
    NSResult result;
    result.assignment.resize(state.n_rows, -1);
    result.total_cost = 0.0;
    result.optimal = true;
    result.pivot_count = state.pivot_count;

    // Find matched pairs from row->col arcs with flow = 1
    for (int i = 0; i < state.n_rows; ++i) {
        for (int j = 0; j < state.n_cols; ++j) {
            int arc = state.row_to_col_arc(i, j);
            if (state.arc_flow[arc] == 1) {
                result.assignment[i] = j;
                result.total_cost += state.arc_cost[arc];
                break;
            }
        }
    }

    // Check if all rows are matched
    for (int i = 0; i < state.n_rows; ++i) {
        if (result.assignment[i] < 0) {
            result.optimal = false;
            result.status = "infeasible";
            return result;
        }
    }

    result.status = "optimal";
    return result;
}

} // namespace ns
} // namespace couplr

#endif // COUPLR_NS_PIVOT_H
