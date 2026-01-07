# Network Simplex Implementation Plan

## Algorithm Overview

The Network Simplex algorithm solves the minimum-cost flow problem,
specialized for assignment: - Model n×m assignment as bipartite network
flow - Basis = spanning tree (not matrix) - Dual variables = node
potentials - 200-300x faster than general simplex on networks

**Complexity:** O(VE log V log(VC)) polynomial; O(n³) typical in
practice

## Network Structure for Assignment

             source (node 0, supply = n)
                |
        +-------+-------+
        |       |       |
       row₀   row₁ ... row_{n-1}   (nodes 1..n, transshipment)
        |\      |\      |
        | \     | \     |
       col₀  col₁ ... col_{m-1}    (nodes n+1..n+m, transshipment)
        |       |       |
        +-------+-------+
                |
             sink (node n+m+1, demand = n)

**Arcs:** - source → row_i: cost=0, cap=1, lower=1 (n arcs) - row_i →
col_j: cost=C\[i,j\], cap=1, lower=0 (n×m arcs) - col_j → sink: cost=0,
cap=1, lower=0 (m arcs)

Total: 2n + m + n×m arcs, n + m + 2 nodes

## File Structure

    src/solvers/network_simplex/
    ├── ns_types.h                    # Data structures, constants
    ├── ns_graph.h                    # Build assignment network
    ├── ns_init.h                     # Big-M initialization
    ├── ns_pivot.h                    # Pivot operations (all-in-one)
    └── solve_network_simplex.cpp     # Main algorithm loop

## Implementation Parts

### Part 1: ns_types.h

**State structure:**

``` cpp
struct NSState {
    // Problem size
    int n_rows, n_cols;
    int num_nodes;           // n + m + 2
    int num_arcs;            // 2n + m + n*m

    // Arc data (indexed 0..num_arcs-1)
    std::vector<int> arc_source;    // Source node of arc
    std::vector<int> arc_target;    // Target node of arc
    std::vector<double> arc_cost;   // Cost of arc
    std::vector<int> arc_flow;      // Current flow (0 or 1)

    // Arc state: 0=LOWER, 1=TREE, 2=UPPER
    std::vector<int> arc_state;

    // Spanning tree (threaded representation)
    std::vector<int> parent;        // Parent node (-1 for root)
    std::vector<int> parent_arc;    // Arc connecting to parent
    std::vector<int> thread;        // Next node in DFS preorder
    std::vector<int> rev_thread;    // Previous in DFS preorder
    std::vector<int> depth;         // Depth in tree
    std::vector<int> subtree_size;  // Size of subtree

    // Dual variables
    std::vector<double> potential;  // Node potentials π[i]

    // Assignment result
    std::vector<int> row_to_col;    // row i matched to col j
};
```

**Constants:**

``` cpp
const int STATE_LOWER = 0;
const int STATE_TREE = 1;
const int STATE_UPPER = 2;

const int ROOT_NODE = -1;
const double BIG_M = 1e12;
```

### Part 2: ns_graph.h

**Functions:**

``` cpp
// Node indexing:
//   0 = source
//   1..n = rows
//   n+1..n+m = columns
//   n+m+1 = sink

inline int row_node(int i) { return i + 1; }
inline int col_node(int j, int n) { return n + 1 + j; }
inline int source_node() { return 0; }
inline int sink_node(int n, int m) { return n + m + 1; }

// Arc indexing:
//   0..n-1 = source->row arcs
//   n..n+n*m-1 = row->col arcs (arc n + i*m + j for row i, col j)
//   n+n*m..n+n*m+m-1 = col->sink arcs

void build_assignment_network(NSState& state,
                               const double* cost_matrix,
                               int n_rows, int n_cols);
```

### Part 3: ns_init.h

**Big-M Method:** - Add artificial arc from each node to a super-root
with cost BIG_M - Initial tree = star from super-root - Or simpler: use
source as root, artificial arcs to unconnected nodes

``` cpp
void initialize_spanning_tree(NSState& state);
void compute_initial_potentials(NSState& state);
```

**Potential computation:** - Set π\[root\] = 0 - For each tree arc
(u,v): π\[v\] = π\[u\] + cost\[u,v\] if arc points u→v in tree -
Traverse in thread order

### Part 4: ns_pivot.h

**Key operations:**

``` cpp
// Reduced cost of arc e
inline double reduced_cost(const NSState& state, int arc) {
    int u = state.arc_source[arc];
    int v = state.arc_target[arc];
    return state.arc_cost[arc] - state.potential[u] + state.potential[v];
}

// Find entering arc (block search)
int find_entering_arc(const NSState& state);

// Find cycle and leaving arc when adding entering arc
struct PivotInfo {
    int entering_arc;
    int leaving_arc;
    int join_node;      // LCA of arc endpoints
    int delta;          // Flow augmentation (0 or 1 for assignment)
    bool change_root;   // Whether tree structure changes
};

PivotInfo find_leaving_arc(const NSState& state, int entering_arc);

// Execute pivot
void do_pivot(NSState& state, const PivotInfo& info);
```

**Pivot steps:** 1. Find join (LCA) of entering arc endpoints 2. Trace
cycle from both endpoints to join 3. Find leaving arc (min residual on
cycle) 4. Augment flow around cycle 5. Update tree: remove leaving arc,
add entering arc 6. Update thread order for moved subtree 7. Update
potentials in moved subtree

### Part 5: solve_network_simplex.cpp

``` cpp
NSResult solve_network_simplex_impl(const Rcpp::NumericMatrix& cost_matrix) {
    NSState state;
    int n = cost_matrix.nrow();
    int m = cost_matrix.ncol();

    // Build network
    build_assignment_network(state, &cost_matrix[0], n, m);

    // Initialize spanning tree and potentials
    initialize_spanning_tree(state);
    compute_initial_potentials(state);

    // Main loop
    int max_iter = state.num_arcs * state.num_nodes;
    for (int iter = 0; iter < max_iter; ++iter) {
        int entering = find_entering_arc(state);
        if (entering < 0) break;  // Optimal!

        PivotInfo info = find_leaving_arc(state, entering);
        do_pivot(state, info);
    }

    // Extract solution
    return extract_assignment(state);
}
```

## Critical Implementation Details

### 1. Thread Representation

The spanning tree is stored implicitly via DFS preorder threading: -
`thread[u]` = next node in DFS preorder - `rev_thread[u]` = previous
node in DFS preorder - `subtree_size[u]` = nodes in subtree rooted at u

This enables O(subtree_size) traversal of any subtree without explicit
child pointers.

### 2. Anti-Cycling (Strongly Feasible Basis)

Use Cunningham’s leaving arc rule: - Among arcs with minimum residual,
choose the one closest to join - Guarantees at most O(n) consecutive
degenerate pivots

### 3. Block Search Pricing

- Divide arcs into blocks of size B = ceil(sqrt(num_arcs))
- In each block, find arc with most negative reduced cost
- If found, use it; otherwise move to next block
- Balances work per iteration vs number of iterations

### 4. Assignment Simplifications

Since all capacities = 1 and demands = 1: - Flow is always 0 or 1 -
Residual capacity is 1-flow (forward) or flow (backward) - Delta
(augmentation amount) is always 0 or 1 - Degenerate pivots (delta=0) are
common

### 5. Potential Updates

After pivot, only potentials in the moved subtree change:

``` cpp
// sigma = new_potential - old_potential for entering arc endpoint
double sigma = state.potential[u_in] + state.arc_cost[entering]
             - state.potential[v_in];

// Update all nodes in moved subtree
for (int v = moved_root; v != end_of_subtree; v = state.thread[v]) {
    state.potential[v] += sigma;
}
```

## Testing Strategy

1.  **Correctness tests:**
    - Compare with JV solver on random matrices
    - Test edge cases: 1×1, rectangular, ties in costs
2.  **Optimality verification:**
    - Check all reduced costs ≥ 0 for lower-bound arcs
    - Check all reduced costs ≤ 0 for upper-bound arcs
    - Verify complementary slackness
3.  **Performance tests:**
    - Benchmark against JV, auction, Orlin-Ahuja
    - Measure pivot count vs problem size

## References

- Király & Kovács (2012): “Efficient implementation of minimum-cost flow
  algorithms”
- LEMON library: network_simplex.h
- NetworkX: networksimplex.py
- Ahuja, Magnanti, Orlin: “Network Flows: Theory, Algorithms, and
  Applications”
