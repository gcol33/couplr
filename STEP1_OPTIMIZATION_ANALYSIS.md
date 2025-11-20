# Step 1 Performance Analysis and Optimizations

## Summary

Your Step 1 implementation has **3 critical bottlenecks** that make it slower than the paper's O(√nm) target:

1. **O(n) augmentation** instead of O(path_length)
2. **Sequential BFS** (O(√n) iterations) instead of ONE DFS pass
3. **Full graph rebuild** every call instead of incremental updates

## Detailed Analysis

### Bottleneck #1: `augment_along_path()` - Lines 155-201

**Current Code:**
```cpp
void augment_along_path(const std::vector<std::pair<int,int>>& edges,
                        MatchVec& row_match,
                        MatchVec& col_match) {
    // 1. Build entire matching into set M - O(n)
    std::set<std::pair<int,int>> M;
    for (int i = 0; i < row_match.size(); ++i) {
        if (row_match[i] != NIL) M.emplace(i, row_match[i]);
    }

    // 2. Symmetric difference operations - O(n)
    std::set<std::pair<int,int>> P(edges.begin(), edges.end());
    std::set<std::pair<int,int>> M_sym;
    // ... set operations ...

    // 3. Clear and rebuild - O(n)
    // ... clear arrays ...
    // ... rebuild from M_sym ...
}
```

**Complexity:** O(n) per path
- With O(√n) paths found: **O(n·√n) = O(n^1.5) total augmentation time**

**Paper's Approach (Page 6):**
> "Augmenting the matching along P means enlarging the matching M to M ⊕ P"

This is just a **simple flip** along the path edges!

**Optimized Code:**
```cpp
void augment_along_path_fast(const std::vector<std::pair<int,int>>& path,
                              std::vector<int>& row_match,
                              std::vector<int>& col_match) {
    for (const auto& edge : path) {
        int i = edge.first;
        int j = edge.second;
        row_match[i] = j;
        col_match[j] = i;
    }
}
```

**Complexity:** O(path_length)
- Typical path length: O(√n)
- Total for O(√n) paths: **O(n) total augmentation time**

**Speedup:** **O(√n) faster!**

---

### Bottleneck #2: Sequential Path Finding - Lines 342-361

**Current Code:**
```cpp
std::vector<std::vector<std::pair<int,int>>>
find_maximal_augmenting_paths(...) {
    std::vector<bool> banned_row(n, false);
    std::vector<bool> banned_col(m, false);
    std::vector<std::vector<std::pair<int,int>>> paths;

    while (true) {
        // BFS to find ONE path - O(m)
        auto path = find_one_augmenting_path_eq(...);
        if (path.empty()) break;

        paths.push_back(path);

        // Ban vertices in this path
        for (const auto& e : path) {
            banned_row[e.first] = true;
            banned_col[e.second] = true;
        }
    }
    return paths;
}
```

**Complexity:** O(√n) BFS iterations × O(m) per BFS = **O(√n·m)**

This is the **MAIN BOTTLENECK** in your implementation!

**Paper's Approach (Page 6, Step 1):**
> "Find a maximal set A of vertex-disjoint augmenting paths of eligible edges"

The paper uses a **single DFS from all free vertices simultaneously** (similar to Hopcroft-Karp).

**Optimized Approach:**
```cpp
// Single DFS pass marking vertices as visited
std::function<bool(int)> dfs = [&](int i) -> bool {
    visited_row[i] = true;
    for (int j : eq_graph[i]) {
        if (visited_col[j]) continue;
        visited_col[j] = true;

        if (col_match[j] == NIL) {
            // Found augmenting path!
            return true;
        }

        if (dfs(col_match[j])) {
            return true;
        }
    }
    return false;
};

// Start from ALL free rows
for (int i = 0; i < n; ++i) {
    if (row_match[i] == NIL) dfs(i);
}
```

**Complexity:** **O(m)** - Single graph traversal!

**Speedup:** **O(√n) faster!**

---

### Bottleneck #3: Full Graph Rebuild - Line 796

**Current Code:**
```cpp
bool apply_step1(...) {
    // EVERY CALL: Rebuild entire equality graph - O(nm)
    auto eq_graph = build_equality_graph(cost, row_match, y_u, y_v);
    // ...
}
```

**In `match_gt` loop:** Step 1 is called O(√n) times

**Total graph building cost:** O(√n) × O(nm) = **O(√n·nm)**

For **dense graphs** (m = n²), this is **O(n^2.5)** - very expensive!

**Optimization Strategies:**

**Option A: Incremental Updates** (Most efficient)
- Only rebuild adjacency lists for vertices whose duals changed
- After Step 1: only columns on augmenting paths had y_v decreased
- Only need to update O(√n) rows × m columns = O(√n·m) edges

**Option B: Sparse Graph Assumption** (Practical)
- For sparse graphs (m = O(n)), building is only O(n²)
- Combined with other optimizations, this may be acceptable

**Option C: Cache Eligible Edges** (Memory tradeoff)
- Maintain a hash set of all eligible edges
- Update only when duals change

---

## Combined Complexity Analysis

### Current Implementation:
```
Per Step 1 call:
1. build_equality_graph()       : O(nm)
2. find_maximal_paths (seq BFS) : O(√n · m)
3. augment_along_path (n times) : O(n√n)
-------------------------------------------
Total per Step 1:                 O(nm + √n·m + n√n)
                                 = O(nm)  [for dense graphs]

Total for match_gt (√n iterations):
                                 = O(√n · nm) = O(n^2.5)
```

### Paper's Target Complexity:
```
Per Step 1 call:
1. Build equality graph          : O(m)   [assuming sparse]
2. DFS for all maximal paths     : O(m)
3. Fast augmentation             : O(n)
-------------------------------------------
Total per Step 1:                 O(m + n)

Total for match_gt (√n iterations):
                                 = O(√n(m + n)) = O(√nm)  ✓
```

### Optimized Implementation:
```
Per Step 1 call:
1. build_equality_graph()        : O(nm)  [still slow for dense]
2. DFS for all maximal paths     : O(m)   [FIXED!]
3. Fast augmentation             : O(n)   [FIXED!]
-------------------------------------------
Total per Step 1:                 O(nm)   [graph building dominates]

Total for match_gt:               O(√n · nm) = O(n^2.5)  [still quadratic]

BUT: For sparse graphs (m = O(n)):
                                 = O(√n · n²) = O(n^2.5) → O(n^1.5)
```

## Practical Speedup Expectations

### Small Graphs (n < 1000):
- **2-5x faster**
- Main win: Eliminating O(√n) BFS calls
- Graph building is fast enough

### Medium Graphs (n = 1,000 - 10,000):
- **5-20x faster**
- BFS elimination becomes critical
- Fast augmentation avoids set operations

### Large Dense Graphs (n > 10,000, m ≈ n²):
- **10-50x faster**
- Would benefit from incremental graph updates
- Memory access patterns matter more

## Recommended Implementation Priority

### Phase 1: Quick Wins (Implement Now) ⭐
1. **Replace `augment_along_path` with fast version**
   - 30 minutes to implement
   - O(√n) speedup guaranteed
   - No risk

2. **Replace sequential BFS with single DFS**
   - 1-2 hours to implement
   - O(√n) speedup guaranteed
   - Biggest single improvement

**Expected combined speedup:** 5-20x on typical graphs

### Phase 2: Advanced Optimization (If Needed)
3. **Incremental equality graph updates**
   - 4-8 hours to implement correctly
   - Additional 2-5x speedup on dense graphs
   - More complex, higher risk

## Testing Strategy

1. **Unit test each optimization separately**
   - Test `augment_along_path_fast` on small examples
   - Test `find_maximal_paths_dfs` produces same results

2. **Benchmark on graphs of increasing size**
   ```r
   sizes <- c(100, 500, 1000, 5000, 10000)
   for (n in sizes) {
       cost <- matrix(runif(n*n), n, n)

       # Original
       t1 <- system.time(solve_gabow_tarjan(cost))

       # Optimized
       t2 <- system.time(solve_gabow_tarjan_optimized(cost))

       cat(sprintf("n=%d: speedup = %.2fx\n", n, t1[3]/t2[3]))
   }
   ```

3. **Verify correctness**
   - Compare costs with Hungarian algorithm
   - Check 1-feasibility conditions
   - Use existing test suite

## Code Changes Required

See `optimized_step1.cpp` for full implementation.

### Key files to modify:
1. `src/gabow_tarjan/utils_gabow_tarjan.cpp`
   - Replace `augment_along_path()` (lines 155-201)
   - Replace `find_maximal_augmenting_paths()` (lines 322-364)

2. `src/gabow_tarjan/utils_gabow_tarjan.h`
   - Update function signatures if needed

### Estimated time:
- Phase 1 (quick wins): **2-3 hours** including testing
- Phase 2 (incremental updates): **8-12 hours** including testing

## References

- **Paper:** Gabow & Tarjan (1989), page 6
  - Step 1 description: "Find a maximal set A of vertex-disjoint augmenting paths"
  - Augmentation: "M ⊕ P" (symmetric difference)

- **Hopcroft-Karp Algorithm** (1973)
  - Original O(√n) phases with DFS pathfinding
  - Your Step 1 should be nearly identical to this

- **Your existing tests:**
  - `tests/testthat/gabow-tarjan/test_gabow_tarjan_moduleD.R`
  - Use these to verify correctness after changes
