# couplr Algorithm Roadmap

> Goal: Make couplr THE definitive LAP solver in R

## Current Inventory (v1.0.0)

### Optimal Solvers

| Algorithm | Method | Year | Complexity | Notes |
|----|----|----|----|----|
| Hungarian | `hungarian` | 1955 | O(n³) | Kuhn-Munkres; foundational |
| Jonker-Volgenant | `jv` | 1987 | O(n³) | 10-50x faster than Hungarian in practice |
| **LAPMOD** | `lapmod` | 1996 | O(n³) typical O(n²·⁵) sparse | Sparse JV variant; **NEW** |
| Auction | `auction` | 1979 | O(n²m log(nC)) | Bertsekas; parallelizable |
| Auction (Gauss-Seidel) | `auction_gs` | 1988 | O(n²m log(nC)) | Better convergence |
| Auction (Scaled) | `auction_scaled` | 1988 | O(n²m log(nC)) | ε-scaling; best for large dense |
| Shortest Successive Path | `ssp` | 1970s | O(n²m) | Dijkstra-based |
| Shortest Augmenting Path | `sap` | 1970s | O(n²m) | Handles sparse/rectangular well |
| Cost-Scaling Flow | `csflow` | 1988 | O(n²m log(nC)) | Push-relabel reduction |
| Cycle Canceling | `cycle_cancel` | 1989 | O(n²m log(nN)) | Karp’s negative cycle detection |
| **Gabow-Tarjan** | `gabow_tarjan` | 1989 | O(√nm log(nC)) | **First R implementation ever** |
| **CSA** | `csa` | 1995 | O(√n m log(nC)) | Goldberg-Kennedy cost-scaling; **NEW** |
| SSAP with Buckets | `ssap_bucket` | 1969 | O(nm + nC) | Dial’s algorithm; integer costs |
| Hopcroft-Karp | `hk01` | 1973 | O(√n m) | Binary/uniform costs only |
| Line Metric | `line_metric` | \- | O(n log n) | 1D optimal transport |
| **Ramshaw-Tarjan** | `ramshaw_tarjan` | 2012 | O(nm + n² log n) | Rectangular specialization; **NEW** |
| **Push-Relabel** | `push_relabel` | 1988 | O(n²m) | Goldberg-Tarjan preflow-push; **NEW** |
| Bruteforce | `bruteforce` | \- | O(n!) | Exhaustive; n ≤ 8 |

### Dual Variable Extraction

| Algorithm | Function | Notes |
|----|----|----|
| **JV with Duals** | [`assignment_duals()`](https://gcol33.github.io/couplr/reference/assignment_duals.md) | Returns (u, v) potentials; sensitivity analysis; **NEW** |

### Specialized Problem Variants

| Algorithm | Function | Year | Complexity | Notes |
|----|----|----|----|----|
| **Bottleneck Assignment** | [`bottleneck_assignment()`](https://gcol33.github.io/couplr/reference/bottleneck_assignment.md) | 1959 | O(E√V log U) | Minimax objective; **NEW** |

### Approximate / Soft Solvers

| Algorithm | Method | Complexity | Notes |
|----|----|----|----|
| Greedy (sorted) | `greedy_sorted` | O(n² log n) | ~5-15% gap |
| Greedy (row-best) | `greedy_row_best` | O(n²) | ~5-15% gap |
| Greedy (priority queue) | `greedy_pq` | O(n² log n) | ~3-10% gap |
| **Sinkhorn-Knopp** | [`sinkhorn()`](https://gcol33.github.io/couplr/reference/sinkhorn.md) | O(n²/ε²) | Soft assignment; entropy-regularized OT; **NEW** |

### K-Best Solvers

| Algorithm | Method | Complexity | Notes |
|----|----|----|----|
| Murty’s Algorithm | `murty` | O(k n⁴) | Lawler’s partitioning |
| Batch k-best | [`lap_solve_kbest()`](https://gcol33.github.io/couplr/reference/lap_solve_kbest.md) | O(k n⁴) | Parallelizable |

------------------------------------------------------------------------

## Planned Additions

### Tier 1: High Priority

#### 1. LAPMOD (Sparse Jonker-Volgenant)

- **Year:** 1996 (Volgenant)
- **Complexity:** O(n³) worst-case, O(n^2.5) typical on sparse
- **Implementation effort:** Medium (3-4 days)
- **Why:**
  - 10-100x speedup for n \> 5000 with \< 50% finite costs
  - The \#1 gap in R’s LAP ecosystem
  - Python’s `lap` library’s main advantage over scipy
  - Essential for large-scale bioinformatics matching
- **Key insight:** Exploits sparsity by maintaining candidate lists per
  row

#### 2. Bottleneck Assignment Problem (BAP)

- **Year:** 1961 (Gross)
- **Complexity:** O(n^2.5) or O(n² log n) with binary search
- **Implementation effort:** Low (1-2 days)
- **Why:**
  - Different objective: minimize maximum edge cost
  - Useful for fairness-constrained matching
  - No R package offers this
  - Can reuse Hopcroft-Karp as subroutine
- **Algorithm:** Binary search on threshold + bipartite matching

------------------------------------------------------------------------

### Tier 2: Completeness & Theory

#### 4. Orlin’s Algorithm

- **Year:** 1993
- **Complexity:** O(m√n log(nC)) for sparse graphs
- **Implementation effort:** High (5-7 days)
- **Why:**
  - Theoretical improvement over Gabow-Tarjan for sparse
  - Combines scaling with blocking flow techniques
  - Academic completeness
- **Note:** May not beat simpler algorithms in practice

#### 5. Ramshaw-Tarjan Rectangular Specialization

- **Year:** 2012
- **Complexity:** O(nm + n² log n) for m \>\> n
- **Implementation effort:** Medium (2-3 days)
- **Why:**
  - Elegant handling of unbalanced problems
  - Avoids dummy node overhead
  - Published in “On minimum-cost assignments in unbalanced bipartite
    graphs”

#### 6. Push-Relabel Assignment

- **Year:** 1988 (Goldberg-Tarjan base)
- **Complexity:** O(n²m)
- **Implementation effort:** Medium (2-3 days)
- **Why:**
  - Classic algorithm, good for teaching
  - Different from cost-scaling variants
  - Basis for many modern implementations

#### 7. Capacity-Scaling Assignment

- **Year:** 1972 (Edmonds-Karp style)
- **Complexity:** O(n²m log n)
- **Implementation effort:** Low-Medium (2 days)
- **Why:**
  - Bit-scaling approach
  - Bridges to network flow theory

------------------------------------------------------------------------

### Tier 3: Cutting Edge

#### 8. Sinkhorn-Knopp (Entropy-Regularized OT)

- **Year:** 2013 (Cuturi revival)
- **Complexity:** O(n² / ε²) iterations, O(n²) per iteration
- **Implementation effort:** Low (1-2 days)
- **Why:**
  - Connection to optimal transport literature
  - Differentiable (useful for ML pipelines)
  - Approximate but very fast
  - GPU-friendly (future)
- **Note:** Returns soft assignment; round for hard

#### 9. Auction with ε-Complementary Slackness Proof

- **Year:** 1991 (Bertsekas refinement)
- **Complexity:** Same as auction
- **Implementation effort:** Low (enhance existing)
- **Why:**
  - Return dual variables for optimality certificate
  - Educational value

#### 10. Network Simplex for Assignment

- **Year:** 1947 (Dantzig) / 1990s refinements
- **Complexity:** O(n³) typical, exponential worst-case
- **Implementation effort:** High (5+ days)
- **Why:**
  - Classic operations research method
  - Handles side constraints naturally
  - COIN-OR’s LEMON uses this

------------------------------------------------------------------------

### Tier 4: Research / Future

#### 11. GPU-Accelerated Hungarian

- **Year:** 2016+
- **Complexity:** O(n³/p) with p processors
- **Implementation effort:** Very High (requires CUDA/OpenCL)
- **Why:**
  - Orders of magnitude speedup for n \> 10,000
  - Active research area
- **Dependency:** Would need RcppParallel or custom CUDA kernels

#### 12. Learning-Augmented Assignment

- **Year:** 2020+
- **Complexity:** Depends on predictor quality
- **Implementation effort:** High
- **Why:**
  - Use ML to warm-start or guide search
  - Emerging research direction

------------------------------------------------------------------------

## Implementation Priority Queue

    Phase 1 (Completed):
      [✓] LAPMOD (sparse JV) ← DONE! 52 tests passing
      [✓] Bottleneck Assignment ← DONE! 59 tests passing
      [✓] Goldberg-Kennedy CSA ← DONE! 80 tests passing
      [✓] Sinkhorn-Knopp ← DONE! 48 tests passing

    Phase 2 (Completed):
      [✓] Ramshaw-Tarjan rectangular ← DONE! 77 tests passing

    Phase 3 (Completed):
      [✓] Push-Relabel Assignment ← DONE! 77 tests passing

    Phase 4 (Completed):
      [✓] Return dual variables from existing solvers ← DONE! 86 tests passing
          - assignment_duals() function for sensitivity analysis
          - Complementary slackness: u[i] + v[j] = cost[i,j] for assigned pairs
          - Strong duality: sum(u) + sum(v) = total_cost

    Future:
      [6] Orlin's algorithm
      [7] Network Simplex
      [8] GPU acceleration (if demand exists)

------------------------------------------------------------------------

## Comparison with Other Ecosystems

### R Packages

| Package       | Algorithms | Sparse? | Rectangular? | K-best? |
|---------------|------------|---------|--------------|---------|
| **couplr**    | 18+        | Yes     | Yes          | Yes     |
| lpSolve       | Simplex    | No      | Yes          | No      |
| clue          | Hungarian  | No      | Yes          | No      |
| RcppHungarian | Hungarian  | No      | No           | No      |

### Python

| Library   | Algorithms  | Notes                |
|-----------|-------------|----------------------|
| scipy     | JV          | Default, competitive |
| lap       | JV + LAPMOD | Best for sparse      |
| lapjv     | JV (SIMD)   | Fastest dense        |
| lapsolver | SAP         | Good balance         |

### C++

| Library  | Notes                          |
|----------|--------------------------------|
| OR-Tools | Production-grade, cost-scaling |
| LEMON    | Network simplex, very fast     |
| Boost    | Basic matching only            |

------------------------------------------------------------------------

## References

**Classical:** - Kuhn (1955): The Hungarian Method - Jonker & Volgenant
(1987): A shortest augmenting path algorithm - Bertsekas (1988): The
auction algorithm - Gabow & Tarjan (1989): Faster scaling algorithms

**Sparse:** - Volgenant (1996): Linear and semi-assignment problems: A
core oriented approach

**Modern:** - Ramshaw & Tarjan (2012): On minimum-cost assignments in
unbalanced bipartite graphs - Cuturi (2013): Sinkhorn distances (optimal
transport)

**Bottleneck:** - Gross (1959): The bottleneck assignment problem -
Garfinkel (1971): An improved algorithm for the bottleneck assignment
problem

------------------------------------------------------------------------

## Notes

- All new algorithms follow the existing export pattern: `_impl` in
  subdirectory, wrapper in `rcpp_interface.cpp`
- Test files go in `tests/testthat/test-assignment-{name}.R`
- Method string added to
  [`assignment()`](https://gcol33.github.io/couplr/reference/assignment.md)
  switch in `R/lap_solve.R`
- Benchmark against existing methods before merging
