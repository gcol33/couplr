# Linear assignment solver

Solve the linear assignment problem (minimum- or maximum-cost matching)
using several algorithms. Forbidden edges can be marked as `NA` or
`Inf`.

## Usage

``` r
assignment(
  cost,
  maximize = FALSE,
  method = c("auto", "jv", "hungarian", "auction", "auction_gs", "auction_scaled", "sap",
    "ssp", "csflow", "hk01", "bruteforce", "ssap_bucket", "cycle_cancel", "gabow_tarjan",
    "lapmod", "csa", "ramshaw_tarjan", "push_relabel", "orlin", "network_simplex"),
  auction_eps = NULL,
  eps = NULL
)
```

## Arguments

- cost:

  Numeric matrix; rows = tasks, columns = agents. `NA` or `Inf` entries
  are treated as forbidden assignments.

- maximize:

  Logical; if `TRUE`, maximizes the total cost instead of minimizing.

- method:

  Character string indicating the algorithm to use. Options:

  **General-purpose solvers:**

  - `"auto"` — Automatic selection based on problem characteristics
    (default)

  - `"jv"` — Jonker-Volgenant, fast general-purpose O(n³)

  - `"hungarian"` — Classic Hungarian algorithm O(n³)

  **Auction-based solvers:**

  - `"auction"` — Bertsekas auction with adaptive epsilon

  - `"auction_gs"` — Gauss-Seidel variant, good for spatial structure

  - `"auction_scaled"` — Epsilon-scaling, fastest for large dense
    problems

  **Specialized solvers:**

  - `"sap"` / `"ssp"` — Shortest augmenting path, handles sparsity well

  - `"lapmod"` — Sparse JV variant, faster when \>50\\

  - `"hk01"` — Hopcroft-Karp for binary (0/1) costs only

  - `"ssap_bucket"` — Dial's algorithm for integer costs

  - `"line_metric"` — O(n log n) for 1D assignment problems

  - `"bruteforce"` — Exact enumeration for tiny problems (n ≤ 8)

  **Advanced solvers:**

  - `"csa"` — Goldberg-Kennedy cost-scaling, often fastest for
    medium-large

  - `"gabow_tarjan"` — Bit-scaling with complementary slackness O(n³ log
    C)

  - `"cycle_cancel"` — Cycle-canceling with Karp's algorithm

  - `"csflow"` — Cost-scaling network flow

  - `"network_simplex"` — Network simplex with spanning tree
    representation

  - `"orlin"` — Orlin-Ahuja scaling O(√n · m · log(nC))

  - `"push_relabel"` — Push-relabel max-flow based solver

  - `"ramshaw_tarjan"` — Optimized for rectangular matrices (n ≠ m)

- auction_eps:

  Optional numeric epsilon for the Auction/Auction-GS methods. If
  `NULL`, an internal default (e.g., `1e-9`) is used.

- eps:

  Deprecated. Use `auction_eps`. If provided and `auction_eps` is
  `NULL`, its value is used for `auction_eps`.

## Value

An object of class `lap_solve_result`, a list with elements:

- `match` — integer vector of length `min(nrow(cost), ncol(cost))`
  giving the assigned column for each row (0 if unassigned).

- `total_cost` — numeric scalar, the objective value.

- `status` — character scalar, e.g. `"optimal"`.

- `method_used` — character scalar, the algorithm actually used.

## Details

`method = "auto"` selects an algorithm based on problem size/shape and
data characteristics:

- Very small (n≤8): `"bruteforce"` — exact enumeration

- Binary/constant costs: `"hk01"` — specialized for 0/1 costs

- Large sparse (n\>100, \>50\\

- Sparse or very rectangular: `"sap"` — handles sparsity well

- Small-medium (8\<n≤50): `"hungarian"` — provides exact dual solutions

- Medium (50\<n≤75): `"jv"` — fast general-purpose solver

- Large (n\>75): `"auction_scaled"` — fastest for large dense problems

Benchmarks show auction_scaled and JV are 100-1500x faster than
Hungarian at n=500.

## See also

- [`lap_solve()`](https://gcol33.github.io/couplr/reference/lap_solve.md)
  — Tidy interface returning tibbles

- [`lap_solve_kbest()`](https://gcol33.github.io/couplr/reference/lap_solve_kbest.md)
  — Find k-best assignments (Murty's algorithm)

- [`assignment_duals()`](https://gcol33.github.io/couplr/reference/assignment_duals.md)
  — Extract dual variables for sensitivity analysis

- [`bottleneck_assignment()`](https://gcol33.github.io/couplr/reference/bottleneck_assignment.md)
  — Minimize maximum edge cost (minimax)

- [`sinkhorn()`](https://gcol33.github.io/couplr/reference/sinkhorn.md)
  — Entropy-regularized optimal transport

## Examples

``` r
cost <- matrix(c(4,2,5, 3,3,6, 7,5,4), nrow = 3, byrow = TRUE)
res  <- assignment(cost)
res$match; res$total_cost
#> [1] 2 1 3
#> [1] 9
```
