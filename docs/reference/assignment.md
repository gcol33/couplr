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

  Character string indicating the algorithm to use. One of `"auto"`,
  `"jv"`, `"hungarian"`, `"auction"`, `"auction_gs"`, `"sap"`, `"ssp"`,
  `"csflow"`, `"hk01"`, `"lapmod"`, `"csa"`, `"orlin"`, or
  `"bruteforce"`. `"ssp"` is accepted as an alias for `"sap"`.
  `"lapmod"` is a sparse variant of JV, faster for large matrices with
  \>50% NA/Inf. `"csa"` is Goldberg-Kennedy cost-scaling, often fastest
  for medium-large problems. `"orlin"` is the Orlin-Ahuja scaling
  algorithm with O(sqrt(n) \* m \* log(nC)) complexity.

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

## Examples

``` r
cost <- matrix(c(4,2,5, 3,3,6, 7,5,4), nrow = 3, byrow = TRUE)
res  <- assignment(cost)
res$match; res$total_cost
```
