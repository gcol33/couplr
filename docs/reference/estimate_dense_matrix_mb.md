# Estimate dense cost-matrix memory footprint in megabytes

The true peak footprint is not 8 bytes/cell: `matrix(0, n, m)` at the R
level (8B) is followed by `rcpp_to_cost_matrix()`'s copy into a
`lap::CostMatrix` (8B data + 4B mask), then `prepare_for_solve()`'s
unconditional copy (another 12B), and a possible
[`t()`](https://rdrr.io/r/base/t.html) transpose copy (8B). Several of
these are transient but can coexist during GC lag. A conservative
multiplier avoids systematically under-warning; `n`/`m` are coerced to
`double` before multiplying so the estimate itself can't overflow the
way `lap::CostMatrix`'s old `int` flat-index arithmetic did.

## Usage

``` r
estimate_dense_matrix_mb(n, m, overhead_factor = 4)
```
