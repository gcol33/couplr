# Compute paired (not cross) distances for specific matched pairs

Given matched row/column index pairs (as produced by a solve),
recomputes each pair's distance directly from left_mat/right_mat. This
is cheap regardless of n_left/n_right: the number of matched pairs never
exceeds min(n_left, n_right), so this never approaches the O(n\*m) cost
the lazy path exists to avoid. Mirrors compute_distance_matrix()'s
per-metric formulas exactly, but pairwise rather than all-pairs.

## Usage

``` r
lazy_pair_distances(spec, matched_rows, matched_cols)
```

## Value

Numeric vector of length length(matched_rows).
