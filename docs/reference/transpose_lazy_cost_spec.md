# Swap left/right in a lazy cost spec

A cheap metadata field-swap (left_mat \<-\> right_mat, n_left \<-\>
n_right), unlike the dense path's [`t()`](https://rdrr.io/r/base/t.html)
matrix copy. Calipers/max_distance are unaffected: a caliper's
`var_index` refers to a matching VARIABLE (a column shared by both
sides), not a left/right unit index, so it does not need to change when
the roles of left/right are swapped.

## Usage

``` r
transpose_lazy_cost_spec(spec)
```
