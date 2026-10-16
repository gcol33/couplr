# Swap left/right in a lazy cost spec

A cheap metadata field-swap (left_mat \<-\> right_mat, n_left \<-\>
n_right), unlike the dense path's [`t()`](https://rdrr.io/r/base/t.html)
matrix copy. max_distance is unaffected, and each caliper's raw values
swap sides with the units they belong to.

## Usage

``` r
transpose_lazy_cost_spec(spec)
```
