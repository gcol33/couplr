# Solve a lazy_cost_spec (memory_mode = "lazy" backend for assignment())

Mirrors assignment()'s contract (same result shape) but computes costs
on demand from the underlying feature data instead of a materialized
matrix. Only "jv" and "auction" are supported – every other method is
fundamentally dense (repeated full-matrix scans, or an algorithm not yet
templated for a lazy cost source) and gets a clear error here rather
than a silent dense fallback that would defeat the point of
`memory_mode = "lazy"`.

## Usage

``` r
.assignment_lazy(cost, maximize = FALSE, method = "auto", auction_eps = NULL)
```
