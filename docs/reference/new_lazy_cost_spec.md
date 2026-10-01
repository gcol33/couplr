# Construct a lazy cost specification

`mode` is the memory mode that resolved to this specification: `"lazy"`,
solved over every pair, or `"implicit"`, solved by generating the pairs
the answer turns out to need.

## Usage

``` r
new_lazy_cost_spec(
  left_mat,
  right_mat,
  distance,
  sigma,
  weights,
  vars,
  mode = c("lazy", "implicit")
)
```

## Value

An object of class "lazy_cost_spec".
