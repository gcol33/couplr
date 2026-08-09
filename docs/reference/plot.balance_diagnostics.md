# Plot method for balance diagnostics

Produces a Love plot (dot plot) of standardized differences.

## Usage

``` r
# S3 method for class 'balance_diagnostics'
plot(x, type = c("love", "histogram", "variance"), threshold = 0.1, ...)
```

## Arguments

- x:

  A balance_diagnostics object

- type:

  Type of plot: "love" (default), "histogram", or "variance"

- threshold:

  Threshold line for standardized differences (default: 0.1)

- ...:

  Additional arguments passed to plotting functions

## Value

The balance_diagnostics object (invisibly)
