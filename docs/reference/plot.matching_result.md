# Plot method for matching results

Produces a histogram of pairwise distances from a matching result.

## Usage

``` r
# S3 method for class 'matching_result'
plot(x, type = c("histogram", "density", "ecdf"), ...)
```

## Arguments

- x:

  A matching_result object

- type:

  Type of plot: "histogram" (default), "density", or "ecdf"

- ...:

  Additional arguments passed to plotting functions

## Value

The matching_result object (invisibly)
