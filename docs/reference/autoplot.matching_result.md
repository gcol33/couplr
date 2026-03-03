# ggplot2 autoplot for matching results

Produces ggplot2-based visualizations of matching distance
distributions. Returns a ggplot object that can be further customized.

## Usage

``` r
# S3 method for class 'matching_result'
autoplot(object, type = c("histogram", "density", "ecdf"), ...)
```

## Arguments

- object:

  A matching_result object

- type:

  Type of plot: "histogram" (default), "density", or "ecdf"

- ...:

  Additional arguments (ignored)

## Value

A ggplot object

## Details

Use [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for base
graphics or `autoplot()` for ggplot2 output. The ggplot2 package must be
installed.

## Examples

``` r
# \donttest{
if (requireNamespace("ggplot2", quietly = TRUE)) {
  left <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 5))
  right <- data.frame(id = 6:10, x = c(1.1, 2.2, 3.1, 4.2, 5.1))
  result <- match_couples(left, right, vars = "x")
  ggplot2::autoplot(result)
  ggplot2::autoplot(result, type = "density")
}
# }
```
