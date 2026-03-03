# ggplot2 autoplot for balance diagnostics

Produces ggplot2-based balance assessment plots. Returns a ggplot
object.

## Usage

``` r
# S3 method for class 'balance_diagnostics'
autoplot(
  object,
  type = c("love", "histogram", "variance"),
  threshold = 0.1,
  ...
)
```

## Arguments

- object:

  A balance_diagnostics object

- type:

  Type of plot: "love" (default), "histogram", or "variance"

- threshold:

  Threshold for standardized differences (default: 0.1)

- ...:

  Additional arguments (ignored)

## Value

A ggplot object

## Examples

``` r
# \donttest{
if (requireNamespace("ggplot2", quietly = TRUE)) {
  set.seed(42)
  left <- data.frame(id = 1:10, age = rnorm(10, 45, 10),
                     income = rnorm(10, 50000, 15000))
  right <- data.frame(id = 11:30, age = rnorm(20, 47, 10),
                       income = rnorm(20, 52000, 15000))
  result <- match_couples(left, right, vars = c("age", "income"))
  bal <- balance_diagnostics(result, left, right, vars = c("age", "income"))
  ggplot2::autoplot(bal)
}
# }
```
