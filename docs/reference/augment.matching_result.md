# Augment Matching Results with Original Data (broom-style)

S3 method for augmenting matching results following the broom package
conventions. This is a thin wrapper around
[`join_matched()`](https://gcol33.github.io/couplr/reference/join_matched.md)
with sensible defaults for quick exploration.

## Usage

``` r
# S3 method for class 'matching_result'
augment(x, left, right, ...)
```

## Arguments

- x:

  A matching_result object

- left:

  The original left dataset

- right:

  The original right dataset

- ...:

  Additional arguments passed to
  [`join_matched()`](https://gcol33.github.io/couplr/reference/join_matched.md)

## Value

A tibble with matched pairs and original data (see
[`join_matched()`](https://gcol33.github.io/couplr/reference/join_matched.md))

## Details

This method follows the
[`augment()`](https://gcol33.github.io/couplr/reference/augment.md)
convention from the broom package, making it easy to integrate couplr
into tidymodels workflows. It's equivalent to calling
[`join_matched()`](https://gcol33.github.io/couplr/reference/join_matched.md)
with default parameters.

If the broom package is not loaded, you can use
[`couplr::augment()`](https://gcol33.github.io/couplr/reference/augment.md)
to access this function.

## Examples

``` r
left <- data.frame(
  id = 1:5,
  treatment = 1,
  age = c(25, 30, 35, 40, 45)
)

right <- data.frame(
  id = 6:10,
  treatment = 0,
  age = c(24, 29, 36, 41, 44)
)

result <- match_couples(left, right, vars = "age")
couplr::augment(result, left, right)
#> # A tibble: 5 × 9
#>   pair_id left_id right_id distance .age_diff treatment_left age_left
#>     <int>   <int>    <int>    <dbl>     <dbl>          <dbl>    <dbl>
#> 1       1       1        6        1         1              1       25
#> 2       2       2        7        1         1              1       30
#> 3       3       3        8        1        -1              1       35
#> 4       4       4        9        1        -1              1       40
#> 5       5       5       10        1         1              1       45
#> # ℹ 2 more variables: treatment_right <dbl>, age_right <dbl>
```
