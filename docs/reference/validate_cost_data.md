# Validate and prepare cost data

Internal helper that ensures a numeric, non-empty cost matrix.

## Usage

``` r
validate_cost_data(x, forbidden = NA)
```

## Arguments

- x:

  Cost matrix or data frame

- forbidden:

  Value representing forbidden assignments (use NA or Inf)

## Value

Numeric cost matrix
