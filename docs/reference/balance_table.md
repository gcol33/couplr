# Create Balance Table

Formats balance diagnostics into a clean table for display or export.

## Usage

``` r
balance_table(balance, digits = 3)
```

## Arguments

- balance:

  A balance_diagnostics object from
  [`balance_diagnostics()`](https://gcol33.github.io/couplr/reference/balance_diagnostics.md)

- digits:

  Number of decimal places for rounding (default: 3)

## Value

A tibble with formatted balance statistics
