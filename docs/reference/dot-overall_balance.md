# Summarise a per-variable balance table

The three headline numbers every balance object reports, read off the
per-variable table each of them builds.

## Usage

``` r
.overall_balance(var_stats, n_vars)
```

## Arguments

- var_stats:

  A per-variable balance table, as
  [`calculate_var_balance()`](https://gillescolling.com/couplr/reference/calculate_var_balance.md)
  rows bound together.

- n_vars:

  How many variables the balance was asked about. This is the number
  asked for rather than `nrow(var_stats)`, which is smaller when a
  variable produced no statistics.

## Value

List with `mean_abs_std_diff`, `max_abs_std_diff`, `pct_large_imbalance`
and `n_vars`.
