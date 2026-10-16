# Add the standardized differences before matching to a balance table

Each variable's standardized difference over all left and all right
units, the reference a matched sample's balance is read against.

## Usage

``` r
.with_unmatched_balance(var_stats, left_all, right_all)
```

## Value

`var_stats` with the column `std_diff_unmatched`.
