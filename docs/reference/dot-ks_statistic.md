# Two-sample Kolmogorov-Smirnov statistic of weighted samples

The largest absolute difference between the two weighted empirical
distribution functions, evaluated at every observed value. Tied values
are summed into one step, so ties need no special treatment; with unit
weights this is the statistic of
[`stats::ks.test()`](https://rdrr.io/r/stats/ks.test.html). Balance is
described by the statistic alone, without a p-value.

## Usage

``` r
.ks_statistic(x, y, wx = rep(1, length(x)), wy = rep(1, length(y)))
```

## Value

The statistic, or `NA` when either sample is empty.
