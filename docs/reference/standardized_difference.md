# Calculate Standardized Difference

Computes the standardized mean difference between two groups. This is a
key metric for assessing balance in matched samples.

## Usage

``` r
standardized_difference(x1, x2, pooled = TRUE)
```

## Arguments

- x1:

  Numeric vector for group 1

- x2:

  Numeric vector for group 2

- pooled:

  Logical, if TRUE use pooled standard deviation (default), if FALSE use
  group 1 standard deviation

## Value

Numeric value representing the standardized difference

## Details

Standardized difference = (mean1 - mean2) / pooled_sd where pooled_sd =
sqrt((sd1^2 + sd2^2) / 2)

Common thresholds: less than 0.1 is excellent balance, 0.1-0.25 is good
balance, 0.25-0.5 is acceptable balance, and greater than 0.5 is poor
balance.
