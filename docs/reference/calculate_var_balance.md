# Calculate Variable-Level Balance Statistics

Calculate Variable-Level Balance Statistics

## Usage

``` r
calculate_var_balance(
  left_vals,
  right_vals,
  var_name,
  w_left = NULL,
  w_right = NULL
)
```

## Arguments

- left_vals:

  Numeric vector of values from left group

- right_vals:

  Numeric vector of values from right group

- var_name:

  Character, name of the variable

- w_left:

  Optional numeric vector of weights for `left_vals` (e.g. stratum
  weights from full matching, CEM, or subclassification). Defaults to
  equal weights.

- w_right:

  Optional numeric vector of weights for `right_vals`.

## Value

List with balance statistics for this variable
