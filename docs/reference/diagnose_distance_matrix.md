# Diagnose a Distance Matrix

Comprehensive diagnostics for a distance matrix with actionable
suggestions.

## Usage

``` r
diagnose_distance_matrix(
  cost_matrix,
  left = NULL,
  right = NULL,
  vars = NULL,
  warn = TRUE
)

# S3 method for class 'distance_diagnostics'
print(x, ...)

# S3 method for class 'distance_diagnostics'
summary(object, ...)
```

## Arguments

- cost_matrix:

  Numeric matrix of distances

- left:

  Left dataset (for variable checking)

- right:

  Right dataset (for variable checking)

- vars:

  Variables used for matching

- warn:

  If TRUE, issue warnings

- x, object:

  A `distance_diagnostics` object.

- ...:

  Ignored.

## Value

A `distance_diagnostics` list with the cost distribution, variable
issues, problem variables, suggestions, and quality assessment.
[`summary()`](https://rdrr.io/r/base/summary.html) returns these
findings as `summary.distance_diagnostics`.
