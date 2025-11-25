# Diagnose distance matrix and suggest fixes

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

## Value

List with diagnostic results and suggestions
