# Check cost distribution for problems

Examines the distance matrix for common issues and provides helpful
warnings.

## Usage

``` r
check_cost_distribution(cost_matrix, threshold_zero = 1e-10, warn = TRUE)
```

## Arguments

- cost_matrix:

  Numeric matrix of distances

- threshold_zero:

  Threshold for considering distance "zero" (default: 1e-10)

- warn:

  If TRUE, issue warnings for problems found

## Value

List with diagnostic information
