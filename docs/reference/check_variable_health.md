# Check variable health for matching

Analyzes variables for common problems that can affect matching quality:
constant columns, high missingness, extreme skewness, and outliers.

## Usage

``` r
check_variable_health(
  left,
  right,
  vars,
  high_missingness_threshold = 0.5,
  low_variance_threshold = 1e-06
)
```

## Arguments

- left:

  Data frame of left units

- right:

  Data frame of right units

- vars:

  Character vector of variable names to check

- high_missingness_threshold:

  Threshold for high missingness warning (default: 0.5)

- low_variance_threshold:

  Threshold for nearly-constant variables (default: 1e-6)

## Value

A list with class "variable_health" containing:

- `summary`: Tibble with per-variable diagnostics

- `issues`: List of detected issues with severity levels

- `exclude_vars`: Variables that should be excluded

- `warnings`: Human-readable warnings
