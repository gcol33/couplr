# Preprocess matching variables with automatic checks and scaling

Main preprocessing function that orchestrates variable health checks,
categorical encoding, and automatic scaling selection.

## Usage

``` r
preprocess_matching_vars(
  left,
  right,
  vars,
  auto_scale = TRUE,
  scale_method = "auto",
  check_health = TRUE,
  remove_problematic = TRUE,
  verbose = TRUE
)
```

## Arguments

- left:

  Data frame of left units

- right:

  Data frame of right units

- vars:

  Character vector of variable names

- auto_scale:

  Logical, whether to perform automatic preprocessing (default: TRUE)

- scale_method:

  Scaling method: "auto", "standardize", "range", "robust", or FALSE

- check_health:

  Logical, whether to check variable health (default: TRUE)

- remove_problematic:

  Logical, automatically exclude constant/all-NA variables (default:
  TRUE)

- verbose:

  Logical, whether to print warnings (default: TRUE)

## Value

A list with class "preprocessing_result" containing:

- `left`: Preprocessed left data frame

- `right`: Preprocessed right data frame

- `vars`: Final variable names (after exclusions)

- `health`: Variable health diagnostics

- `scaling_method`: Selected scaling method

- `excluded_vars`: Variables that were excluded

- `warnings`: List of warnings issued
