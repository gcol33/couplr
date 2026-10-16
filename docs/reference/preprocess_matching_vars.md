# Check Matching Variables and Choose Scaling

Main preprocessing function that orchestrates variable health checks,
and automatic scaling selection. The returned data frames retain their
input values;
[`match_couples()`](https://gillescolling.com/couplr/reference/match_couples.md)
applies the selected scaling when building distances.

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

# S3 method for class 'variable_health'
print(x, ...)

# S3 method for class 'variable_health'
summary(object, ...)

# S3 method for class 'preprocessing_result'
print(x, ...)

# S3 method for class 'preprocessing_result'
summary(object, ...)
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

- x:

  A preprocessing_result object

- ...:

  Additional arguments (ignored)

- object:

  A preprocessing result or its nested variable-health report.

## Value

A list with class "preprocessing_result" containing:

- `left`: Input left data frame

- `right`: Input right data frame

- `vars`: Final variable names (after exclusions)

- `health`: Variable health diagnostics

- `scaling_method`: Selected scaling method

- `excluded_vars`: Variables that were excluded

- `warnings`: List of warnings issued

[`summary()`](https://rdrr.io/r/base/summary.html) returns a
`summary.preprocessing_result` with the retained variables, group sizes,
scaling choice, exclusions, warnings, and the detailed variable-health
report.
