# Suggest scaling method based on variable characteristics

Analyzes variable distributions and suggests appropriate scaling
methods.

## Usage

``` r
suggest_scaling(left, right, vars)
```

## Arguments

- left:

  Data frame of left units

- right:

  Data frame of right units

- vars:

  Character vector of variable names

## Value

A character string with the suggested scaling method: "standardize",
"range", "robust", or "none"
