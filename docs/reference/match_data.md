# Extract Analysis-Ready Data from Matching Results

A generic function that converts any couplr matching result into a
single analysis-ready data frame with `weights`, `subclass`, and
`distance` columns. This is the couplr equivalent of MatchIt's
`match.data()`.

## Usage

``` r
match_data(result, ...)

# S3 method for class 'matching_result'
match_data(result, left, right, left_id = "id", right_id = "id", ...)

# S3 method for class 'full_matching_result'
match_data(result, left, right, left_id = "id", right_id = "id", ...)

# S3 method for class 'cem_result'
match_data(result, left, right, left_id = "id", right_id = "id", ...)

# S3 method for class 'subclass_result'
match_data(result, data = NULL, ...)
```

## Arguments

- result:

  A couplr result object (matching_result, full_matching_result,
  cem_result, or subclass_result)

- ...:

  Additional arguments passed to methods

- left:

  Data frame of left (treated) units

- right:

  Data frame of right (control) units

- left_id:

  Name of ID column in left (default: `"id"`)

- right_id:

  Name of ID column in right (default: `"id"`)

- data:

  Data frame containing all units (for CEM and subclassification, left
  and right are not always needed separately)

## Value

A tibble with all original variables plus standardized columns:

- id:

  Unit identifier

- treatment:

  1 for left/treated, 0 for right/control

- weights:

  Matching weights

- subclass:

  Matched group/stratum identifier

- distance:

  Matching distance (where applicable)

## Details

The output format is compatible with downstream packages like cobalt,
WeightIt, and marginaleffects. The stacked (long) format with
`treatment` and `weights` columns is the standard layout expected by
these tools.

## Examples

``` r
set.seed(42)
left <- data.frame(id = 1:5, age = c(25, 35, 45, 55, 65))
right <- data.frame(id = 6:15, age = runif(10, 20, 70))
result <- match_couples(left, right, vars = "age")
md <- match_data(result, left, right)
head(md)
```
