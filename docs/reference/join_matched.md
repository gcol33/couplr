# Join Matched Pairs with Original Data

Creates an analysis-ready dataset by joining matched pairs with
variables from the original left and right datasets. This eliminates the
need for manual joins and provides a convenient format for downstream
analysis.

## Usage

``` r
join_matched(
  result,
  left,
  right,
  left_vars = NULL,
  right_vars = NULL,
  left_id = "id",
  right_id = "id",
  suffix = c("_left", "_right"),
  include_distance = TRUE,
  include_pair_id = TRUE,
  include_block_id = TRUE
)
```

## Arguments

- result:

  A matching_result object from
  [`match_couples()`](https://gcol33.github.io/couplr/reference/match_couples.md)
  or
  [`greedy_couples()`](https://gcol33.github.io/couplr/reference/greedy_couples.md)

- left:

  The original left dataset

- right:

  The original right dataset

- left_vars:

  Character vector of variable names to include from left. If NULL
  (default), includes all variables except the ID column.

- right_vars:

  Character vector of variable names to include from right. If NULL
  (default), includes all variables except the ID column.

- left_id:

  Name of the ID column in left dataset (default: "id")

- right_id:

  Name of the ID column in right dataset (default: "id")

- suffix:

  Character vector of length 2 specifying suffixes for left and right
  variables (default: c("\_left", "\_right"))

- include_distance:

  Include the matching distance in output (default: TRUE)

- include_pair_id:

  Include pair_id column (default: TRUE)

- include_block_id:

  Include block_id if blocking was used (default: TRUE)

## Value

A tibble with one row per matched pair, containing:

- `pair_id`: Sequential pair identifier (if include_pair_id = TRUE)

- `left_id`: ID from left dataset

- `right_id`: ID from right dataset

- `distance`: Matching distance (if include_distance = TRUE)

- `block_id`: Block identifier (if blocking used and include_block_id =
  TRUE)

- Variables from left dataset (with left suffix)

- Variables from right dataset (with right suffix)

## Details

This function simplifies the common workflow of joining matched pairs
with original data. Instead of manually merging result\$pairs with left
and right datasets, `join_matched()` handles the joins automatically and
applies consistent naming conventions.

When variables appear in both left and right datasets, suffixes are
appended to distinguish them (e.g., "age_left" and "age_right"). This
makes it easy to compute differences or use both values in models.

## Examples

``` r
# Basic usage
left <- data.frame(
  id = 1:5,
  treatment = 1,
  age = c(25, 30, 35, 40, 45),
  income = c(45000, 52000, 48000, 61000, 55000)
)

right <- data.frame(
  id = 6:10,
  treatment = 0,
  age = c(24, 29, 36, 41, 44),
  income = c(46000, 51500, 47500, 60000, 54000)
)

result <- match_couples(left, right, vars = c("age", "income"))
matched_data <- join_matched(result, left, right)
head(matched_data)

# Specify which variables to include
matched_data <- join_matched(
  result, left, right,
  left_vars = c("treatment", "age", "income"),
  right_vars = c("age", "income"),
  suffix = c("_treated", "_control")
)

# Without distance or pair_id
matched_data <- join_matched(
  result, left, right,
  include_distance = FALSE,
  include_pair_id = FALSE
)
```
