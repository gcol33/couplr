# Coarsened Exact Matching

Coarsens continuous variables into bins, then performs exact matching on
the coarsened values. Units in strata containing both left and right
units are kept; others are pruned. Matched units receive weights
inversely proportional to stratum sizes to maintain balance.

## Usage

``` r
cem_match(
  left,
  right,
  vars,
  cutpoints = NULL,
  n_bins = "sturges",
  grouping = NULL,
  keep = "all",
  left_id = "id",
  right_id = "id"
)
```

## Arguments

- left:

  Data frame of left (treated) units

- right:

  Data frame of right (control) units

- vars:

  Character vector of variable names to coarsen and match on

- cutpoints:

  Named list of break vectors per variable. If NULL, automatic binning
  is used.

- n_bins:

  Binning method when `cutpoints` is NULL: `"sturges"` (default), `"fd"`
  (Freedman-Diaconis), `"scott"`, or an integer specifying the number of
  bins for all variables.

- grouping:

  Character vector of variable names to match exactly (without
  coarsening). These are typically categorical variables.

- keep:

  Which units to return: `"all"` (default) returns all units with weight
  0 for unmatched, `"matched"` drops unmatched units.

- left_id:

  Name of ID column in left (default: `"id"`)

- right_id:

  Name of ID column in right (default: `"id"`)

## Value

An S3 object of class `c("cem_result", "couplr_result")` containing:

- matched:

  Tibble with columns `id`, `side`, `stratum`, `weight`

- strata_summary:

  Tibble with per-stratum counts

- info:

  List with `n_strata`, `n_matched_left`, `n_matched_right`,
  `n_pruned_left`, `n_pruned_right`, `method`, `vars`

## Details

CEM algorithm:

1.  Coarsen each numeric variable using
    [`cut`](https://rdrr.io/r/base/cut.html) with either user-specified
    breakpoints or automatic binning (Sturges, FD, or Scott rule)

2.  Categorical variables in `grouping` are kept as-is

3.  Create strata by concatenating all coarsened values

4.  Drop strata with 0 left or 0 right units

5.  Compute CEM weights: left units get weight 1, right units get weight
    `n_left_in_stratum / n_right_in_stratum` so that the total weight of
    right units in each stratum equals the number of left units

## Examples

``` r
set.seed(42)
left <- data.frame(
  id = 1:20, age = rnorm(20, 40, 10),
  income = rnorm(20, 50000, 10000)
)
right <- data.frame(
  id = 21:60, age = rnorm(40, 42, 10),
  income = rnorm(40, 52000, 10000)
)
result <- cem_match(left, right, vars = c("age", "income"))
print(result)
```
