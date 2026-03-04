# Full Matching

Assigns every unit (left and right) to a matched group with variable
ratios (1:k or k:1). Unlike 1:1 matching, full matching does not discard
units, producing matched groups where each group contains at least one
left and one right unit.

## Usage

``` r
full_match(
  left,
  right,
  vars,
  distance = "euclidean",
  min_controls = 1,
  max_controls = Inf,
  caliper = NULL,
  caliper_sd = NULL,
  weights = NULL,
  scale = FALSE,
  auto_scale = FALSE,
  sigma = NULL,
  left_id = "id",
  right_id = "id",
  method = "optimal"
)
```

## Arguments

- left:

  Data frame of left (treated) units

- right:

  Data frame of right (control) units

- vars:

  Character vector of variable names to match on

- distance:

  Distance metric: `"euclidean"` (default), `"mahalanobis"`,
  `"manhattan"`, or a custom function

- min_controls:

  Minimum number of right units per group (default: 1)

- max_controls:

  Maximum number of right units per group (default: Inf)

- caliper:

  Maximum allowable distance for a match. Units with no eligible partner
  within the caliper are left unmatched.

- caliper_sd:

  If not NULL, caliper is expressed in standard deviations of the pooled
  distance distribution rather than absolute units.

- weights:

  Named numeric vector of variable weights

- scale:

  Scaling method: `FALSE` (default), `"robust"`, `"standardize"`, or
  `"range"`

- auto_scale:

  If TRUE, automatically preprocess and scale variables

- sigma:

  Optional covariance matrix for Mahalanobis distance

- left_id:

  Name of ID column in left (default: `"id"`)

- right_id:

  Name of ID column in right (default: `"id"`)

- method:

  Matching algorithm: `"optimal"` (default) uses min-cost max-flow to
  find the globally optimal group assignment minimizing total distance;
  `"greedy"` uses a fast two-pass heuristic.

## Value

An S3 object of class `c("full_matching_result", "couplr_result")`
containing:

- groups:

  Tibble with columns `group_id`, `id`, `side` (`"left"`/`"right"`), and
  `weight`

- info:

  List with `n_groups`, `n_left`, `n_right`, `n_unmatched_left`,
  `n_unmatched_right`, `method`, `vars`

- unmatched:

  List of unmatched left and right IDs (if caliper excludes units)

## Details

Full matching creates matched groups of variable size. Two algorithms
are available:

**Optimal** (`method = "optimal"`, default): Solves a min-cost max-flow
problem that minimizes total distance across all group assignments
simultaneously. Each left unit becomes a group center absorbing 1 to
`max_controls` right units, with the globally optimal assignment found
via Dijkstra's algorithm with Johnson potentials. When
`n_left > n_right`, roles are transposed automatically.

**Greedy** (`method = "greedy"`): A fast two-pass heuristic:

1.  Each left unit picks its nearest eligible right unit

2.  Remaining right units are assigned to their nearest already-matched
    left unit, respecting `max_controls`

This is faster but does not guarantee globally optimal results.

Weights are computed so that within each group, the total weight of
right units equals the total weight of left units (which is 1). For a
group with 1 left and k right units, each right unit receives weight
1/k.

## Examples

``` r
set.seed(42)
left <- data.frame(id = 1:5, age = c(25, 35, 45, 55, 65))
right <- data.frame(id = 6:20, age = runif(15, 20, 70))
result <- full_match(left, right, vars = "age")
print(result)
```
