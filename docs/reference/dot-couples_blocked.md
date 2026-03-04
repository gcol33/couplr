# Shared blocked matching implementation

Core logic for both optimal (LAP) and greedy blocked matching. Called by
match_couples_blocked() and greedy_couples_blocked().

## Usage

``` r
.couples_blocked(
  left,
  right,
  left_ids,
  right_ids,
  block_col,
  vars,
  distance,
  weights,
  scale,
  max_distance,
  calipers,
  solver_fn,
  solver_params = list(),
  check_costs = FALSE,
  strict_no_pairs = FALSE,
  parallel = FALSE,
  replace = FALSE,
  ratio = 1L,
  sigma = NULL
)
```

## Arguments

- solver_fn:

  Solver function (assignment or greedy_matching)

- solver_params:

  Named list of extra args passed to solver_fn

- check_costs:

  If TRUE, passed through to .couples_single

- strict_no_pairs:

  If TRUE, passed through to .couples_single

## Value

List with pairs tibble, unmatched list, and info list.
