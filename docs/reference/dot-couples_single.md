# Shared single matching implementation

Core logic for both optimal (LAP) and greedy matching without blocking.
Called by match_couples_single() and greedy_couples_single().

## Usage

``` r
.couples_single(
  left,
  right,
  left_ids,
  right_ids,
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

  If TRUE, run check_cost_distribution before solving

- strict_no_pairs:

  If TRUE, call err_no_valid_pairs (stops); else warn

## Value

List with pairs tibble, unmatched list, and info list.
