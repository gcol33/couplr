# Shared matching from precomputed distance object

Core logic for both optimal (LAP) and greedy matching from distance
objects. Called by match_couples_from_distance() and
greedy_couples_from_distance().

## Usage

``` r
.couples_from_distance(
  dist_obj,
  max_distance = Inf,
  calipers = NULL,
  ignore_blocks = FALSE,
  require_full_matching = FALSE,
  return_unmatched = TRUE,
  return_diagnostics = FALSE,
  solver_fn,
  solver_params = list(),
  check_costs = FALSE,
  strict_no_pairs = FALSE,
  method_label = "from_distance_object",
  extra_info = list(),
  diagnostics_fields = c("method", "n_matched", "total_distance")
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

- method_label:

  String for info\$method (e.g., "from_distance_object")

- extra_info:

  Named list of extra fields to add to info

## Value

A matching_result object with pairs, info, and optional diagnostics.
