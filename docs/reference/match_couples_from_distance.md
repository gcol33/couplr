# Match from Precomputed Distance Object

Internal function to handle matching when a distance_object is provided

## Usage

``` r
match_couples_from_distance(
  dist_obj,
  max_distance = Inf,
  calipers = NULL,
  ignore_blocks = FALSE,
  require_full_matching = FALSE,
  method = "auto",
  return_unmatched = TRUE,
  return_diagnostics = FALSE,
  check_costs = TRUE
)
```
