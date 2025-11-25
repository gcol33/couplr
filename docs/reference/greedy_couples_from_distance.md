# Greedy Matching from Precomputed Distance Object

Internal function to handle greedy matching when a distance_object is
provided

## Usage

``` r
greedy_couples_from_distance(
  dist_obj,
  max_distance = Inf,
  calipers = NULL,
  ignore_blocks = FALSE,
  require_full_matching = FALSE,
  strategy = "row_best",
  return_unmatched = TRUE,
  return_diagnostics = FALSE
)
```
