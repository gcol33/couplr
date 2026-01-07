# Fast approximate matching using greedy algorithm

Performs fast one-to-one matching using greedy strategies. Does not
guarantee optimal total distance but is much faster than
[`match_couples()`](https://gcol33.github.io/couplr/reference/match_couples.md)
for large datasets. Supports blocking, distance constraints, and various
distance metrics.

## Usage

``` r
greedy_couples(
  left,
  right = NULL,
  vars = NULL,
  distance = "euclidean",
  weights = NULL,
  scale = FALSE,
  auto_scale = FALSE,
  max_distance = Inf,
  calipers = NULL,
  block_id = NULL,
  ignore_blocks = FALSE,
  require_full_matching = FALSE,
  strategy = c("row_best", "sorted", "pq"),
  return_unmatched = TRUE,
  return_diagnostics = FALSE,
  parallel = FALSE,
  check_costs = TRUE
)
```

## Arguments

- left:

  Data frame of "left" units (e.g., treated, cases)

- right:

  Data frame of "right" units (e.g., control, controls)

- vars:

  Variable names to use for distance computation

- distance:

  Distance metric: "euclidean", "manhattan", "mahalanobis", or a custom
  function

- weights:

  Optional named vector of variable weights

- scale:

  Scaling method: FALSE (none), "standardize", "range", or "robust"

- auto_scale:

  If TRUE, automatically check variable health and select scaling method
  (default: FALSE)

- max_distance:

  Maximum allowed distance (pairs exceeding this are forbidden)

- calipers:

  Named list of per-variable maximum absolute differences

- block_id:

  Column name containing block IDs (for stratified matching)

- ignore_blocks:

  If TRUE, ignore block_id even if present

- require_full_matching:

  If TRUE, error if any units remain unmatched

- strategy:

  Greedy strategy:

  - "row_best": For each row, find best available column (default)

  - "sorted": Sort all pairs by distance, greedily assign

  - "pq": Use priority queue (good for very large problems)

- return_unmatched:

  Include unmatched units in output

- return_diagnostics:

  Include detailed diagnostics in output

- parallel:

  Enable parallel processing for blocked matching. Requires 'future' and
  'future.apply' packages. Can be:

  - `FALSE`: Sequential processing (default)

  - `TRUE`: Auto-configure parallel backend

  - Character: Specify future plan (e.g., "multisession", "multicore")

- check_costs:

  If TRUE, check distance distribution for potential problems and
  provide helpful warnings before matching (default: TRUE)

## Value

A list with class "matching_result" (same structure as match_couples)

## Details

Greedy strategies do not guarantee optimal total distance but are much
faster:

- "row_best": O(n\*m) time, simple and often produces good results

- "sorted": O(n*m*log(n\*m)) time, better quality but slower

- "pq": O(n*m*log(n\*m)) time, memory-efficient for large problems

Use greedy_couples when:

- Dataset is very large (\> 10,000 x 10,000)

- Approximate solution is acceptable

- Speed is more important than optimality

## Examples

``` r
# Basic greedy matching
left <- data.frame(id = 1:100, x = rnorm(100))
right <- data.frame(id = 101:200, x = rnorm(100))
result <- greedy_couples(left, right, vars = "x")

# Compare to optimal
result_opt <- match_couples(left, right, vars = "x")
result_greedy <- greedy_couples(left, right, vars = "x")
result_greedy$info$total_distance / result_opt$info$total_distance  # Quality ratio
#> [1] 1.391962
```
