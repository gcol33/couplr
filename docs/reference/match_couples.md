# Optimal matching using linear assignment

Performs optimal one-to-one matching between two datasets using linear
assignment problem (LAP) solvers. Supports blocking, distance
constraints, and various distance metrics.

## Usage

``` r
match_couples(
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
  method = "auto",
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

- method:

  LAP solver: "auto", "hungarian", "jv", "gabow_tarjan", etc.

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

A list with class "matching_result" containing:

- `pairs`: Tibble of matched pairs with distances

- `unmatched`: List of unmatched left and right IDs

- `info`: Matching diagnostics and metadata

## Details

This function finds the matching that minimizes total distance among all
feasible matchings, subject to constraints. Use
[`greedy_couples()`](https://gillescolling.com/couplr/reference/greedy_couples.md)
for faster approximate matching on large datasets.

## Examples

``` r
# Basic matching
left <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 5), y = c(2, 4, 6, 8, 10))
right <- data.frame(id = 6:10, x = c(1.1, 2.2, 3.1, 4.2, 5.1), y = c(2.1, 4.1, 6.2, 8.1, 10.1))
result <- match_couples(left, right, vars = c("x", "y"))
print(result$pairs)

# With constraints
result <- match_couples(left, right, vars = c("x", "y"),
                        max_distance = 1,
                        calipers = list(x = 0.5))

# With blocking
left$region <- c("A", "A", "B", "B", "B")
right$region <- c("A", "A", "B", "B", "B")
blocks <- matchmaker(left, right, block_type = "group", block_by = "region")
result <- match_couples(blocks$left, blocks$right, vars = c("x", "y"))
```
