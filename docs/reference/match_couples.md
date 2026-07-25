# Match two datasets into couples

Performs one-to-one matching between two datasets. Supports blocking,
distance constraints, and various distance metrics.

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
  strategy = c("row_best", "sorted", "pq"),
  return_unmatched = TRUE,
  return_diagnostics = FALSE,
  parallel = FALSE,
  replace = FALSE,
  ratio = 1L,
  check_costs = TRUE,
  sigma = NULL,
  memory_mode = "auto"
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

  Matching method. A LAP solver for optimal matching ("auto",
  "hungarian", "jv", "gabow_tarjan", ...), or "greedy" for fast
  approximate matching (see `strategy`).

- strategy:

  Greedy strategy, used only when `method = "greedy"`. All three
  strategies solve the same full cost matrix already built by
  `match_couples()`; none of them reduce the O(n\*m) memory that matrix
  takes.

  - "row_best": for each row, take its best available column (default).
    The only strategy that needs no extra storage beyond the cost
    matrix.

  - "sorted": collect every valid pair, sort by distance, greedily
    assign

  - "pq": collect every valid pair into a heap and pop the smallest
    first. Avoids the upfront sort but holds the same number of
    candidate pairs as "sorted", so it is not more memory-efficient

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

- replace:

  If TRUE, allow matching with replacement (same right unit can be
  matched to multiple left units). Default: FALSE.

- ratio:

  Integer, number of right units to match per left unit. Default: 1
  (one-to-one matching). For k:1 matching, set ratio = k.

- check_costs:

  If TRUE, check distance distribution for potential problems and
  provide helpful warnings before matching (default: TRUE)

- sigma:

  Optional covariance matrix for Mahalanobis distance. If NULL
  (default), the pooled sample covariance is used. Only relevant when
  `distance = "mahalanobis"`.

- memory_mode:

  One of "auto" (default), "dense", or "lazy". "auto" warns (or, when
  `method` is `"jv"`/`"auction"` with a built-in distance metric,
  switches) when the dense cost matrix would consume a large fraction of
  free system RAM. "lazy" computes each pairwise distance from the
  underlying feature data as the solver needs it, instead of allocating
  the full n_left x n_right matrix; supported for
  `method = "jv"`/`"auction"` with a built-in distance metric, and not
  yet for `replace = TRUE`, `ratio > 1`, `method = "greedy"`, or custom
  distance functions (blocking via `block_id` is the other option that
  reduces memory, by solving smaller sub-problems). "dense" skips the
  RAM check entirely.

## Value

A list with class "matching_result" containing:

- `pairs`: Tibble of matched pairs with distances

- `unmatched`: List of unmatched left and right IDs

- `info`: Matching diagnostics and metadata

## Details

With `method` set to a LAP solver (the default `"auto"`, or `"jv"`,
`"hungarian"`, ...) it finds the matching that minimizes total distance
among all feasible matchings. With `method = "greedy"` it uses a fast
greedy strategy (selected by `strategy`) that does not guarantee the
optimal total distance but scales to very large datasets.

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

# Fast greedy matching for large datasets
result <- match_couples(left, right, vars = c("x", "y"),
                        method = "greedy", strategy = "sorted")
```
