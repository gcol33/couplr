# Match two datasets into couples

Performs one-to-one matching between two datasets. Supports blocking,
distance constraints, and various distance metrics.

## Usage

``` r
match_couples(
  left,
  right = NULL,
  vars = NULL,
  left_id = NULL,
  right_id = NULL,
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
  memory_mode = "auto",
  certify = NULL
)
```

## Arguments

- left:

  Data frame of "left" units (e.g., treated, cases)

- right:

  Data frame of "right" units (e.g., control, controls)

- vars:

  Variable names to use for distance computation

- left_id, right_id:

  Name of the column holding the unit identifier, or NULL (default) to
  use a column called `id`, then meaningful row names, then synthesized
  ids `left_1 ... left_n` / `right_1 ... right_m` with a warning. The
  values of this column are what `pairs$left_id` and `pairs$right_id`
  carry, and what
  [`join_matched()`](https://gillescolling.com/couplr/reference/join_matched.md),
  [`match_data()`](https://gillescolling.com/couplr/reference/match_data.md),
  [`balance_diagnostics()`](https://gillescolling.com/couplr/reference/balance_diagnostics.md),
  [`sensitivity_analysis()`](https://gillescolling.com/couplr/reference/sensitivity_analysis.md)
  and
  [`as_matchit()`](https://gillescolling.com/couplr/reference/as_matchit.md)
  join on, so the same column name is passed to those verbs. Ids read
  from the data must be unique.

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

  One of "auto" (default), "dense", "lazy" or "implicit". "auto" warns
  (or, when `method` is `"jv"`/`"auction"`, switches) when the dense
  cost matrix would consume a large fraction of free system RAM. "lazy"
  computes each pairwise distance from the underlying feature data as
  the solver needs it, instead of allocating the full n_left x n_right
  matrix; supported for `method = "jv"`/`"auction"`, including
  `replace = TRUE`, where each left unit's cheapest partners are found
  one row at a time, and `ratio > 1`, where the left units' covariates
  are replicated rather than their rows of distances, and not yet for
  `method = "greedy"` (blocking via `block_id` is the other option that
  reduces memory, by solving smaller sub-problems). A custom distance
  function is called on a block of left units against every right unit,
  the same contract the dense path calls it under, with the block sized
  so the matrix it returns stays near a million cells; each matched pair
  is then evaluated again alone, and a distance that depends on the
  other units in its call is an error. When the constraints admit no
  complete matching, the largest matching they admit, cheapest among
  those, is found by the edge-generation loop over the same
  specification, as the dense path finds it by padding. Where the metric
  carries a ball bound, the column set is held in a ball tree and a
  subtree whose bound cannot beat the current threshold is discarded
  without being read: `"mahalanobis"` always, the metrics linear in the
  covariates up to six of them. `"manhattan"` and `"chebyshev"`, a
  covariance with no Cholesky factor, and a higher-dimensional linear
  metric read the columns instead. "implicit" states the problem over
  every pair and solves it over a fraction of them, generating the pairs
  the answer turns out to need and proving that the ones it never
  generated could not have improved it; same requirements as "lazy". On
  the eight-covariate problem the benchmarks use it leads "lazy" from
  5,000 units upward, by 1.1x at 5,000 rising to 3.1x at 50,000, and
  loses below that where the loop's fixed costs are still visible; what
  it buys at every size is the certificate over the complete problem.
  "auto" never selects it. "dense" skips the RAM check entirely.

- certify:

  Logical; whether the result carries a checked `assignment_certificate`
  as `certificate`. Applies to `memory_mode = "implicit"`, where it
  defaults to `TRUE`: the certificate is what separates the answer from
  an approximate one. On the other paths the matching is certified after
  the fact with
  [`verify_assignment()`](https://gillescolling.com/couplr/reference/verify_assignment.md),
  against the cost matrix it was solved from.

## Value

A list with class "matching_result" containing:

- `pairs`: Tibble of matched pairs with distances

- `unmatched`: List of unmatched left and right IDs

- `info`: Matching diagnostics and metadata

- `status`: One of
  [`solver_status_values()`](https://gillescolling.com/couplr/reference/solver_status_values.md),
  computed from what the solve achieved. `"optimal"` when every left
  unit found a partner under an optimal method, `"partial"` when
  constraints left some unmatched, `"heuristic"` when a greedy method
  ran, either because it was asked for or because the constrained path
  fell back to it, and `"infeasible"` when nothing could be matched.

- `potentials`: The dual potentials of the design's linear program, a
  list with elements `left` and `right` holding one value per unit,
  named by id. The reduced cost of a pair is its distance minus the two
  potentials: at least zero on every admissible pair the matching left
  out, zero on every pair of a 1:1 matching, and at most zero on the
  pairs of a k:1 or with-replacement matching, whose pair arcs carry at
  most one unit. A left unit's potential on the k:1 design is the
  largest of its replicas'. With replacement the LP separates by row: a
  left unit's potential is the distance to the farthest partner it took
  and every right unit's is zero. A unit with no admissible partner, or
  in a block with none, reads `NA`. Absent for `method = "greedy"` and
  for a constrained problem answered by maximum cardinality, which is
  not the LP the potentials belong to. On the 1:1 design they are the
  duals
  [`verify_assignment()`](https://gillescolling.com/couplr/reference/verify_assignment.md)
  checks, and a method that returns none has them computed by
  [`assignment_duals()`](https://gillescolling.com/couplr/reference/assignment_duals.md).

Under `memory_mode = "implicit"` it also carries `certificate`, the
checked statement of optimality (see
[`verify_assignment()`](https://gillescolling.com/couplr/reference/verify_assignment.md),
which names the arithmetic it was decided in), and `search`: the pairs
the loop generated out of the pairs the problem states, the pairs a cost
was computed for, and one row per round of what each round did. An
infeasible answer carries `witness` instead, naming the units that could
not be matched and the partners they have between them.

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
