# Cardinality Matching

Maximizes the number of matched pairs subject to balance constraints.
Uses iterative pruning: starts with a full match, then removes pairs
that contribute most to imbalance until all variables satisfy the
standardized difference threshold.

## Usage

``` r
cardinality_match(
  left,
  right,
  vars,
  max_std_diff = 0.1,
  distance = "euclidean",
  weights = NULL,
  scale = FALSE,
  auto_scale = FALSE,
  method = "auto",
  max_iter = 100L,
  batch_fraction = 0.1
)
```

## Arguments

- left:

  Data frame of "left" units

- right:

  Data frame of "right" units

- vars:

  Character vector of matching variable names

- max_std_diff:

  Maximum allowed absolute standardized difference (default: 0.1,
  corresponding to "excellent" balance)

- distance:

  Distance metric (default: "euclidean")

- weights:

  Optional named vector of variable weights

- scale:

  Scaling method (default: FALSE)

- auto_scale:

  If TRUE, automatically select scaling (default: FALSE)

- method:

  LAP solver method (default: "auto")

- max_iter:

  Maximum pruning iterations (default: 100)

- batch_fraction:

  Fraction of worst pairs to remove per iteration (default: 0.1). Larger
  values speed up convergence but may over-prune.

## Value

A matching_result object with additional cardinality info:
`result$info$pruning_iterations`, `result$info$pairs_removed`,
`result$info$final_balance`.

## Details

Cardinality matching (Zubizarreta 2014) finds the largest matched sample
that satisfies pre-specified balance constraints. This implementation
uses an iterative pruning heuristic:

1.  Run full optimal matching via
    [`match_couples()`](https://gillescolling.com/couplr/reference/match_couples.md)

2.  Compute balance diagnostics

3.  While any \|std_diff\| exceeds `max_std_diff`:

    - Identify the variable with worst balance

    - Remove the batch of pairs contributing most to that imbalance

    - Recompute balance

## Examples

``` r
set.seed(42)
left <- data.frame(id = 1:20, x = rnorm(20, 0, 1), y = rnorm(20, 0, 1))
right <- data.frame(id = 21:50, x = rnorm(30, 0.5, 1), y = rnorm(30, 0.3, 1))
result <- cardinality_match(left, right, vars = c("x", "y"), max_std_diff = 0.2)
print(result)
```
