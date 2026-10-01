# Balance pruning heuristic

Runs a full optimal match, then deletes pairs until every matching
variable sits inside the standardized-difference threshold or the
iteration budget runs out.

## Usage

``` r
.cardinality_prune(
  left,
  right,
  vars,
  max_std_diff,
  distance = "euclidean",
  weights = NULL,
  scale = FALSE,
  auto_scale = FALSE,
  sigma = NULL,
  max_distance = Inf,
  calipers = NULL,
  left_id = NULL,
  right_id = NULL,
  method = "auto",
  max_iter = 100L,
  batch_fraction = 0.1
)
```

## Arguments

- left, right:

  Data frames of units, one row each.

- vars:

  Character vector of matching variable names.

- max_std_diff:

  Standardized-difference threshold the loop prunes toward.

- distance, weights, scale, auto_scale, sigma:

  Distance construction, as in
  [`match_couples()`](https://gillescolling.com/couplr/reference/match_couples.md).

- max_distance, calipers:

  Pair constraints, as in
  [`match_couples()`](https://gillescolling.com/couplr/reference/match_couples.md).

- left_id, right_id:

  Id columns, as in
  [`match_couples()`](https://gillescolling.com/couplr/reference/match_couples.md).

- method:

  LAP solver method for the initial match.

- max_iter:

  Maximum deletion rounds.

- batch_fraction:

  Share of the remaining pairs deleted each round.

## Value

A `matching_result` carrying `info$pruning_iterations` and
`info$pairs_removed`.
