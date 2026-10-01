# Assemble a matching_result from a cardinality report

Assemble a matching_result from a cardinality report

## Usage

``` r
.cardinality_result(
  report,
  left,
  right,
  vars,
  left_ids,
  right_ids,
  distance = "euclidean",
  max_std_diff = NULL
)
```

## Arguments

- report:

  A `cardinality_report` from
  [`.cardinality_solve()`](https://gillescolling.com/couplr/reference/dot-cardinality_solve.md).

- left, right:

  The data frames the match was solved on.

- vars:

  The matching variables, for the per-variable difference columns.

- left_ids, right_ids:

  The ids the pairs are keyed on.

- distance:

  The distance metric the cost matrix was built from.

- max_std_diff:

  The standardized-difference bound the call stated.

## Value

A `matching_result` carrying `cardinality`, `status`, `info$engine`, and
a `certificate` when the search certified optimality.
