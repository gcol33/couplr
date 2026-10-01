# Explain which solver `method = "auto"` selects, and why

`assignment(method = "auto")` picks a solver from a small set of rules
evaluated against one pass over the cost matrix. This reports the
decision without solving: which rule fired, the property that triggered
it, the rules that were tested first and did not fire, and the internal
representation the problem will be handed to the solver in.

## Usage

``` r
explain_dispatch(
  cost,
  method = "auto",
  maximize = FALSE,
  cardinality = c("complete", "maximum", "fixed"),
  n_matches = NULL,
  unmatched_penalty = NULL
)

# S3 method for class 'dispatch_explanation'
print(x, ...)
```

## Arguments

- cost:

  Numeric matrix, as passed to
  [`assignment()`](https://gillescolling.com/couplr/reference/assignment.md).
  `NA` or `Inf` entries are treated as forbidden.

- method:

  Character scalar. `"auto"` (default) reports the automatic decision.
  Naming any other method reports that the choice was explicit and which
  rule `"auto"` would have picked instead.

- maximize, cardinality, n_matches, unmatched_penalty:

  As in
  [`assignment()`](https://gillescolling.com/couplr/reference/assignment.md).
  A cardinality other than `"complete"` appends dummy columns before the
  solver sees the problem, which changes its shape and sparsity, so the
  rules are evaluated against the same matrix
  [`assignment()`](https://gillescolling.com/couplr/reference/assignment.md)
  would hand the solver.

- x:

  A `dispatch_explanation` object.

- ...:

  Ignored.

## Value

An object of class `dispatch_explanation`, a list with elements:

- `method` - the solver that will run.

- `explicit` - `TRUE` when `method` was named rather than dispatched.

- `rule`, `condition`, `reason` - the rule that fired under `"auto"`.

- `considered` - data frame of every rule tested, in order, with whether
  it fired.

- `n`, `m`, `transposed` - problem shape, and whether the solver sees
  the transpose. Rectangular problems are transposed internally so the
  solver always has at least as many columns as rows.

- `probe` - the single-pass summary the rules read.

Invisibly returns `x`.

## Details

The same rule table drives the dispatch itself, so the report cannot
drift from the behaviour.

## See also

[`assignment()`](https://gillescolling.com/couplr/reference/assignment.md)

## Examples

``` r
explain_dispatch(matrix(runif(400), 20, 20))
explain_dispatch(matrix(sample(0:1, 400, TRUE), 20, 20))
```
