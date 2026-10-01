# Balance-constrained cardinality match

Builds the balance network for a distance matrix and a nested partition,
searches it under whatever moment constraints are stated, and reports
the matched sample with the bound that goes with it.

## Usage

``` r
.cardinality_solve(
  left,
  right,
  cost,
  refined = NULL,
  exact = 1L,
  moments = NULL,
  max_std_diff = NULL,
  vars = NULL,
  dual_steps = 20L,
  branch = c("unit", "pair"),
  node_limit = 500L,
  time_limit = Inf,
  should_stop = NULL
)
```

## Arguments

- left, right:

  Data frames of units, one row each.

- cost:

  Numeric distance matrix, `nrow(left)` by `nrow(right)`. A cell that is
  `Inf` or `NA` is a pair the match may not use.

- refined:

  The nested groupings balance is required on, coarsest first.

- exact:

  How many of the coarsest levels are enforced exactly.

- moments, max_std_diff, vars:

  Moment constraints, as `.moment_specs()` takes them.

- dual_steps:

  Multiplier updates per node.

- branch:

  Whether to branch on left-unit inclusion or on pairs.

- node_limit, time_limit:

  Search budget.

- should_stop:

  Optional predicate of the search state; `TRUE` stops the search the
  way an interrupt would.

## Value

A `cardinality_report`.
