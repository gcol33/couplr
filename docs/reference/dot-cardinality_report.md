# Report of a cardinality match

Turns a completed search into the object callers read: the matched
sample's size, the largest size the bound admits, the gap between them,
and the state of every constraint the match was asked to meet.

## Usage

``` r
.cardinality_report(run, specs = NULL)

# S3 method for class 'cardinality_report'
print(x, ...)
```

## Arguments

- run:

  A `cardinality_run` from
  [`.cardinality_branch_bound()`](https://gillescolling.com/couplr/reference/dot-cardinality_branch_bound.md).

- specs:

  The moment rows the run was given, from `.moment_specs()`.

- x:

  A `cardinality_report`.

- ...:

  Ignored.

## Value

An object of class `cardinality_report`, a list with elements:

- `n_matched`, `n_left_matched` - matched pairs, and the left units they
  use.

- `best_possible` - the largest matched sample the bound admits.

- `gap`, `gap_fraction` - `best_possible - n_matched`, in matched units
  and as a share of `best_possible`.

- `certified` - `TRUE` only when the search settled, every solve its
  bound rests on was certified, the incumbent's own solve was certified
  and audited, and `gap` is zero.

- `objective`, `bound` - the incumbent's value of the network objective
  and the global lower bound on it. `best_possible` is read from
  `bound`.

- `stopped_on`, `n_nodes`, `engine`, `status`.

- `constraints` - one row per stated constraint, with what it asked for
  and what the matched sample achieved.

- `balance` - matched counts and imbalance per category at every level.

- `tiers`, `precision_headroom`, `shift` - the weights that order the
  objective, how many times its range fits inside the range a double
  orders exactly, and the constant taken off every distance.

- `total_distance`, `pairs` - the matched set itself.

- `potentials` - the pair potentials of the solve the matched set came
  from, as `.cardinality_pair_duals()` reads them, or `NULL` when that
  solve was not certified.

Invisibly returns `x`.
