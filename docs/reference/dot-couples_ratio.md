# k:1 matching via cost matrix expansion

Replicates left-side rows k times so each left unit can match up to k
different right units. Solves as standard LAP, then maps expanded rows
back to original left indices.

## Usage

``` r
.couples_ratio(
  cost_matrix,
  left,
  right,
  left_ids,
  right_ids,
  vars,
  ratio,
  solver_fn,
  solver_params
)
```

## Value

List with pairs tibble, unmatched list, and info list.
