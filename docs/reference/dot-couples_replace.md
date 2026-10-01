# Replacement matching: each left picks its best right independently

The compiled design gives every column capacity for every row, so the
rows never compete and the optimum of the whole network is each row's
own cheapest columns. `plan$per_row` is how many of them a row takes:
the requested ratio, or the column count when there are fewer columns
than that. A lazy cost spec is answered one row at a time in C++,
through the same row search the implicit loop seeds with, so no row of
costs is ever held in R.

## Usage

``` r
.couples_replace(
  cost_matrix,
  left,
  right,
  left_ids,
  right_ids,
  vars,
  ratio = 1L,
  plan
)
```

## Value

List with pairs tibble, unmatched list, info list, and potentials.

## Details

The LP separates by row, so its duals are explicit. A row's dual is the
cost of the most expensive partner it took, which is its k-th cheapest
admissible cost, and every column's is zero, since no column has a
capacity to price. The pairs taken then reduce to at most zero and the
pairs passed over to at least zero, which are the optimality conditions
of the flow LP with each pair arc carrying at most one unit.
