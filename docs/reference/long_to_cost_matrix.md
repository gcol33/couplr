# Build a cost matrix from source / target / cost columns

The long-format door of
[`lap_solve()`](https://gillescolling.com/couplr/reference/lap_solve.md)
and
[`lap_solve_kbest()`](https://gillescolling.com/couplr/reference/lap_solve_kbest.md).
Cells no row names are `forbidden`, which is what makes an absent pair
an absent edge rather than a zero-cost one.

## Usage

``` r
long_to_cost_matrix(source_vals, target_vals, cost_vals, forbidden = NA)
```

## Value

List with the matrix and the source / target level vectors its row and
column indices stand for.
