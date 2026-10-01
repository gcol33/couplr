# Apply the `forbidden` sentinel to a cost matrix

Every front door that documents a `forbidden` argument masks with this
one, so
[`lap_solve()`](https://gillescolling.com/couplr/reference/lap_solve.md),
[`lap_solve_batch()`](https://gillescolling.com/couplr/reference/lap_solve_batch.md)
and
[`lap_solve_kbest()`](https://gillescolling.com/couplr/reference/lap_solve_kbest.md)
read the same sentinel the same way. NA and Inf cells are forbidden to
the solvers already, so `forbidden = NA` is the identity.

## Usage

``` r
mask_forbidden(cost_matrix, forbidden = NA)
```

## Value

The cost matrix with sentinel cells replaced by `Inf`.
