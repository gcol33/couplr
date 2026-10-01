# Estimate dense cost-matrix memory footprint in megabytes

What the matrix itself costs a process, which is more than the 8 bytes a
cell occupies: `matrix(0, n, m)` at the R level is copied into a
`lap::CostMatrix` (8B data + 4B mask), and the two coexist while garbage
collection lags. Building the matrix and stopping there has peaked at
between 1.5 and 1.9 times the raw cell bytes from 5,000 to 20,000 units
on the memory benchmark, so the default multiplier is above what has
been measured rather than fitted to it. `n`/`m` are coerced to `double`
before multiplying so the estimate itself can't overflow the way
`lap::CostMatrix`'s old `int` flat-index arithmetic did.

## Usage

``` r
estimate_dense_matrix_mb(n, m, overhead_factor = 4)
```

## Arguments

- n, m:

  Problem dimensions.

- overhead_factor:

  Multiplier on the raw cell bytes.

## Value

Numeric scalar, the estimated footprint of the matrix in megabytes.

## Details

This is the matrix, not the solve.
[`estimate_dense_solve_mb()`](https://gillescolling.com/couplr/reference/estimate_dense_solve_mb.md)
is what the memory guard reads; see there for why the two differ by more
than a copy.

## Examples

``` r
estimate_dense_matrix_mb(5000, 5000)
```
