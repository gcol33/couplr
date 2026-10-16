# Estimate the Memory of a Dense Cost Matrix

What building the matrix costs a process: the 8 bytes of each cell,
written in place by the distance code, and little besides. Building the
matrix and stopping there has peaked at between 1.0 and 1.1 times the
raw cell bytes from 2,000 to 20,000 units on the memory benchmark, so
the default multiplier is above what has been measured rather than
fitted to it. `n`/`m` are coerced to `double` before multiplying so the
estimate itself can't overflow.

## Usage

``` r
estimate_dense_matrix_mb(n, m, overhead_factor = 2)
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
