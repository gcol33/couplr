# Estimate the Peak Memory of a Dense Solve

The matrix is the allocation a caller can reason about and it is not
what bounds a dense solve. The solver works on its own copy of the
costs, with a byte per cell marking the forbidden pairs, beside the
matrix R holds, and the peak is what decides whether the solve fits, so
that is the quantity the guard compares against available RAM.

## Usage

``` r
estimate_dense_solve_mb(n, m, solve_factor = 4)
```

## Arguments

- n, m:

  Problem dimensions.

- solve_factor:

  Multiplier on the raw cell bytes.

## Value

Numeric scalar, the estimated peak footprint of a dense solve in
megabytes.

## Details

The multiplier is read off the measurement. On the memory benchmark –
one fresh R session per arm, peak resident set taken from outside and
read against an idle session that loaded the same packages – a dense
one-to-one solve has peaked at between 2.2 and 3.0 times the raw matrix
bytes from 2,000 to 20,000 units, the larger ratios at the small sizes,
where fixed costs weigh most. The default of 4 sits above every measured
peak, which is the direction a guard should err in: it exists to refuse
a solve that will not fit, not to predict where the peak lands.

## Examples

``` r
estimate_dense_solve_mb(5000, 5000)
```
