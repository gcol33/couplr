# Precision matrix of a covariance matrix, through its Cholesky factor

The Mahalanobis distances need the inverse of a symmetric positive
definite matrix. [`chol()`](https://rdrr.io/r/base/chol.html) factors it
as `R'R`, its failure or a reciprocal condition number below machine
precision marks a singular matrix, and
[`chol2inv()`](https://rdrr.io/r/base/chol2inv.html) forms the inverse
from the triangular factor.

## Usage

``` r
mahalanobis_precision(cov_mat)
```

## Value

The `p x p` inverse of `cov_mat`.
