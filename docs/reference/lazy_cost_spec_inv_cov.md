# Precompute the Mahalanobis inverse covariance matrix for a lazy cost spec

Mirrors compute_distance_matrix()'s pooled within-group covariance logic
exactly (R/matching_distance.R) – computed once in R rather than
reimplemented in C++, so the two code paths can't drift apart.

## Usage

``` r
lazy_cost_spec_inv_cov(spec)
```

## Value

p x p inverse covariance matrix, or NULL if distance != "mahalanobis".
