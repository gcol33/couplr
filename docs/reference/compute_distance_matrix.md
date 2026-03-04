# Compute pairwise distance matrix

Compute pairwise distance matrix

## Usage

``` r
compute_distance_matrix(
  left_mat,
  right_mat,
  distance = "euclidean",
  sigma = NULL
)
```

## Arguments

- left_mat:

  Numeric matrix of left units (rows = units, cols = variables).

- right_mat:

  Numeric matrix of right units (rows = units, cols = variables).

- distance:

  Character string specifying distance metric, or a function.

- sigma:

  Optional covariance matrix for Mahalanobis distance. If NULL
  (default), the pooled covariance is estimated from data.

## Value

Numeric matrix of pairwise distances (n_left x n_right).
