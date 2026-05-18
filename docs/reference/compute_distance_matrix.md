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

  Optional covariance matrix for Mahalanobis distance. If `NULL`
  (default), the pooled within-group covariance is estimated from
  `left_mat` and `right_mat`: \\\Sigma = ((n_L-1)\\S_L + (n_R-1)\\S_R) /
  (n_L+n_R-2)\\. This matches the convention used by
  [`optmatch::match_on()`](https://rdrr.io/pkg/optmatch/man/match_on-methods.html)
  for treated/control Mahalanobis matching. If either group has fewer
  than two rows, falls back to the overall-sample covariance of the two
  groups stacked together.

## Value

Numeric matrix of pairwise distances (n_left x n_right).
