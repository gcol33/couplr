# Apply caliper constraints

A caliper is a maximum absolute difference on one column of both data
frames: a matching variable, or another one such as a propensity score.

## Usage

``` r
apply_calipers(cost_matrix, left, right, calipers)
```

## Value

Modified cost matrix with forbidden pairs marked.
