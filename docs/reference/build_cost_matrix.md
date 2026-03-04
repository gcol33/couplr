# Build cost matrix for matching

This is the main entry point for distance computation.

## Usage

``` r
build_cost_matrix(
  left,
  right,
  vars,
  distance = "euclidean",
  weights = NULL,
  scale = FALSE,
  sigma = NULL
)
```

## Value

Numeric matrix of distances with optional scaling/weights applied.
