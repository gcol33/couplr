# Create blocks for stratified matching

Constructs blocks (strata) for matching, using either grouping variables
or clustering algorithms. Returns the input data frames with block IDs
assigned, along with block summary statistics.

## Usage

``` r
matchmaker(
  left,
  right,
  block_type = c("none", "group", "cluster"),
  block_by = NULL,
  block_vars = NULL,
  block_method = "kmeans",
  n_blocks = NULL,
  min_left = 1,
  min_right = 1,
  drop_imbalanced = FALSE,
  imbalance_threshold = Inf,
  return_dropped = TRUE,
  ...
)
```

## Arguments

- left:

  Data frame of "left" units (e.g., treated, cases)

- right:

  Data frame of "right" units (e.g., control, controls)

- block_type:

  Type of blocking to use:

  - "none": No blocking (default)

  - "group": Block by existing categorical variable(s)

  - "cluster": Block using clustering algorithm

- block_by:

  Variable name(s) for grouping (if block_type = "group")

- block_vars:

  Variable names for clustering (if block_type = "cluster")

- block_method:

  Clustering method (if block_type = "cluster"):

  - "kmeans": K-means clustering

  - "hclust": Hierarchical clustering

- n_blocks:

  Target number of blocks (for clustering)

- min_left:

  Minimum number of left units per block

- min_right:

  Minimum number of right units per block

- drop_imbalanced:

  Drop blocks with extreme imbalance

- imbalance_threshold:

  Maximum allowed \|n_left - n_right\| / max(n_left, n_right)

- return_dropped:

  Include dropped blocks in output

- ...:

  Additional arguments passed to clustering function

## Value

A list with class "matchmaker_result" containing:

- `left`: Left data frame with block_id column added

- `right`: Right data frame with block_id column added

- `block_summary`: Summary statistics for each block

- `dropped`: Information about dropped blocks (if any)

- `info`: Metadata about blocking process

## Details

This function does NOT perform matching - it only creates the block
structure. Use
[`match_couples()`](https://gillescolling.com/couplr/reference/match_couples.md)
or
[`greedy_couples()`](https://gillescolling.com/couplr/reference/greedy_couples.md)
to perform matching within blocks.

## Examples

``` r
# Group blocking
left <- data.frame(id = 1:10, region = rep(c("A", "B"), each = 5), x = rnorm(10))
right <- data.frame(id = 11:20, region = rep(c("A", "B"), each = 5), x = rnorm(10))
blocks <- matchmaker(left, right, block_type = "group", block_by = "region")
print(blocks$block_summary)

# Clustering
blocks <- matchmaker(left, right, block_type = "cluster",
                     block_vars = "x", n_blocks = 3)
```
