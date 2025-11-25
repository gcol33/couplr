# Match blocks in parallel

Match blocks in parallel

## Usage

``` r
match_blocks_parallel(
  blocks,
  left,
  right,
  left_ids,
  right_ids,
  block_col,
  vars,
  distance,
  weights,
  scale,
  max_distance,
  calipers,
  method,
  parallel = FALSE
)
```

## Arguments

- blocks:

  Vector of block IDs

- left:

  Left dataset with block_col

- right:

  Right dataset with block_col

- left_ids:

  IDs from left

- right_ids:

  IDs from right

- block_col:

  Name of blocking column

- vars:

  Variables for matching

- distance:

  Distance metric

- weights:

  Variable weights

- scale:

  Scaling method

- max_distance:

  Maximum distance

- calipers:

  Caliper constraints

- method:

  LAP method

- parallel:

  Whether to use parallel processing

## Value

List with combined results from all blocks
