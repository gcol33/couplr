# Compute and Cache Distance Matrix for Reuse

Precomputes a distance matrix between left and right datasets, allowing
it to be reused across multiple matching operations with different
constraints. This is particularly useful when exploring different
matching parameters (max_distance, calipers, methods) without
recomputing distances.

## Usage

``` r
compute_distances(
  left,
  right,
  vars,
  distance = "euclidean",
  weights = NULL,
  scale = FALSE,
  auto_scale = FALSE,
  left_id = "id",
  right_id = "id",
  block_id = NULL
)
```

## Arguments

- left:

  Left dataset (data frame)

- right:

  Right dataset (data frame)

- vars:

  Character vector of variable names to use for distance computation

- distance:

  Distance metric (default: "euclidean")

- weights:

  Optional numeric vector of variable weights

- scale:

  Scaling method: FALSE, "standardize", "range", or "robust"

- auto_scale:

  Apply automatic preprocessing (default: FALSE)

- left_id:

  Name of ID column in left (default: "id")

- right_id:

  Name of ID column in right (default: "id")

- block_id:

  Optional block ID column name for blocked matching

## Value

An S3 object of class "distance_object" containing:

- `cost_matrix`: Numeric matrix of distances

- `left_ids`: Character vector of left IDs

- `right_ids`: Character vector of right IDs

- `block_id`: Block ID column name (if specified)

- `metadata`: List with computation details (vars, distance, scale,
  etc.)

- `original_left`: Original left dataset (for later joining)

- `original_right`: Original right dataset (for later joining)

## Details

This function computes distances once and stores them in a reusable
object. The resulting distance_object can be passed to
[`match_couples()`](https://gillescolling.com/couplr/reference/match_couples.md)
or
[`greedy_couples()`](https://gillescolling.com/couplr/reference/greedy_couples.md)
instead of providing datasets and variables.

Benefits:

- **Performance**: Avoid recomputing distances when trying different
  constraints

- **Exploration**: Quickly test max_distance, calipers, or methods

- **Consistency**: Ensures same distances used across comparisons

- **Memory efficient**: Can use sparse matrices when many pairs are
  forbidden

The distance_object stores the original datasets, allowing downstream
functions like
[`join_matched()`](https://gillescolling.com/couplr/reference/join_matched.md)
to work seamlessly.

## Examples

``` r
# Compute distances once
left <- data.frame(id = 1:5, age = c(25, 30, 35, 40, 45), income = c(45, 52, 48, 61, 55) * 1000)
right <- data.frame(id = 6:10, age = c(24, 29, 36, 41, 44), income = c(46, 51, 47, 60, 54) * 1000)

dist_obj <- compute_distances(
  left, right,
  vars = c("age", "income"),
  scale = "standardize"
)

# Reuse for different matching strategies
result1 <- match_couples(dist_obj, max_distance = 0.5)
result2 <- match_couples(dist_obj, max_distance = 1.0)
result3 <- greedy_couples(dist_obj, strategy = "sorted")

# All use the same precomputed distances
```
