# Update Constraints on Distance Object

Apply new constraints to a precomputed distance object without
recomputing the underlying distances. This is useful for exploring
different constraint scenarios quickly.

## Usage

``` r
update_constraints(dist_obj, max_distance = Inf, calipers = NULL)
```

## Arguments

- dist_obj:

  A distance_object from
  [`compute_distances()`](https://gillescolling.com/couplr/reference/compute_distances.md)

- max_distance:

  Maximum allowed distance (pairs with distance \> max_distance become
  Inf)

- calipers:

  Named list of per-variable calipers

## Value

A new distance_object with updated cost_matrix

## Details

This function creates a new distance_object with modified constraints
applied to the cost matrix. The original distance_object is not
modified.

Constraints:

- `max_distance`: Sets cost to Inf for pairs exceeding this threshold

- `calipers`: Per-variable restrictions (e.g., calipers = list(age = 5))

The function returns a new object rather than modifying in place,
following R's copy-on-modify semantics.

## Examples

``` r
left <- data.frame(id = 1:5, age = c(25, 30, 35, 40, 45))
right <- data.frame(id = 6:10, age = c(24, 29, 36, 41, 44))
dist_obj <- compute_distances(left, right, vars = "age")

# Apply constraints
constrained <- update_constraints(dist_obj, max_distance = 2)
result <- match_couples(constrained)
```
