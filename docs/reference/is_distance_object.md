# Check if Object is a Distance Object

Check if Object is a Distance Object

## Usage

``` r
is_distance_object(x)
```

## Arguments

- x:

  Object to check

## Value

Logical: TRUE if x is a distance_object

## Examples

``` r
left <- data.frame(id = 1:3, x = c(1, 2, 3))
right <- data.frame(id = 4:6, x = c(1.1, 2.1, 3.1))
dist_obj <- compute_distances(left, right, vars = "x")
is_distance_object(dist_obj)  # TRUE
is_distance_object(list())    # FALSE
```
