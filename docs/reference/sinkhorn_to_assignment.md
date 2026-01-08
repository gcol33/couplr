# Round 'Sinkhorn' transport plan to hard assignment

Convert a soft transport plan from
[`sinkhorn()`](https://gcol33.github.io/couplr/reference/sinkhorn.md) to
a hard 1-to-1 assignment using greedy rounding.

## Usage

``` r
sinkhorn_to_assignment(result)
```

## Arguments

- result:

  Either a result from
  [`sinkhorn()`](https://gcol33.github.io/couplr/reference/sinkhorn.md)
  or a transport plan matrix.

## Value

Integer vector of column assignments (1-based), same format as
[`assignment()`](https://gcol33.github.io/couplr/reference/assignment.md).

## Details

Greedy rounding iteratively assigns each row to its most probable
column, ensuring no column is assigned twice. This may not give the
globally optimal hard assignment; for that, use the transport plan as a
cost matrix with
[`assignment()`](https://gcol33.github.io/couplr/reference/assignment.md).

## See also

[`sinkhorn()`](https://gcol33.github.io/couplr/reference/sinkhorn.md)

## Examples

``` r
cost <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, byrow = TRUE)
result <- sinkhorn(cost, lambda = 20)
hard_match <- sinkhorn_to_assignment(result)
print(hard_match)
```
