# Solve the Bottleneck Assignment Problem

Finds an assignment that minimizes (or maximizes) the maximum edge cost
in a perfect matching. Unlike standard LAP which minimizes the sum of
costs, BAP minimizes the maximum (bottleneck) cost.

## Usage

``` r
bottleneck_assignment(cost, maximize = FALSE)
```

## Arguments

- cost:

  Numeric matrix; rows = tasks, columns = agents. `NA` or `Inf` entries
  are treated as forbidden assignments.

- maximize:

  Logical; if `TRUE`, maximizes the minimum edge cost instead of
  minimizing the maximum (maximin objective). Default is `FALSE`
  (minimax).

## Value

A list with class `"bottleneck_result"` containing:

- `match` - integer vector of length `nrow(cost)` giving the assigned
  column for each row (1-based indexing)

- `bottleneck` - numeric scalar, the bottleneck (max/min edge) value

- `status` - character scalar, e.g. `"optimal"`

## Details

The Bottleneck Assignment Problem (BAP) is a variant of the Linear
Assignment Problem where instead of minimizing the sum of assignment
costs, we minimize the maximum cost among all assignments (minimax
objective).

**Algorithm:** Uses binary search on the sorted unique costs combined
with Hopcroft-Karp bipartite matching to find the minimum threshold that
allows a perfect matching.

**Complexity:** O(E \* sqrt(V) \* log(unique costs)) where E = edges, V
= vertices.

**Applications:**

- Task scheduling with deadline constraints (minimize latest completion)

- Resource allocation (minimize maximum load/distance)

- Network routing (minimize maximum link utilization)

- Fair division problems (minimize maximum disparity)

## See also

[`assignment()`](https://gcol33.github.io/couplr/reference/assignment.md)
for standard LAP (sum objective),
[`lap_solve()`](https://gcol33.github.io/couplr/reference/lap_solve.md)
for tidy LAP interface

## Examples

``` r
# Simple example: minimize max cost
cost <- matrix(c(1, 5, 3,
                 2, 4, 6,
                 7, 1, 2), nrow = 3, byrow = TRUE)
result <- bottleneck_assignment(cost)
result$bottleneck  # Maximum edge cost in optimal assignment

# Maximize minimum (fair allocation)
profits <- matrix(c(10, 5, 8,
                    6, 12, 4,
                    3, 7, 11), nrow = 3, byrow = TRUE)
result <- bottleneck_assignment(profits, maximize = TRUE)
result$bottleneck  # Minimum profit among all assignments

# With forbidden assignments
cost <- matrix(c(1, NA, 3,
                 2, 4, Inf,
                 5, 1, 2), nrow = 3, byrow = TRUE)
result <- bottleneck_assignment(cost)
```
