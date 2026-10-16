# Find the k Best Assignments

Returns the top k optimal (or near-optimal) assignments using 'Murty'
algorithm. Useful for exploring alternative optimal solutions or finding
robust assignments.

## Usage

``` r
lap_solve_kbest(
  x,
  k = 3,
  source = NULL,
  target = NULL,
  cost = NULL,
  maximize = FALSE,
  method = "murty",
  single_method = "jv",
  forbidden = NA
)

# S3 method for class 'lap_solve_kbest_result'
print(x, ...)

# S3 method for class 'lap_solve_kbest_result'
summary(object, ...)

is_lap_solve_kbest_result(x)
```

## Arguments

- x:

  Object to test

- k:

  Number of best solutions to return (default: 3)

- source:

  Column name for source/row indices (if `x` is a data frame)

- target:

  Column name for target/column indices (if `x` is a data frame)

- cost:

  Column name for costs (if `x` is a data frame)

- maximize:

  Logical; if TRUE, finds k-best maximizing assignments (default: FALSE)

- method:

  Algorithm for each sub-problem (default: "murty"). Future versions may
  support additional methods.

- single_method:

  Algorithm used for solving each node in the search tree (default:
  "jv")

- forbidden:

  Value to mark forbidden assignments (default: NA)

- ...:

  Additional arguments (unused).

- object:

  An object of class `lap_solve_kbest_result`.

## Value

A tibble with columns:

- `rank`: ranking of solutions (1 = best, 2 = second best, etc.)

- `solution_id`: unique identifier for each solution

- `source`: source indices

- `target`: target indices

- `cost`: cost of each edge in the assignment

- `total_cost`: total cost of the complete solution

## Examples

``` r
# Matrix input - find 5 best solutions
cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), nrow = 3)
lap_solve_kbest(cost, k = 5)

# Data frame input
library(dplyr)
df <- tibble(
  source = rep(1:3, each = 3),
  target = rep(1:3, times = 3),
  cost = c(4, 2, 5, 3, 3, 6, 7, 5, 4)
)
lap_solve_kbest(df, k = 3, source, target, cost)

# With maximization
lap_solve_kbest(cost, k = 3, maximize = TRUE)
```
