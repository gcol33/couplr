# Solve multiple assignment problems efficiently

Solve many independent assignment problems at once. Supports lists of
matrices, 3D arrays, or grouped data frames. Optional parallel execution
via `n_threads`.

## Usage

``` r
lap_solve_batch(
  x,
  source = NULL,
  target = NULL,
  cost = NULL,
  maximize = FALSE,
  method = "auto",
  n_threads = 1,
  forbidden = NA
)
```

## Arguments

- x:

  One of: List of cost matrices, 3D array, or grouped data frame

- source:

  Column name for source indices (if `x` is a grouped data frame)

- target:

  Column name for target indices (if `x` is a grouped data frame)

- cost:

  Column name for costs (if `x` is a grouped data frame)

- maximize:

  Logical; if TRUE, maximizes total cost (default: FALSE)

- method:

  Algorithm to use (default: "auto"). See `lap_solve` for options.

- n_threads:

  Number of threads for parallel execution (default: 1). Set to NULL to
  use all available cores.

- forbidden:

  Value to mark forbidden assignments (default: NA)

## Value

A tibble with columns:

- `problem_id`: identifier for each problem

- `source`: source indices for assignments

- `target`: target indices for assignments

- `cost`: cost of each assignment

- `total_cost`: total cost for each problem

- `method_used`: algorithm used for each problem

## Examples

``` r
# List of matrices
costs <- list(
  matrix(c(1, 2, 3, 4), 2, 2),
  matrix(c(5, 6, 7, 8), 2, 2)
)
lap_solve_batch(costs)

# 3D array
arr <- array(runif(2 * 2 * 10), dim = c(2, 2, 10))
lap_solve_batch(arr)

# Grouped data frame
library(dplyr)
df <- tibble(
  sim = rep(1:5, each = 9),
  source = rep(1:3, times = 15),
  target = rep(1:3, each = 3, times = 5),
  cost = runif(45, 1, 10)
)
df |> group_by(sim) |> lap_solve_batch(source, target, cost)

# Parallel execution (requires n_threads > 1)
lap_solve_batch(costs, n_threads = 2)
```
