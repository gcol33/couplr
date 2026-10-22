# Solve Many Assignment Problems

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

# S3 method for class 'lap_solve_batch_result'
print(x, ...)

# S3 method for class 'lap_solve_batch_result'
summary(object, ...)

is_lap_solve_batch_result(x)
```

## Arguments

- x:

  Object to test

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
  use all available cores but one, as reported by
  [`parallelly::availableCores()`](https://parallelly.futureverse.org/reference/availableCores.html).

- forbidden:

  Value to mark forbidden assignments (default: NA)

- ...:

  Additional arguments passed to
  [`print()`](https://rdrr.io/r/base/print.html). Currently ignored.

- object:

  A `lap_solve_batch_result`.

## Value

A tibble with columns:

- `problem_id`: identifier for each problem

- `source`: source indices for assignments

- `target`: target indices for assignments

- `cost`: cost of each assignment

- `total_cost`: total cost for each problem

- `method_used`: algorithm used for each problem

[`summary()`](https://rdrr.io/r/base/summary.html) returns a
`summary.lap_solve_batch_result` whose `problems` table has one row per
input problem, with grouping keys, assignment counts, total costs, and
solver names. Each problem contributes once.

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
