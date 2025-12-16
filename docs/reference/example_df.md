# Example assignment problem data frame

A tidy data frame representation of assignment problems, suitable for
use with grouped workflows.

## Usage

``` r
example_df
```

## Format

A tibble with 18 rows and 4 columns:

- sim:

  Simulation/problem identifier (1 or 2)

- source:

  Source node index (1, 2, or 3)

- target:

  Target node index (1, 2, or 3)

- cost:

  Cost of assignment

## Examples

``` r
library(dplyr)

# Solve both problems
example_df |>
  group_by(sim) |>
  lap_solve(source, target, cost)

# Or use batch solving
example_df |>
  group_by(sim) |>
  lap_solve_batch(source, target, cost)
```
