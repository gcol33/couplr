# Example assignment problem data frame

A tidy data frame representation of assignment problems, suitable for
use with grouped workflows and batch solving. Contains two independent
3x3 assignment problems in long format.

## Usage

``` r
example_df
```

## Format

A tibble with 18 rows and 4 columns:

- sim:

  Simulation/problem identifier. Integer with values 1 or 2,
  distinguishing two independent assignment problems. Use with
  `group_by(sim)` for grouped solving.

- source:

  Source node index. Integer 1-3 representing the row (source) in each
  3x3 cost matrix.

- target:

  Target node index. Integer 1-3 representing the column (target) in
  each 3x3 cost matrix.

- cost:

  Cost of assigning source to target. Numeric values ranging from 1-7.
  Each source-target pair has exactly one cost entry.

## Details

This dataset demonstrates couplr's data frame interface for LAP solving.
The long format (one row per source-target pair) is converted internally
to a cost matrix for solving.

**Simulation 1**: Costs from `example_costs$simple_3x3`

- Optimal assignment: (1-\>2, 2-\>1, 3-\>3)

- Total cost: 9

**Simulation 2**: Different cost structure

- Optimal assignment: (1-\>1, 2-\>3, 3-\>3) or equivalent

- Total cost: 4

## See also

[`lap_solve`](https://gcol33.github.io/couplr/reference/lap_solve.md),
[`lap_solve_batch`](https://gcol33.github.io/couplr/reference/lap_solve_batch.md),
[`example_costs`](https://gcol33.github.io/couplr/reference/example_costs.md)

## Examples

``` r
library(dplyr)

# Solve both problems with grouped workflow
example_df |>
  group_by(sim) |>
  lap_solve(source, target, cost)

# Batch solving for efficiency
example_df |>
  group_by(sim) |>
  lap_solve_batch(source, target, cost)

# Inspect the data structure
example_df |>
  group_by(sim) |>
  summarise(
    n_pairs = n(),
    min_cost = min(cost),
    max_cost = max(cost)
  )
```
