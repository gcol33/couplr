# Quick Start

## What couplr Does

couplr solves the Linear Assignment Problem (LAP): given a cost matrix
where entry (i, j) represents the cost of assigning source i to target
j, find the one-to-one assignment that minimizes total cost. It also
provides production-ready matching workflows for observational studies.

### Documentation Roadmap

| Vignette | Focus | Audience | Time |
|----|----|----|----|
| **Quick Start** (this) | Basic LAP solving, API introduction | Everyone | 10 min |
| [Algorithms](https://gcol33.github.io/couplr/articles/algorithms.md) | Mathematical foundations, solver selection | Technical users | 30 min |
| [Matching Workflows](https://gcol33.github.io/couplr/articles/matching-workflows.md) | Production matching pipelines | Researchers | 45 min |
| [Comparison](https://gcol33.github.io/couplr/articles/comparison.md) | vs MatchIt, optmatch, designmatch | Package evaluators | 20 min |
| [Troubleshooting](https://gcol33.github.io/couplr/articles/troubleshooting.md) | Common issues and solutions | All users | As needed |
| [Pixel Morphing](https://gcol33.github.io/couplr/articles/pixel-morphing.md) | Scientific applications | Advanced | 30 min |

**Start here**, then proceed to whichever vignette matches your use
case.

## Interface Hierarchy

couplr provides three levels of interface:

### Level 1: LAP Solvers

**lap_solve()** - Core assignment problem solver:

- Accepts matrices, data frames, or grouped data
- Returns tidy tibble output
- Automatic algorithm selection

**assignment()** - Low-level solver:

- Direct C++ backend access
- Returns list with match vector

### Level 2: Extended Solving

**lap_solve_batch()** - Solve many problems at once:

- List of cost matrices input
- Optional parallel execution
- Efficient for many small problems

**lap_solve_kbest()** - Find multiple solutions:

- Returns k-best assignments (Murty’s algorithm)
- Useful for robustness analysis

### Level 3: Matching Workflows

**match_couples()** - Optimal one-to-one matching:

- Automatic preprocessing and scaling
- Balance diagnostics
- Production-ready for observational studies

**greedy_couples()** - Fast approximate matching:

- 10-100x faster than optimal
- Three strategy options

------------------------------------------------------------------------

## Quick Examples

### lap_solve(): Basic Assignment

``` r

library(couplr)

# Cost matrix: 3 nurses × 3 shifts
cost <- matrix(c(
  4, 2, 5,
  3, 3, 6,
  7, 5, 4
), nrow = 3, byrow = TRUE)

result <- lap_solve(cost)
print(result)
#> Assignment Result
#> =================
#> 
#> # A tibble: 3 × 3
#>   source target  cost
#>    <int>  <int> <dbl>
#> 1      1      2     2
#> 2      2      1     3
#> 3      3      3     4
#> 
#> Total cost: 9 
#> Method: bruteforce
```

**Reading the output**: Row 1 shows source 1 assigned to target 2 with
cost 2. The `total_cost` column shows the cumulative cost (9).

### Data Frame Input

``` r

library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

# Long-format data frame
schedule_df <- tibble(

  nurse = rep(1:3, each = 3),
  shift = rep(1:3, times = 3),
  cost = c(4, 2, 5, 3, 3, 6, 7, 5, 4)
)

# Solve using column names
lap_solve(schedule_df, nurse, shift, cost)
#> Assignment Result
#> =================
#> 
#> # A tibble: 3 × 3
#>   source target  cost
#>    <int>  <int> <dbl>
#> 1      1      2     2
#> 2      2      1     3
#> 3      3      3     4
#> 
#> Total cost: 9 
#> Method: bruteforce
```

### Rectangular Problems

``` r

# 3 nurses, 5 shifts - assign each nurse to one shift
cost_rect <- matrix(c(
  1, 2, 3, 4, 5,
  6, 5, 4, 3, 2,
  2, 3, 4, 5, 6
), nrow = 3, byrow = TRUE)

lap_solve(cost_rect)
#> Assignment Result
#> =================
#> 
#> # A tibble: 3 × 3
#>   source target  cost
#>    <int>  <int> <dbl>
#> 1      1      1     1
#> 2      2      5     2
#> 3      3      2     3
#> 
#> Total cost: 6 
#> Method: bruteforce
```

When rows \< columns, each row gets one column, and some columns remain
unassigned.

### Forbidden Assignments

Use `NA` or `Inf` to mark impossible assignments:

``` r

cost_forbidden <- matrix(c(
  4, 2, NA,   # Source 1 cannot go to target 3
  Inf, 3, 6,  # Source 2 cannot go to target 1
  7, 5, 4
), nrow = 3, byrow = TRUE)

lap_solve(cost_forbidden)
#> Assignment Result
#> =================
#> 
#> # A tibble: 3 × 3
#>   source target  cost
#>    <int>  <int> <dbl>
#> 1      1      1     4
#> 2      2      2     3
#> 3      3      3     4
#> 
#> Total cost: 11 
#> Method: bruteforce
```

------------------------------------------------------------------------

## Maximization Problems

Switch to maximization with `maximize = TRUE`:

``` r

# Preference scores (higher = better)
preferences <- matrix(c(
  8, 5, 3,
  4, 7, 6,
  2, 4, 9
), nrow = 3, byrow = TRUE)

lap_solve(preferences, maximize = TRUE)
#> Assignment Result
#> =================
#> 
#> # A tibble: 3 × 3
#>   source target  cost
#>    <int>  <int> <dbl>
#> 1      1      1     8
#> 2      2      2     7
#> 3      3      3     9
#> 
#> Total cost: 24 
#> Method: bruteforce
```

------------------------------------------------------------------------

## Grouped Data

Solve multiple problems at once with grouped data frames:

``` r

# Weekly schedule: 3 days × 3 nurses × 3 shifts
weekly_df <- tibble(
  day = rep(c("Mon", "Tue", "Wed"), each = 9),
  nurse = rep(rep(1:3, each = 3), times = 3),
  shift = rep(1:3, times = 9),
  cost = runif(27, 1, 10)
)

# Solve all days at once
weekly_df |>
  group_by(day) |>
  lap_solve(nurse, shift, cost) |>
  group_by(day) |>
  summarise(total_cost = sum(cost), .groups = "drop")
#> # A tibble: 3 × 2
#>   day   total_cost
#>   <chr>      <dbl>
#> 1 Mon        10.4 
#> 2 Tue         8.51
#> 3 Wed        13.6
```

------------------------------------------------------------------------

## Batch Solving

For many independent problems, use
[`lap_solve_batch()`](https://gcol33.github.io/couplr/reference/lap_solve_batch.md):

``` r

# 100 random cost matrices
set.seed(123)
cost_list <- lapply(1:100, function(i) matrix(runif(9, 1, 10), 3, 3))

# Solve all at once
batch_results <- lap_solve_batch(cost_list)

# Summary
batch_results |>
  distinct(problem_id, total_cost) |>
  summarise(
    n_problems = n(),
    mean_cost = mean(total_cost),
    min_cost = min(total_cost),
    max_cost = max(total_cost)
  )
#> # A tibble: 1 × 4
#>   n_problems mean_cost min_cost max_cost
#>        <int>     <dbl>    <dbl>    <dbl>
#> 1        100      11.3     5.20     19.1
```

Enable parallel processing for large batches:

``` r

lap_solve_batch(cost_list, n_threads = 4)
```

------------------------------------------------------------------------

## K-Best Solutions

Find multiple near-optimal solutions:

``` r

cost <- matrix(c(1, 2, 3, 4, 3, 2, 5, 4, 1), nrow = 3, byrow = TRUE)

# Find top 5 solutions
kbest <- lap_solve_kbest(cost, k = 5)
print(kbest)
#> K-Best Assignment Results
#> =========================
#> 
#> Number of solutions: 5 
#> 
#> Solution costs:
#>   Rank 1: 5.0000
#>   Rank 2: 7.0000
#>   Rank 3: 7.0000
#>   Rank 4: 9.0000
#>   Rank 5: 11.0000
#> 
#> Assignments:
#> # A tibble: 15 × 6
#>     rank solution_id source target  cost total_cost
#>    <int>       <int>  <int>  <int> <dbl>      <dbl>
#>  1     1           1      1      1     1          5
#>  2     1           1      2      2     3          5
#>  3     1           1      3      3     1          5
#>  4     2           2      1      2     2          7
#>  5     2           2      2      1     4          7
#>  6     2           2      3      3     1          7
#>  7     3           3      1      1     1          7
#>  8     3           3      2      3     2          7
#>  9     3           3      3      2     4          7
#> 10     4           4      1      2     2          9
#> 11     4           4      2      3     2          9
#> 12     4           4      3      1     5          9
#> 13     5           5      1      3     3         11
#> 14     5           5      2      2     3         11
#> 15     5           5      3      1     5         11
```

Useful for exploring alternatives when the optimal solution is
infeasible in practice.

------------------------------------------------------------------------

## Algorithm Selection

couplr includes 12+ algorithms with automatic selection:

``` r

# Let couplr choose (default)
result <- lap_solve(cost, method = "auto")
get_method_used(result)
#> [1] "bruteforce"
```

Force specific algorithms:

``` r

lap_solve(cost, method = "jv")         # Jonker-Volgenant (general purpose)
lap_solve(cost, method = "hungarian")  # Hungarian algorithm
lap_solve(cost, method = "auction")    # Auction algorithm
lap_solve(cost, method = "hk01")       # For binary (0/1) costs
```

**Selection guide:**

| Problem Type            | Recommended Method |
|-------------------------|--------------------|
| General case            | `"auto"` or `"jv"` |
| Binary costs (0/1)      | `"hk01"`           |
| Large dense (n \> 1000) | `"auction"`        |
| Sparse/many forbidden   | `"sap"`            |

------------------------------------------------------------------------

## Utility Functions

``` r

result <- lap_solve(cost)

# Extract total cost
get_total_cost(result)
#> [1] 5

# Get algorithm used
get_method_used(result)
#> [1] "bruteforce"

# Convert to binary assignment matrix
as_assignment_matrix(result)
#>      [,1] [,2] [,3]
#> [1,]    1    0    0
#> [2,]    0    1    0
#> [3,]    0    0    1
```

------------------------------------------------------------------------

## Performance Guide

| Problem Size | Typical Runtime | Notes |
|----|----|----|
| \< 100×100 | \< 0.01s | Any method works |
| 100-500 | 0.01-0.1s | `method = "auto"` |
| 500-1000 | 0.1-1s | Consider `"jv"` |
| 1000-3000 | 1-30s | Use `"auction"` |
| \> 3000 | \> 30s | See [`vignette("matching-workflows")`](https://gcol33.github.io/couplr/articles/matching-workflows.md) |

------------------------------------------------------------------------

## Common Issues

### “All assignments have Inf cost”

**Cause**: Too many forbidden entries make the problem infeasible.

**Solution**: Ensure each row has at least one finite value.

``` r

cost_check <- matrix(c(1, NA, NA, NA, NA, NA, NA, 2, 3), nrow = 3, byrow = TRUE)
feasible <- rowSums(is.finite(cost_check)) > 0
if (!all(feasible)) cat("Infeasible rows:", which(!feasible), "\n")
#> Infeasible rows: 2
```

### Different results with different methods

**Cause**: Multiple optimal solutions may exist.

**Solution**: Use `method = "hungarian"` for deterministic tie-breaking,
or verify total costs match.

### Slow performance on large problems

**Solutions**:

- n \> 1000: Use `method = "auction"`
- n \> 3000: Use blocking via
  [`vignette("matching-workflows")`](https://gcol33.github.io/couplr/articles/matching-workflows.md)
- n \> 5000: Use
  [`greedy_couples()`](https://gcol33.github.io/couplr/reference/greedy_couples.md)
  for approximate solutions

------------------------------------------------------------------------

## See Also

- [`vignette("algorithms")`](https://gcol33.github.io/couplr/articles/algorithms.md) -
  Mathematical foundations and solver internals
- [`vignette("matching-workflows")`](https://gcol33.github.io/couplr/articles/matching-workflows.md) -
  Production matching for observational studies
- [`vignette("comparison")`](https://gcol33.github.io/couplr/articles/comparison.md) -
  How couplr compares to MatchIt, optmatch, designmatch
- [`vignette("troubleshooting")`](https://gcol33.github.io/couplr/articles/troubleshooting.md) -
  Common issues and solutions
- [`vignette("pixel-morphing")`](https://gcol33.github.io/couplr/articles/pixel-morphing.md) -
  Large-scale approximation strategies
- [`?lap_solve`](https://gcol33.github.io/couplr/reference/lap_solve.md),
  [`?lap_solve_batch`](https://gcol33.github.io/couplr/reference/lap_solve_batch.md),
  [`?lap_solve_kbest`](https://gcol33.github.io/couplr/reference/lap_solve_kbest.md),
  [`?match_couples`](https://gcol33.github.io/couplr/reference/match_couples.md)
