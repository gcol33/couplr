# Getting Started with couplr

## Overview

The linear assignment problem (LAP) is a fundamental optimization
problem: given a cost matrix where entry (i, j) represents the cost of
assigning source i to target j, find the one-to-one assignment that
minimizes (or maximizes) total cost. LAP appears throughout data
science, operations research, and scientific computing – from scheduling
and resource allocation to computer vision and bioinformatics. `couplr`
provides efficient LAP solvers with a tidy interface that integrates
naturally with modern R workflows.

### Why couplr?

While other R packages address LAP, `couplr` distinguishes itself
through: 1. **Tidy integration**: First-class support for tibbles, dplyr
workflows, and grouped data 2. **Algorithm selection**: 12+ algorithms
with automatic selection based on problem characteristics 3.
**Production matching**: High-level matching functions for observational
studies (v1.0.0) 4. **Scalability**: From small matrices to thousands of
parallel problems

**Alternative packages** and how they differ:

- `clue`: General-purpose optimization toolkit where LAP is one feature
  among many
- `lpSolve`: Broad linear programming focus, less specialized for
  assignment
- `RcppHungarian`: Single algorithm implementation without tidy
  interface

`couplr` focuses specifically on assignment problems with a
user-friendly API and modern R idioms.

### Who This Vignette Is For

**Audience**: Beginners to couplr, R users familiar with basic matrix
operations

**Prerequisites**:

- Basic R knowledge (data frames, functions)
- Understanding of what a “cost” or “distance” matrix means

**What You’ll Learn**:

- How to solve basic assignment problems with
  [`lap_solve()`](https://gcol33.github.io/couplr/reference/lap_solve.md)
- Working with data frames, rectangular problems, and forbidden
  assignments
- Batch solving and finding multiple solutions
- When to use different algorithms

**Time to complete**: 20-30 minutes

### Documentation Roadmap

The couplr documentation is organized as follows:

| Vignette | Focus | Difficulty |
|----|----|----|
| **Getting Started** | Basic LAP solving, API introduction | Beginner |
| Algorithms | Mathematical foundations, solver selection | Intermediate |
| Matching Workflows | Production matching pipelines | Intermediate |
| Pixel Morphing | Scientific applications, approximations | Advanced |

*You are here: Getting Started*

### Key Features

- **Tidy interface**: Works with data frames, tibbles, and grouped data
- **Efficient**: Implements multiple algorithms with automatic selection
- **Flexible inputs**: Handles matrices, data frames, rectangular
  problems, and forbidden assignments
- **Batch processing**: Solve thousands of problems efficiently with
  optional parallelization
- **K-best solutions**: Find multiple near-optimal solutions using
  Murty’s algorithm

## Installation

``` r

# Install from CRAN
install.packages("couplr")

# Or install development version from GitHub
# install.packages("remotes")
remotes::install_github("gcol33/couplr")
```

## The Assignment Problem

### Problem Definition

Consider assigning nurses to shifts in a hospital. Each nurse has
different suitability for each shift based on skills, travel time, and
overtime likelihood. The assignment problem finds the optimal one-to-one
matching that minimizes total cost (or maximizes total preference).

This vignette uses a recurring **hospital staff scheduling** example
that demonstrates increasingly complex scenarios. The same conceptual
problem – assigning nurses to shifts – evolves to show different couplr
features.

**Simple Example**: Three nurses can cover three shifts with the
following costs (lower = better fit):

|         | Shift 1 | Shift 2 | Shift 3 |
|---------|---------|---------|---------|
| Nurse 1 | 4       | 2       | 5       |
| Nurse 2 | 3       | 3       | 6       |
| Nurse 3 | 7       | 5       | 4       |

The optimal assignment is: Nurse 1 → Shift 2 (cost 2), Nurse 2 → Shift 1
(cost 3), Nurse 3 → Shift 3 (cost 4), for a total cost of 9.

> **Note**: couplr includes a comprehensive `hospital_staff` dataset
> that we’ll use throughout this vignette. Type
> [`?hospital_staff`](https://gcol33.github.io/couplr/reference/hospital_staff.md)
> for details.

## Basic Usage

### Matrix Input

The simplest way to use `couplr` is with a numeric cost matrix:

``` r

# Cost matrix: 3 nurses × 3 shifts (from our simple example above)
cost <- matrix(c(
  4, 2, 5,  # Nurse 1 costs for shifts 1, 2, 3
  3, 3, 6,  # Nurse 2 costs
  7, 5, 4   # Nurse 3 costs
), nrow = 3, byrow = TRUE)

# Solve the assignment problem
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

The result is a tidy tibble showing which source (nurse) is assigned to
which target (shift), along with individual and total costs.

**Reading the output**: Row 1 shows Nurse 1 assigned to Shift 2 with
cost 2. The `total_cost` column shows the cumulative cost of all
assignments (9).

For larger problems, use the built-in `hospital_staff` dataset:

``` r

# 10 nurses × 10 shifts
result_10x10 <- lap_solve(hospital_staff$basic_costs)
head(result_10x10)
#> Assignment Result
#> =================
#> 
#> # A tibble: 6 × 3
#>   source target  cost
#>    <int>  <int> <dbl>
#> 1      1      8     2
#> 2      2      2     2
#> 3      3      7     2
#> 4      4      4     2
#> 5      5      9     2
#> 6      6     10     3
#> 
#> Total cost: 22 
#> Method: hungarian

# Total cost
cat("Total assignment cost:", get_total_cost(result_10x10), "\n")
#> Total assignment cost: 22
```

### Data Frame Input

For more complex workflows, especially when working with existing data
pipelines, use data frames:

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

# The hospital_staff dataset includes a data frame version
head(hospital_staff$schedule_df)
#> # A tibble: 6 × 5
#>   nurse_id shift_id  cost preference skill_match
#>      <int>    <int> <dbl>      <dbl>       <int>
#> 1        1        1     3          8           0
#> 2        1        2     7          4           0
#> 3        1        3     5          6           1
#> 4        1        4     9          2           0
#> 5        1        5     4          7           1
#> 6        1        6     8          3           1

# Solve using column names
result <- lap_solve(hospital_staff$schedule_df, nurse_id, shift_id, cost)
print(result)
#> Assignment Result
#> =================
#> 
#> # A tibble: 10 × 3
#>    source target  cost
#>     <int>  <int> <dbl>
#>  1      1      8     2
#>  2      2      2     2
#>  3      3      7     2
#>  4      4      4     2
#>  5      5      9     2
#>  6      6     10     3
#>  7      7      1     2
#>  8      8      3     2
#>  9      9      6     3
#> 10     10      5     2
#> 
#> Total cost: 22 
#> Method: hungarian
```

This approach is particularly useful when your data is already in long
format, when pulling from databases, or when integrating with dplyr
workflows.

------------------------------------------------------------------------

*Now that we can solve the basic problem, what happens when we have more
shifts than nurses to fill them?*

## Working with Rectangular Problems

Real scheduling problems rarely have equal numbers of nurses and shifts.
A hospital might have 8 nurses available but need to cover 12 shifts, or
have 15 nurses but only 10 shifts to fill.

Unlike many assignment solvers, `couplr` handles rectangular problems
(unequal numbers of sources and targets) without manual padding:

``` r

# 3 nurses, 5 available shifts - assign each nurse to one shift
cost_rect <- matrix(c(
  1, 2, 3, 4, 5,   # Nurse 1's costs for 5 shifts
  6, 5, 4, 3, 2,   # Nurse 2's costs
  2, 3, 4, 5, 6    # Nurse 3's costs
), nrow = 3, byrow = TRUE)

result <- lap_solve(cost_rect)
print(result)
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

The solver automatically handles the rectangular structure, assigning
each of the 3 nurses to their best available shift among the 5 options.
Notice that shifts 3 and 4 remain unassigned (they’re not in the
`target` column).

**Interpretation**: When rows \< columns, each row (nurse) gets exactly
one column (shift), and some columns remain unassigned. When rows \>
columns, the matrix is transposed internally so each column gets one
row.

------------------------------------------------------------------------

*What if certain nurse-shift combinations are impossible due to
scheduling conflicts or skill requirements?*

## Forbidden Assignments

In real scheduling, some assignments are simply impossible: a nurse
lacks the required certification for an ICU shift, or has a scheduling
conflict that makes a night shift impossible.

Use `NA` or `Inf` to mark impossible or forbidden assignments:

``` r

# Fresh cost matrix
cost <- matrix(c(
  4, 2, 5,
  3, 3, 6,
  7, 5, 4
), nrow = 3, byrow = TRUE)

# Nurse 1 cannot work Shift 3 (lacks ICU certification)
cost[1, 3] <- NA

# Nurse 2 cannot work Shift 1 (scheduling conflict)
cost[2, 1] <- Inf

result <- lap_solve(cost)
print(result)
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

The solver respects these constraints and finds the optimal assignment
among the feasible options. Compare this result to the unconstrained
solution: Nurse 1 still gets Shift 2 (cost 2), but the other assignments
adjust around the constraints.

**When to use `NA` vs `Inf`**: Both mark forbidden assignments. Use `NA`
for “not applicable” (e.g., skill mismatch) and `Inf` for “infinitely
costly” (e.g., severe overtime). Functionally they behave identically.

------------------------------------------------------------------------

*So far we’ve minimized costs. What about maximizing preferences
instead?*

## Maximization Problems

Sometimes you want to maximize preferences rather than minimize costs.
Nurses have shift preferences (higher = more preferred), and happier
nurses mean lower turnover.

Switch to maximization by setting `maximize = TRUE`:

``` r

# Preference matrix from hospital_staff (0-10 scale, higher = more preferred)
head(hospital_staff$preferences)
#>         Shift_1 Shift_2 Shift_3 Shift_4 Shift_5 Shift_6 Shift_7 Shift_8 Shift_9
#> Nurse_1       8       4       6       2       7       3       5       9       4
#> Nurse_2       5       9       3       7       4       8       2       6       7
#> Nurse_3       6       7       8       4       3       5       9       2       6
#> Nurse_4       3       5       4       9       6       7       8       5       3
#> Nurse_5       7       2       5       6       8       4       3       7       9
#> Nurse_6       4       8       7       3       5       9       6       4       2
#>         Shift_10
#> Nurse_1        6
#> Nurse_2        5
#> Nurse_3        7
#> Nurse_4        4
#> Nurse_5        2
#> Nurse_6        8

# Maximize total nurse satisfaction
result <- lap_solve(hospital_staff$preferences, maximize = TRUE)
head(result)
#> Assignment Result
#> =================
#> 
#> # A tibble: 6 × 3
#>   source target  cost
#>    <int>  <int> <dbl>
#> 1      1      8     9
#> 2      2      2     9
#> 3      3      7     9
#> 4      4      4     9
#> 5      5      9     9
#> 6      6     10     8
#> 
#> Total cost: 88 
#> Method: hungarian

cat("\nTotal preference score:", get_total_cost(result), "\n")
#> 
#> Total preference score: 88
```

**Behind the scenes**: `maximize = TRUE` internally negates the costs,
solves the minimization problem, then reports results on the original
scale.

------------------------------------------------------------------------

*What if you need to schedule multiple days at once, each with its own
cost matrix?*

## Grouped Data Frames

One of `couplr`’s most powerful features is seamless integration with
grouped data frames, allowing you to solve many related problems at
once.

The `hospital_staff$weekly_df` dataset contains scheduling costs for 5
days. Each day needs its own optimal assignment:

``` r

# Weekly schedule: 5 days × 10 nurses × 10 shifts
head(hospital_staff$weekly_df)
#> # A tibble: 6 × 5
#>   day    nurse_id shift_id  cost preference
#>   <fct>     <int>    <int> <dbl>      <dbl>
#> 1 Monday        1        1     2          7
#> 2 Monday        1        2     5          3
#> 3 Monday        1        3     4          6
#> 4 Monday        1        4    11          3
#> 5 Monday        1        5     6          8
#> 6 Monday        1        6     9          4

# Solve all days at once
weekly_results <- hospital_staff$weekly_df |>
  group_by(day) |>
  lap_solve(nurse_id, shift_id, cost)

# View assignments by day
weekly_results |>
  group_by(day) |>
  summarise(total_cost = sum(cost), .groups = "drop")
#> # A tibble: 5 × 2
#>   day       total_cost
#>   <fct>          <dbl>
#> 1 Monday            18
#> 2 Tuesday           15
#> 3 Wednesday         25
#> 4 Thursday          18
#> 5 Friday            20
```

This pattern—grouping, then solving—works naturally with dplyr pipelines
and is much cleaner than writing loops.

## Batch Solving

------------------------------------------------------------------------

*For even larger scale—hundreds or thousands of independent
problems—grouped data frames may become unwieldy. That’s where batch
solving shines.*

For maximum efficiency when solving many independent problems, use
[`lap_solve_batch()`](https://gcol33.github.io/couplr/reference/lap_solve_batch.md):

``` r

# Create list of cost matrices
set.seed(123)
cost_list <- lapply(1:100, function(i) matrix(runif(9, 1, 10), 3, 3))

# Solve all problems at once
batch_results <- lap_solve_batch(cost_list)

# View summary statistics
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

### Parallel Batch Solving

For large numbers of problems, enable parallel processing:

``` r

# Solve with 4 threads
batch_results <- lap_solve_batch(cost_list, n_threads = 4)

# Use all available cores
batch_results <- lap_solve_batch(cost_list, n_threads = NULL)
```

## Finding Multiple Solutions

------------------------------------------------------------------------

*The optimal schedule might be mathematically perfect but practically
impossible—Nurse 1 is on vacation that week. What are the alternatives?*

Sometimes you want to explore alternative near-optimal solutions. Use
[`lap_solve_kbest()`](https://gcol33.github.io/couplr/reference/lap_solve_kbest.md)
to find the k best solutions:

``` r

cost <- matrix(c(
  1, 2, 3,
  4, 3, 2,
  5, 4, 1
), nrow = 3, byrow = TRUE)

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

# Get summary of solutions
summary(kbest)
#> # A tibble: 5 × 4
#>    rank solution_id total_cost n_assignments
#>   <int>       <int>      <dbl>         <int>
#> 1     1           1          5             3
#> 2     2           2          7             3
#> 3     3           3          7             3
#> 4     4           4          9             3
#> 5     5           5         11             3
```

This is useful for:

- Exploring alternative plans when the optimal solution is infeasible in
  practice
- Robustness analysis
- Understanding the cost landscape around the optimum

## Extracting and Working with Results

`couplr` provides several utility functions for working with results:

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

# Check result type
is_lap_solve_result(result)
#> [1] TRUE
```

## Algorithm Selection

------------------------------------------------------------------------

*By default, couplr chooses an algorithm automatically. But
understanding the options can help with performance tuning or
debugging.*

`couplr` includes multiple algorithms optimized for different scenarios.
The default `"auto"` method automatically selects the best algorithm:

``` r

# Let couplr choose (default)
lap_solve(cost, method = "auto")
#> Assignment Result
#> =================
#> 
#> # A tibble: 3 × 3
#>   source target  cost
#>    <int>  <int> <dbl>
#> 1      1      1     1
#> 2      2      2     3
#> 3      3      3     1
#> 
#> Total cost: 5 
#> Method: bruteforce

# Force specific algorithm:
lap_solve(cost, method = "jv")         # Jonker-Volgenant (general purpose)
#> Assignment Result
#> =================
#> 
#> # A tibble: 3 × 3
#>   source target  cost
#>    <int>  <int> <dbl>
#> 1      1      1     1
#> 2      2      2     3
#> 3      3      3     1
#> 
#> Total cost: 5 
#> Method: jv
lap_solve(cost, method = "hungarian")  # Hungarian algorithm
#> Assignment Result
#> =================
#> 
#> # A tibble: 3 × 3
#>   source target  cost
#>    <int>  <int> <dbl>
#> 1      1      1     1
#> 2      2      2     3
#> 3      3      3     1
#> 
#> Total cost: 5 
#> Method: hungarian
lap_solve(cost, method = "auction")    # Auction algorithm
#> Assignment Result
#> =================
#> 
#> # A tibble: 3 × 3
#>   source target  cost
#>    <int>  <int> <dbl>
#> 1      1      1     1
#> 2      2      2     3
#> 3      3      3     1
#> 
#> Total cost: 5 
#> Method: auction
lap_solve(cost, method = "sap")        # Sparse assignment (for sparse problems)
#> Assignment Result
#> =================
#> 
#> # A tibble: 3 × 3
#>   source target  cost
#>    <int>  <int> <dbl>
#> 1      1      1     1
#> 2      2      2     3
#> 3      3      3     1
#> 
#> Total cost: 5 
#> Method: sap
```

### Auction Algorithm Variants

The auction algorithm has multiple variants for different problem
characteristics:

``` r

# Standard fixed-epsilon auction (default)
lap_solve(cost, method = "auction")

# Scaled-epsilon auction with epsilon-scaling phases
# Better for problems with large cost ranges
lap_solve(cost, method = "auction_scaled")

# Gauss-Seidel auction with sequential bidding
# Can be faster for certain problem structures
lap_solve(cost, method = "auction_gs")
```

### Binary Cost Problems

For problems where all costs are 0 or 1, use the specialized HK01
algorithm:

``` r

# Create binary cost matrix
binary_cost <- matrix(sample(0:1, 9, replace = TRUE), 3, 3)

# Specialized algorithm for binary costs
lap_solve(binary_cost, method = "hk01")
#> Assignment Result
#> =================
#> 
#> # A tibble: 3 × 3
#>   source target  cost
#>    <int>  <int> <int>
#> 1      1      2     0
#> 2      2      3     0
#> 3      3      1     0
#> 
#> Total cost: 0 
#> Method: hk01
```

The `"auto"` method selects algorithms based on problem characteristics:

- **Binary/uniform costs** → HK01 algorithm
- **Sparse/rectangular** → SAP algorithm  
- **Large dense** → Auction algorithm (standard variant)
- **General case** → Jonker-Volgenant

For most users, `method = "auto"` (the default) provides good
performance.

### Parallelization

**Note on parallelization**: Currently, only
**[`lap_solve_batch()`](https://gcol33.github.io/couplr/reference/lap_solve_batch.md)**
supports parallel execution via the `n_threads` parameter. The
individual LAP solvers
([`lap_solve()`](https://gcol33.github.io/couplr/reference/lap_solve.md),
[`lap_solve_kbest()`](https://gcol33.github.io/couplr/reference/lap_solve_kbest.md))
run sequentially. Future versions may add parallel support for single
large problems.

``` r

# Parallel batch solving (supported)
lap_solve_batch(cost_list, n_threads = 4)

# Single problem (currently sequential)
lap_solve(cost)  # No parallelization yet
```

## Real-World Example: Employee Scheduling

Here’s a practical example of using `couplr` to create a weekly work
schedule:

``` r

set.seed(42)

# Generate employee preferences for shifts
schedule_data <- tibble(
  day = rep(c("Mon", "Tue", "Wed", "Thu", "Fri"), each = 12),
  employee = rep(1:4, times = 15),
  shift = rep(c("morning", "afternoon", "night"), each = 4, times = 5),
  preference_score = rnorm(60, mean = 5, sd = 2)
) |>
  mutate(
    # Some employees can't work certain shifts
    preference_score = case_when(
      employee == 1 & shift == "night" ~ NA_real_,
      employee == 2 & shift == "morning" ~ NA_real_,
      TRUE ~ preference_score
    ),
    # Create shift IDs within each day
    shift_id = as.integer(factor(shift, levels = c("morning", "afternoon", "night")))
  )

# Solve for optimal assignments per day (maximize preferences)
optimal_schedule <- schedule_data |>
  group_by(day) |>
  lap_solve(employee, shift_id, preference_score, maximize = TRUE)

# View first few assignments
head(optimal_schedule, 10)
#> # A tibble: 10 × 4
#>    day   source target  cost
#>    <chr>  <int>  <int> <dbl>
#>  1 Fri        1      2  8.15
#>  2 Fri        3      1  5.64
#>  3 Fri        4      3  5.57
#>  4 Mon        1      1  7.74
#>  5 Mon        3      2  8.02
#>  6 Mon        4      3  9.57
#>  7 Thu        1      1  3.43
#>  8 Thu        3      2  6.52
#>  9 Thu        4      3  7.89
#> 10 Tue        1      2  4.43

# Summary statistics by day
optimal_schedule |>
  group_by(day) |>
  summarise(
    shifts_filled = n(),
    avg_preference = mean(cost, na.rm = TRUE),
    .groups = "drop"
  )
#> # A tibble: 5 × 3
#>   day   shifts_filled avg_preference
#>   <chr>         <int>          <dbl>
#> 1 Fri               3           6.46
#> 2 Mon               3           8.45
#> 3 Thu               3           5.95
#> 4 Tue               3           5.53
#> 5 Wed               3           7.07
```

## Performance Considerations

`couplr` is designed for efficiency:

- **C++ backend**: Core algorithms implemented in optimized C++ using
  Rcpp
- **Automatic algorithm selection**: Chooses an efficient method for
  your problem
- **Batch processing**: Amortizes overhead when solving many problems
- **Parallel execution**: Distributes work across cores for large
  batches

For small problems (\< 100×100), setup overhead dominates and all
methods are fast. For large problems (\> 1000×1000) or many problems,
algorithm selection and parallelization become important.

| Problem Size | Typical Runtime | Recommendation |
|----|----|----|
| \< 100×100 | \< 0.01s | Any method works |
| 100-500 | 0.01-0.1s | `method = "auto"` |
| 500-1000 | 0.1-1s | Consider `method = "jv"` |
| 1000-3000 | 1-30s | Use `method = "auction"` |
| \> 3000 | \> 30s | See [`vignette("matching-workflows")`](https://gcol33.github.io/couplr/articles/matching-workflows.md) for blocking strategies |

## Common Pitfalls and Troubleshooting

### Problem: “All assignments have Inf cost”

**Cause**: Too many forbidden entries (NA/Inf) make the problem
infeasible.

**Solution**: Check that a feasible solution exists. For rectangular
problems with forbidden entries, ensure at least one valid assignment
exists for each source.

``` r

# Check feasibility before solving
cost_problematic <- matrix(c(
  1, NA, NA,
  NA, NA, NA,  # Row 2 has no valid targets!
  NA, 2, 3
), nrow = 3, byrow = TRUE)

# Check which rows have at least one finite value
feasible <- rowSums(is.finite(cost_problematic)) > 0
if (!all(feasible)) {
  cat("Infeasible rows:", which(!feasible), "\n")
}
#> Infeasible rows: 2
```

### Problem: Unexpected assignments

**Cause**: Cost matrix orientation wrong (rows vs columns swapped).

**Solution**: Remember: rows = sources (nurses), columns = targets
(shifts). The result shows source→target assignments.

``` r

# Verify orientation
cost_check <- matrix(c(1, 100, 100, 1), nrow = 2)
rownames(cost_check) <- c("Nurse_A", "Nurse_B")
colnames(cost_check) <- c("Shift_1", "Shift_2")
print(cost_check)
#>         Shift_1 Shift_2
#> Nurse_A       1     100
#> Nurse_B     100       1

# Nurse_A should get Shift_1 (cost 1), Nurse_B should get Shift_2 (cost 1)
lap_solve(cost_check)
#> Assignment Result
#> =================
#> 
#> # A tibble: 2 × 3
#>   source target  cost
#>    <int>  <int> <dbl>
#> 1      1      1     1
#> 2      2      2     1
#> 
#> Total cost: 2 
#> Method: bruteforce
```

### Problem: Different results with different methods

**Cause**: Multiple optimal solutions may exist. Different algorithms
may find different optima with the same total cost.

**Solution**: If you need deterministic results, use
`method = "hungarian"` which has deterministic tie-breaking. Or verify
that total costs match even if assignments differ.

### Problem: Slow performance on large problems

**Cause**: $`O(n^3)`$ complexity for exact algorithms.

**Solutions**:

- For n \> 1000: Try `method = "auction"`
- For n \> 3000: Use blocking via
  [`vignette("matching-workflows")`](https://gcol33.github.io/couplr/articles/matching-workflows.md)
- For n \> 5000: Consider greedy matching with
  [`greedy_couples()`](https://gcol33.github.io/couplr/reference/greedy_couples.md)

``` r

# For large problems, auction algorithm is often faster
large_cost <- matrix(runif(1000000), nrow = 1000)
system.time(lap_solve(large_cost, method = "auction"))
```

### Problem: Memory errors

**Cause**: Cost matrix too large for available RAM. A 10,000×10,000
matrix requires ~800 MB.

**Solution**: Use blocking to divide the problem, or switch to greedy
matching which doesn’t require the full cost matrix in memory.

## Summary

This vignette walked through hospital staff scheduling to demonstrate
couplr’s core functionality:

1.  **Basic solving**: Matrix and data frame inputs with
    [`lap_solve()`](https://gcol33.github.io/couplr/reference/lap_solve.md)
2.  **Rectangular problems**: More shifts than nurses (or vice versa)
3.  **Forbidden assignments**: Using NA/Inf for impossible pairings
4.  **Maximization**: Optimizing preferences instead of minimizing costs
5.  **Grouped data**: Solving multiple days at once with dplyr
6.  **Batch solving**: Handling hundreds of independent problems
7.  **K-best solutions**: Exploring alternatives to the optimal
8.  **Algorithm selection**: Choosing the right solver for your problem

**What’s Next?**

| If you want to… | Read… |
|----|----|
| Understand algorithm internals | [`vignette("algorithms")`](https://gcol33.github.io/couplr/articles/algorithms.md) |
| Match observations in a study | [`vignette("matching-workflows")`](https://gcol33.github.io/couplr/articles/matching-workflows.md) |
| Handle very large problems | [`vignette("pixel-morphing")`](https://gcol33.github.io/couplr/articles/pixel-morphing.md) for approximation strategies |

**Function reference**:
[`?lap_solve`](https://gcol33.github.io/couplr/reference/lap_solve.md),
[`?lap_solve_batch`](https://gcol33.github.io/couplr/reference/lap_solve_batch.md),
[`?lap_solve_kbest`](https://gcol33.github.io/couplr/reference/lap_solve_kbest.md),
[`?hospital_staff`](https://gcol33.github.io/couplr/reference/hospital_staff.md)

**Source code**:
[github.com/gcol33/couplr](https://github.com/gcol33/couplr)
