# Find k-best optimal assignments

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
```

## Arguments

- x:

  Cost matrix, data frame, or tibble. If a data frame/tibble, must
  include columns specified by `source`, `target`, and `cost`.

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
#> K-Best Assignment Results
#> =========================
#> 
#> Number of solutions: 5 
#> 
#> Solution costs:
#>   Rank 1: 9.0000
#>   Rank 2: 11.0000
#>   Rank 3: 13.0000
#>   Rank 4: 15.0000
#>   Rank 5: 15.0000
#> 
#> Assignments:
#> # A tibble: 15 × 6
#>     rank solution_id source target  cost total_cost
#>    <int>       <int>  <int>  <int> <dbl>      <dbl>
#>  1     1           1      1      2     3          9
#>  2     1           1      2      1     2          9
#>  3     1           1      3      3     4          9
#>  4     2           2      1      1     4         11
#>  5     2           2      2      2     3         11
#>  6     2           2      3      3     4         11
#>  7     3           3      1      2     3         13
#>  8     3           3      2      3     5         13
#>  9     3           3      3      1     5         13
#> 10     4           4      1      1     4         15
#> 11     4           4      2      3     5         15
#> 12     4           4      3      2     6         15
#> 13     5           5      1      3     7         15
#> 14     5           5      2      1     2         15
#> 15     5           5      3      2     6         15

# Data frame input
library(dplyr)
df <- tibble(
  source = rep(1:3, each = 3),
  target = rep(1:3, times = 3),
  cost = c(4, 2, 5, 3, 3, 6, 7, 5, 4)
)
lap_solve_kbest(df, k = 3, source, target, cost)
#> K-Best Assignment Results
#> =========================
#> 
#> Number of solutions: 3 
#> 
#> Solution costs:
#>   Rank 1: 9.0000
#>   Rank 2: 11.0000
#>   Rank 3: 13.0000
#> 
#> Assignments:
#> # A tibble: 9 × 6
#>    rank solution_id source target  cost total_cost
#>   <int>       <int>  <int>  <int> <dbl>      <dbl>
#> 1     1           1      1      2     2          9
#> 2     1           1      2      1     3          9
#> 3     1           1      3      3     4          9
#> 4     2           2      1      1     4         11
#> 5     2           2      2      2     3         11
#> 6     2           2      3      3     4         11
#> 7     3           3      1      3     5         13
#> 8     3           3      2      1     3         13
#> 9     3           3      3      2     5         13

# With maximization
lap_solve_kbest(cost, k = 3, maximize = TRUE)
#> K-Best Assignment Results
#> =========================
#> 
#> Number of solutions: 3 
#> 
#> Solution costs:
#>   Rank 1: 15.0000
#>   Rank 2: 15.0000
#>   Rank 3: 11.0000
#> 
#> Assignments:
#> # A tibble: 9 × 6
#>    rank solution_id source target  cost total_cost
#>   <int>       <int>  <int>  <int> <dbl>      <dbl>
#> 1     1           1      1      3     7         15
#> 2     1           1      2      2     3         15
#> 3     1           1      3      1     5         15
#> 4     2           2      1      1     4         15
#> 5     2           2      2      3     5         15
#> 6     2           2      3      2     6         15
#> 7     3           3      1      1     4         11
#> 8     3           3      2      2     3         11
#> 9     3           3      3      3     4         11
```
