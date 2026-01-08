# Solve linear assignment problems

Provides a tidy interface for solving the linear assignment problem
using 'Hungarian' or 'Jonker-Volgenant' algorithms. Supports rectangular
matrices, NA/Inf masking, and data frame inputs.

## Usage

``` r
lap_solve(
  x,
  source = NULL,
  target = NULL,
  cost = NULL,
  maximize = FALSE,
  method = "auto",
  forbidden = NA
)
```

## Arguments

- x:

  Cost matrix, data frame, or tibble. If a data frame/tibble, must
  include columns specified by `source`, `target`, and `cost`.

- source:

  Column name for source/row indices (if `x` is a data frame)

- target:

  Column name for target/column indices (if `x` is a data frame)

- cost:

  Column name for costs (if `x` is a data frame)

- maximize:

  Logical; if TRUE, maximizes total cost instead of minimizing (default:
  FALSE)

- method:

  Algorithm to use. One of:

  - "auto" (default): automatically selects best algorithm

  - "jv": 'Jonker-Volgenant' algorithm (general purpose, fast)

  - "hungarian": Classic 'Hungarian' algorithm

  - "auction": 'Bertsekas' auction algorithm (good for large dense
    problems)

  - "sap": Sparse assignment (good for sparse/rectangular problems)

  - "hk01": 'Hopcroft-Karp' for binary/uniform costs

- forbidden:

  Value to mark forbidden assignments (default: NA). Can also use Inf.

## Value

A tibble with columns:

- `source`: row/source indices

- `target`: column/target indices

- `cost`: cost of each assignment

- `total_cost`: total cost (attribute)

## Examples

``` r
# Matrix input
cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), nrow = 3)
lap_solve(cost)
#> Assignment Result
#> =================
#> 
#> # A tibble: 3 × 3
#>   source target  cost
#>    <int>  <int> <dbl>
#> 1      1      2     3
#> 2      2      1     2
#> 3      3      3     4
#> 
#> Total cost: 9 
#> Method: bruteforce 

# Data frame input
library(dplyr)
df <- tibble(
  source = rep(1:3, each = 3),
  target = rep(1:3, times = 3),
  cost = c(4, 2, 5, 3, 3, 6, 7, 5, 4)
)
lap_solve(df, source, target, cost)
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

# With NA masking (forbidden assignments)
cost[1, 3] <- NA
lap_solve(cost)
#> Assignment Result
#> =================
#> 
#> # A tibble: 3 × 3
#>   source target  cost
#>    <int>  <int> <dbl>
#> 1      1      2     3
#> 2      2      1     2
#> 3      3      3     4
#> 
#> Total cost: 9 
#> Method: bruteforce 

# Grouped data frames
df <- tibble(
  sim = rep(1:2, each = 9),
  source = rep(1:3, times = 6),
  target = rep(1:3, each = 3, times = 2),
  cost = runif(18, 1, 10)
)
df |> group_by(sim) |> lap_solve(source, target, cost)
#> # A tibble: 6 × 4
#>     sim source target  cost
#>   <int>  <int>  <int> <dbl>
#> 1     1      1      2  1.82
#> 2     1      2      1  3.47
#> 3     1      3      3  1.73
#> 4     2      1      2  3.95
#> 5     2      2      3  5.77
#> 6     2      3      1  4.47
```
