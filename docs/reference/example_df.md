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
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

# Solve both problems
example_df |>
  group_by(sim) |>
  lap_solve(source, target, cost)
#> # A tibble: 6 × 4
#>     sim source target  cost
#>   <int>  <int>  <int> <dbl>
#> 1     1      1      2     3
#> 2     1      2      1     2
#> 3     1      3      3     4
#> 4     2      1      1     1
#> 5     2      2      2     3
#> 6     2      3      3     1

# Or use batch solving
example_df |>
  group_by(sim) |>
  lap_solve_batch(source, target, cost)
#> Batch Assignment Results
#> ========================
#> 
#> 
#> # A tibble: 6 × 6
#>     sim source target  cost total_cost method_used
#>   <int>  <int>  <int> <dbl>      <dbl> <chr>      
#> 1     1      1      2     3          9 bruteforce 
#> 2     1      2      1     2          9 bruteforce 
#> 3     1      3      3     4          9 bruteforce 
#> 4     2      1      1     1          5 bruteforce 
#> 5     2      2      2     3          5 bruteforce 
#> 6     2      3      3     1          5 bruteforce 
```
