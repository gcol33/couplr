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
#> Batch Assignment Results
#> ========================
#> 
#> Number of problems solved: 2 
#> Total cost range: [5.00, 13.00] 
#> 
#> # A tibble: 4 × 6
#>   problem_id source target  cost total_cost method_used
#>        <int>  <int>  <int> <dbl>      <dbl> <chr>      
#> 1          1      1      1     1          5 bruteforce 
#> 2          1      2      2     4          5 bruteforce 
#> 3          2      1      1     5         13 bruteforce 
#> 4          2      2      2     8         13 bruteforce 

# 3D array
arr <- array(runif(2 * 2 * 10), dim = c(2, 2, 10))
lap_solve_batch(arr)
#> Batch Assignment Results
#> ========================
#> 
#> Number of problems solved: 10 
#> Total cost range: [0.06, 1.37] 
#> 
#> # A tibble: 20 × 6
#>    problem_id source target   cost total_cost method_used
#>         <int>  <int>  <int>  <dbl>      <dbl> <chr>      
#>  1          1      1      2 0.0477     0.0639 bruteforce 
#>  2          1      2      1 0.0162     0.0639 bruteforce 
#>  3          2      1      2 0.650      0.851  bruteforce 
#>  4          2      2      1 0.201      0.851  bruteforce 
#>  5          3      1      1 0.395      1.28   bruteforce 
#>  6          3      2      2 0.885      1.28   bruteforce 
#>  7          4      1      1 0.553      0.977  bruteforce 
#>  8          4      2      2 0.423      0.977  bruteforce 
#>  9          5      1      1 0.950      0.968  bruteforce 
#> 10          5      2      2 0.0184     0.968  bruteforce 
#> 11          6      1      2 0.879      1.37   bruteforce 
#> 12          6      2      1 0.490      1.37   bruteforce 
#> 13          7      1      1 0.854      1.01   bruteforce 
#> 14          7      2      2 0.151      1.01   bruteforce 
#> 15          8      1      1 0.282      0.865  bruteforce 
#> 16          8      2      2 0.583      0.865  bruteforce 
#> 17          9      1      1 0.527      0.647  bruteforce 
#> 18          9      2      2 0.120      0.647  bruteforce 
#> 19         10      1      1 0.0884     0.426  bruteforce 
#> 20         10      2      2 0.337      0.426  bruteforce 

# Grouped data frame
library(dplyr)
df <- tibble(
  sim = rep(1:5, each = 9),
  source = rep(1:3, times = 15),
  target = rep(1:3, each = 3, times = 5),
  cost = runif(45, 1, 10)
)
df |> group_by(sim) |> lap_solve_batch(source, target, cost)
#> Batch Assignment Results
#> ========================
#> 
#> 
#> # A tibble: 15 × 6
#>      sim source target  cost total_cost method_used
#>    <int>  <int>  <int> <dbl>      <dbl> <chr>      
#>  1     1      1      3  2.57       7.72 bruteforce 
#>  2     1      2      1  1.29       7.72 bruteforce 
#>  3     1      3      2  3.87       7.72 bruteforce 
#>  4     2      1      3  9.94      15.1  bruteforce 
#>  5     2      2      1  3.98      15.1  bruteforce 
#>  6     2      3      2  1.20      15.1  bruteforce 
#>  7     3      1      2  1.40      10.6  bruteforce 
#>  8     3      2      1  7.79      10.6  bruteforce 
#>  9     3      3      3  1.38      10.6  bruteforce 
#> 10     4      1      2  4.26      15.3  bruteforce 
#> 11     4      2      1  3.46      15.3  bruteforce 
#> 12     4      3      3  7.57      15.3  bruteforce 
#> 13     5      1      3  6.34      14.1  bruteforce 
#> 14     5      2      1  2.98      14.1  bruteforce 
#> 15     5      3      2  4.76      14.1  bruteforce 

# Parallel execution (requires n_threads > 1)
lap_solve_batch(costs, n_threads = 2)
#> Batch Assignment Results
#> ========================
#> 
#> Number of problems solved: 2 
#> Total cost range: [5.00, 13.00] 
#> 
#> # A tibble: 4 × 6
#>   problem_id source target  cost total_cost method_used
#>        <int>  <int>  <int> <dbl>      <dbl> <chr>      
#> 1          1      1      1     1          5 bruteforce 
#> 2          1      2      2     4          5 bruteforce 
#> 3          2      1      1     5         13 bruteforce 
#> 4          2      2      2     8         13 bruteforce 
```
