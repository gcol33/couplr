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
#> Total cost range: [0.33, 1.15] 
#> 
#> # A tibble: 20 × 6
#>    problem_id source target   cost total_cost method_used
#>         <int>  <int>  <int>  <dbl>      <dbl> <chr>      
#>  1          1      1      2 0.516       0.615 bruteforce 
#>  2          1      2      1 0.0986      0.615 bruteforce 
#>  3          2      1      2 0.159       0.809 bruteforce 
#>  4          2      2      1 0.650       0.809 bruteforce 
#>  5          3      1      1 0.891       1.15  bruteforce 
#>  6          3      2      2 0.260       1.15  bruteforce 
#>  7          4      1      1 0.318       0.329 bruteforce 
#>  8          4      2      2 0.0109      0.329 bruteforce 
#>  9          5      1      1 0.0631      0.361 bruteforce 
#> 10          5      2      2 0.298       0.361 bruteforce 
#> 11          6      1      1 0.0946      0.497 bruteforce 
#> 12          6      2      2 0.402       0.497 bruteforce 
#> 13          7      1      1 0.0591      0.335 bruteforce 
#> 14          7      2      2 0.276       0.335 bruteforce 
#> 15          8      1      2 0.199       0.743 bruteforce 
#> 16          8      2      1 0.544       0.743 bruteforce 
#> 17          9      1      2 0.876       1.15  bruteforce 
#> 18          9      2      1 0.271       1.15  bruteforce 
#> 19         10      1      2 0.183       0.564 bruteforce 
#> 20         10      2      1 0.381       0.564 bruteforce 

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
#>  1     1      1      3  1.69       8.95 bruteforce 
#>  2     1      2      1  2.74       8.95 bruteforce 
#>  3     1      3      2  4.52       8.95 bruteforce 
#>  4     2      1      2  5.22      13.9  bruteforce 
#>  5     2      2      1  5.51      13.9  bruteforce 
#>  6     2      3      3  3.18      13.9  bruteforce 
#>  7     3      1      3  3.03       8.85 bruteforce 
#>  8     3      2      2  2.05       8.85 bruteforce 
#>  9     3      3      1  3.77       8.85 bruteforce 
#> 10     4      1      3  1.91       8.59 bruteforce 
#> 11     4      2      1  2.83       8.59 bruteforce 
#> 12     4      3      2  3.84       8.59 bruteforce 
#> 13     5      1      3  5.46       9.56 bruteforce 
#> 14     5      2      1  1.24       9.56 bruteforce 
#> 15     5      3      2  2.85       9.56 bruteforce 

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
