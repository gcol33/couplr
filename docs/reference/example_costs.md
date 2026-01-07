# Example cost matrices for assignment problems

Small example datasets for demonstrating couplr functionality

## Usage

``` r
example_costs
```

## Format

A list containing several example cost matrices:

- simple_3x3:

  A simple 3x3 cost matrix

- rectangular_3x5:

  A 3x5 rectangular cost matrix

- sparse_with_na:

  A matrix with NA values indicating forbidden assignments

- binary_costs:

  A matrix with binary (0/1) costs

## Examples

``` r
# Use simple example
lap_solve(example_costs$simple_3x3)
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

# Rectangular problem
lap_solve(example_costs$rectangular_3x5)
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

# With forbidden assignments
lap_solve(example_costs$sparse_with_na)
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
