# Example cost matrices for assignment problems

Small example datasets for demonstrating couplr functionality across
different assignment problem types: square, rectangular, sparse, and
binary.

## Usage

``` r
example_costs
```

## Format

A list containing four example cost matrices:

- simple_3x3:

  A 3x3 cost matrix with costs ranging from 2-7. Optimal assignment: row
  1 -\> col 2 (cost 2), row 2 -\> col 1 (cost 3), row 3 -\> col 3 (cost
  4). Total optimal cost: 9.

- rectangular_3x5:

  A 3x5 rectangular cost matrix demonstrating assignment when rows \<
  columns. Each of 3 rows is assigned to one of 5 columns; 2 columns
  remain unassigned. Costs range 1-6.

- sparse_with_na:

  A 3x3 matrix with NA values indicating forbidden assignments. Use this
  to test algorithms' handling of constraints. Position (1,3), (2,2),
  and (3,1) are forbidden.

- binary_costs:

  A 3x3 matrix with binary (0/1) costs, suitable for testing the HK01
  algorithm. Diagonal entries are 0 (preferred), off-diagonal entries
  are 1 (penalty).

## Details

These matrices are designed to test different aspects of LAP solvers:

**simple_3x3**: Basic functionality test. Any correct solver should find
total cost = 9.

**rectangular_3x5**: Tests handling of non-square problems. The optimal
solution assigns all 3 rows with minimum total cost.

**sparse_with_na**: Tests constraint handling. Algorithms must avoid NA
positions while finding an optimal assignment among valid entries.

**binary_costs**: Tests specialized binary cost algorithms. The optimal
assignment uses all diagonal entries (total cost = 0).

## See also

[`lap_solve`](https://gcol33.github.io/couplr/reference/lap_solve.md),
[`example_df`](https://gcol33.github.io/couplr/reference/example_df.md)

## Examples

``` r
# Simple 3x3 assignment
result <- lap_solve(example_costs$simple_3x3)
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
# Optimal: sources 1,2,3 -> targets 2,1,3 with cost 9

# Rectangular problem (3 sources, 5 targets)
result <- lap_solve(example_costs$rectangular_3x5)
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
# All 3 sources assigned; 2 targets unassigned

# Sparse problem with forbidden assignments
result <- lap_solve(example_costs$sparse_with_na)
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
# Avoids NA positions

# Binary costs - test HK01 algorithm
result <- lap_solve(example_costs$binary_costs, method = "hk01")
print(result)
#> Assignment Result
#> =================
#> 
#> # A tibble: 3 × 3
#>   source target  cost
#>    <int>  <int> <dbl>
#> 1      1      1     0
#> 2      2      2     0
#> 3      3      3     0
#> 
#> Total cost: 0 
#> Method: hk01 
# Finds diagonal assignment (cost = 0)
```
