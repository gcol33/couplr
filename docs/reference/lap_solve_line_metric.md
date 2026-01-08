# Solve 1-D Line Assignment Problem

Solves the linear assignment problem when both sources and targets are
ordered points on a line. Uses efficient O(n\*m) dynamic programming for
rectangular problems and O(n) sorting for square problems.

## Usage

``` r
lap_solve_line_metric(x, y, cost = "L1", maximize = FALSE)
```

## Arguments

- x:

  Numeric vector of source positions (will be sorted internally)

- y:

  Numeric vector of target positions (will be sorted internally)

- cost:

  Cost function for distance. Either:

  - "L1" (default): absolute distance ('Manhattan' distance)

  - "L2": squared distance (squared 'Euclidean' distance) Can also use
    aliases: "abs", "manhattan" for L1; "sq", "squared", "quadratic" for
    L2

- maximize:

  Logical; if TRUE, maximizes total cost instead of minimizing (default:
  FALSE)

## Value

A list with components:

- `match`: Integer vector of length n with 1-based column indices

- `total_cost`: Total cost of the assignment

## Details

This is a specialized solver that exploits the structure of
1-dimensional assignment problems where costs depend only on the
distance between points on a line. It is much faster than general LAP
solvers for this special case.

The algorithm works as follows:

**Square case (n == m):** Both vectors are sorted and matched in order:
`x[1] -> y[1]`, `x[2] -> y[2]`, etc. This is optimal for any metric cost
function on a line.

**Rectangular case (n \< m):** Uses dynamic programming to find the
optimal assignment that matches all n sources to a subset of the m
targets, minimizing total distance. The DP recurrence is:

`dp[i][j] = min(dp[i][j-1], dp[i-1][j-1] + cost(x[i], y[j]))`

This finds the minimum cost to match the first i sources to the first j
targets.

**Complexity:**

- Time: O(n\*m) for rectangular, O(n log n) for square

- Space: O(n\*m) for DP table

## Examples

``` r
# Square case: equal number of sources and targets
x <- c(1.5, 3.2, 5.1)
y <- c(2.0, 3.0, 5.5)
result <- lap_solve_line_metric(x, y, cost = "L1")
print(result)

# Rectangular case: more targets than sources
x <- c(1.0, 3.0, 5.0)
y <- c(0.5, 2.0, 3.5, 4.5, 6.0)
result <- lap_solve_line_metric(x, y, cost = "L2")
print(result)

# With unsorted inputs (will be sorted internally)
x <- c(5.0, 1.0, 3.0)
y <- c(4.5, 0.5, 6.0, 2.0, 3.5)
result <- lap_solve_line_metric(x, y, cost = "L1")
print(result)
```
