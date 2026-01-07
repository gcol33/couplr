# Solve assignment problem and return dual variables

Solves the linear assignment problem and returns dual potentials (u, v)
in addition to the optimal matching. The dual variables provide an
optimality certificate and enable sensitivity analysis.

## Usage

``` r
assignment_duals(cost, maximize = FALSE)
```

## Arguments

- cost:

  Numeric matrix; rows = tasks, columns = agents. `NA` or `Inf` entries
  are treated as forbidden assignments.

- maximize:

  Logical; if `TRUE`, maximizes the total cost instead of minimizing.

## Value

A list with class `"assignment_duals_result"` containing:

- `match` - integer vector of column assignments (1-based)

- `total_cost` - optimal objective value

- `u` - numeric vector of row dual variables (length n)

- `v` - numeric vector of column dual variables (length m)

- `status` - character, e.g. "optimal"

## Details

The dual variables satisfy the complementary slackness conditions:

- For minimization: `u[i] + v[j] <= cost[i,j]` for all (i,j)

- For any assigned pair (i,j): `u[i] + v[j] = cost[i,j]`

This implies that `sum(u) + sum(v) = total_cost` (strong duality).

**Applications of dual variables:**

- **Optimality verification**: Check that duals satisfy constraints

- **Sensitivity analysis**: Reduced cost `c[i,j] - u[i] - v[j]` shows
  how much an edge cost must decrease before it enters the solution

- **Pricing in column generation**: Use duals to price new columns

- **Warm starting**: Reuse duals when costs change slightly

## See also

[`assignment()`](https://gcol33.github.io/couplr/reference/assignment.md)
for standard assignment without duals

## Examples

``` r
cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), nrow = 3, byrow = TRUE)
result <- assignment_duals(cost)

# Check optimality: u + v should equal cost for assigned pairs
for (i in 1:3) {
  j <- result$match[i]
  cat(sprintf("Row %d -> Col %d: u + v = %.2f, cost = %.2f\n",
              i, j, result$u[i] + result$v[j], cost[i, j]))
}
#> Row 1 -> Col 2: u + v = 2.00, cost = 2.00
#> Row 2 -> Col 1: u + v = 3.00, cost = 3.00
#> Row 3 -> Col 3: u + v = 4.00, cost = 4.00

# Verify strong duality
cat("sum(u) + sum(v) =", sum(result$u) + sum(result$v), "\n")
#> sum(u) + sum(v) = 9 
cat("total_cost =", result$total_cost, "\n")
#> total_cost = 9 

# Reduced costs (how much must cost decrease to enter solution)
reduced <- outer(result$u, result$v, "+")
reduced_cost <- cost - reduced
print(round(reduced_cost, 2))
#>      [,1] [,2] [,3]
#> [1,]    2    0    3
#> [2,]    0    0    3
#> [3,]    3    1    0
```
