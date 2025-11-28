# Theory and Algorithms

### Overview

This vignette presents the mathematical formulation and algorithmic
foundations underlying couplr. Understanding these concepts helps you
choose the right solver for your problem, debug unexpected behavior, and
appreciate the theoretical guarantees. For practical usage examples, see
[`vignette("getting-started")`](https://gcol33.github.io/couplr/articles/getting-started.md)
and
[`vignette("matching-workflows")`](https://gcol33.github.io/couplr/articles/matching-workflows.md).

------------------------------------------------------------------------

### Terminology

#### Cost Matrix

A matrix $`C \in \mathbb{R}^{n \times m}`$ where entry $`c_{ij}`$
represents the cost of assigning source $`i`$ to target $`j`$. Sources
correspond to rows, targets to columns.

#### Assignment

A selection of (source, target) pairs such that each source is assigned
to at most one target, and each target receives at most one source.
Mathematically: binary variables $`x_{ij} \in \{0,1\}`$ with row-sum and
column-sum constraints.

#### Optimal Assignment

An assignment minimizing total cost $`\sum_{i,j} c_{ij} x_{ij}`$ (or
maximizing, for preference problems).

#### Dual Variables

Auxiliary variables $`(u_i, v_j)`$ associated with rows and columns.
Dual feasibility requires $`u_i + v_j \leq c_{ij}`$ for all pairs.
Strong duality ensures primal-dual optimality.

#### Tight Edge

An edge $`(i,j)`$ where the dual constraint holds with equality:
$`u_i + v_j = c_{ij}`$. Only tight edges can appear in optimal
solutions.

#### Augmenting Path

A path in the bipartite graph alternating between matched and unmatched
edges, starting and ending at unmatched vertices. Augmenting along such
a path increases matching cardinality.

------------------------------------------------------------------------

### Problem Formulation

#### Formal Statement

Given cost matrix $`C \in \mathbb{R}^{n \times m}`$, the linear
assignment problem (LAP) seeks:

``` math
\min \sum_{i=1}^{n} \sum_{j=1}^{m} c_{ij} x_{ij}
```

subject to:

``` math
\begin{aligned}
\sum_{j=1}^{m} x_{ij} &\leq 1 \quad \forall i \in \{1,\ldots,n\} \\[6pt]
\sum_{i=1}^{n} x_{ij} &\leq 1 \quad \forall j \in \{1,\ldots,m\} \\[6pt]
x_{ij} &\in \{0,1\} \quad \forall i,j
\end{aligned}
```

#### Dual Formulation

Associate dual variables $`u_i`$ (rows) and $`v_j`$ (columns):

``` math
\max \sum_{i=1}^{n} u_i + \sum_{j=1}^{m} v_j
```

subject to:

``` math
u_i + v_j \leq c_{ij} \quad \forall i,j
```

#### Complementary Slackness

Optimality condition connecting primal and dual:

``` math
x_{ij}^* > 0 \implies u_i^* + v_j^* = c_{ij}
```

Only assignments along tight edges can be optimal. This principle
underlies most efficient LAP algorithms.

------------------------------------------------------------------------

### Visualizing the Problem

The LAP corresponds to finding a minimum-weight perfect matching in a
weighted bipartite graph:

![Bipartite graph showing sources S1, S2, S3 on the left connected to
targets T1, T2, T3 on the right with weighted edges representing
assignment costs. Optimal edges shown in
green.](algorithms_files/figure-html/bipartite-graph-1.png)

The optimal solution (green edges) assigns S1→T1, S2→T2, S3→T3 with
total cost 2 + 1 + 2 = 5.

------------------------------------------------------------------------

### Algorithm Selection Guide

![Flowchart showing algorithm selection based on cost matrix
properties](algorithms_files/figure-html/decision-flowchart-1.png)

#### Quick Reference

| Algorithm | Complexity | Best For | Avoid When |
|----|----|----|----|
| Hungarian | $`O(n^3)`$ | Small problems, pedagogy | n \> 500 |
| Jonker-Volgenant | $`O(n^3)`$ expected | General purpose (default) | Extremely sparse |
| Auction | $`O(n^2 \log nC/\epsilon)`$ | Large dense (n \> 1000) | Small problems |
| SAP | $`O(n^2 + nm)`$ | Sparse (\>50% forbidden) | Dense problems |
| HK01 | $`O(n^{2.5})`$ | Binary costs only | Non-binary costs |

------------------------------------------------------------------------

## Algorithms

### 1. Hungarian Algorithm

**Complexity**: $`O(n^3)`$

The classical method (Kuhn, 1955) based on work by Kőnig and Egerváry.
Maintains dual feasibility while iteratively improving the primal
solution.

#### Algorithm Steps

1.  **Initialize** dual variables: $`u_i = \min_j c_{ij}`$, $`v_j = 0`$
2.  **Construct equality graph** $`G_=`$ with tight edges only
3.  **Find maximum matching** in $`G_=`$
4.  **If complete**: optimal solution found
5.  **Otherwise**: compute dual update $`\Delta`$ and repeat

#### When to Use

- Educational purposes (clear conceptual structure)
- Small problems (n \< 500)
- When numerical stability is paramount

``` r

cost <- matrix(c(10, 19, 8, 15, 10, 11, 9, 12, 14), nrow = 3, byrow = TRUE)
result <- lap_solve(cost, method = "hungarian")
print(result)
#> Assignment Result
#> =================
#> 
#> # A tibble: 3 × 3
#>   source target  cost
#>    <int>  <int> <dbl>
#> 1      1      3     8
#> 2      2      2    10
#> 3      3      1     9
#> 
#> Total cost: 27 
#> Method: hungarian
```

------------------------------------------------------------------------

### 2. Jonker-Volgenant Algorithm

**Complexity**: $`O(n^3)`$ expected, $`O(n^2)`$ space

The default algorithm in couplr (1987). Uses shortest augmenting paths
with efficient column reduction preprocessing.

#### Key Features

- **Column reduction**: Greedy initial assignment
- **Shortest path augmentation**: Dijkstra-style search
- **ε-complementary slackness**: Allows larger steps than Hungarian

#### When to Use

- General-purpose default (`method = "auto"`)
- Dense problems up to n ≈ 2000
- When you need reliable, predictable performance

``` r

set.seed(123)
n <- 100
cost <- matrix(runif(n * n, 0, 100), n, n)
result <- lap_solve(cost, method = "jv")
cat("Total cost:", get_total_cost(result), "\n")
#> Total cost: 149.0911
```

------------------------------------------------------------------------

### 3. Auction Algorithm Family

**Complexity**: $`O(n^2 \log(nC) / \epsilon)`$

Economic approach (Bertsekas, 1988): sources “bid” for targets, prices
adjust based on competition.

#### Variants

| Variant      | Method Name        | Key Feature                   |
|--------------|--------------------|-------------------------------|
| Standard     | `"auction"`        | Fixed adaptive ε, queue-based |
| Scaled       | `"auction_scaled"` | ε-scaling phases              |
| Gauss-Seidel | `"auction_gs"`     | Sequential sweep              |

#### Core Algorithm

1.  Each unmatched source finds best target
2.  Compute bid increment based on first-best minus second-best
3.  Highest bidder wins; price increases
4.  Repeat until all matched

#### When to Use

- Large dense problems (n \> 1000)
- `"auction_scaled"` for large cost ranges (\> 10⁶)
- `"auction_gs"` for problems with spatial structure

``` r

set.seed(123)
n <- 200
cost <- matrix(runif(n * n, 0, 100), n, n)
result <- lap_solve(cost, method = "auction")
cat("Total cost:", get_total_cost(result), "\n")
#> Total cost: 159.057
```

------------------------------------------------------------------------

### 4. Sparse Assignment (SAP)

**Complexity**: $`O(n^2 + nm)`$ for $`m`$ edges

Optimized for sparse problems where most entries are forbidden (NA or
Inf).

#### Key Features

- Adjacency list representation
- Sparse priority queues
- Efficient for rectangular problems

#### When to Use

- Sparsity \> 50% (many forbidden entries)
- Rectangular problems (n ≠ m)
- Large but sparse structures

``` r

set.seed(789)
n <- 200
cost <- matrix(Inf, n, n)
edges <- sample(1:(n^2), floor(0.3 * n^2))
cost[edges] <- runif(length(edges), 0, 100)

result <- lap_solve(cost, method = "sap")
cat("Total cost:", get_total_cost(result), "\n")
#> Total cost: 564.3393
```

------------------------------------------------------------------------

### 5. Hopcroft-Karp for Binary Costs (HK01)

**Complexity**: $`O(n^{2.5})`$

Specialized for binary cost matrices where $`c_{ij} \in \{0, 1\}`$.

#### Algorithm

1.  Find maximum matching using only zero-cost edges
2.  Augment with minimum 1-cost edges if incomplete

#### When to Use

- Binary costs only (0 or 1)
- Unweighted bipartite matching
- Very large binary problems (n \> 10000)

``` r

set.seed(101)
n <- 300
cost <- matrix(sample(0:1, n^2, replace = TRUE, prob = c(0.3, 0.7)), n, n)
result <- lap_solve(cost, method = "hk01")
cat("Total cost:", get_total_cost(result), "\n")
#> Total cost: 0
```

------------------------------------------------------------------------

### 6. K-Best Solutions (Murty’s Algorithm)

**Complexity**: $`O(k \cdot T(n))`$ where $`T(n)`$ is single LAP
complexity

Finds the k best assignments in order of increasing cost.

#### Algorithm Structure

1.  Solve initial LAP
2.  Partition solution space by forbidding/forcing edges
3.  Maintain priority queue of partial solutions
4.  Extract k best

#### When to Use

- Robustness analysis
- Alternative plans when optimal is infeasible
- Understanding cost landscape

``` r

cost <- matrix(c(10, 19, 8, 15, 10, 18, 7, 17, 13, 16, 9, 14, 12, 19, 8, 18),
               nrow = 4, byrow = TRUE)
kbest <- lap_solve_kbest(cost, k = 5)
summary(kbest)
#> # A tibble: 5 × 4
#>    rank solution_id total_cost n_assignments
#>   <int>       <int>      <dbl>         <int>
#> 1     1           1         49             4
#> 2     2           2         50             4
#> 3     3           3         50             4
#> 4     4           4         51             4
#> 5     5           5         51             4
```

------------------------------------------------------------------------

### Numerical Considerations

#### Floating Point Precision

- Complementary slackness checked with $`\epsilon = 10^{-10}`$
- Avoid cost ranges \> $`10^{12}`$
- Scale costs to reasonable range if needed

#### Edge Cases

**Infeasible problems**: When a row has no finite entries

``` r

cost <- matrix(c(1, 2, 3, Inf, Inf, Inf, 4, 5, 6), nrow = 3, byrow = TRUE)
feasible <- all(rowSums(is.finite(cost)) > 0)
cat("Feasible:", feasible, "\n")
#> Feasible: FALSE
```

**Degenerate problems**: Many tied costs may produce different (but
equally optimal) solutions across algorithms

------------------------------------------------------------------------

### Performance Summary

| Size     | Hungarian | JV  | Auction | SAP  | HK01 |
|----------|-----------|-----|---------|------|------|
| \< 100   | ✓✓✓       | ✓✓✓ | ✓✓      | ✓†   | ✓‡   |
| 100-500  | ✓✓        | ✓✓✓ | ✓✓      | ✓✓†  | ✓✓‡  |
| 500-2000 | ✓         | ✓✓✓ | ✓✓✓     | ✓✓✓† | ✓✓✓‡ |
| \> 2000  | ✗         | ✓✓  | ✓✓✓     | ✓✓✓† | ✓✓✓‡ |

† For sparse problems \| ‡ For binary costs only

------------------------------------------------------------------------

### References

- Kuhn, H. W. (1955). The Hungarian method for the assignment problem.
  *Naval Research Logistics Quarterly*.
- Jonker, R., & Volgenant, A. (1987). A shortest augmenting path
  algorithm for dense and sparse linear assignment problems.
  *Computing*.
- Bertsekas, D. P. (1988). The auction algorithm: A distributed
  relaxation method. *Annals of Operations Research*.
- Murty, K. G. (1968). An algorithm for ranking all assignments in order
  of increasing cost. *Operations Research*.
- Burkard, R., Dell’Amico, M., & Martello, S. (2009). *Assignment
  Problems*. SIAM.

------------------------------------------------------------------------

### See Also

- [`vignette("getting-started")`](https://gcol33.github.io/couplr/articles/getting-started.md) -
  Basic usage and quick start
- [`vignette("matching-workflows")`](https://gcol33.github.io/couplr/articles/matching-workflows.md) -
  Production matching pipelines
- [`vignette("pixel-morphing")`](https://gcol33.github.io/couplr/articles/pixel-morphing.md) -
  Large-scale approximation strategies
- [`?lap_solve`](https://gcol33.github.io/couplr/reference/lap_solve.md),
  [`?assignment`](https://gcol33.github.io/couplr/reference/assignment.md),
  [`?lap_solve_kbest`](https://gcol33.github.io/couplr/reference/lap_solve_kbest.md)
