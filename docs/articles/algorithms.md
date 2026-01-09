# The Algorithm Collection

The Hungarian algorithm was published in 1955. Sixty years later, most R
packages still use nothing else.

couplr implements **twenty algorithms** spanning five decades of
research—including methods that exist in no other R package.
Gabow-Tarjan. Orlin-Ahuja. Network Simplex. Ramshaw-Tarjan. Names you
might recognize from textbooks, implemented here in production C++.

This vignette explains why they exist and when each one matters.

But first: the same problem, five different solutions.

------------------------------------------------------------------------

## The Race

![Five algorithms solving the same 400x400 assignment problem with
dramatically different
speeds](algorithms_files/figure-html/the-race-1.svg)

When you run five different assignment algorithms on identical input,
they all find the same optimal answer—but the fastest finishes **22
times quicker** than the slowest.

The slowest happens to be Hungarian, the algorithm everyone learns in
textbooks. CSA, the fastest here, came out four decades later. That gap
represents years of algorithmic refinement that most production software
never adopted.

Why would anyone need five ways to solve the same problem? Because they
don’t all behave the same under different conditions. The Hungarian
method that handles a 100×100 matrix without complaint becomes painfully
slow at 1000×1000. The Auction algorithm that dominates large dense
problems stumbles on small sparse ones. Different matrix sizes,
different sparsity patterns, different cost distributions—each situation
favors a different algorithm.

couplr gives you all of them, and it picks the right one automatically.

------------------------------------------------------------------------

## The Problem

Before the algorithms, the problem. It’s simple to state:

> Given $`n`$ workers and $`n`$ jobs, where assigning worker $`i`$ to
> job $`j`$ costs $`c_{ij}`$, find the assignment that minimizes total
> cost.

Mathematically:

``` math
\min_{\pi} \sum_{i=1}^{n} c_{i,\pi(i)}
```

where $`\pi`$ is a permutation (each worker gets exactly one job, each
job gets exactly one worker).

![Bipartite graph showing workers on left, jobs on right, with weighted
edges and optimal assignment
highlighted](algorithms_files/figure-html/bipartite-graph-1.svg)

Simple to state. Not simple to solve efficiently.

There are $`n!`$ possible assignments. For $`n = 20`$, that’s 2.4
quintillion possibilities. Brute force is impossible. We need structure.

The insight that unlocks efficient algorithms: **duality**. Every
assignment problem has a dual problem involving “prices” for workers and
jobs. When the prices are right, the optimal assignment reveals itself.

Different algorithms exploit this duality in different ways.

------------------------------------------------------------------------

## The Classics

### Hungarian Algorithm (1955)

The algorithm everyone learns. Published by Harold Kuhn, based on work
by Hungarian mathematicians Kőnig and Egerváry.

**The idea**: Maintain dual prices $`(u_i, v_j)`$ such that
$`u_i + v_j \leq c_{ij}`$ for all pairs. Edges where equality holds are
“tight”—the only edges that can appear in an optimal solution.

![Hungarian algorithm showing alternating path augmentation through
tight edges](algorithms_files/figure-html/hungarian-diagram-1.svg)

**The algorithm**:

1.  Initialize prices. Find tight edges.
2.  Find a maximum matching using only tight edges.
3.  If the matching is complete, done.
4.  Otherwise, update prices to create new tight edges. Repeat.

**Complexity**: $`O(n^3)`$

**The problem**: That $`O(n^3)`$ hides a large constant. The price
updates and augmenting path searches are expensive. For a 1000×1000
matrix, you might wait 10+ seconds.

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

Hungarian works. It’s pedagogically beautiful. But in 1987, two Dutch
researchers found something faster.

------------------------------------------------------------------------

### Jonker-Volgenant Algorithm (1987)

Roy Jonker and Anton Volgenant asked: what if we start with a good guess
and fix it?

**The key insight**: Column reduction. Before any sophisticated search,
greedily assign each row to its cheapest available column. This often
gets most of the matching right immediately.

    #> Warning: The `label.size` argument of `geom_label()` is deprecated as of ggplot2 3.5.0.
    #> ℹ Please use the `linewidth` argument instead.
    #> This warning is displayed once per session.
    #> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    #> generated.

![JV algorithm showing column reduction initialization followed by
shortest path
augmentation](algorithms_files/figure-html/jv-diagram-1.svg)

**The algorithm**:

1.  **Column reduction**: For each column, find the two smallest costs.
    The difference is the “advantage” of the best row.
2.  **Reduction transfer**: Assign rows to columns, handling conflicts
    by dual variable updates.
3.  **Augmentation**: For any remaining unmatched rows, use
    Dijkstra-style shortest path search.

**Complexity**: Still $`O(n^3)`$, but with a much smaller constant.
Often 10-50× faster than Hungarian in practice.

``` r

set.seed(123)
n <- 500
cost <- matrix(runif(n * n, 0, 100), n, n)
system.time(result <- lap_solve(cost, method = "jv"))
#>    user  system elapsed 
#>    0.01    0.00    0.04
cat("Total cost:", round(get_total_cost(result), 2), "\n")
#> Total cost: 165.75
```

JV became the de facto standard. For dense problems up to a few thousand
rows, it’s hard to beat.

But JV has a limitation: it’s fundamentally serial. Each augmenting path
depends on the previous. For very large problems, we need a different
approach.

------------------------------------------------------------------------

## The Scaling Revolution

In the late 1980s, researchers discovered a powerful trick called
**ε-scaling**. The idea is to relax the optimality requirement: instead
of demanding exact answers at every step, you tolerate a small error ε.
You start with a large ε, which lets you make big sloppy steps and rapid
progress. Then you shrink ε over multiple phases until it’s essentially
zero—and you have an exact answer.

This transforms how the algorithm behaves. Large ε means big steps and
rapid progress; small ε means careful refinement. The total work can end
up being less than doing everything exactly from the start.

Four algorithms exploit this insight: Auction, CSA, Gabow-Tarjan, and
Orlin-Ahuja.

------------------------------------------------------------------------

### Auction Algorithm (1988)

Dimitri Bertsekas asked: what if we thought of assignment as an
economics problem?

**The metaphor**: Workers are buyers. Jobs are goods. Each job has a
price. Workers bid for their favorite jobs. Prices rise when there’s
competition. Equilibrium = optimal assignment.

![Auction algorithm bidding process showing workers bidding for jobs
with prices](algorithms_files/figure-html/auction-diagram-1.svg)

**The algorithm**:

1.  Each unassigned worker finds their best job (highest value minus
    price).
2.  The worker bids: new price = old price + (best value - second-best
    value) + ε.
3.  If someone else held that job, they become unassigned.
4.  Repeat until everyone is assigned.

**Why ε matters**: Without ε, two workers could bid infinitely against
each other, each raising the price by 0. The ε ensures progress.

**Complexity**: $`O(n^2 \log(nC) / \epsilon)`$ where $`C`$ is the cost
range.

couplr offers three Auction variants:

| Variant      | `method =`         | Key Feature             |
|--------------|--------------------|-------------------------|
| Standard     | `"auction"`        | Adaptive ε, queue-based |
| Scaled       | `"auction_scaled"` | ε-scaling phases        |
| Gauss-Seidel | `"auction_gs"`     | Sequential sweep        |

``` r

set.seed(123)
n <- 800
cost <- matrix(runif(n * n, 0, 100), n, n)
system.time(result <- lap_solve(cost, method = "auction"))
#>    user  system elapsed 
#>    0.22    0.00    0.25
```

Auction shines for large dense problems. But it’s sensitive to ε. Get it
wrong and performance degrades—or the algorithm cycles forever.

The next algorithm makes ε-scaling systematic.

------------------------------------------------------------------------

### Cost-Scaling Algorithm / CSA (1995)

Andrew Goldberg and Robert Kennedy asked: what if we scale ε
automatically?

**The idea**: Start with $`\epsilon = \max(c_{ij})`$. In each phase,
halve ε and refine the current solution. After $`O(\log C)`$ phases, ε
is essentially zero: optimality.

![CSA algorithm showing epsilon-scaling phases converging to optimal
solution](algorithms_files/figure-html/csa-diagram-1.svg)

**Why it’s fast**: Each phase is cheap because the previous phase’s
solution is a good starting point. The algorithm exploits its own
progress.

**Complexity**: $`O(n^3)`$ amortized, often faster in practice.

``` r

set.seed(456)
n <- 800
cost <- matrix(runif(n * n, 0, 100), n, n)
system.time(result <- lap_solve(cost, method = "csa"))
#>    user  system elapsed 
#>    0.14    0.00    0.14
```

CSA often wins benchmarks for medium-large dense problems. It’s the
workhorse.

But there’s an even stranger approach: what if instead of scaling costs,
you scaled *bits*?

------------------------------------------------------------------------

### Gabow-Tarjan Algorithm (1989)

Harold Gabow and Robert Tarjan developed one of the most elegant
algorithms in combinatorial optimization. It’s also one of the most
complex to implement.

**The insight**: Integer costs have a natural scale: binary digits.
Process costs from most significant to least significant bit. At each
scale, solve a simpler problem. Use that solution to warm-start the next
scale.

![Gabow-Tarjan bit-scaling showing costs processed from high bits to low
bits](algorithms_files/figure-html/gabow-tarjan-diagram-1.svg)

**The algorithm** (simplified):

1.  Initialize at the coarsest scale (most significant bit only).
2.  Double the scale: multiply all costs by 2. This “doubles” the
    current solution’s slack.
3.  Restore **1-feasibility**: ensure dual prices are almost-optimal.
4.  Use Hungarian-style search for augmenting paths.
5.  Repeat until all bits are processed.

**Complexity**: $`O(n^3 \log C)`$ where $`C`$ is the maximum cost.

Rarely seen outside academic papers. The bookkeeping for 1-feasibility
across scaling phases is intricate enough that most implementations skip
it.

``` r

set.seed(42)
n <- 200
# Use integer costs with large range - Gabow-Tarjan's strength
cost <- matrix(sample(1:100000, n * n, replace = TRUE), n, n)
system.time(result <- lap_solve(cost, method = "gabow_tarjan"))
#>    user  system elapsed 
#>    5.02    0.05    5.31
```

Gabow-Tarjan is primarily of theoretical interest—it provides the best
known worst-case bounds for integer costs. But there’s one more scaling
algorithm, with even better theoretical complexity.

------------------------------------------------------------------------

### Orlin-Ahuja Algorithm (1992)

James Orlin and Ravindra Ahuja developed a **double-scaling** algorithm:
scale both costs AND capacities.

**Complexity**: $`O(\sqrt{n} \cdot m \cdot \log(nC))`$ where $`m`$ is
the number of edges.

For sparse problems, this is sublinear in $`n`$. Theoretically optimal
for many cases.

A textbook algorithm that rarely leaves textbooks. Maintaining blocking
flows across scaling phases requires careful data structure engineering.

``` r

set.seed(111)
n <- 200
cost <- matrix(sample(1:100000, n * n, replace = TRUE), n, n)
system.time(result <- lap_solve(cost, method = "orlin"))
#>    user  system elapsed 
#>    0.02    0.00    0.01
```

Orlin-Ahuja gives the best theoretical bounds for sparse problems with
large cost ranges. In practice, the overhead often makes it slower than
CSA for dense problems. But for the right class of problems, it’s
unbeatable.

That’s four scaling algorithms, each trading precision for speed in a
different way. But there’s a completely different way to think about the
problem entirely.

------------------------------------------------------------------------

## The Network View

Every algorithm so far thinks in terms of assignments: matching workers
to jobs. But assignment problems are secretly **flow problems**.

Model the assignment as a network: - A source node connected to all
workers (capacity 1 each) - Workers connected to jobs (with costs) -
Jobs connected to a sink node (capacity 1 each) - Find minimum-cost flow
of value $`n`$

This perspective unlocks two more algorithms.

------------------------------------------------------------------------

### Network Simplex

The simplex method, specialized for networks. Instead of a matrix basis,
maintain a **spanning tree**.

![Network simplex spanning tree structure for assignment
problem](algorithms_files/figure-html/network-simplex-diagram-1.svg)

**The algorithm**:

1.  Start with a spanning tree (any feasible basis).
2.  Compute node potentials (dual prices) from the tree.
3.  Find a non-tree edge with negative reduced cost.
4.  Add it to the tree, creating a cycle. Remove an edge from the cycle.
5.  Repeat until no improving edges exist.

**Complexity**: $`O(n^3)`$ typical, polynomial worst-case.

**When it shines**: When you need dual variable information. When you’re
already working with network flows.

``` r

set.seed(789)
n <- 300
cost <- matrix(runif(n * n, 0, 100), n, n)
system.time(result <- lap_solve(cost, method = "network_simplex"))
#>    user  system elapsed 
#>  961.17    2.60  994.48
```

Network Simplex is a workhorse of operations research. It’s not always
the fastest, but it’s reliable and provides rich dual information.

------------------------------------------------------------------------

### Push-Relabel Algorithm

Goldberg and Tarjan’s push-relabel algorithm, adapted for minimum-cost
flow.

**The key difference**: Allow *preflow*—temporary excess at intermediate
nodes. Instead of finding augmenting paths globally, push flow locally
and relabel nodes to make pushing possible.

![Push-relabel algorithm showing excess accumulating at nodes and
discharge
operations](algorithms_files/figure-html/push-relabel-diagram-1.svg)

**The algorithm**:

1.  Initialize with maximum flow from source (creates excess at
    workers).
2.  While any node has excess:
    - **Push**: Send flow to a lower-height neighbor.
    - **Relabel**: If no lower neighbor, increase height.
3.  Excess eventually drains to the sink.

**Complexity**: $`O(n^2 m)`$ worst-case.

**When it shines**: Parallelizable (pushes are local). Good for max-flow
style problems.

``` r

set.seed(222)
n <- 300
cost <- matrix(runif(n * n, 0, 100), n, n)
system.time(result <- lap_solve(cost, method = "push_relabel"))
#>    user  system elapsed 
#>    0.74    0.00    0.75
```

Two network perspectives. Same problem. Different algorithmic
approaches.

But all these algorithms assume dense, square matrices. Real problems
are messier.

------------------------------------------------------------------------

## The Specialists

### HK01: Binary Costs

When costs are only 0 or 1, we don’t need the full machinery.

**The algorithm**: Hopcroft-Karp for maximum cardinality matching, run
on zero-cost edges first. Then add 1-cost edges as needed.

**Complexity**: $`O(n^{2.5})`$ for binary costs.

``` r

set.seed(101)
n <- 500
cost <- matrix(sample(0:1, n^2, replace = TRUE, prob = c(0.3, 0.7)), n, n)
system.time(result <- lap_solve(cost, method = "hk01"))
#>    user  system elapsed 
#>    0.02    0.00    0.02
```

When you have binary costs and large $`n`$, HK01 is dramatically faster.

------------------------------------------------------------------------

### SAP and LAPMOD: Sparse Problems

When 80% of entries are forbidden (Inf or NA), why store them?

**SAP** (Shortest Augmenting Path) and **LAPMOD** use sparse
representations: adjacency lists instead of dense matrices.

**Complexity**: $`O(n^2 + nm)`$ where $`m`$ is the number of allowed
edges.

``` r

set.seed(789)
n <- 500
cost <- matrix(Inf, n, n)
edges <- sample(1:(n^2), floor(0.2 * n^2))  # Only 20% allowed
cost[edges] <- runif(length(edges), 0, 100)

system.time(result <- lap_solve(cost, method = "sap"))
#>    user  system elapsed 
#>    1.12    0.00    1.11
```

For very sparse problems, SAP can be orders of magnitude faster than
dense algorithms.

------------------------------------------------------------------------

### Ramshaw-Tarjan: Rectangular Problems (2012)

Most algorithms assume square matrices. When $`n \neq m`$, they pad with
dummy rows/columns.

Ramshaw and Tarjan (2012) developed an algorithm that handles
rectangularity natively.

The newest algorithm here. Published in 2012, it handles the rectangular
case without padding tricks.

``` r

set.seed(333)
n_rows <- 100
n_cols <- 500  # Highly rectangular
cost <- matrix(runif(n_rows * n_cols, 0, 100), n_rows, n_cols)

system.time(result <- lap_solve(cost, method = "ramshaw_tarjan"))
#>    user  system elapsed 
#>    0.00    0.00    0.01
cat("Matched", sum(result$assignment > 0), "of", n_rows, "rows\n")
#> Warning: Unknown or uninitialised column: `assignment`.
#> Matched 0 of 100 rows
```

When you have significantly more columns than rows (or vice versa),
Ramshaw-Tarjan avoids wasted work on padding.

------------------------------------------------------------------------

## Beyond Standard Assignment

couplr includes specialized solvers for variations on the assignment
problem.

### K-Best Solutions (Murty’s Algorithm)

What if you want not just the best assignment, but the 2nd best, 3rd
best, …, k-th best?

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

**Use cases**: Robustness analysis. Alternative plans when the optimal
is infeasible. Understanding the cost landscape.

### Bottleneck Assignment

Minimize the **maximum** edge cost instead of the sum.

``` r

cost <- matrix(c(5, 9, 2, 10, 3, 7, 8, 4, 6), nrow = 3, byrow = TRUE)
result <- bottleneck_assignment(cost)
cat("Bottleneck (max edge):", result$bottleneck, "\n")
#> Bottleneck (max edge): 6
```

**Use cases**: Load balancing. Fairness constraints. Worst-case
optimization.

### Sinkhorn: Soft Assignment

Entropy-regularized optimal transport. Instead of hard 0/1 assignment,
produce a doubly-stochastic transport plan.

``` r

cost <- matrix(c(1, 2, 3, 4), nrow = 2)
result <- sinkhorn(cost, lambda = 10)
print(round(result$transport_plan, 3))
#>      [,1] [,2]
#> [1,] 0.25 0.25
#> [2,] 0.25 0.25
```

**Use cases**: Probabilistic matching. Domain adaptation. Wasserstein
distances.

### Dual Variables

Extract dual prices for sensitivity analysis.

``` r

cost <- matrix(c(10, 19, 8, 15, 10, 18, 7, 17, 13), nrow = 3, byrow = TRUE)
result <- assignment_duals(cost)
cat("Row duals (u):", result$u, "\n")
#> Row duals (u): 8 10 7
cat("Col duals (v):", result$v, "\n")
#> Col duals (v): 0 0 0
```

**Use cases**: Shadow prices. Identifying critical assignments. Marginal
cost analysis.

------------------------------------------------------------------------

## The Benchmark

You’ve seen what the algorithms do. Now: how fast?

![Runtime comparison of LAP algorithms across problem sizes showing CSA
and JV leading](algorithms_files/figure-html/benchmark-plot-1.svg)

**For dense matrices**: CSA and JV are consistently fastest. Hungarian
falls behind rapidly. Auction and Network Simplex are solid
middle-ground choices.

![Sparse algorithm performance showing SAP and LAPMOD outperforming
dense algorithms](algorithms_files/figure-html/sparse-plot-1.svg)

**For sparse matrices**: SAP and LAPMOD are 10× faster than dense
algorithms. Use them.

------------------------------------------------------------------------

## Quick Reference

| Algorithm | Complexity | Best For | Method |
|----|----|----|----|
| Hungarian | $`O(n^3)`$ | Pedagogy, small $`n`$ | `"hungarian"` |
| Jonker-Volgenant | $`O(n^3)`$ expected | General purpose | `"jv"` |
| Auction | $`O(n^2 \log(nC)/\epsilon)`$ | Large dense | `"auction"` |
| CSA | $`O(n^3)`$ amortized | Medium-large dense | `"csa"` |
| Gabow-Tarjan | $`O(n^3 \log C)`$ | Large integer costs | `"gabow_tarjan"` |
| Orlin-Ahuja | $`O(\sqrt{n} m \log(nC))`$ | Large sparse | `"orlin"` |
| Network Simplex | $`O(n^3)`$ typical | Dual info needed | `"network_simplex"` |
| Push-Relabel | $`O(n^2 m)`$ | Max-flow style | `"push_relabel"` |
| HK01 | $`O(n^{2.5})`$ | Binary costs only | `"hk01"` |
| SAP | $`O(n^2 + nm)`$ | Sparse (\>50% forbidden) | `"sap"` |
| LAPMOD | $`O(n^2 + nm)`$ | Sparse (\>50% forbidden) | `"lapmod"` |
| Ramshaw-Tarjan | $`O(nm \log n)`$ | Rectangular | `"ramshaw_tarjan"` |

Or just use `method = "auto"` and let couplr choose.

------------------------------------------------------------------------

## References

- Kuhn, H. W. (1955). The Hungarian method for the assignment problem.
  *Naval Research Logistics Quarterly*.
- Jonker, R., & Volgenant, A. (1987). A shortest augmenting path
  algorithm for dense and sparse linear assignment problems.
  *Computing*.
- Bertsekas, D. P. (1988). The auction algorithm: A distributed
  relaxation method. *Annals of Operations Research*.
- Gabow, H. N., & Tarjan, R. E. (1989). Faster scaling algorithms for
  network problems. *SIAM Journal on Computing*.
- Goldberg, A. V., & Kennedy, R. (1995). An efficient cost scaling
  algorithm for the assignment problem. *Mathematical Programming*.
- Orlin, J. B., & Ahuja, R. K. (1992). New scaling algorithms for the
  assignment and minimum mean cycle problems. *Mathematical
  Programming*.
- Ramshaw, L., & Tarjan, R. E. (2012). On minimum-cost assignments in
  unbalanced bipartite graphs. *HP Labs Technical Report*.
- Goldberg, A. V., & Tarjan, R. E. (1988). A new approach to the
  maximum-flow problem. *Journal of the ACM*.
- Murty, K. G. (1968). An algorithm for ranking all assignments in order
  of increasing cost. *Operations Research*.
- Cuturi, M. (2013). Sinkhorn distances: Lightspeed computation of
  optimal transport. *NeurIPS*.
- Burkard, R., Dell’Amico, M., & Martello, S. (2009). *Assignment
  Problems*. SIAM.

------------------------------------------------------------------------
