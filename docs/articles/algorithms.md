# The Algorithm Collection

## Overview

The Hungarian algorithm was published in 1955. Sixty years later, most R
packages still use nothing else.

couplr implements **20 algorithms** (19 solvers plus automatic
selection), including methods that exist in no other R package:
Gabow-Tarjan, Orlin-Ahuja, Network Simplex, Ramshaw-Tarjan.

### Who This Vignette Is For

**Audience**: Researchers curious about optimization algorithms,
developers choosing the right method for their problem, anyone wondering
why modern algorithms beat Hungarian by 20x or more.

**Prerequisites**:

- Basic understanding of assignment problems (matching rows to columns)

- Familiarity with cost matrices (you want to minimize total cost)

- Comfort with big-O notation (helpful but not required)

**What You’ll Learn**:

- Why Hungarian is slow and how later algorithms improved it

- How primal-dual, auction, and network flow approaches work

- Which algorithm to choose for your problem

- How couplr’s automatic selection works

------------------------------------------------------------------------

## The Race

But first: the same problem, five different solutions.

![Five algorithms solving the same 400x400 assignment problem with
dramatically different
speeds](algorithms_files/figure-html/the-race-1.svg)

When you run five different assignment algorithms on identical input,
they all find the same optimal answer, but the fastest finishes **22
times quicker** than the slowest.

The slowest happens to be Hungarian, the algorithm everyone learns in
textbooks. CSA, the fastest here, came out four decades later. That gap
represents years of algorithmic refinement that most production software
never adopted.

Why would anyone need five ways to solve the same problem? Because they
don’t all behave the same under different conditions. The Hungarian
method that handles a 100x100 matrix without complaint becomes painfully
slow at 1000x1000. The Auction algorithm that dominates large dense
problems stumbles on small sparse ones. Different matrix sizes,
different sparsity patterns, different cost distributions: each
situation favors a different algorithm.

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

### The LP Formulation

The assignment problem is a linear program. Let $`x_{ij} \in \{0,1\}`$
indicate whether worker $`i`$ is assigned to job $`j`$:

``` math
\begin{aligned}
\min \quad & \sum_{i,j} c_{ij} x_{ij} \\
\text{s.t.} \quad & \sum_j x_{ij} = 1 \quad \forall i \quad \text{(each worker assigned once)} \\
& \sum_i x_{ij} = 1 \quad \forall j \quad \text{(each job filled once)} \\
& x_{ij} \geq 0
\end{aligned}
```

The constraint matrix is **totally unimodular**: every square submatrix
has determinant 0, 1, or -1. This guarantees integer solutions even when
we relax $`x_{ij} \in \{0,1\}`$ to $`x_{ij} \geq 0`$.

### Duality: The Key to Efficiency

The dual LP introduces prices $`u_i`$ for workers and $`v_j`$ for jobs:

``` math
\begin{aligned}
\max \quad & \sum_i u_i + \sum_j v_j \\
\text{s.t.} \quad & u_i + v_j \leq c_{ij} \quad \forall i,j
\end{aligned}
```

**Complementary slackness** links primal and dual: if $`x_{ij} = 1`$ in
an optimal assignment, then $`u_i + v_j = c_{ij}`$ (the constraint is
tight). This means optimal assignments use only **tight edges** where
the dual constraint holds with equality.

The **reduced cost** of edge $`(i,j)`$ is
$`\bar{c}_{ij} = c_{ij} - u_i - v_j`$. An edge is tight when
$`\bar{c}_{ij} = 0`$.

Different algorithms exploit this duality in different ways.

------------------------------------------------------------------------

## The Classics

### Hungarian Algorithm (1955)

The algorithm everyone learns. Published by Harold Kuhn, based on work
by Hungarian mathematicians Koenig and Egervary.

**The idea**: Maintain dual prices $`(u_i, v_j)`$ such that
$`u_i + v_j \leq c_{ij}`$ for all pairs. Edges where equality holds are
“tight”: the only edges that can appear in an optimal solution.

![Hungarian algorithm showing alternating path augmentation through
tight edges](algorithms_files/figure-html/hungarian-diagram-1.svg)

**The algorithm**:

1.  Initialize prices. Find tight edges.

2.  Find a maximum matching using only tight edges.

3.  If the matching is complete, done.

4.  Otherwise, update prices to create new tight edges. Repeat.

**Complexity**: $`O(n^3)`$

**The problem**: That $`O(n^3)`$ hides a large constant. The price
updates and augmenting path searches are expensive. For a 1000x1000
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

Hungarian works. It’s clean and easy to teach. But in 1987, two Dutch
researchers found something faster.

------------------------------------------------------------------------

### Jonker-Volgenant Algorithm (1987)

Roy Jonker and Anton Volgenant asked: what if we start with a good guess
and fix it?

**The key insight**: Column reduction. Before any sophisticated search,
greedily assign each row to its cheapest available column. This often
gets most of the matching right immediately.

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
Often 10-50x faster than Hungarian in practice.

``` r

set.seed(123)
n <- 100
cost <- matrix(runif(n * n, 0, 100), n, n)
result <- lap_solve(cost, method = "jv")
cat("Total cost:", round(get_total_cost(result), 2), "\n")
#> Total cost: 149.09
```

JV became the de facto standard. For dense problems up to a few thousand
rows, it’s hard to beat.

But JV has a limitation: it’s fundamentally serial. Each augmenting path
depends on the previous. For very large problems, we need a different
approach.

------------------------------------------------------------------------

## The Scaling Revolution

In the late 1980s, researchers discovered a powerful trick called
**epsilon-scaling**. The idea: relax the optimality requirement. Instead
of demanding exact answers at every step, tolerate a small error
epsilon. Start with a large epsilon, which lets you make big sloppy
steps and rapid progress. Then shrink epsilon over multiple phases until
it’s essentially zero. Now you have an exact answer.

This transforms how the algorithm behaves. Large epsilon means big steps
and rapid progress; small epsilon means careful refinement. The total
work can end up being less than doing everything exactly from the start.

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
    value) + epsilon.

3.  If someone else held that job, they become unassigned.

4.  Repeat until everyone is assigned.

**Why epsilon matters**: Without epsilon, two workers could bid
infinitely against each other, each raising the price by 0. The epsilon
ensures progress.

**Complexity**: $`O(n^2 \log(nC) / \epsilon)`$ where $`C`$ is the cost
range.

couplr offers three Auction variants:

| Variant      | `method =`         | Key Feature                   |
|--------------|--------------------|-------------------------------|
| Standard     | `"auction"`        | Adaptive epsilon, queue-based |
| Scaled       | `"auction_scaled"` | Epsilon-scaling phases        |
| Gauss-Seidel | `"auction_gs"`     | Sequential sweep              |

``` r

set.seed(123)
n <- 100
cost <- matrix(runif(n * n, 0, 100), n, n)
result <- lap_solve(cost, method = "auction")
cat("Total cost:", round(get_total_cost(result), 2), "\n")
#> Total cost: 149.09
```

Auction shines for large dense problems. But it’s sensitive to epsilon.
Get it wrong and performance degrades, or the algorithm cycles forever.

The next algorithm makes epsilon-scaling systematic.

------------------------------------------------------------------------

### Cost-Scaling Algorithm / CSA (1995)

Andrew Goldberg and Robert Kennedy asked: what if we scale epsilon
automatically?

**The idea**: Start with $`\epsilon = \max(c_{ij})`$. In each phase,
halve epsilon and refine the current solution. After $`O(\log C)`$
phases, epsilon is essentially zero: optimality.

![CSA algorithm showing epsilon-scaling phases converging to optimal
solution](algorithms_files/figure-html/csa-diagram-1.svg)

**Why it’s fast**: Each phase is cheap because the previous phase’s
solution is a good starting point. The algorithm exploits its own
progress.

**Complexity**: $`O(n^3)`$ amortized, often faster in practice.

``` r

set.seed(456)
n <- 100
cost <- matrix(runif(n * n, 0, 100), n, n)
result <- lap_solve(cost, method = "csa")
cat("Total cost:", round(get_total_cost(result), 2), "\n")
#> Total cost: 192.48
```

CSA often wins benchmarks for medium-large dense problems. It’s the
workhorse.

But there’s an even stranger approach: what if instead of scaling costs,
you scaled *bits*?

------------------------------------------------------------------------

### Gabow-Tarjan Algorithm (1989)

Harold Gabow and Robert Tarjan developed a clever algorithm based on
binary representations. It’s also one of the most complex to implement.

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

3.  Restore **1-feasibility** (see below).

4.  Use Hungarian-style search for augmenting paths.

5.  Repeat until all bits are processed.

**What is 1-feasibility?** Standard dual feasibility requires
$`u_i + v_j \leq c_{ij}`$ for all edges. 1-feasibility relaxes this: we
allow $`u_i + v_j \leq c_{ij} + 1`$. The “1” comes from the current bit
position. At each scaling phase, we only need reduced costs to be within
1 of optimal. When we refine to the next bit, we tighten the bound.
After processing all $`\log C`$ bits, the slack is less than 1, which
for integers means exactly 0: true optimality.

**Complexity**: $`O(n^3 \log C)`$ where $`C`$ is the maximum cost.

Rarely seen outside academic papers. The bookkeeping across scaling
phases is complex enough that most implementations skip it.

``` r

set.seed(42)
n <- 50
# Use integer costs with large range - Gabow-Tarjan's strength
cost <- matrix(sample(1:100000, n * n, replace = TRUE), n, n)
result <- lap_solve(cost, method = "gabow_tarjan")
cat("Total cost:", get_total_cost(result), "\n")
#> Total cost: 151632
```

Gabow-Tarjan is primarily of theoretical interest. It provides the best
known worst-case bounds for integer costs. But there’s one more scaling
algorithm, with even better theoretical complexity.

------------------------------------------------------------------------

### Orlin-Ahuja Algorithm (1992)

James Orlin and Ravindra Ahuja developed a **double-scaling** algorithm
that scales both costs AND capacities simultaneously.

**The insight**: Cost-scaling alone gives $`O(n^3 \log C)`$. But if we
also scale *flow capacities*, we can exploit the structure of sparse
graphs. At each scale, we only need to push $`O(\sqrt{n})`$ units of
flow before refining.

**The algorithm**:

1.  Scale costs as in Gabow-Tarjan (process bits from high to low).

2.  At each cost scale, use **capacity scaling**:

    - Start with large capacity increments $`\Delta = 2^k`$

    - Find augmenting paths that can carry $`\Delta`$ flow

    - Halve $`\Delta`$ and repeat until $`\Delta = 1`$

3.  The capacity scaling limits work per phase to $`O(m)`$
    augmentations.

**Why $`\sqrt{n}`$ appears**: The assignment problem has $`n`$ units of
flow total. With capacity scaling, each phase handles $`O(\sqrt{n})`$
flow units, and there are $`O(\sqrt{n})`$ phases per cost scale. This
geometric structure yields the improved bound.

**Complexity**: $`O(\sqrt{n} \cdot m \cdot \log(nC))`$ where $`m`$ is
the number of edges.

For sparse problems where $`m \ll n^2`$, this is dramatically better
than $`O(n^3)`$.

``` r

set.seed(111)
n <- 50
cost <- matrix(sample(1:100000, n * n, replace = TRUE), n, n)
result <- lap_solve(cost, method = "orlin")
cat("Total cost:", get_total_cost(result), "\n")
#> Total cost: 123035
```

Orlin-Ahuja provides the best theoretical bounds for sparse problems
with large cost ranges. The implementation complexity is substantial:
maintaining blocking flows across scaling phases requires careful data
structure engineering. In practice, the overhead often makes it slower
than CSA for dense problems. But for large sparse instances, it’s
asymptotically optimal.

That’s four scaling algorithms, each trading precision for speed in a
different way. But there’s a completely different way to think about the
problem entirely.

------------------------------------------------------------------------

## The Network View

Every algorithm so far thinks in terms of assignments: matching workers
to jobs. But assignment problems are secretly **flow problems**.

Model the assignment as a network: - A source node connected to all
workers (capacity 1 each)

- Workers connected to jobs (with costs)

- Jobs connected to a sink node (capacity 1 each)

- Find minimum-cost flow of value $`n`$

This perspective gives us two more algorithms.

------------------------------------------------------------------------

### Network Simplex

The simplex method, specialized for networks. The key insight: for
network flow problems, the simplex basis corresponds to a **spanning
tree** of the graph. This makes basis operations (pivoting) much faster
than general LP.

![Network simplex spanning tree structure for assignment
problem](algorithms_files/figure-html/network-simplex-diagram-1.svg)

**The algorithm**:

1.  **Initialize**: Find a spanning tree $`T`$ that supports a feasible
    flow (e.g., all flow through a single hub).

2.  **Compute potentials**: For tree edges, set
    $`\pi_i - \pi_j = c_{ij}`$. This determines all node potentials
    uniquely (up to a constant).

3.  **Price non-tree edges**: For each edge $`(i,j) \notin T`$, compute
    reduced cost $`\bar{c}_{ij} = c_{ij} - \pi_i + \pi_j`$.

4.  **Pivot**: If any non-tree edge has $`\bar{c}_{ij} < 0`$, adding it
    to $`T`$ creates a cycle. Push flow around the cycle until some tree
    edge saturates. Remove that edge; the non-tree edge enters the
    basis.

5.  **Repeat** until all reduced costs are non-negative (optimality).

**Why trees?**: In a tree, there’s exactly one path between any two
nodes. This means: - Flow is uniquely determined by supplies/demands

- Potentials can be computed in $`O(n)`$ by tree traversal

- Each pivot changes $`O(n)`$ potentials (along a tree path), not
  $`O(n^2)`$

**Complexity**: $`O(n^3)`$ typical, strongly polynomial with
anti-cycling rules.

**Best for**: Problems where you need dual variables for sensitivity
analysis. Problems already formulated as network flows. Cases where you
want guaranteed finite convergence.

``` r

set.seed(789)
n <- 100
cost <- matrix(runif(n * n, 0, 100), n, n)
result <- lap_solve(cost, method = "network_simplex")
cat("Total cost:", round(get_total_cost(result), 2), "\n")
#> Total cost: 156.88
```

Network Simplex is a standard tool in operations research. Not always
the fastest, but reliable and provides rich dual information.

------------------------------------------------------------------------

### Push-Relabel Algorithm

Goldberg and Tarjan’s push-relabel algorithm (1988), adapted for
minimum-cost flow.

**The key idea**: Traditional algorithms maintain a valid flow and
search for augmenting paths. Push-relabel inverts this: maintain a
**preflow** (flow that may violate conservation at intermediate nodes)
and work locally to eliminate violations.

**Key concepts**:

- **Excess**: $`e(v) = \text{flow in} - \text{flow out}`$. Preflow
  allows $`e(v) \geq 0`$ at non-sink nodes.

- **Height function**: $`h: V \to \mathbb{Z}_{\geq 0}`$ with
  $`h(t) = 0`$ and $`h(u) \leq h(v) + 1`$ for residual edges $`(u,v)`$.

- **Admissible edge**: Residual edge $`(u,v)`$ where $`h(u) = h(v) + 1`$
  (flow goes “downhill”).

**The algorithm**:

1.  **Initialize**: Saturate all edges from source. Set $`h(s) = n`$,
    $`h(v) = 0`$ for $`v \neq s`$.

2.  While any node $`v`$ has excess $`e(v) > 0`$:

    - **Push**: If admissible edge $`(v, w)`$ exists, push
      $`\min(e(v), \text{capacity})`$ flow along it.

    - **Relabel**: If no admissible edge, set
      $`h(v) = 1 + \min\{h(w) : (v,w) \text{ is residual}\}`$.

3.  When no excess remains at intermediate nodes, we have a valid
    maximum flow.

**For minimum-cost flow**: Modify to only push along edges with zero
reduced cost, and relabel using potential updates. This gives the
**cost-scaling push-relabel** variant.

**Complexity**: $`O(n^2 m)`$ for max-flow, $`O(n^3 \log(nC))`$ for
min-cost flow.

**Strengths**: Highly parallelizable since pushes are local operations.
Excellent cache behavior. Dominates in practice for max-flow;
competitive for min-cost flow on dense graphs.

``` r

set.seed(222)
n <- 100
cost <- matrix(runif(n * n, 0, 100), n, n)
result <- lap_solve(cost, method = "push_relabel")
cat("Total cost:", round(get_total_cost(result), 2), "\n")
#> Total cost: 169.7
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
n <- 100
cost <- matrix(sample(0:1, n^2, replace = TRUE, prob = c(0.3, 0.7)), n, n)
result <- lap_solve(cost, method = "hk01")
cat("Total cost:", get_total_cost(result), "\n")
#> Total cost: 0
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
n <- 100
cost <- matrix(Inf, n, n)
edges <- sample(1:(n^2), floor(0.2 * n^2))  # Only 20% allowed
cost[edges] <- runif(length(edges), 0, 100)
result <- lap_solve(cost, method = "sap")
cat("Total cost:", round(get_total_cost(result), 2), "\n")
#> Total cost: 794.94
```

For very sparse problems, SAP can be orders of magnitude faster than
dense algorithms.

------------------------------------------------------------------------

### Ramshaw-Tarjan: Rectangular Problems (2012)

Most algorithms assume square matrices. When you have $`n`$ workers and
$`m > n`$ jobs, standard approaches pad with $`m - n`$ dummy workers at
zero cost. This works but wastes effort: the algorithm processes
$`m \times m`$ entries when only $`n \times m`$ matter.

Ramshaw and Tarjan (2012) developed an algorithm that handles
rectangularity natively by exploiting the structure of **unbalanced
bipartite graphs**.

**The key insight**: In a rectangular assignment, we match all $`n`$
rows but only $`n`$ of the $`m`$ columns. The dual problem has different
structure: row duals $`u_i`$ are unconstrained, but column duals $`v_j`$
must satisfy $`v_j \leq 0`$ for unmatched columns.

**The algorithm**:

1.  Maintain dual variables $`(u, v)`$ with $`u_i + v_j \leq c_{ij}`$
    and $`v_j \leq 0`$ for free columns.

2.  Use a modified Dijkstra search that respects the asymmetric dual
    constraints.

3.  When augmenting, update duals to preserve feasibility without
    padding.

**Complexity**: $`O(nm \log n)`$ using Fibonacci heaps, or
$`O(nm + n^2 \log n)`$ with simpler structures.

For highly rectangular problems (e.g., matching 100 treatments to 10,000
controls), this avoids the $`O(m^3)`$ cost of padding to square.

``` r

set.seed(333)
n_rows <- 30
n_cols <- 100  # Highly rectangular: 30 x 100
cost <- matrix(runif(n_rows * n_cols, 0, 100), n_rows, n_cols)
result <- lap_solve(cost, method = "ramshaw_tarjan")
cat("Matched", sum(result$assignment > 0), "of", n_rows, "rows\n")
#> Warning: Unknown or uninitialised column: `assignment`.
#> Matched 0 of 30 rows
```

When you have significantly more columns than rows (or vice versa),
Ramshaw-Tarjan avoids the wasted work of padding. The newest algorithm
in couplr’s collection, and essential for large-scale matching problems
where treatment and control pools have very different sizes.

------------------------------------------------------------------------

## Beyond Standard Assignment

couplr includes specialized solvers for variations on the assignment
problem.

### K-Best Solutions (Murty’s Algorithm)

What if you want the 2nd best assignment? The 3rd best? The k-th best?

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

**Use cases**: Sensitivity analysis. Alternative plans when the optimal
is infeasible. Understanding how costs affect solutions.

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

**For sparse matrices**: SAP and LAPMOD are 10x faster than dense
algorithms. Use them.

------------------------------------------------------------------------

## Quick Reference

| Algorithm | Complexity | Best For | Method |
|----|----|----|----|
| Hungarian | $`O(n^3)`$ | Pedagogy, small $`n`$ | `"hungarian"` |
| Jonker-Volgenant | $`O(n^3)`$ expected | General purpose | `"jv"` |
| Auction | $`O(n^2 \log(nC)/\epsilon)`$ | Large dense | `"auction"` |
| Auction (Gauss-Seidel) | $`O(n^2 \log(nC)/\epsilon)`$ | Spatial structure | `"auction_gs"` |
| Auction (Scaled) | $`O(n^2 \log(nC)/\epsilon)`$ | Large dense, fastest | `"auction_scaled"` |
| CSA | $`O(n^3)`$ amortized | Medium-large dense | `"csa"` |
| Cost-Scaling Flow | $`O(n^3 \log(nC))`$ | General min-cost flow | `"csflow"` |
| Gabow-Tarjan | $`O(n^3 \log C)`$ | Large integer costs | `"gabow_tarjan"` |
| Orlin-Ahuja | $`O(\sqrt{n} m \log(nC))`$ | Large sparse | `"orlin"` |
| Network Simplex | $`O(n^3)`$ typical | Dual info needed | `"network_simplex"` |
| Push-Relabel | $`O(n^2 m)`$ | Max-flow style | `"push_relabel"` |
| Cycle Canceling | $`O(n^2 m \cdot C)`$ | Theoretical interest | `"cycle_cancel"` |
| HK01 | $`O(n^{2.5})`$ | Binary costs only | `"hk01"` |
| SSAP (Dial) | $`O(n^2 + nm)`$ | Integer costs, buckets | `"ssap_bucket"` |
| SAP | $`O(n^2 + nm)`$ | Sparse (\>50% forbidden) | `"sap"` |
| LAPMOD | $`O(n^2 + nm)`$ | Sparse (\>50% forbidden) | `"lapmod"` |
| Ramshaw-Tarjan | $`O(nm \log n)`$ | Rectangular matrices | `"ramshaw_tarjan"` |
| Brute Force | $`O(n!)`$ | Tiny ($`n \leq 8`$) | `"bruteforce"` |

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
