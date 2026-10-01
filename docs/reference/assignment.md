# Linear assignment solver

Solve the linear assignment problem (minimum- or maximum-cost matching)
using several algorithms. Forbidden edges can be marked as `NA` or
`Inf`.

## Usage

``` r
assignment(
  cost,
  maximize = FALSE,
  method = c("auto", "jv", "hungarian", "munkres", "auction", "auction_gs",
    "auction_scaled", "sap", "ssp", "sap_dense", "csflow", "hk01", "bruteforce",
    "ssap_bucket", "cycle_cancel", "gabow_tarjan", "lapmod", "csa", "ramshaw_tarjan",
    "push_relabel", "network_simplex"),
  auction_eps = NULL,
  eps = NULL,
  memory_mode = "auto",
  certify = NULL,
  cardinality = c("complete", "maximum", "fixed"),
  n_matches = NULL,
  unmatched_penalty = NULL
)
```

## Arguments

- cost:

  Numeric matrix; rows = tasks, columns = agents. `NA` or `Inf` entries
  are treated as forbidden assignments.

- maximize:

  Logical; if `TRUE`, maximizes the total cost instead of minimizing.

- method:

  Character string indicating the algorithm to use. Options:

  **General-purpose solvers:**

  - `"auto"` — Automatic selection based on problem characteristics
    (default)

  - `"jv"` — 'Jonker-Volgenant', fast general-purpose O(n^3) with
    warm-start

  - `"hungarian"` — Classic 'Hungarian' (shortest augmenting path)
    O(n^3)

  - `"munkres"` — Matrix-form 'Kuhn-Munkres' O(n^4), reference
    implementation

  **Auction-based solvers:**

  - `"auction"` — 'Bertsekas' auction with adaptive epsilon

  - `"auction_gs"` — 'Gauss-Seidel' variant, good for spatial structure

  - `"auction_scaled"` — 'Epsilon-scaling' auction, bidding at a
    decreasing sequence of epsilon values. Not chosen by `"auto"`; a
    rule sending heavily tied costs to it was tested and did not meet
    its criterion, and it remains available by name for tied costs

  **Specialized solvers:**

  - `"sap"` — Shortest augmenting path over the shared flow model's
    network. `"ssp"` is accepted as a second spelling of this method and
    resolves to `"sap"`.

  - `"sap_dense"` — Shortest augmenting path with a linear scan in place
    of a heap, O(n \* m^2), suited to a dense cost matrix

  - `"lapmod"` — 'Jonker-Volgenant' over the finite entries alone,
    stored row by row (CSR). Written for sparse input; see below for why
    `"auto"` does not divert sparse matrices to it

  - `"hk01"` — 'Hopcroft-Karp' for binary (0/1) or constant costs.
    Constant costs make every perfect matching optimal. On a `{0,1}`
    matrix the search runs over the zero-cost edges alone, where a
    perfect matching totals zero and is therefore optimal; if none
    exists the problem is passed to the weighted solver on the original
    costs.

  - `"ssap_bucket"` — 'Dial' algorithm for integer costs

  - `"bruteforce"` — Exact enumeration for tiny problems (n \<= 8)

  **Advanced solvers:**

  - `"csa"` — 'Goldberg-Kennedy' cost-scaling assignment (CSA-Q):
    epsilon starts at the span of the costs and is divided by 10 each
    refine, rows are discharged from a stack by double-push, and each
    row keeps its three cheapest arcs between scans (the fourth-best
    heuristic). Real-valued costs are read as supplied; the final
    epsilon-optimal assignment is repaired to an optimal one

  - `"gabow_tarjan"` — 'Gabow-Tarjan' bit-scaling with complementary
    slackness. On a graph of `V` vertices and `E` edges the bound is
    O(sqrt(V) \* E \* log(V \* C)), which for an `n` by `n` cost matrix
    is O(n^2.5 \* log(n \* C)). Its optimality bound holds for a
    matching that saturates both sides, so a rectangular problem gains a
    dummy side of zero cost. The dummies are copies of one node and are
    carried as a single unit holding as many partners as there are
    dummies, so the problem is solved at its own `n` by `m` shape.

  - `"cycle_cancel"` — Cycle-canceling with 'Karp' algorithm

  - `"csflow"` — Successive shortest paths with 'Johnson' potentials

  - `"network_simplex"` — 'Network simplex' with spanning tree
    representation

  - `"push_relabel"` — 'Goldberg-Tarjan' cost-scaling push-relabel

  - `"ramshaw_tarjan"` — 'Ramshaw-Tarjan', optimized for rectangular
    matrices (n != m)

  One-dimensional problems have their own entry point,
  [`lap_solve_line_metric()`](https://gillescolling.com/couplr/reference/lap_solve_line_metric.md),
  which takes two point vectors rather than a cost matrix and runs in
  O(n log n).

  Under `"auto"`, a single pass over `cost` supplies the facts the
  following rules need, and the first matching rule wins:

  1.  at most 8 rows and 8 columns: `"bruteforce"`, exact and faster
      than setting up a general solver;

  2.  finite entries all equal, or all either 0 or 1: `"hk01"`, which
      exploits the absence of a real cost scale;

  3.  everything else: `"jv"`.

  Sparsity and aspect ratio used to divert the choice to `"lapmod"` and
  `"sap"`. Neither beat `"jv"` on the regime grid, at any of the four
  admissibility densities or three aspect ratios measured, so both now
  fall through to it; each stays reachable by naming it.

  Naming a method skips the pass. Rectangular problems are transposed
  internally so the solver always sees at least as many columns as rows,
  and the assignment is mapped back afterwards.

- auction_eps:

  Optional numeric epsilon for the 'Auction'/'Auction-GS' methods. If
  `NULL`, an internal default (e.g., `1e-9`) is used.

- eps:

  Deprecated. Use `auction_eps`. If provided and `auction_eps` is
  `NULL`, its value is used for `auction_eps`.

- memory_mode:

  One of "auto" (default), "dense", "lazy" or "implicit". `cost` is
  already a materialized matrix by the time it reaches `assignment()`,
  so "auto" here is diagnostic only: it warns if the matrix is large
  relative to free system RAM (nothing else can be done post-hoc once
  the matrix already exists – build it via
  `compute_distances(memory_mode = ...)` instead to avoid materializing
  it in the first place). `"lazy"` and `"implicit"` describe how a cost
  source is read rather than how a matrix is stored, so they apply to a
  lazy cost specification; `"implicit"` also accepts a matrix, where it
  solves the same problem by generating the pairs it needs and saves
  nothing, which is what makes it a check on the complete solve rather
  than a faster one. Against `"lazy"` on a lazy cost specification,
  `"implicit"` leads from 5,000 units upward on the eight-covariate
  problem the benchmarks use, by 1.1x at 5,000 rising to 3.1x at 50,000,
  and loses below that where the loop's fixed costs are still visible.
  What it buys at every size is the certificate over the complete
  problem.

- certify:

  Logical; whether to attach a checked `assignment_certificate` as
  `certificate`. `NULL`, the default, takes the path's own answer:
  `TRUE` under `memory_mode = "implicit"`, where the certificate is what
  distinguishes the answer from an approximate one and the loop has
  already done most of the scan, and `FALSE` elsewhere. The check reads
  the duals the solve returned where it returned some (see `u` and `v`
  below), so it costs one pass over the admissible pairs; elsewhere it
  costs the solve
  [`verify_assignment()`](https://gillescolling.com/couplr/reference/verify_assignment.md)
  runs to get them. A solve that did not reach a complete optimal
  matching has nothing to certify and gets no certificate.

- cardinality:

  How many pairs to produce.

  - `"complete"` (default) — every row is matched; an input admitting no
    complete matching is an error.

  - `"maximum"` — as many pairs as the admissible edges allow, and the
    cheapest total among matchings of that size.

  - `"fixed"` — exactly `n_matches` pairs, chosen to minimize total
    cost.

  All three are solved exactly by the same solver: the two non-complete
  modes append dummy columns priced so that the solver's own optimum is
  the requested objective.

- n_matches:

  Integer; the number of pairs to produce. Required when
  `cardinality = "fixed"`, and not accepted otherwise.

- unmatched_penalty:

  Numeric; the cost charged for leaving one row unmatched, under
  `cardinality = "maximum"`. Supplying it replaces the lexicographic
  objective with a single one: a pair costing more than the penalty is
  worth dropping. Left `NULL`, no pair is ever traded away for a cost
  saving.

## Value

An object of class `lap_solve_result`, a list with elements:

- `match` — integer vector of length `min(nrow(cost), ncol(cost))`
  giving the assigned column for each row (0 if unassigned).

- `total_cost` — numeric scalar, the objective value.

- `status` — character scalar drawn from
  [`solver_status_values()`](https://gillescolling.com/couplr/reference/solver_status_values.md),
  computed from what the solver terminated on. `"optimal"` means the
  solver reached its own optimality condition with every row matched; it
  is not a checked proof. Use
  [`verify_assignment()`](https://gillescolling.com/couplr/reference/verify_assignment.md)
  for that.

- `method_used` — character scalar, the algorithm actually used.

- `dispatch` — list recording how `method` was chosen: the rule that
  fired under `"auto"`, the condition that triggered it, and whether the
  method was named explicitly. See
  [`explain_dispatch()`](https://gillescolling.com/couplr/reference/explain_dispatch.md).

- `certificate` — an `assignment_certificate`, present when one was
  checked. See `certify`.

- `u`, `v` — row and column duals in the orientation of `cost`, present
  when the solver produced optimal duals for `cost`: `"jv"` (and so
  `"auto"` on every problem it sends there), `"hungarian"`, the lazy
  `"jv"` path, and `memory_mode = "implicit"`, where they are the duals
  the last restricted master produced.
  [`verify_assignment()`](https://gillescolling.com/couplr/reference/verify_assignment.md)
  reads them instead of solving again. Absent under
  `cardinality = "maximum"` and `"fixed"`, whose solver duals belong to
  the padded problem rather than to `cost`, and for `"gabow_tarjan"`,
  whose duals are those of its scaled integer instance.

Under `memory_mode = "implicit"` the result also carries `search`: the
columns the first round gave each row (`seed_width`), the pairs the
candidate set ended up holding (`candidate_edges`) out of
`possible_edges`, the pairs a cost was computed for (`edges_evaluated`),
the round count, and `rounds`, one row per round of what the master
held, what priced out and what each step cost.

## Details

`method = "auto"` selects an algorithm based on problem size and the
costs:

- Very small (n \<= 8 and m \<= 8): `"bruteforce"` — exact enumeration

- Binary/constant costs: `"hk01"` — specialized for 0/1 costs

- Otherwise: `"jv"`

[`explain_dispatch()`](https://gillescolling.com/couplr/reference/explain_dispatch.md)
reports which rule fired and why. The other solvers are available by
naming them explicitly.

## Integer conversion for bit-scaling

`"gabow_tarjan"` is a bit-scaling algorithm and runs on integer costs.
The conversion is part of the method, so the rule it follows and what it
claims afterwards are both properties of the solve rather than of the
caller's preparation.

Finite costs are shifted so the smallest is zero, multiplied by a scale
factor `s`, and rounded to the nearest integer. `s` is set so that
`K * s * (max - min)` stays at or below `10^13`, where `K` is `n + 1` on
a square problem and `2 * min(n, m) + 1` on a rectangular one. `K` is
the separation the algorithm needs between the optimum and a 1-optimal
matching, and the bound holds every scaled cost clear of the sentinel
that marks a forbidden pair while keeping the path sums the solver forms
inside 64-bit arithmetic. Forbidden pairs take the sentinel and take
part in neither the range nor the conversion.

A matrix whose finite entries are each within `1e-9` of an integer is
taken as an integer matrix: `s` is one, the shift is an integer, and
every cost reaches the solver as the integer nearest to it. Where those
entries are integers the conversion is exact and the optimum is the
optimum of the instance as supplied; where they are merely near one,
each cost moves by at most `1e-9`, so the matching returned costs at
most `2 * min(n, m) * 1e-9` more than that optimum. Such a matrix is
refused when its range exceeds `1.25 * 10^14 / K`, since a scale of one
is the only one available and no choice brings the instance inside the
bound; the error names the limit and points at `"jv"` or `"auction"`.

Costs that are not integers are solved on the rounded instance. Rounding
moves each cost by at most `1 / (2 * s)`, so the matching returned costs
at most `min(n, m) * K * (max - min) / 10^13` more than the optimum of
the matrix as supplied. On a square problem that is about
`n^2 * (max - min) / 10^13`, which is `10^-7` of the range at `n = 1000`
and `10^-5` of it at `n = 10000`. Duals are divided by `s` and shifted
back, so they belong to the original matrix; whether the matching is
optimal for that matrix and not only for the rounded one is a question
[`verify_assignment()`](https://gillescolling.com/couplr/reference/verify_assignment.md)
answers rather than one this bound settles.

## See also

- [`lap_solve()`](https://gillescolling.com/couplr/reference/lap_solve.md)
  — Tidy interface returning tibbles

- [`lap_solve_kbest()`](https://gillescolling.com/couplr/reference/lap_solve_kbest.md)
  — Find k-best assignments ('Murty' algorithm)

- [`assignment_duals()`](https://gillescolling.com/couplr/reference/assignment_duals.md)
  — Extract dual variables for sensitivity analysis

- [`bottleneck_assignment()`](https://gillescolling.com/couplr/reference/bottleneck_assignment.md)
  — Minimize maximum edge cost (minimax)

- [`sinkhorn()`](https://gillescolling.com/couplr/reference/sinkhorn.md)
  — Entropy-regularized optimal transport

## Examples

``` r
cost <- matrix(c(4,2,5, 3,3,6, 7,5,4), nrow = 3, byrow = TRUE)
res  <- assignment(cost)
res$match; res$total_cost
```
