# Branch and bound over the moment constraints

Searches the tree of inclusion decisions, bounding every node by its
Lagrangian relaxation and keeping an incumbent that satisfies every
stated constraint. Returns at any interruption with that incumbent and a
global bound that is valid for the whole problem, never with an unproven
claim of optimality.

## Usage

``` r
.cardinality_branch_bound(
  problem,
  index = NULL,
  coefs = NULL,
  dual_steps = 20L,
  branch = c("unit", "pair"),
  node_limit = 500L,
  time_limit = Inf,
  should_stop = NULL,
  cost = NULL
)
```

## Arguments

- problem:

  The network, or the pair
  [`.balance_flow_problem()`](https://gillescolling.com/couplr/reference/dot-balance_flow_problem.md)
  returns.

- index:

  The network's index, unless `problem` carries one.

- coefs:

  Moment coefficients, one per one-sided row.

- dual_steps:

  Multiplier updates per node.

- branch:

  Whether to branch on left-unit inclusion or on pairs.

- node_limit, time_limit:

  Search budget, in nodes and in seconds.

- should_stop:

  Optional predicate of the search state; `TRUE` stops the search the
  way an interrupt would.

- cost:

  Optional distance matrix for the audit.

## Value

A list of class `cardinality_run`.

## Details

`time_limit` reaches the solver. A solve that runs out of budget stops
between augmentations and comes back saying so, and the node it belonged
to goes back on the frontier unopened, so the bound the search reports
still covers the whole tree.
