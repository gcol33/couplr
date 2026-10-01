# Lagrangian bound for one node

Solves the node's network at a sequence of multipliers, each step one
`.flow_solve()` read exactly by
[`.cardinality_flow()`](https://gillescolling.com/couplr/reference/dot-cardinality_flow.md),
and returns the largest bound the steps proved. Multipliers move along
the projected subgradient with step `t_0 / (1 + k)`, warm-started from
whatever `lambda` is handed in, which is the parent's best set during a
search.

## Usage

``` r
.cardinality_lagrangian(
  problem,
  coefs = NULL,
  lambda = NULL,
  steps = 20L,
  index = NULL,
  edits = NULL,
  incumbent = Inf,
  step0 = NULL,
  cost = NULL,
  warm = NULL,
  deadline = Inf
)
```

## Arguments

- problem:

  The node's network, or the pair
  [`.balance_flow_problem()`](https://gillescolling.com/couplr/reference/dot-balance_flow_problem.md)
  returns.

- coefs:

  Moment coefficients, one per one-sided row.

- lambda:

  Starting multipliers, one per row.

- steps:

  How many multiplier updates to take.

- index:

  The network's index, unless `problem` carries one.

- edits:

  The node's arc-bound decisions.

- incumbent:

  The best objective known, which sets the step scale.

- step0:

  An explicit `t_0`, overriding that scale.

- cost:

  Optional distance matrix for the audit.

- warm:

  Optional `list(flow, potential)` to start the first solve from.

- deadline:

  Elapsed time, on
  [`proc.time()`](https://rdrr.io/r/base/proc.time.html)'s clock, past
  which a solve stops where it stands. `Inf` is no budget.

## Value

A list with `bound` and `bound_exact`, the best bound rounded down and
exact, the multipliers that attained it, the relaxed solve at those
multipliers, any moment-feasible solutions the ascent passed through,
`certified`, whether any step proved a bound, and `warm`, the last
complete solve's flow and potentials. A `status` of `"interrupted"`
means a solve ran out of time; the bound and the solutions reported
alongside it came from the steps that finished, and are as valid as any
others.

## Details

Consecutive steps solve one network. The topology, the arc bounds and
every non-pair cost are the ones the previous step solved; only the pair
costs moved, by one multiplier step, so the previous step's flow and
potentials are a near-optimal starting point for this one and are
carried across. `warm` gives the first step the same footing, from
whichever solve the caller has to hand.
