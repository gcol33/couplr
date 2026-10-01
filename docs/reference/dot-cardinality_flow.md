# One solve of a balance network

Applies a node's arc bounds and multipliers, solves the network, reads
the solve exactly, audits the flow against the objective identity the
design encodes, and reads the matched set back.

## Usage

``` r
.cardinality_flow(
  problem,
  index = NULL,
  coefs = NULL,
  lambda = NULL,
  edits = NULL,
  cost = NULL,
  warm = NULL,
  time_limit = Inf
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

- lambda:

  Multipliers, one per row.

- edits:

  The node's arc-bound decisions.

- cost:

  Optional distance matrix for the audit.

- warm:

  Optional `list(flow, potential)` from an earlier solve of the same
  network, used as the solver's starting point.

- time_limit:

  Seconds this one solve may run.

## Value

A list with the solve status, the flow and potentials, the pair
potentials in distance terms with the multipliers folded in
(`pair_duals`, see `.cardinality_pair_duals()`), `bound` and
`bound_exact`, the node bound the solve proves, rounded down and exact,
`certified`, whether the flow is exactly optimal for the node's
Lagrangian over every admissible pair, the audit, the matched set, and
the true objective as `objective_exact` and as `objective`, rounded up.
A solve that ran out of time comes back with status `"interrupted"`, its
flow and potentials, and nothing else: it proved neither an optimum nor
the absence of one, so reading and auditing it would be work spent on a
number no caller may read.

## Details

The solver is handed the multiplier-repriced costs rounded to doubles,
and the exact reading is taken against the costs themselves: the bound
it returns is D(pi) of the node's Lagrangian at the solve's potentials,
which weak duality makes a lower bound on the node whether or not the
solve was optimal, and which is the solve's exact Lagrangian value when
it was. A generating search prices the pairs it omits against the same
potentials, at zero and exactly, and adds any that price below until
none do, so the bound holds over every admissible pair.
