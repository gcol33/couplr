# One solve of a balance network

Applies a solve's arc bounds, solves the network, reads the solve
exactly, audits the flow against the objective identity the design
encodes, and reads the matched set back.

## Usage

``` r
.cardinality_flow(
  problem,
  index = NULL,
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

- edits:

  The solve's arc-bound decisions.

- cost:

  Optional distance matrix for the audit.

- warm:

  Optional `list(flow, potential)` from an earlier solve of the same
  network, used as the solver's starting point.

- time_limit:

  Seconds this one solve may run.

## Value

A list with the solve status, the flow and potentials, the pair
potentials in distance terms (`pair_duals`, see
`.cardinality_pair_duals()`) and exactly (`exact_duals`, present when
they were recovered), `bound` and `bound_exact`, the bound the solve
proves, rounded down and exact, `certified`, whether the flow is exactly
optimal over every admissible pair, the audit, the matched set, and the
objective as `objective_exact` and as `objective`, rounded up. A solve
that ran out of time comes back with status `"interrupted"`, its flow
and potentials, and nothing else: it proved neither an optimum nor the
absence of one.

## Details

The bound returned is D(pi) at the solve's potentials, which weak
duality makes a lower bound on the network optimum whether or not the
solve was optimal, and which is the optimum when it was. A generating
search prices the pairs it omits against the same potentials, at zero
and exactly, and adds any that price below until none do, so the bound
holds over every admissible pair.
