# Compile a balance design into a flow problem

Compile a balance design into a flow problem

## Usage

``` r
.balance_flow_problem(
  cost,
  hier,
  codes = NULL,
  tiers = NULL,
  arc_bounds = NULL
)
```

## Arguments

- cost:

  A matrix of distances, or a `balance_pair_pool` naming the pairs to
  carry and the distance range over every admissible pair.

## Value

A list with `problem`, a `couplr_flow_problem`, and `index`, holding the
arc ranges and the units and cells behind each arc.
