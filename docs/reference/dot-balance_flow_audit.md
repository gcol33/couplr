# Check a balance flow against the objective it is meant to encode

Recomputes the matched sample's cardinality, its imbalance at every
level and its total distance from the flow's own pair arcs, and compares
the arc costs against
`P * (K - pairs) + sum_h eps_h * imbalance_h + sum_ij d_ij x_ij`.

## Usage

``` r
.balance_flow_audit(problem, index, flow, cost = NULL, tiers = index$tiers)
```

## Value

A list holding the feasibility counts, the objective read both ways, and
the gap between them.
