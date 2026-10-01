# The flow a matched set corresponds to

Builds the flow vector a candidate matched set maps to: unit and pair
arcs at one, the slack each category needs to fill its budget, and the
transfers that carry its imbalance, each crossing at the lowest level
its two cells share.

## Usage

``` r
.balance_flow_encode(matching, index, hier = index$hier)
```

## Details

Returns `NULL` when the matched set is not balanced at the levels the
design enforces exactly, since no flow in this network represents it.
