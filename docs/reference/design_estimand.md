# What the design identifies

Every matching design in the package weights the left side at 1 and the
right side to reproduce the left distribution within a pair, group or
stratum, so what it targets is the effect on the left population: the
ATT, in the orientation the matching layer is written in, where `left`
holds the treated units.

## Usage

``` r
design_estimand(n_left, n_left_matched)
```

## Value

Named list of fields to merge into a result's `info`.

## Details

`focal_discarded` is how many left units the design did not retain.
Calipers, `max_distance` and coarsened strata all drop focal units, and
the estimate then speaks about the retained ones rather than about the
whole treated group, which is what
[`as_matchit()`](https://gillescolling.com/couplr/reference/as_matchit.md)
reports when it hands the estimand to MatchIt and marginaleffects.
