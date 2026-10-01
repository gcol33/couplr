# Calipers of a lazy cost spec, keyed by variable name

The C++ lazy cost source takes its calipers as a named list of
thresholds, while the spec stores them as records carrying an index into
`spec$vars`.

## Usage

``` r
lazy_cost_spec_calipers(spec)
```

## Value

Named list of numeric thresholds, one per caliper.
