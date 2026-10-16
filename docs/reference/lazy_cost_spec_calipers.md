# Calipers of a lazy cost spec, as the C++ cost source reads them

One `list(threshold, left, right)` per caliper, named by its variable:
the threshold and the variable's raw values for every left and right
unit, in the row order of `left_mat` and `right_mat`.

## Usage

``` r
lazy_cost_spec_calipers(spec)
```

## Value

Named list of caliper records.
