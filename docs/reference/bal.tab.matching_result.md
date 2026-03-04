# Balance Table for Matching Results (cobalt integration)

S3 method enabling `cobalt::bal.tab()` on couplr result objects.
Requires the cobalt package to be installed.

## Usage

``` r
bal.tab.matching_result(x, left, right, ...)

bal.tab.full_matching_result(x, left, right, ...)

bal.tab.cem_result(x, left, right, ...)

bal.tab.subclass_result(x, data = NULL, ...)
```

## Arguments

- x:

  A couplr result object

- left:

  Data frame of left (treated) units

- right:

  Data frame of right (control) units

- ...:

  Additional arguments passed to `cobalt::bal.tab()`

- data:

  Data frame used for subclassification (for subclass_result only)

## Value

A cobalt balance table object

## Details

These methods convert couplr results to the format cobalt expects (a
matchit-class object) and then delegate to cobalt's own
`bal.tab.matchit()` method. The cobalt package must be installed but is
not required for couplr to function.
