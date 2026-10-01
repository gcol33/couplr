# Balance Table for Matching Results (cobalt integration)

S3 method enabling
[`cobalt::bal.tab()`](https://ngreifer.github.io/cobalt/reference/bal.tab.html)
on couplr result objects. Requires the cobalt package to be installed.

## Usage

``` r
# S3 method for class 'matching_result'
bal.tab(x, left, right, ...)

# S3 method for class 'full_matching_result'
bal.tab(x, left, right, ...)

# S3 method for class 'cem_result'
bal.tab(x, left, right, ...)

# S3 method for class 'subclass_result'
bal.tab(x, data = NULL, ...)
```

## Arguments

- x:

  A couplr result object

- left:

  Data frame of left (treated) units

- right:

  Data frame of right (control) units

- ...:

  Additional arguments. Arguments named in
  [`as_matchit()`](https://gillescolling.com/couplr/reference/as_matchit.md)'s
  signature go to the conversion; the rest go to
  [`cobalt::bal.tab()`](https://ngreifer.github.io/cobalt/reference/bal.tab.html).

- data:

  Data frame used for subclassification (for subclass_result only)

## Value

A cobalt balance table object

## Details

These methods convert couplr results to the format cobalt expects (a
matchit-class object) and then delegate to cobalt's own
`bal.tab.matchit()` method. The cobalt package must be installed but is
not required for couplr to function.
