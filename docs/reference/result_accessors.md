# Certificate, Potentials and Unmatched Units of a Result

Accessors for the parts of a matching or assignment result that later
analyses read.

## Usage

``` r
certificate(x, ...)

potentials(x, ...)

unmatched_units(x, ...)

# S3 method for class 'cardinality_certificate'
print(x, ...)

# S3 method for class 'cardinality_certificate'
summary(object, ...)

# S3 method for class 'cardinality_certificate'
glance(x, ...)

# S3 method for class 'assignment_certificate'
glance(x, ...)

# S3 method for class 'flow_certificate'
glance(x, ...)
```

## Arguments

- x, object:

  A result of
  [`match_couples()`](https://gillescolling.com/couplr/reference/match_couples.md),
  [`full_match()`](https://gillescolling.com/couplr/reference/full_match.md),
  [`cardinality_match()`](https://gillescolling.com/couplr/reference/cardinality_match.md),
  [`cem_match()`](https://gillescolling.com/couplr/reference/cem_match.md),
  [`subclass_match()`](https://gillescolling.com/couplr/reference/subclass_match.md),
  [`match_path()`](https://gillescolling.com/couplr/reference/match_path.md),
  [`assignment()`](https://gillescolling.com/couplr/reference/assignment.md)
  or
  [`assignment_duals()`](https://gillescolling.com/couplr/reference/assignment_duals.md);
  for [`glance()`](https://generics.r-lib.org/reference/glance.html), a
  certificate from
  [`verify_assignment()`](https://gillescolling.com/couplr/reference/verify_assignment.md)
  or
  [`verify_flow()`](https://gillescolling.com/couplr/reference/verify_flow.md).

- ...:

  Ignored.

## Value

See Details.

## Details

- `certificate()` returns the checked optimality certificate, an
  `assignment_certificate` or `flow_certificate` (see
  [`verify_assignment()`](https://gillescolling.com/couplr/reference/verify_assignment.md)),
  or `NULL` when none was computed. A matching carries one when it was
  solved with `certify = TRUE` or in implicit mode. Cardinality matching
  returns a `cardinality_certificate` recording its completed search and
  checked bounds. Its summary exposes the size, balance, and distance
  stages and the constraint report.

- `potentials()` returns the dual potentials as a list with elements
  `left` and `right` (named by unit id) for a matching, or `u` and `v`
  for an assignment of a cost matrix; `NULL` when the solver returned
  none.

- `unmatched_units()` returns the ids of the unmatched units as a list
  with elements `left` and `right`, or the unmatched row indices of an
  assignment.

[`glance()`](https://generics.r-lib.org/reference/glance.html) on a
certificate returns one row with `certified`, `arithmetic`,
`primal_objective`, `dual_objective`, `duality_gap` and, for an
assignment certificate, `max_suboptimality`.

## Examples

``` r
set.seed(1)
cost <- matrix(runif(16), 4)
res <- assignment_duals(cost)
potentials(res)
certificate(assignment(cost, certify = TRUE))
```
