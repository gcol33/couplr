## Release notes (1.9.0)

A feature release, 34 days after 1.8.1. The main changes:

* The matching functions return objects of one parent class,
  `"couplr_result"`, with shared `print()`, `summary()`, `glance()`, `tidy()`
  and `as.data.frame()` methods, and accessors `certificate()`,
  `potentials()` and `unmatched_units()`.
* `assignment()` looks its method up in a solver registry;
  `lap_solvers()` lists the solvers and `register_lap_solver()` adds one.
* `cardinality_match()` solves moment-constrained selections by branch and
  bound with an exact certificate.
* Dense matching uses less memory (a 20,000-unit Mahalanobis matching peaks
  at 1.5 GB instead of 6.4 GB).
* No `\dontrun{}` examples remain; Mahalanobis distances use a Cholesky
  factorisation instead of `solve()`.

One unused output column (`ks_pvalue` in `balance_diagnostics()`) was removed.
There are no reverse dependencies on CRAN.

## R CMD check results

PENDING

## Test environments

PENDING

## Downstream dependencies

None.
