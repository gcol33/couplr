## Resubmission after archival

couplr was archived on 2026-10-26 because the M1-SAN and gcc-UBSAN problems
reported on 2026-10-05 were not corrected in time. Both reports show one
error: `build_ball_tree()` took a reference to an element of an empty vector
(`&cal_lo[id * n_cal]` with no caliper columns), "reference binding to null
pointer". 1.9.0 takes these pointers as `data() + offset`, at that site and at
the twelve other row and node pointers of the same form, so no element of an
empty vector is referenced.

Checked on R-hub (2026-11-07) with the clang-asan, clang-ubsan and gcc-asan
containers: no sanitizer reports. Under valgrind: 0 errors, no definitely or
possibly lost memory. One test asserted that a search finished before the
default 30 s time limit, which valgrind's slowdown exceeded; it is now bounded
by a node limit alone.

## Release notes (1.9.0)

The main changes since 1.8.1:

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

0 errors | 0 warnings | 1 note

The note is the incoming-feasibility one: new submission after archival, and
possibly misspelled words in DESCRIPTION. These are the names of the authors
of the cited algorithms (Bertsekas, Cuturi, Gabow, Jonker, Tarjan,
Volgenant) and of the Sinkhorn algorithm.

## Test environments

* local: Windows 11 x64, R 4.6.1, Rtools45: Status 1 NOTE (as above),
  tests FAIL 0 | PASS 8975.
* win-builder: R-devel (2026-11-06 r90845 ucrt), check time 378 s: Status
  1 NOTE (as above), tests FAIL 0 | PASS 8975.
* R-hub: clang-asan, clang-ubsan, gcc-asan, valgrind (see above).
* win-builder: R 4.6.2: Status 1 NOTE.

## Downstream dependencies

None.
