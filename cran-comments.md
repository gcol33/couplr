## Release notes (2.0.0)

This is a new version of a package on CRAN (1.9.0, published 2026-11-07).
The main changes since 1.9.0:

* Results are the same on every platform. The scaling statistics, the pooled
  covariance and the Mahalanobis distance of a pair are computed by one set
  of C++ routines in double arithmetic with a fixed order of summation, so
  x86_64 and arm64 give the same distances and the same matching. On 1.9.0
  the distances differed from about the 15th significant digit between the
  two, which could select another of several equally good matchings.
* Certified solves, `full_match()` included, end exactly optimal on the
  stored distances, and their certificates are decided in exact arithmetic.
* A partial matching is certified over the units that can be matched, in the
  same way in dense, lazy and implicit mode and in `match_path()`.
* A solver added with `register_lap_solver()` is reported as optimal only
  when a certificate proved its answer; an error of a solver, a named solver
  that a memory mode cannot run and a distance function of the wrong shape
  stop with a message.
* `print()` is the short form and `summary()` the full report for every
  result class.

The version is 2.0.0 because distances agree with those of 1.9.0 to rounding
and not to the last bit, and because the second class `"couplr_distance"` of
a distance object, which had no methods, is gone. There are no reverse
dependencies on CRAN.

## R CMD check results

0 errors | 0 warnings | 1 note

The note is the incoming-feasibility one: "Number of updates in past 6
months: 7".

## Test environments

* win-builder: R-devel (2026-11-17 r90926 ucrt), check time 495 s: Status
  1 NOTE (as above), tests FAIL 0 | PASS 9810.
* win-builder: R 4.6.2, check time 460 s: Status 1 NOTE (as above), tests
  FAIL 0 | PASS 9810.
* GitHub Actions: macOS (release), Windows (release), Ubuntu (devel,
  release, oldrel-1): OK.
* R-hub (2026-11-25): clang-asan and clang-ubsan Status OK with no sanitizer
  reports; valgrind 0 errors, no definitely or possibly lost memory.
* R-hub gcc-asan (2026-11-26, `-fsanitize=address,undefined,bounds`): tests
  OK with no sanitizer reports. ggraph and tidygraph do not build in that
  container and were left out, so the R code of the `algorithms` vignette,
  which draws with them, stopped there; all other checks OK.
* local: macOS 26 arm64, R 4.5.3: tests FAIL 0.

## Downstream dependencies

None.
