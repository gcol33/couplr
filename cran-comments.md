## Release notes (1.5.0)

This is a feature and correctness release.

One user-visible breaking change: `greedy_couples()` is removed and greedy
matching is now `match_couples(method = "greedy")`. The two functions shared
the same engine over ~130 lines of duplicated scaffolding; they are now a
single front door with a `strategy` argument.

The bulk of the release hardens the C++ solvers. Each solver's shipped Rcpp
entry point previously ran a second copy of the algorithm that had drifted
from the pure `lap::solve_*` implementation exercised by the C++ tests; every
wrapper now delegates to that single tested implementation, checked against
brute force over randomised integer, fractional, rectangular, maximize, and
forbidden-edge inputs. Several solvers gained exact-optimum fixes as a result
(`auction*`, `csa`, `ssap_bucket`, `hk01`, `line_metric`, `gabow_tarjan`), plus
64-bit indexing and overflow guards at extreme scale. The statistical layer
fixes matched-pair scrambling in `sensitivity_analysis()`, ATE subclass
weights, and stratum-weighted balance diagnostics. See NEWS.md for the full
list.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local: Windows 11 x64, R 4.6.0 ucrt, Rtools45 g++ 14.3.0
* win-builder: r-devel
* GitHub Actions: macOS-latest, windows-latest, ubuntu-latest
  (devel, release, oldrel-1)

## Downstream dependencies

None.
