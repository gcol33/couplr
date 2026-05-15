## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local: Windows 11 x64, R 4.5.2
* GitHub Actions: macOS-latest (R release), windows-latest (R release),
  ubuntu-latest (R devel, release, oldrel-1)
* win-builder: R-devel (Status: OK)
* mac-builder: macOS ARM64, R-devel (Status: OK)
* R-hub: clang-asan, clang-ubsan, gcc-asan, valgrind (all PASS)

## Downstream dependencies

None.

## Changes in this version (1.3.1)

This release bundles the 1.3.0 feature additions and a 1.3.1 default change:

* `full_match()` gains `method = "optimal"` (new default) using a min-cost
  max-flow solver (Dijkstra + Johnson potentials) that finds the globally
  optimal group assignment minimizing total distance. `method = "greedy"`
  is preserved for fast approximate matching.
* Mahalanobis distance now uses the pooled within-group covariance by
  default, matching the convention used by `optmatch::match_on()` and
  aligning behaviour across matching packages.
* Vignette updates: getting-started and matching-workflows now cover
  full matching; comparison table reflects couplr's full-matching support.

All tests passing across platforms.
