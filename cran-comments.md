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

## Changes in this version (1.2.0)

Feature release adding matching extensions and analysis tools:

* Ratio matching (k:1) and with-replacement matching
* Propensity score matching (`ps_match()`) with logit caliper
* Cardinality matching (`cardinality_match()`) for balance-constrained matching
* Sensitivity analysis (`sensitivity_analysis()`) via Rosenbaum bounds
* `autoplot()` methods for matching results, balance diagnostics, and
  sensitivity analysis (requires 'ggplot2')
* Enhanced `summary()` with match rate and distance percentiles

All 4916 tests passing across platforms.
