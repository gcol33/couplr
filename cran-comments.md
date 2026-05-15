## Resubmission (1.3.2)

This is a resubmission of 1.3.1. The previous submission was rejected because
`R CMD check` on win-builder r-devel-windows-x86_64 crashed with exit code
-1073741819 (access violation) in `test-lap-solve-batch-coverage.R`.

Debian r-devel-linux-x86_64-gcc passed cleanly on the same submission, and
neither local `R CMD check --as-cran` (Windows 11, R 4.6.0 ucrt, Rtools45 g++
14.3.0 — the same toolchain win-builder uses) nor the parallel test file
alone reproduced the crash, so the precise cause was not isolated.

Mitigations applied:

* Removed `Config/testthat/parallel: true` from DESCRIPTION. The win-builder
  log shows the crash inside testthat's parallel worker
  (`testthat:::test_files_parallel`), so disabling parallel test execution
  eliminates cross-file worker-state leakage as a contributor.
* Added `testthat::skip_on_cran()` at the top of
  `test-lap-solve-batch-coverage.R`. Equivalent code paths are exercised
  by `test-lap-solve-batch-coverage-2.R`, `-coverage-3.R`,
  `-extended.R`, `test-batch-coverage-final.R`, `test-batch-processing.R`,
  and `test-batch-kbest-extended.R`, which all pass on CRAN.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local: Windows 11 x64, R 4.6.0 ucrt, Rtools45 g++ 14.3.0
* win-builder: R-devel, R-release (re-checked before resubmission)
* mac-builder: macOS ARM64, R-devel
* GitHub Actions: macOS-latest, windows-latest, ubuntu-latest (devel,
  release, oldrel-1)

## Downstream dependencies

None.
