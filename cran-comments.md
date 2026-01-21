## R CMD check results

0 errors | 0 warnings | 1 note

### NOTE: "Days since last update: 0"
This is a hotfix for UBSAN, vignette, and encoding failures in v1.0.6/1.0.7/1.0.8.

## Test environments

* local Windows 11 x64, R 4.5.2
* GitHub Actions: ubuntu-latest (R release), windows-latest (R release), macOS-latest (R release)
* R-hub: gcc-asan, clang-asan, clang-ubsan, m1-san
* win-builder: R-devel

## Downstream dependencies

None.

## Resubmission

This is a **hotfix** addressing failures detected during CRAN's additional checks:

### Issue 1: gcc-UBSAN failure (C++ undefined behavior) - Fixed in v1.0.8
**Source:** https://www.stats.ox.ac.uk/pub/bdr/memtests/gcc-UBSAN/couplr

**Error:** Left shift of negative value -1 in `utils_gabow_tarjan.cpp:1384`

**Fix:** Replaced `c_current[i][j] << 1` with `c_current[i][j] * 2` to avoid
undefined behavior when intermediate cost values are negative (which can occur
during the bit-scaling phase of the Gabow-Tarjan algorithm).

### Issue 2: M1mac vignette failure - Fixed in v1.0.8
**Source:** https://www.stats.ox.ac.uk/pub/bdr/M1mac/couplr.out

**Error:** `Error in select(): unused arguments (variable, std_diff)` in
comparison.Rmd during vignette rebuild.

**Root cause:** The `do.call(rbind, lapply(list, as.data.frame))` pattern in
`balance_diagnostics()` was converting named lists to a matrix with generic
column names instead of preserving the original names like `variable` and
`std_diff`.

**Fix:** Replaced with `dplyr::bind_rows(lapply(list, tibble::as_tibble))`
which properly preserves column names. Applied same fix to:
- `R/matching_diagnostics.R` (2 locations)
- `R/matching_preprocessing.R` (1 location)
- `R/matching_parallel.R` (2 locations)

### Issue 3: gcc-asan encoding failure - Fixed in v1.0.9
**Source:** R-hub gcc-asan check on v1.0.8

**Error:** `conversion failure on 'CSA: Systematic e-Scaling' in 'mbcsToSbcs': for e (U+03B5)`

**Root cause:** Vignettes contained Unicode characters (Greek epsilon,
multiplication sign, arrows, accented characters) that fail to convert in
C locale environments used by some CRAN check platforms.

**Fix:** Replaced all non-ASCII characters in vignettes with ASCII equivalents:
- Greek epsilon (U+03B5) -> "epsilon"
- Multiplication sign (U+00D7) -> "x"
- Right arrow (U+2192) -> "->"
- Left arrow (U+2190) -> "<-"
- Check mark (U+2713) -> "Yes"
- X mark (U+2717) -> "No"
- Accented characters (Koenig, Egervary) -> ASCII equivalents

## R-hub sanitizer verification

Verified fixes pass R-hub sanitizer checks:
https://github.com/gcol33/couplr/actions/runs/21200137638

Results:
- m1-san (ASAN+UBSAN on macOS): PASSED
- clang-asan: PASSED
- clang-ubsan: PASSED
- gcc-asan: encoding fix applied in v1.0.9
