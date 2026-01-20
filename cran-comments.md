## R CMD check results

0 errors | 0 warnings | 1 note

### NOTE: "unable to verify current time"
This is a network-related check that does not affect package functionality.

## Test environments

* local Windows 11 x64, R 4.5.2
* GitHub Actions: ubuntu-latest (R release), windows-latest (R release), macOS-latest (R release)
* R-hub: windows-x86_64-devel, ubuntu-gcc-release, fedora-clang-devel

## Downstream dependencies

None.

## Resubmission

This is a **hotfix** addressing failures on the M1-SAN (UndefinedBehaviorSanitizer) checks:

1. **C++ undefined behavior**: Fixed left bit-shift of negative value in
   Gabow-Tarjan algorithm (`utils_gabow_tarjan.cpp:1398`). Replaced `(y << 1) - 1`
   with `2 * y - 1` to avoid UB when dual variables are negative (which is
   expected behavior in this scaling algorithm).

2. **Vignette build failure**: Fixed `select()` namespace conflict by using
   explicit `dplyr::select()` to prevent masking by MASS or other packages.

See: https://www.stats.ox.ac.uk/pub/bdr/M1-SAN/couplr/
