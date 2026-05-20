## Release notes (1.4.1)

This is an out-of-cycle patch release because 1.4.0 (accepted two days ago)
caused 1.5-hour test timeouts on the M1mac and linux-arm64 additional CRAN
checks (https://www.stats.ox.ac.uk/pub/bdr/M1mac/couplr.out,
https://github.com/r-devel/linux-arm64-checks/tree/HEAD/couplr). The same
expected NOTE on CRAN incoming feasibility ("Days since last update: 2") is
therefore unavoidable; the alternative is letting 1.4.0 hit the additional-
check archival threshold.

In 1.4.0, two solver paths could stall indefinitely when `match_couples()`
was called with `max_distance`, calipers, or other forbidden-edge
constraints:

* Forbidden cells were marked with a large finite value (`BIG_COST`). The
  Jonker-Volgenant and small-`n` SSP solvers saw `BIG_COST` as a regular
  expensive edge and could degenerate on sparse, near-square inputs
  rather than short-circuiting on infeasibility. Switched to `Inf` so the
  C++ solvers' non-finite check fires.

* Auto-dispatch routed sparse inputs with `n <= 100` through SSP, which
  has its own worst-case stall on near-square highly-sparse matrices.
  All sparse inputs now go through `lapmod` regardless of size.

`match_couples()` additionally now drops rows/columns with no allowed
edges before the LAP call and falls back to `greedy_matching()` if the
feasibility-pruned submatrix still has no perfect matching.

The full test suite (with `NOT_CRAN=true`, i.e. all `skip_on_cran()`
guards lifted) now finishes in ~6.4 minutes locally with 0 failures;
on CRAN, where `skip_on_cran()` is honoured, the constraint tests
in `test-matching*.R` no longer hit the stall paths.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local: Windows 11 x64, R 4.6.0 ucrt, Rtools45 g++ 14.3.0
* GitHub Actions: macOS-latest, windows-latest, ubuntu-latest
  (devel, release, oldrel-1)

## Downstream dependencies

None.
