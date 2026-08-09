## Release notes (1.5.5)

This release supersedes 1.5.3, the version currently on CRAN. It collects the
performance work in 1.5.5 and the macOS memory-detection fixes in 1.5.4, which
was tagged but not submitted.

### Performance (1.5.5)

`method = "auto"` selects a solver from three data-dependent facts: whether any
entry is `NaN`, whether the finite entries are constant or binary, and what
fraction of entries are non-finite. These were read with `any(is.nan())`,
`range(finite = TRUE)` and `mean(is.na() | is.infinite())`, each of which
allocates a temporary the size of the cost matrix, so selecting a solver cost
several full-size allocations and several passes before any solving began. A
single C++ pass now returns all of them without allocating. The probe is 7 to
18 times faster than the code it replaces, measured from `n = 10` to
`n = 5000`, and the gap between `method = "auto"` and naming the solver it
selects falls from as much as 2x to a few percent.

Dispatch decisions are unchanged. The new probe reproduces the previous solver
choice on every case tested, including all-`Inf`, exactly-half-sparse,
constant, binary-with-`NA` and integer inputs; `tests/testthat/test-dispatch-probe.R`
covers these along with the `NaN` rejection that runs for every method.

Integer cost matrices are read as `INTSXP` in place during selection rather
than coerced, so an integer matrix no longer pays for a full double copy on the
way to the dispatcher.

### Bug fixes (1.5.4)

* `memory_mode = "auto"` under-read available memory on Apple Silicon.
  `get_free_ram_mb()` converted `vm_stat`'s page counts with a hardcoded
  4096-byte page, but Apple Silicon pages are 16384 bytes, so every M-series
  Mac saw a quarter of the memory it had (7.7 GB reported against 41.7 GB
  available on a 64 GB M4 Pro). The page size is now read from `vm_stat`'s own
  header, with `sysctl hw.pagesize` as a fallback.
* The macOS memory figure now counts inactive and speculative pages, which are
  reclaimed on demand, matching the `MemAvailable` semantics the Linux branch
  already used.

### Documentation

* `?assignment` listed `"line_metric"` among the `method` values, but it was
  never accepted by `match.arg()`; one-dimensional problems are solved by
  `lap_solve_line_metric()`, and the documentation now points there.
* The five `"auto"` selection rules are stated in `?assignment` rather than
  only summarised as "automatic selection".

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local: Windows 11 x64, R 4.6.0 ucrt, Rtools45 g++ 14.3.0
* win-builder: r-devel
* GitHub Actions: macOS-latest, windows-latest, ubuntu-latest
  (devel, release, oldrel-1)

## Downstream dependencies

None.
