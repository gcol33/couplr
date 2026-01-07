# couplr Algorithm Integration TODO

## Completed

Network Simplex implementation (all 20 tests passing)

Orlin-Ahuja solver

Push-Relabel solver

Ramshaw-Tarjan rectangular solver

Assignment duals extraction

All algorithms added to
[`assignment()`](https://gcol33.github.io/couplr/reference/assignment.md)
main routine

## Remaining Tasks

Run
[`devtools::document()`](https://devtools.r-lib.org/reference/document.html)
to regenerate `.Rd` documentation

Rebuild pkgdown website using `build_site.R`

Commit and push all changes

## Notes

- Documentation in `man/assignment.Rd` is outdated (missing: lapmod,
  csa, ramshaw_tarjan, push_relabel, orlin, network_simplex)
- Website `docs/reference/assignment.html` shows old method list
- Network simplex files in `src/solvers/network_simplex/` are untracked
