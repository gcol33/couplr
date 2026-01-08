# couplr Algorithm Integration TODO

## Completed

Network Simplex implementation (all 20 tests passing)

Orlin-Ahuja solver

Push-Relabel solver

Ramshaw-Tarjan rectangular solver

Assignment duals extraction

All algorithms added to
[`assignment()`](https://gillescolling.com/couplr/reference/assignment.md)
main routine

Run
[`devtools::document()`](https://devtools.r-lib.org/reference/document.html)
to regenerate `.Rd` documentation

Update `_pkgdown.yml` with new functions (assignment_duals,
bottleneck_assignment, sinkhorn, sinkhorn_to_assignment)

Rebuild pkgdown website using `build_site.R`

Commit network simplex changes (b9a3094)

Update CLAUDE.md with new algorithms (7861e87)

Comprehensive documentation update for all 20 algorithms (8db9f55)

- Reorganized `@param method` into categorized groups
- Expanded algorithms vignette with 6 new algorithm sections
- Added specialized functions section (duals, bottleneck, sinkhorn)
- Updated quick reference and performance tables

## All Done!
