## Release notes (1.8.1)

This is a bug-fix release four days after 1.8.0. The first fix concerns
results that were returned without any error or warning:

* With scaled or weighted matching variables, the low-memory modes
  (`memory_mode = "lazy"` and `"implicit"`) compared calipers against the
  scaled values, so they admitted pairs that the caliper excludes and that
  the default mode excludes. All modes now apply calipers to the variables
  as supplied.

* `as_matchit()` failed for designs with more than one control per treated
  unit, with replacement, or when both groups used the same ids.

* The column-generation mode now returns an exact optimality certificate on
  scaled costs, where it previously fell back to a tolerance.

No exported function is removed or renamed, and no dependency is added.

## R CMD check results

0 errors | 0 warnings | 1 note

The note is the incoming-feasibility one: days since last update 4, number
of updates in the past 6 months 7. This release fixes the results described
above, which is why it follows 1.8.0 so closely.

## Test environments

* local: Windows 11 x64, R 4.6.1, Rtools45 g++ 14.3.0: Status 1 NOTE (as
  above), tests FAIL 0 | PASS 8382.
* win-builder: R-devel (2026-09-30 r90605 ucrt): Status 1 NOTE (as above),
  tests FAIL 0 | PASS 8382.

## Downstream dependencies

None.
