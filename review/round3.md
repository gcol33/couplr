# Round 3 review

Referee-style audit of `paper/rjournal/rjournal.pdf` (20 pages, the 2026-08-31
build). Verdict: **major revision if submitted now**. Do not submit in the
present form.

The structure is not what is holding the paper back; the reviewer says so
explicitly and asks that the seven-section architecture stay. Correctness and
claim discipline are the problem.

## Verified in the repository

Each blocker below was checked against the source before it was accepted.

| Claim | Verdict | Evidence |
|---|---|---|
| Numerical certificate accepts unmatched rows | **Confirmed** | `src/core/lap_certify.h:425` omits `all_rows_matched`; `:441` includes it. Reproduced on installed 1.7.0 |
| Ball tree ships and is default for Mahalanobis | **Confirmed** | `flow_row_search.h:81,96`; landed `6580679` 2026-08-18; present at `a63b760` |
| Implicit benchmarks ran with the tree active | **Confirmed** | `paper/bench_implicit.R:95` passes `distance = "mahalanobis"` |
| No outward rounding on the prune | **Confirmed** | no `nextafter`/`nexttoward` in `src/flow/` or `src/core/` |
| CRAN serves 1.6.1, not 1.7.0 | **Confirmed** | `available.packages()` against cloud.r-project.org |
| Test coverage misses the default arithmetic | **Confirmed** | `tests/testthat/test-certify-exact.R:92` pins `arithmetic = "exact"` |

## Blockers, tracked as issues

- **#44** `verify_assignment()` certifies an assignment with unmatched rows under
  `"auto"` and `"double"`. The paper's central claim is false on the public
  default path.
- **#45** The ball-tree pricer ships and is unconditional for Mahalanobis, while
  the article describes a block scan and proposes the tree as future work.
  `NEWS.md` never recorded the tree, which is the proximate cause.
- **#46** The prune bound is plain double arithmetic with no outward rounding and
  no error budget, so a prune is not yet a certificate.

## Version and CRAN

CRAN serves 1.6.1. The R Journal requires the proposed package to be on CRAN or
Bioconductor and tests this editorially. The paper says 1.7.0 and "available on
CRAN"; `DESCRIPTION` at `a63b760` says 1.6.2.

The sources at `a63b760` and `v1.7.0` are identical under `R/` and `src/`, and
`logs/ENVIRONMENT.txt` states this. The article does not. Wording to adopt:

> Benchmarks were run from commit `a63b760`, whose DESCRIPTION reported version
> 1.6.2. The R and C++ sources at that commit are identical to those tagged as
> couplr 1.7.0.

Because #44 and #46 change the code, the final benchmark commit and version move
again. The certificate-sensitive and implicit benchmarks have to be rerun on the
build that reaches CRAN.

## Mathematical corrections

- **Square duals.** "The column constraints bind, and `v` is unrestricted" is
  the wrong argument: binding at an optimum does not remove a sign restriction.
  The correct one is redundancy — when `n = m` the column inequalities may be
  replaced by equalities without changing the feasible set, and the equality
  formulation has unrestricted column potentials.
- **The column-generation bound.** `delta + n*epsilon` assumes more than the text
  states. Name the parts separately: restricted-master dual-feasibility error,
  pricing threshold, column-sign error on `v <= 0`, tree-bound error, and the
  reported gap. State whether `v <= 0` is structurally imposed or numerically
  checked; shifting `u` alone does not repair a positive `v_j`.
- **Return the bound users need.** The `(n + 2m)*epsilon` derivation is in the
  paper but the object reports the tolerance rather than the resulting maximum
  suboptimality. Consider returning `certificate_type`, `arithmetic`,
  `tolerance`, `max_suboptimality_bound` and its contributions.

## Claims that fail under multiple optima

The paper demonstrates two distinct optima on page 6 and then forgets it.

- "All three return the same assignment" becomes "all three solve the same
  problem; on the instances reported they returned the same pairing where
  pairing identity was compared."
- Drop "same total and same pairing" as a universal correctness requirement and
  "a disagreement is a defect". Correctness needs equal certified objectives,
  primal feasibility and certificates. Pair identity is required only under a
  known-unique optimum or an explicit canonical tie-breaking contract.
- Table 3's "the path is the same sequence of matchings" is false as stated; equal
  status, count and total prove the same objective sequence. Either compare pair
  identities or weaken it.
- "18 of 18 returned the same pairing unit by unit" is fine, because it is
  reported as an empirical result.

## Overbroad statements

- "Every solver returns the same optimal value" conflicts with the Gabow-Tarjan
  scaling discussion that follows it. Say the nineteen target the same objective,
  that solvers which scale or round real costs are exact for the transformed
  integer problem, and that `verify_assignment()` decides whether the result is
  optimal for the matrix as supplied.
- "One solver, one certificate and one warm start serve all" overstates the warm
  start, whose public scope is unit-capacity. Say the flow solver and flow
  certificate serve all flow-representable designs, with shared warm-start
  machinery where the interface exposes it.
- Full matching's greedy path: the API defaults to the optimal method and offers
  greedy on request. Say so; the current wording implies an automatic switch.
- "One round" after widening a caliper: reoptimization changes the duals, so
  previously omitted arcs acquire new reduced costs and a full pricing sweep is
  still needed. Say that newly admitted arcs are repaired first, then the sweep
  rechecks the omitted set under the updated duals.

## Competitors and novelty

- **lpSolve.** The blanket claim that none of the named packages exports duals is
  false: `lp()` documents a `duals` component and `lp.assign()` has
  `compute.sens`. Narrow to: among the matching-specific interfaces surveyed,
  none exposes potentials together with an independent verifier; general LP
  interfaces return duals but no assignment-specific certificate API.
- **Table 6's rectangular row.** "via padding" misdescribes optmatch, which goes
  through its full-matching min-cost-flow formulation, and MatchIt, which calls
  `optmatch::fullmatch()`. Either drop the row or rename it to name the API
  distinction rather than a capability one.
- **Certificate row.** optmatch documents an `exceedances` attribute bounding how
  far the objective may exceed the best feasible one. Rename the row to "public
  dual-potential verifier" so the distinction is accurate without erasing that.
- **Gabow-Tarjan priority.** "No publicly available open-source implementation in
  any language" is unprovable. Narrow to "we are not aware of a prior publicly
  available open-source implementation", and record the search date and scope in
  the supplement if the claim is kept.

## Empirical evaluation

The reviewer recalculated the key quantities from the CSVs and they agree with
the prose: 189 cells, 94 direct fastest selections, median 1.02x; 2-10 rounds;
0.29-73.32 percent graph retained; 0.24-6.67 complete-pair evaluation
equivalents; 20/20 certified; 18/18 identical pairings; path ratio about 0.47;
scaling, memory and the 50,000-unit timings consistent.

Asked for:

- rename Table 3's `Ratio` to `Cold/path` and `Solve time` to `Solve-time ratio`;
- explain why the hostile contested case is 3.2x faster despite 6.7x pair
  evaluations, rather than leaving the decomposition implicit;
- explain the optmatch/MatchIt peak-memory difference, given MatchIt calls
  optmatch;
- say that the implicit robustness grid is one-factor-at-a-time, and consider a
  small crossed hostile grid, since tree effectiveness interacts;
- keep the 50,000 one-instance caveat conspicuous;
- run the final benchmark from a clean detached worktree writing outputs
  elsewhere, and record a clean source hash.

## Structure

Keep the seven sections. The remaining weakness is density inside section 2: a
general R reader waits until page 6 for the first workflow, and exact-sign
arithmetic and Gabow scaling arrive before the data-frame interface.

Move the full `(n + 2m)*epsilon` derivation, the Shewchuk cancellation example
and the detailed Gabow conversion into implementation or the supplement. Keep
the LAP formulation, certificate output and solver portfolio in section 2, and
put a compact solver glossary in the supplement rather than shrinking the
portfolio.

## Supplement

55 pages and about 23,500 words, of which roughly 37 pages are raw solver tables
duplicating the CSVs. Target 12-20 pages: the maths omitted from the article,
benchmark definitions and protocol, selected and worst-case tables, a solver
glossary, pointers to the complete CSVs, the outcome caveats, A4 throughout.

## Layout and accessibility

The build is otherwise clean: 20 A4 pages, 234-word abstract, fonts embedded,
vector figures, no clipping or corrupt glyphs, metadata present.

- Figure 1 floats before the subsection heading and paragraph that introduce it.
- The dispatcher sentence splits across pages 15-16, ending "at a median" with
  Table 4 intervening before "1.02x".
- Table alt text: the figures carry `fig.alt`, the `kable` tables do not. Round 2
  concluded no change was needed on the grounds that `rjtools`' own templates put
  `fig.alt` only on figure chunks. The reviewer raises it again, so the reply has
  to make that argument explicitly rather than leaving it silent.
- Figure 1's labels (`SAP-D`, `Gabow-T`, `Cycle-C`, `Net-S`) are cryptic for a
  headline contribution; add a mapping to public method name, family and regime.

## Smaller corrections

- Test counts: the paper says 138 R and 37 C++ test files; the tagged source has
  139 and 38. Count during rendering or drop the numbers.
- "The Performance section" is stale; it is "Empirical evaluation".
- Standardize `optimization` against `optimisation`.
- Capitalize "Euclidean" in the regime-grid paragraph.
- Simplify Table 4's caption explanation of the below-1.00 ratios.
- Distinguish exact from numerical certificates typographically wherever
  "certified optimum" appears unqualified.
- Prefer "an optimal assignment" to "the assignment" except where unit-level
  identity was verified.

## Found independently, not in the review

- `rjournal.Rmd:1107` says the loop "is still the fastest arm at five of the six
  sizes". The table above it gives four: lazy is faster at 500 (8 ms against 12)
  and at 2,000 (53 ms against 56). It is the one hardcoded count in a paragraph
  that otherwise reads every number off the CSVs.
- Four places state that `"implicit"` is slower than `"lazy"` on every shape
  measured — `R/lap_solve.R:103`, `R/matching_core.R:947`, and the generated
  `man/assignment.Rd:120` and `man/match_couples.Rd:125`. `NEWS.md:74` records
  that the seed-width change reversed this; the roxygen never followed. Round 1
  already flagged a manual-against-paper contradiction on the Gabow-Tarjan bound.
- `motivation-letter/motivation-letter.md` still says the 50,000-unit run
  "returns the assignment the dense solve returns". That is round 2's item 2,
  fixed in the abstract and missed in the letter.
  `data/implicit-equivalence.csv` carries `identical_pairing` at 500, 2,000,
  5,000 and 10,000 only.
- The limitation bullet at `:1765` says scaled auction is "three to four times
  quicker on the small square cells with most of the matrix forbidden". Inside
  that same category the worst cells are 9.95x and 9.91x, which the evaluation
  section reports correctly.
- The lazy-path limitation names only the custom distance function. The manual
  at `R/matching_core.R:940` also excludes `replace = TRUE`, `ratio > 1` and
  `method = "greedy"`, and `cardinality_match()` forces dense
  (`R/matching_cardinality.R:305`). Tracked as #48.
- `paper/couplr-rjournal-submission.zip` was built at 11:43 against figures
  re-rendered at 12:46 and is stale regardless.

## Submission sequence

1. Fix #44 and add the regression coverage.
2. Resolve #46: one-sided pruning or a proved error budget.
3. Rerun exhaustive-against-tree fuzzing at extreme scales.
4. Rerun the implicit, path and certificate benchmarks on a clean final build.
5. Release that build to CRAN.
6. Resolve #45: rewrite the pricing, limitation and future-work passages.
7. Correct the dual derivation, the multiple-optimum statements, the warm-start
   scope and the competitor table.
8. Fix the floats, table alt text, stale references and test counts.
9. Compress the supplement.
10. Run `rjtools` checks and reproduce the under-ten-minute path from clean.
