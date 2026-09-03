# Round 5, external critical review: triage

An external deep review of the article bundle ("version 10"), delivered
2026-09-02, verdict *do not submit*. This file records what was checked against
the code rather than accepted, what was fixed, and what is left.

Every claim below was reproduced before being acted on, and three were not
reproducible.

## Confirmed and fixed

**`full_match()` bounds counted the wrong side** (`26dac75`). The compiler made
the group centres the smaller side, so above the default `min_controls` bounded
left units on a tall problem. Six left units, two right, `min_controls = 2`
returned two groups of three left units and one right unit each, `status =
"optimal"`, with a certificate, against a documented minimum of two right units
per group. `man/full_match.Rd` held both readings in one paragraph. The bounds
count right units; the centres are the left units whichever side is larger; an
orientation that cannot meet the bound is refused as `"infeasible"`. The
transposed compile, `TransposedOracle` and the `shape$transposed` flag are gone
with the branch.

**`match_couples()` on a distance object dropped what it was given**
(`26dac75`). The branch validated `replace` and `ratio` and forwarded neither,
along with `certify`. It also took `ignore_blocks` and never read the block
variable `compute_distances()` stores, so an object built with `block_id`
matched across strata after the package had printed "Block information stored.
Blocking will be applied during matching." All four reach the solve now, and
`memory_mode` is refused rather than ignored. Two contract tests compare the
data-frame and distance-object interfaces on the same question.

**`full_match()` mis-solved negative costs** (`26dac75`). The edge-cover
reduction needs costs that do not reward extra arcs. With -100 on the diagonal
and -1 off it, the solve takes all four arcs and the minimality prune drops the
diagonal, returning the off-diagonal pairing at -2 and reporting `"optimal"`
against a full matching costing -200. Custom distance functions can produce
negatives; every built-in metric cannot. Negative distances are refused with
the shift named.

**`max_suboptimality` on an infeasible candidate** (`e5f3a22`). The bound
answers what a *feasible* solution can beat this one by, and was computed
unconditionally. A zero-cost problem with an uncovered row and zero duals
reported `primal_feasible = FALSE` beside `max_suboptimality = 0`. The guard
keyed on a NaN objective, which a partial matching does not have; it keys on
primal feasibility now and the report carries `NA`.

**Memory guard underestimated its own measurements** (`df3c712`, fixed before
the review arrived). `solve_factor` 10 against a measured 10.5 at 5,000 units.
Now 12, with the roxygen figures re-read from the shipped benchmark.

**Supplement counts stated in prose** (`f3f7729`). "The other 1 are below",
"Both are scaling methods", "the two deviations" — all hardcoded beside a
computed count that is one. Built from the count now.

**Memory table caption** (`f3f7729`) said the four-fold matrix estimate is what
the dispatcher warns from; it reads `estimate_dense_solve_mb()`.

**Covariate arithmetic** (`f3f7729`). 100,000 by 100,000 pairs 200,000 units,
so 100 covariates is 160 MB, not 80.

**Runner failed open** (`f3f7729`). `run_bench_suite.sh` logged a failed stage,
carried on and wrote `logs/SUITE.done` anyway. Written only on an all-zero run
now, non-zero exit otherwise. `lalonde` became a stage in `752ebd0`, and
`FRESH=1` archives previous outputs after the stamp is taken.

**Supplement citation** (`0cb28b2`, before the review). `[@Shewchuk1997]`
rendered literally; the file had no `bibliography:` field.

## Not reproducible

- **"The supplement refers to `src/interface`, which does not exist."** It
  exists: `src/interface/prepare_cost_matrix.cpp`, `probe_cost_matrix.cpp`.
- **"Deviating solvers are described as lognormal cases while the data calls
  the regime heavy_tailed."** The supplement's own regime table maps
  `heavy_tailed` to the label "lognormal", so the prose is consistent.
- **"Benchmarks do not correspond to an immutable released source state"** was
  true of the shipped stamp (`3a083b1`, 17 modified paths) and is being fixed
  by the re-run stamping `8323a32 0 modified paths`. The related claim that the
  ball-tree fix changed `edges_evaluated` is wrong; see
  `dev_notes/review4/handoff.md`.

## Open, not yet acted on

1. **DONE, and it was a code fix as well** (`c34fd32`). The section derives
   the source term now: the bound, why it is entrywise `|d|'|A||d|` rather
   than relative to `d'Ad`, the maximisation over the node's box, and the
   directed rounding when the slack is applied on the squared distance.

   Deriving it showed the constant was wrong. The term charged
   `gamma_{n+3}`, the count belonging to the tree's sum of squares, for the
   source's double sum; the source recomputes its differences inside the
   inner loop, so the right constant is `gamma_{2n+2}`. Against a long
   double reference, random search reaches a realised error of 5.38 eps at
   n = 2 and 6.30 eps at n = 3 where `gamma_{n+3}` is about 5 eps and 6 eps,
   so the earlier constant was exceeded by found instances rather than
   merely loose. No article number moves.
2. **DONE** (`572f460`). Each objective now carries an envelope of
   `(2u + gamma_n^2) * sum |x_k|`, which dominates Neumaier's
   `(2u + O(n^2 u^2))`, both enter `max_suboptimality`, and the assembly is
   rounded outward at every step. Adding a zero term is not rounded, so a
   bound the arithmetic proved exactly zero stays zero. On a 20 by 20
   instance with costs near 1e6 the envelopes are the whole bound, about
   1.2e-9 against an objective of 2.1e6.

   The review's premise did not extend to the exact path, which needed
   nothing: it concludes from exactly decided sign tests on `c - u - v` plus
   primal feasibility and reports the gap only as a cross-check. No
   relabelling was needed, so "certified" stands as written.
3. **Regime-grid denominator.** The large-tier panel excludes `hk01` while
   dispatch selects it on binary cells, so those ratios compare against a panel
   the numerator is not in. The article states the reason; the review wants the
   selected solver always in the panel, which is a re-measurement.
4. **Correctness oracle in the regime benchmark** is the median total across
   solvers, which a majority can get wrong. Report feasibility, recomputed
   objective and certificate per run instead.
5. **Architecture claims: mostly does not hold, one part fixed.** The article
   says "Every **flow-representable** matching design compiles into one
   internal flow model" in both the abstract and the body, and the flow
   compilation section names the designs that sit outside and why
   (`cem_match()`, `subclass_match()`, `cardinality_match()`). It is a claim
   about compilation, and it is accurate: `.couples_design()` calls
   `lap_flow_compile_couples()` for every design including 1:1, which is
   what makes the potentials available; the solve is then lowered to an
   assignment engine. The article also names `verify_assignment()` and
   `verify_flow()` separately, so it does not claim one certificate serves
   all.

   The one sub-claim that did hold is fixed: "a caliper or a distance ceiling
   is checked before a distance is computed" was true of a per-variable
   caliper, which `node_caliper_out()` reads off the node's covariate box,
   and false of a general `max_distance`, which needs the tree's distance
   bound and is evaluated pair by pair where the metric admits no ball
   bound. The sentence now distinguishes the two.
6. **Scaling headline** rests on one instance at n = 50,000.
7. **Weight semantics** for the k:1 star shape are undocumented.

## Consequences

The release is **1.7.1**, not 1.7.0: `v1.7.0` is a public tag on `985019a`,
which carries two of the defects above, and a released version has to be an
immutable identity.

**Both the CRAN upload and the R Journal submission are held** on this review
closing out, with a floor of 2026-09-06 (1.6.1 was published 2026-08-23 and
`R CMD check` reports seven updates in six months).

### State at the end of the 2026-09-03 session

    DESCRIPTION   1.7.1
    article       20 pages          supplement 17 pages
    R suite       FAIL 0
    R CMD check   0 errors, 0 warnings, 1 note, on the 1.7.1 tarball
    win-builder   1 NOTE on r-devel and r-release, but on the 1.7.0 tarball

### Order of work from here

1. Settle items 3 and 4. They are benchmark methodology, so they decide what
   the final run has to measure.
2. Land items 6 and 7 and anything else touching code.
3. Freeze the code, then run the bench **once**. Budget ten hours:
   `implicit_grid` alone took 8h17m on 2026-09-03, against an estimate of
   1h10m in the round-4 handoff.
4. Sync, re-render both formats, rebuild the zip, confirm 20 pages.
5. `check_win_devel()` on the 1.7.1 tarball.
6. CRAN, not before 2026-09-06. Then the R Journal, once CRAN serves 1.7.1.

### One thing the re-run surfaced that is not in the list above

Every deterministic quantity reproduces exactly: `edges_evaluated` at all six
sizes and `distances_x_med` at all seven clouds are byte-identical to the
shipped CSVs. The wall-clock ratios are not. `speedup_med` moved 25 to 40
percent on five of seven clouds, and `heavy_tailed` went from 1.04 to 0.77,
which is the loop losing to the lazy path. The article quotes that range to
two decimals and builds a sentence on which side of 1 it falls. That is the
review's item 8 arriving with evidence, and it should be decided before the
final run rather than after it.