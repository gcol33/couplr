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

1. **The ball-tree proof does not derive `source_quadform_slack`.** Supplement
   §4 still argues the older residual and whitening-error bound. The term is
   real and reaches the binary. This is the largest remaining item and is
   mathematical writing, not a code fix.
2. **Outward rounding.** The bound combines compensated sums under ordinary
   floating point and clamps at zero, so it is a numerical estimate rather than
   a directed-rounding certificate. Either compute it outward-rounded or say
   "certified within x cost units" and reserve "exactly certified" for the
   exact path.
3. **Regime-grid denominator.** The large-tier panel excludes `hk01` while
   dispatch selects it on binary cells, so those ratios compare against a panel
   the numerator is not in. The article states the reason; the review wants the
   selected solver always in the panel, which is a re-measurement.
4. **Correctness oracle in the regime benchmark** is the median total across
   solvers, which a majority can get wrong. Report feasibility, recomputed
   objective and certificate per run instead.
5. **Architecture claims.** "Every design compiles into one flow model" is
   broader than the code: 1:1 and fixed-ratio lower to assignment engines,
   replacement is separable, cardinality is branch and bound. A narrower claim
   is still a strong one.
6. **Scaling headline** rests on one instance at n = 50,000.
7. **Weight semantics** for the k:1 star shape are undocumented.

## Consequences

CRAN and the R Journal submission are both held. The two API defects are
counterexamples to the paper's own claim, so the release is 1.7.1 rather than
1.7.0 and the article's numbers are rebased on it.
