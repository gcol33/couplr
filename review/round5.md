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
3. **DONE** (`466f3b8`). Nine of the forty-five large cells dispatched to
   `hk01` against a panel that did not time it, and read 0.02x to 0.25x with
   an empty `picked_ratio`. `hk01` is in both tiers now under the condition it
   was already guarded by, and the grid stops rather than record a cell whose
   dispatched solver its panel does not hold; the article asserts the same
   property at render time and no longer explains a reduced panel. On the
   quick grid the `no_cost_scale` rule reads a median picked ratio of 1.00 and
   a worst of 1.03, where before it had none.

   Reading the shipped grid to write that sentence turned up a second thing:
   over the 180 cells whose panel did hold their solver, the lowest ratio is
   0.59x, against a sentence claiming the two sides "scatter by a few percent
   either way". A direct A/B on beast puts `"auto"` within 1% of the solver it
   names, so 0.59x is the harness and not the dispatcher, and it is probably
   the resume: `regime-runs.csv` skips rows already in it, so two solvers of
   one cell can come from two sessions. The final run is `FRESH=1`, which is
   one session by construction. The article reads the minimum off the grid
   now (`858c645`) rather than asserting a magnitude.
4. **DONE** (`466f3b8`). Each instance gets one optimal dual solution from
   `assignment_duals()`, and every solver's matching is certified against it:
   feasibility, the objective recomputed from the matching rather than the
   total the solver reported, the duality gap, and the bound on what a
   feasible solution can beat it by. Optimal duals are shared by all optimal
   solutions, so one dual solution serves the whole cell and the verdict is
   against the instance's own optimum. The certificate is taken outside every
   timed section, so no reported time moves; measured cost is about 0.1 s per
   instance and per solve at the largest shape.

   On a 500 by 500 lognormal instance it separates `auction_scaled` at a gap
   of 5.6e-7 from nine solvers certified exact, which is the finding the
   median oracle reached by majority. The supplement's section is written
   from the certificate columns, so it will not render against a grid measured
   before them.
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
6. **DONE** (`b6ef068`). The table's largest size carried one instance and one
   repetition, so the n = 50,000 figure was a single run with no spread, and
   n = 20,000 carried two. Both go to three, the fewest that gives a median
   rather than a pair; the large sizes still drop repetitions rather than
   instances, since a repetition measures the clock and is the steadier of the
   two. The lazy path had the same shape in the same paragraph, one run per
   size, with the equivalence claim resting on one draw; it now times the
   three instances the dense table uses and checks the pairing on all three.
   The cost is one optmatch timeout at each of the two sizes, about half an
   hour.
7. **DONE** (`e3937ac`). The weighting was documented for one group shape,
   "the total weight of right units equals the total weight of left units
   (which is 1)". At `min_controls = 1` a group is centred on whichever side
   is larger, so a problem with more left units than right gives groups of k
   left units around one right unit, where each side totals k and the right
   unit weighs k. The rule that covers both is the one the code implements:
   every left unit weighs 1, and a group's right units share a total equal to
   the number of left units in it. The comment beside the computation said the
   smaller side gets weight 1, which is the opposite of what runs on that
   shape. A test pins it and checks its fixture produces the shape.

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

Items 3, 4, 6 and 7 are closed. What is left before the freeze is a decision
rather than an edit: the article quotes `speedup_med` to two decimals and
builds a sentence on which side of 1 it falls, and that number is not stable to
two decimals (below).

1. **DONE.** What the article claims about the edge-generation margin, and what
   page 20 gives up so it has slack. Both are recorded at the end of this file.
2. Freeze the code, then run the bench **once**, and as the whole suite:
   `FRESH=1 sh paper/run_bench_suite.sh`, no stage list. Budget eleven hours.
3. Sync, re-render both formats, rebuild the zip, confirm 20 pages.
4. `check_win_devel()` on the 1.7.1 tarball.
5. CRAN, not before 2026-09-06. Then the R Journal, once CRAN serves 1.7.1.

The run is the whole suite rather than the six stages that reach the ball tree,
for two reasons. `regimes` has to be in it: items 3 and 4 changed the shape of
its outputs, so neither document renders against a grid measured before
`466f3b8`. `lalonde` and `figure` have to be in it because
`logs/ENVIRONMENT.txt` is one stamp for the whole run, and leaving them out is
what "the benchmarks do not correspond to an immutable released source state"
describes. They cost minutes: `benchmark-table.csv` is 148 rows at `times = 5`,
165 seconds of timed work by its own medians, and `lalonde` is 185 treated
against 429 controls.

A `regimes` run at `858c645` was taken on 2026-09-03 to unblock rendering. Its
stamp is not the release commit, so it is a baseline and not the run that
ships.

### Item 8, the edge-generation margin, with what the two runs say

Every deterministic quantity reproduces exactly: `seeds`, `rounds_min`,
`rounds_max`, `graph_pct_med` and `distances_x_med` are byte-identical across
the two runs at all seven clouds, as are `edges_evaluated` at all six sizes.
The wall-clock ratio is not. Against the per-seed spread inside the shipped run
itself, five of the seven re-run medians fall outside the whole range:

    cloud          seed range, 5 seeds   shipped   re-run
    contested          3.18 - 3.22        3.19      4.47   outside
    lattice_ties       1.67 - 1.77        1.72      1.38   outside
    gaussian           1.70 - 2.05        1.95      1.48   outside
    clustered          1.76 - 2.11        1.87      1.41   outside
    heavy_tailed       0.98 - 1.08        1.04      0.77   outside
    shell              4.26 - 6.86        5.29      4.39   inside
    shifted           10.60 - 11.56      10.83     10.83   inside

The two that reproduced are the two with the widest seed spread, so the spread
across seeds is not the uncertainty and an interval built from it would look
precise while excluding a value the same code produced on the same machine two
days later. Both runs are the same Mac and the same R.

What that does not establish is that the quantity is unstable. The shipped
stamp reads `3a083b1 17 modified paths`, so the run 1.04x comes from was taken
on a dirty tree of unknown object freshness, and on this machine a stale object
has already made an unchanged path measure 5.4x slower. The defensible claim is
narrower: **nothing in hand supports two decimals**, since the only two runs
available disagree by 25 to 40 percent while their seed spreads are a few
percent.

So the article should not carry a two-decimal point estimate, and should not
carry a clause that appears or vanishes with which side of 1 the low end lands
on. Whole numbers, and the low end stated as what both runs agree on: on the
heavy-tailed cloud there is no margin either way. The paragraph does not lean
on the ratio; its claims are the deterministic quantities above.

Comparing this baseline against the final run settles the rest for free. Both
are cleaned builds on the same machine, so if their `speedup_med` agree, the
shipped run's dirty tree was the cause and a decimal can go back.

### Both decisions, applied

**The margin.** The paragraph's last sentence carries whole numbers read off
the grid, and states the low end as what both runs agree on: about 11x on the
displaced treated group, and none at all on the heavy-tailed cloud, where the
two paths are within measurement of each other. `ig_note` is gone. The reading
is asserted at render time, `ig_slow_x` within a factor of two of parity, since
a median wall-clock ratio does not resolve the two paths there. The contested
core's sentence quoted the same quantity to one decimal, and 3.19 against 4.47
does not support one decimal either; it reads "several times faster" now, under
an assertion that the ratio exceeds two.

**Page 20.** The capability table moves to the supplement, into the section
that already carried what every one of its entries was read from. The article
keeps a paragraph naming what differs, and the balance the caption carried
about where the alternatives lead. Measured on a render against the `858c645`
regime baseline, page 20 goes from 10.0 to 19.5 lines of slack at 20 pages, and
the supplement absorbs the table at 17. A control render, the same data with the
previous Rmd, ends page 20 on the same line as the shipped PDF, so the gain is
the move and not the new grid.