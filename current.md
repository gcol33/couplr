# couplr: current state

Updated 2026-08-15. Read this first, then `roadmap.md` for the plan,
`dev_notes/pricing-probe/findings.md` for phase 0's numbers, and
`dev_notes/phase1/` and `dev_notes/phase2/` for the certification layer's and
the flow model's design, findings and repros.

## Where things stand

**1.6.0 is released on git.** `DESCRIPTION` reads 1.6.0, the release commit is
`9c8fbe3`, tag `v1.6.0` exists, and `main` is level with `origin/main`. The
phase 1 certification layer went in as `7b3dd6e` and is no longer sitting in the
working tree.

**Nothing is staged for CRAN.** `cran-comments.md` still describes 1.5.5 and
opens "This release supersedes 1.5.3, the version currently on CRAN"; it has not
been rewritten for 1.6.0. The 1.5.3-on-CRAN figure is carried from the earlier
note and was not re-checked here, because cran.r-project.org was unreachable
from this machine. Re-check before relying on it. Note also that 1.5.4 was
tagged and never submitted, so tag history is not a record of what CRAN has.

The JOSS submission (#10898) was rejected on 2026-08-08 on demonstrated research
use, not on code quality: the pre-review called the test/CI/release practices
"very good" and the community-input criterion was explicitly signed off.
Resubmission opens around 2027-02-08, and what unblocks it is the east-west
divide study reaching preprint, not more engineering.

Work is on a 1.6 line aimed at an R Journal or JSS methods paper: compile every
matching design into one flow model, solve a certified sliver of it, and never
build the full assignment graph.

## Phase 0 is done and the verdict is GO

An isolated prototype of dual-certified edge generation. Everything lives in
`dev_notes/pricing-probe/` (gitignored).

On the paper's own benchmark (`paper/bench_common.R`, n_total = 50,000, eight
covariates, Mahalanobis, no caliper), against couplr's lazy JV on the same
machine:

| n_total | lazy JV | edge generation | speedup | rounds | arcs retained |
|---|---|---|---|---|---|
| 20,000 | 10.50 s | 2.08 s | 5.05x | 3 | 0.040% of pairs |
| 50,000 | 104.33 s | 14.99 s | 6.96x | 3 | 0.030% of pairs |

Costs match couplr exactly to 15 digits and reproduce
`paper/rjournal/data/scaling-lazy-results.csv` to the last digit.

Five of six go/no-go criteria pass. The one failure is the distance-evaluation
criterion, and chasing it changed the story: at eight covariates the loop
evaluates 152% of the pair count in distances, more than a full sweep, and is
still 7x faster, because the problem it hands the solver holds 0.03% of the
arcs. **The mechanism is arc sparsity, not distance savings.** Few distances is
a low-dimension bonus.

Below roughly n = 10,000 the loop loses at every covariate count, because a
complete solve there already costs hundredths of a second.

## Phase 1 is done: the certification layer

Roadmap section A, closing #28. Suite is green: **0 failed, 6425 passed, 0
warnings, 3 skipped**, up from a 5988-passing baseline.

The design decision everything follows from: `status` and `certificate` are two
different claims and are kept apart. `status` is what the solver terminated on,
computed from its termination state. `certificate` is a proof, present only when
one was checked, and it is the only place optimality is asserted as proven.

What shipped:

- `verify_assignment()`, exported, returning an `assignment_certificate`:
  primal feasibility, dual feasibility, complementary slackness in both halves,
  objective equality, duality gap. Duals are checked, never trusted, so a
  matching from any solver can be certified against duals from another.
- `src/core/lap_certify.h`, templated on the existing cost-source concept, so it
  runs on dense and lazy alike. `scan_reduced_costs()` is the same function
  section C's block pricer needs; it gets written once.
- `src/core/lap_hall.h`, the infeasibility witness: Hopcroft-Karp plus the
  Koenig alternating-reachability cut, returning the deficient row set and its
  neighbourhood, and re-verifying the certificate it returns.
- A closed status vocabulary (`solver_status_values()`), validated at
  construction, replacing the four `status = "optimal"` literals.
- `status` on `matching_result` too, computed from `info$solver` and the
  unmatched count before the default call discards both. The constrained path
  that falls back to greedy now says `"heuristic"` on the object rather than
  only in a warning.
- `cardinality = c("complete", "maximum", "fixed")` with `n_matches` and
  `unmatched_penalty`, all three reduced to one dummy-column mechanism sharing
  the sentinel magnitude `.pad_forbidden()` already used. Checked against
  exhaustive enumeration over partial matchings: 821 checks, 0 mismatches.
- `explain_dispatch()` and a dispatch rule table that `assignment()` itself
  reads, so the reported reason is the one that was acted on.
- `tests/testthat/test-certificate.R` plus a registry in `helper-certify.R`,
  replacing the six-fold copy-paste of `c("jv","hungarian","auction")`.

### One correction the harness forced

The dual sign condition `v_j <= 0` is **conditional on there being more columns
than rows**, and the first draft imposed it unconditionally, which rejected every
square certificate. For a feasible assignment `M'`,
`cost(M') >= sum_i u_i + sum_{j used by M'} v_j`, while the dual objective sums
`v` over every column; the bound needs the omitted columns to contribute nothing
positive. When rows and columns are equal in number, every assignment uses every
column and `v` is unrestricted. Jonker-Volgenant returns free-sign duals on a
square problem and they are correct.

## Phase 2 is done: one internal flow model

Roadmap section B, spec in `dev_notes/phase2/design.md`, ten deliverables B0 to
B9. All ten are in.

**B0 to B6 landed as `e25fcb4`.** `FlowProblem` is the representation every
design compiles into, `solve_min_cost_flow()` returns node potentials for all of
them, `certify_flow()` checks a flow against the bounded-variable LP dual, one
compiler per design, and `full_match()` is compiled and solved there, returning
`potentials` and a `certificate` beside its groups. A compiled problem that is
the assignment problem `R/lap_solve.R` already solves lowers back to a cost
matrix and goes to the existing switch, so no solver was added.

**B8 landed as five commits on 2026-08-15.** The duplicated min-cost flow cores
are gone: csflow, ssp, push_relabel and cycle_cancel's feasible-flow phase all
call `solve_assignment_flow()` in `src/flow/flow_assign.h`, and
`solve_full_matching.cpp` was deleted rather than migrated, because B6 had left
it without a caller. Each solver keeps its own cost preparation, its own status
words and its own relaxation predicate, which travels in `FlowOptions.relax_eps`
and is a different value in all three of csflow, ssp and push_relabel.

Every migration was judged by B0 alone and none of them moved it. Four of the
six solver defects `dev_notes/phase2/findings.md` records are answered by the
deletion or by the collapse; the self-loop `add_edge` is fixed in the one copy
that survives. Suite after: 0 failed, 0 errors, 3 skipped in R, and 69208
assertions over 246 cases in `cpp_tests/`.

**It is also faster, which was not the expectation.** Compiling and expanding a
`FlowProblem` walks the arc set one more time than building a residual graph
directly did, and `sap` is on the auto path, so the migration was measured
rather than assumed: 1.1x to 2.1x faster across three shapes, every cost
identical to 17 digits, table in `dev_notes/phase2/findings.md`. The likely
source is that the shared solver allocates its search vectors once instead of
once per augmentation.

**B7 landed on 2026-08-15.** `match_couples()`'s three designs are named to the
compilers and routed on what the compiled network is: 1:1 and k:1 lower to the
assignment `R/lap_solve.R` already solves, matching with replacement compiles to
a network whose columns cannot bind and is solved one row at a time. The
branches that used to decide it, `if (replace)` and `if (ratio > 1L)`, are gone,
and so is `.couples_ratio()` -- the k:1 path was the 1:1 path with a
hand-written `rep(seq_len(n), each = ratio)` in front of it, and that
replication is now the compiler's row map.

Only the shape crosses to C++. Which network a design compiles to follows from
the row and column counts, so the costs stay in the matrix `matching_core.R`
built and the lowered problem is that matrix read through the maps, which is
what keeps the lazy 1:1 path -- whose premise is that the matrix is never
materialized -- reachable. `is_row_separable()` joins the lowering predicate:
the replacement design's solve relies on it, and each design's route is checked
against the compiled network rather than assumed.

B0 is the judge and it is unmoved: 1537 cases, the same 16 `full_match/*`
differing on the `potentials` and `certificate` B6 added, and every
`match_couples/*` case identical. Suite after: 0 failed, 0 warnings, 3 skipped
in R, and 69252 assertions over 249 cases in `cpp_tests/`.

**B9 landed on 2026-08-15, and phase 2 is done.** The 249 Catch2 cases in
`cpp_tests/` own the solver and the certifier, so `tests/testthat/test-flow-model.R`
covers the layer above them: the constructor's checks, restated in the caller's
terms so a malformed problem is named in R rather than in node indices; the
three steps staying separable; the status vocabulary the R wrapper validates on
the way out; and `verify_flow()`, exported at B5 and until now the one exported
function in the package with no testthat coverage of its own.

Each of the certificate's four conditions is asserted to fail on an input that
breaks it, because a check that passes on potentials it never read proves
nothing. The optimal flow priced by potentials that are not optimal ones fails
complementary slackness, potentials above every arc cost fail dual feasibility
and name the arc that attains the worst price, a flow one unit over capacity
fails primal feasibility and reports no objective, a flow one unit short fails
conservation at both ends, and a fractional flow is not rounded into one that
certifies. 118 assertions.

One documentation defect came out of it. `worst_arc` was described as the most
violated arc, with 0 meaning none; `certify_flow` sets it from the running
minimum over the residual graph in both directions, so it names an arc whenever
any arc can take or give up flow, and on a certified flow it is arc 1 at a
reduced cost of 0. The C++ header always said so and the R doc was the loose
one, so the doc changed. Suite after: 0 failed, 0 warnings, 3 skipped in R.

## The suite runs warning-free, and stays that way on purpose

The 28 warnings the suite used to emit were couplr's own diagnostics firing
correctly on deliberately small or degenerate fixtures. They are now explicit at
each call site, so a new warning means something new happened.

Two conventions, and which one to reach for:

- Tests whose subject is something else pass `check_costs = FALSE`. This is the
  documented public switch (`match_couples()`), it is result-neutral because
  `matching_core.R` discards the return of `check_cost_distribution()`, and it
  keeps a test named for `max_distance` from breaking when a diagnostic
  threshold is retuned. The diagnostics themselves are covered in
  `test-matching-messages.R`, so asserting them inline is duplicate coverage.
- Warnings with no switch behind them get `expect_warning()`. That covers
  "No valid pairs found after applying constraints" (a bare `warning()` in
  `matching_core.R`) and the pixel-morph "Assignment is not a permutation"
  (`Rcpp::warning()` in `src/morph/morph_pixel_level.cpp`, expected under
  `mode = "color_match"`).

`suppressWarnings()` is not used for this. It hides unrelated warnings too,
which is how 28 accumulated without anyone noticing.

## Issues

Closed by the 1.6 work:

- **#28** hardcoded `status = "optimal"`. Closed; the literals are gone and
  `verify_assignment()` is exported.
- **#16** network_simplex. Bug 2 (pivot-cap exhaustion tagged optimal) was real
  and is fixed. Bug 1 no longer reproduced, and had not since commit `ff8c58a`
  collapsed the duplicated pivot loop, on the same day the issue was written; a
  5000-trial sweep against unmodified HEAD showed 0 defects against the issue's
  reported 4-15%.
- **#20** full_match. Fixed: the guard now tests actual flow against required
  flow, and the R layer reads the status it was discarding.

Open, and confirmed open on GitHub:

- **#31** **`gabow_tarjan` is suboptimal on wide problems.** Found by the
  certification layer on its first randomized sweep. 179 of 200 random `m >= n`
  problems came back worse than jv, worst excess 121.9; all 200 square problems
  were fine. A 3 x 6 counterexample returns 20 against a brute-forced optimum
  of 8. The suite could not have caught it: the one rectangular test asserts
  `n_matched == 4` and nothing about cost.
- **#30** `cardinality_match()` is a balance-pruning heuristic under
  Zubizarreta's name. Needs a decision, implement or rename.
- **#29** `as_matchit()` labels every non-subclass design `estimand = "ATT"`.
  Roadmap section H.
- **#27** stale CHANGELOG and cran-comments. Still true, and now one version
  further behind.
- **#26** code-quality sweep, duplication across `match_couples`/`greedy_couples`.
- **#25** vectorize the interpreted cost-matrix loops.
- **#24** latent matching-layer bugs, `.pair_var_diffs` merge misalignment.
- **#22** Murty k-best branching is not true partitioning and can drop solutions.
- **#21** `forbidden` silently ignored on the `.couples_single` greedy path.
- **#18** auction solvers have no global infeasibility detection.
- **#17** `match_data` scrambles weights and subclasses for full matching + CEM.

## Facts that will bite anyone benchmarking

- **couplr lazy JV takes 104.33 s on beast, not the 68.1 s the paper reports.**
  The published timings came from the Mac mini (`~/dev/couplr-bench`), which is
  about 1.5x faster on this single-threaded workload. Re-measure before
  comparing anything to the paper's table.
- **couplr's Mahalanobis uses the pooled within-group covariance**
  (`R/matching_distance.R`), not `cov(rbind(left, right))`. Using the stacked
  covariance is a different metric with a different optimum.
- **Parallel test results from `devtools::load_all()` are not trustworthy.**
  future workers load the *installed* couplr, so `test-matching-parallel.R`
  compares new sequential code against old parallel code. Run
  `devtools::install()` first, and let exactly one install finish: a second one
  launched over an in-flight build fails with `cannot remove earlier
  installation, is it in use?`.

## Next action

**The derived status fixes.** The re-capture they were queued behind is done:
2026-08-15, on `37ebfa6`, 1537 cases, nothing nondeterministic on the two-pass
screen, and `--compare` against it now reports 0 differ, 0 missing, 0 added. The
compare that preceded the capture reported the 16 `full_match/*` cases B6
explains and nothing else, so the instrument was replaced from a state that had
been read rather than an assumed one.

Three things change a value B0 holds and can now be judged on it:

- the derived status bugs 1 to 3 in `dev_notes/phase2/findings.md`: a blocked
  match reporting `partial` where the same data unblocked reports `heuristic`,
  the parallel and sequential blocked branches returning different `info` with
  no `solver` on the parallel side, and a k:1 solve reporting `"optimal"` after
  placing a fraction of the requested pairs, because `n_matched` counts pairs
  while `unmatched` counts units;
- bug 9, the exception type and message that disagree. Not csflow's line: the
  same literal under `LAP_THROW_DIMENSION` is in ten solvers, so it is a
  package-wide decision about an R-visible message and its condition class;
- `drop_forbidden` on the 1:1 path, which reports a pair priced at half
  `.Machine$double.xmax` where the k:1 path drops it.

`dev_notes/phase2/findings.md` also leaves two interface questions open that no
deliverable covers: `map_assignment_duals()` reporting `ok = false` down two
paths with different postconditions, and `R/trace_helpers_mcf.R` stating the
same algorithm in R with its own potential-update rule, so a trace can show an
augmenting path the solver would not take.

Two things phase 1 leaves on the table, both cheap and both feeding phase 3:

- `assignment_duals()` computes a certificate's inputs and does not run the
  check. Wiring `certify = TRUE` there is an afternoon.
- The lazy path has no dual entry point, so lazy solves get
  `certificate = NULL`. Section C needs one anyway.

## Working tree

Clean apart from what this note is committed with. `dev_notes/` stays gitignored;
`roadmap.md` and `current.md` are tracked but `.Rbuildignore`d, so they stay out
of the tarball.

## File map

`dev_notes/phase1/`

| File | What |
|---|---|
| `design.md` | The spec phase 1 implements, with the LP and the four conditions |
| `findings.md` | Everything found that is outside phase 1's scope, and every deviation from the design |
| `repro_gabow_tarjan_rectangular.R` | #31, brute-forced |
| `repro_20.R` | #20 end to end through the public API |

`dev_notes/phase2/`

| File | What |
|---|---|
| `design.md` | The spec phase 2 implements, B0 through B9 |
| `findings.md` | Everything outside phase 2's scope, every deviation from the spec, and where B8 left each solver defect |
| `baseline.R`, `baseline.rds` | B0, the equivalence baseline. `--capture` and `--compare`; `--compare` exits non-zero on any difference |
| `baseline_additive_check.R` | Compares every case on the fields the baseline holds instead of stopping at the first difference, which is what separates an added field from a changed value |
| `b8_timing.R` | What routing four solvers through the flow model costs in wall time |

`dev_notes/pricing-probe/`

| File | What |
|---|---|
| `findings.md` | Full phase 0 write-up, all tables |
| `probe_core.cpp` | Sparse warm-startable SSP, exhaustive pricing oracle, ball tree, tree k-NN, CSR merge |
| `problems.R` | Problem generators, caliper calibration, adversarial cases |
| `run_loop_fns.R` | `edge_gen()`, the loop itself |
| `verify_pricing.R` | Correctness gates 1-4 |
| `dim_sweep.R` | Pruning vs covariate dimension |
| `timing_sweep.R` | Wall time at 1e8 pairs |
| `run_loop.R` | Loop across sizes and dimensions |
| `head_to_head.R` | Against couplr lazy JV on the paper's benchmark |
| `repro_duals.R`, `repro_estimand.R`, `debug_warm.R` | Issue repros and diagnostics |

Run any of them as
`Rscript <file>.R "C:/GillesC/documents/dev/couplr/dev_notes/pricing-probe"`.
