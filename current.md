# couplr: current state

Updated 2026-08-15. Read this first, then `roadmap.md` for the plan,
`dev_notes/pricing-probe/findings.md` for phase 0's numbers, and
`dev_notes/phase1/` and `dev_notes/phase2/` for the certification layer's and
the flow model's design, findings and repros.

## Where things stand

**1.6.0 is released on git.** `DESCRIPTION` reads 1.6.0, the release commit is
`9c8fbe3`, and tag `v1.6.0` exists. `main` is nine commits ahead of
`origin/main`; see "Working tree" at the end. The phase 1 certification layer
went in as `7b3dd6e` and is no longer sitting in the working tree.

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
- **#31** `gabow_tarjan` suboptimal on rectangular problems. Closed against
  `7b2f84f`; see the section above for the cause and what else it turned up.

Filed by this work, all three cleanly separable and none of them blocking
phase 3:

- **#34** the animations restate the solver instead of reading it. The two
  divergences found on 2026-08-15 are fixed, but `trace_helpers_mcf.R` is still
  a second implementation of the SSP core and can drift again. The chunk: the
  flow model emits per-step state and the traces read it. `trace_cycle_cancel`,
  `trace_csa` and `trace_gabow_tarjan` are out of scope -- different algorithms,
  not an SSP step stream.
- **#33** `gabow_tarjan` squares a rectangular instance, so `n << m` is not
  runnable at its own shape. Either a measured size guard pointing at jv, or a
  formulation that keeps the rectangular shape by treating the identical dummy
  rows as one node. Not urgent: the method is opt-in and `"auto"` sends
  `m >= 3n` to `sap`.
- **#32** `push_relabel` is successive shortest paths with Johnson potentials,
  not Goldberg-Tarjan push-relabel. Three places in the repo already say so,
  including the message the animation shows the user. Same shape as #30:
  implement it, or rename and describe what is there.

Open, and confirmed open on GitHub:

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

## The derived status fixes are in

The B0 re-capture they were queued behind ran first, on `37ebfa6`, and the
compare that preceded it reported the 16 `full_match/*` cases B6 explains and
nothing else, so the instrument was replaced from a state that had been read.
Each bug was then re-read in the working tree, because B7 and B8 had moved every
line number `dev_notes/phase2/findings.md` recorded for them. All three were
still there.

- **Status is derived from placed pairs against requested pairs**, the left unit
  count times the ratio, instead of from unmatched left units. A k:1 design
  places pairs while `unmatched` counts units, so a unit holding one of its two
  requested partners made the whole match report `"optimal"`. The rule reduces
  to the old one at ratio 1 and covers the with-replacement design, which had
  the same defect.
- **The blocked path builds its `info` and its `block_summary` rows in one
  place**, `.blocked_info()` and `.block_summary_row()`, called by the
  sequential and the parallel branch alike. They used to build their own and
  disagreed on the field set, the column set, the column order, and whether a
  block with nothing to match got a row. `info$solver` is now one entry per
  block that ran a solve rather than the method that was requested, which is
  what carries a block's greedy fallback out to the status.

B0 after: 1537 cases, 6 differing, all `match_couples/*blocked*`, and the
field-by-field check names only the intended fields. `hall_deficient/blocked/auto`
moves from `partial`/`auto` to `heuristic`/`greedy_sorted`, which is what the
same data reports unblocked. Suite after: 0 failed, 0 warnings, 3 skipped, 6594
passed. The baseline is re-captured on the fixed behaviour.

`block_summary`'s columns and `info`'s field set are user-visible, so both owe a
NEWS entry at the next release; this repo writes NEWS at release rather than per
commit.

## Every reader drops a forbidden pair now

`drop_forbidden` is gone. The 1:1 and precomputed-distance readers reported a
pair whose cost is at or above `BIG_COST`, the k:1 and with-replacement readers
dropped one; all four drop, and the two units come back unmatched. The rest of
the package already read that cost as no edge, in `has_valid_pairs()`,
`count_valid_pairs()` and the row/col pruner, so reporting one put a pair in the
result at a price the same cost calls no pair at all, and `status` read
`optimal` off a matching that placed it.

The predicate was open-coded in five places with two spellings, two of them
missing the `is.finite()` half, which an `NA` distance could reach. It is
`.is_valid_cost()` in `R/matching_constraints.R` now and the five call sites
share it.

B0 could not see any of this: no case in the grid forced the optimum onto a
forbidden edge, so the re-captured 1537 reported 0 differ against the fix. Five
`match_couples/forbidden_edge/*` cases close that gap, one per reader, and the
baseline stands at 1542. Measured against the code before the fix, `one_to_one`
and `from_distance` differ and the other three are identical: `pairs` loses its
row, `unmatched` gains a unit on each side, `info$total_distance` falls from
`1e+308` to `0.1`, and `status` moves from `optimal` to `partial`.

This is user-visible too, so it joins the NEWS entry owed at the next release.

## Bug 9 is closed: the message moved, the type stayed

`cpp_tests` already asserts the split the exception type makes, one section per
solver: `DimensionException` for `n > m`, `InfeasibleException` for a valid shape
whose admissible edges still admit no perfect matching. So the type was right and
the `Infeasible:` prefix was the half that had to go.

The type does not reach R. Every Rcpp wrapper catches `lap::LapException` and
re-raises `e.what()`, so a caller sees `Rcpp::exception` either way and the
message is what carries the condition. It now reports the shape, the way
`bottleneck_assignment()`'s R guard already did: `solver requires nrow <= ncol;
got 3 rows and 2 columns`.

The literal was in 16 places, not the ten the note said, all testing `n > m`
under three spellings of the operands. All 16 are
`lap::require_rows_fit_cols(n, m)` now, defined once in `src/core/lap_error.h`,
which is also the only place the message exists.
`cpp_tests/tests/test_lap_error.cpp` pins the type and the exact string.

B0 never held this either, and could not: the exported surface transposes an
`n > m` problem or stops in R first, so the message reaches R only through the
internal `couplr:::lap_solve_*` wrappers. Second blind spot in the instrument
this week, after the forbidden edge. No baseline cases added, because the string
lives in one place and one `cpp_tests` case asserts it.

Suite after: 0 failed, 0 warnings, 3 skipped, 6614 passed in R, and 69257
assertions over 251 cases in `cpp_tests/`. B0: 1542 cases, 0 differ.

## The two interface questions are answered

Both are closed in the code rather than in a note, and
`dev_notes/phase2/findings.md` records each where it was raised.

**`map_assignment_duals()` says which way it failed.** `AssignmentMapStatus` has
three values and each documents what it leaves behind: `Ok` and
`NotAnAssignment` populate `match`, `u` and `v`, `StructuralMismatch` returns
before anything is read and leaves them empty. That last distinction is the one
a `bool` could not carry, and it is what a caller checking `ok` and continuing
would have indexed into. `ok()` is a method over `status`, not a second field.

**The trace shifts potentials the way the solver does.** The divergence was
real and it was in two places, not one. At each augmentation the solver shifts
unreached nodes by the largest distance label and the trace shifted only what
Dijkstra had labelled; and the Bellman-Ford initialization,
`ifelse(is.finite(bf$dist), bf$dist, 0)`, had the same defect one step earlier,
so the very first search started dual infeasible. Both were invisible because
`mcf_dijkstra()`'s `max(rc, 0)` clamp -- there for floating-point drift -- was
absorbing a real infeasibility. The rule is `mcf_update_potentials()` now,
called at both points.

The trace layer keeps existing: animating a solver needs per-step state a solver
has no reason to produce. What does not keep existing is the drift, and #34
tracks removing the second implementation by having the flow model emit that
state.

## The two phase-1 leftovers are in

**`assignment_duals(certify = TRUE)`** runs the check and attaches an
`assignment_certificate`. `verify_assignment()` reads the duals off the result
instead of solving again, so it costs one pass over the admissible pairs.
Default is `FALSE`, so nothing about the returned fields changes unless it is
asked for.

**The lazy cost source has a dual entry point.** `detail::jv_core` was already
instantiated for `LazyCostMatrix` and already producing `u` and `v`, so what was
missing was a way out: `lap::solve_jv_duals(const LazyCostMatrix&)` and the Rcpp
wrapper beside the dense one. `assignment_duals()` takes a `lazy_cost_spec` and
returns duals identical to the dense path's to the last bit; `.certify_lazy()`
derives its own instead of refusing, and transposes an `n_left > n_right`
specification rather than erroring. This is what section C's pricing oracle
needs, so it is phase 3 groundwork and not only a leftover.

`solve_jv()` and `solve_jv_duals()` were the same twenty-line body three times
over. One templated body now, and `solve_jv()` is `solve_jv_duals()` with the
potentials dropped.

## #31 is fixed, and it was not an implementation bug

Gabow-Tarjan's 1-optimality bound compares the matching found against an optimal
one through the duals, and the column terms cancel only when both use every
column. A rectangular instance breaks that, so the `n`-slack the `(n+1)` scaling
relies on never applies. The code was correct for what it proves; the instance
was outside it.

`solve_gabow_tarjan_inner()` squares the instance with zero-cost dummies before
the first scale, so the matching carried across scales and the duals with it
live on a problem the bound covers. `scale_match()`'s own `n > m` padding is
gone: it padded per scale, on `cost_prime`, which is the same argument failing
one level down.

**Proving it before touching the test turned up a second bug.** On the tall side
`row_match` came back holding the padding column's index. Measured against the
code before the fix -- `git stash` on the one file, rebuild, run -- a 4 x 3
instance returned `match = 4 1 3 2`, column 4 in a three-column matrix, at cost
24 against an optimum of 15. `test-gabow_tarjan_solver.R`'s `n_matched == 4` was
pinning that phantom. The test asserts cardinality, in-range and
claimed-once columns, and cost now.

B0 moved on 34 of 1542 cases, all `gabow_tarjan` and all rectangular. Every
minimization fell and every maximization rose; the six `constant` cases changed
only which optimum they name. Re-captured, and `--compare` is clean against it.

`cert_known_suboptimal()` is empty, so the certification sweep covers
`gabow_tarjan` on every shape again.

Suite after: 0 failed, 0 errors, 0 warnings, 3 skipped, 6700 passed in R, and
69265 assertions over 251 cases in `cpp_tests/`.

## Next action

Phase 3, roadmap sections C and D, which is the centerpiece of the 1.6 line.
Nothing is queued in front of it: the interface questions are answered, the
phase-1 leftovers are in, and the three follow-ups this work surfaced are filed
rather than half-done.

Section C's restricted master is the new solver work, and it is where to start.
The pricing oracle's two prerequisites are both in the tree now: the lazy dual
entry point above, and `scan_reduced_costs()` from phase 1.

## Working tree

Clean apart from what this note is committed with.

**`origin/main` is at `37ebfa6`, nine commits behind local `main`.** Five are
this session's -- `8585ba5`, `e351f2c`, `66ecc9a`, `7b2f84f` and this note --
and four predate it: `d345dea`, `a5aa182`, `fd5d00d`, `46460a5`. So the gap is
not new, and the push is larger than one session's work.

#31 is closed on GitHub against `7b2f84f`, which is not on the remote yet, so
the push is the one thing outstanding.

```
GIT_SSH_COMMAND="ssh -i ~/.ssh/id_ed25519_gcol33" git push origin main
```

`dev_notes/` stays gitignored; `roadmap.md` and `current.md` are tracked but
`.Rbuildignore`d, so they stay out of the tarball.

## What the next release owes NEWS

This repo writes NEWS at release rather than per commit. Owed so far, all
user-visible:

- `block_summary`'s columns and `info`'s field set, from the derived-status
  fixes.
- Every reader dropping a forbidden pair, from the `drop_forbidden` removal.
- `gabow_tarjan` returning the optimum on rectangular problems, and reporting
  an unmatched row as unmatched rather than as a padding column (#31).
- `assignment_duals()` gaining `certify` and accepting a lazy cost
  specification; `verify_assignment()` no longer requiring `duals` for one.

## File map

`dev_notes/phase1/`

| File | What |
|---|---|
| `design.md` | The spec phase 1 implements, with the LP and the four conditions |
| `findings.md` | Everything found that is outside phase 1's scope, and every deviation from the design |
| `repro_gabow_tarjan_rectangular.R` | #31, brute-forced. Reports 0 worse now; kept as the regression check |
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
