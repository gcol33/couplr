# couplr: current state

Updated 2026-08-16. Read this first, then `roadmap.md` for the plan,
`dev_notes/pricing-probe/findings.md` for phase 0's numbers, and
`dev_notes/phase1/` and `dev_notes/phase2/` for the certification layer's and
the flow model's design, findings and repros.

## Where things stand

**1.6.0 is released on git.** `DESCRIPTION` reads 1.6.0, the release commit is
`9c8fbe3`, and tag `v1.6.0` exists. `main` and `origin/main` are both at
`ccafc1b`; see "Working tree" at the end. The phase 1 certification layer went in
as `7b3dd6e` and is no longer sitting in the working tree.

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

## C0 is in, and it found a hang in the flow search

`dev_notes/phase3/differential.R` is the phase 3 instrument. It runs one
matching problem through two paths in the same session and compares the answers
to each other, which is what B0 cannot do: B0 compares HEAD against a snapshot
of HEAD, and the restricted master reaches its optimum through a different arc
set in a different order, so it names a different optimum whenever the optimum
is not unique and B0 would call that a regression.

Condition 1 is asserted as equal total cost at 17 digits, a structurally valid
matching, and a certificate reporting optimal; the match vector is compared only
where the optimum is proven unique. The proof is by edge exclusion, not by the
downward perturbation `design.md` proposed, which can never fail --
`dev_notes/phase3/findings.md` has the argument. Cost is scored against a cost
matrix the harness rebuilds itself, so a path that solves correctly and reports
the total wrongly fails here.

`--self-check` runs the grid with `lazy` as the candidate, which calibrates the
comparator against a path already in the tree. 38 cases in 2.4 s: 26 pass, 0
fail, 7 known, 5 skipped. `KNOWN_DIFFERENCES` carries the seven, each with the
code that decides it, and fails if a listed difference stops reproducing.

Its first full run found a hang, fixed in `ed1ae7f`. `solve_min_cost_flow()`
never returned on a 40 x 134 sentinel-padded matrix and grew to about 16 GB. The
reinserting Dijkstra depends on `cbar >= 0` for termination, and the two
directions of one arc are priced by expressions that are negatives of each other
in exact arithmetic and not in floating point, so both can round below zero at
once. Measured worst violation: one to two ulps. Ties are what make it
reachable, and a sentinel-padded matrix is mostly one value. A 4 x 7 instance
reproduces it and is now in `tests/testthat/test-assignment-ssp.R`.

This is squarely phase 3's business rather than beside it: sections C and D are
`solve_min_cost_flow()` on a partially expanded problem, warm started, and a
warm start is one of the two places the invariant is not free.

## C1 was measured and turned down

The touched-list reset was written, checked, measured and dropped.
`src/flow/flow_solve.cpp` is unchanged. It is 8% to 19% slower on all four
shapes it was run on, and the two reasons matter more than the change did.

The premise was that clearing `dist`, `pv` and `pe` in full costs
O(augmentations * n_nodes) whatever the arc set holds, so on a restricted master
holding a fraction of a percent of its pairs the clearing would dominate. A
restricted arc set does not give a small labelled set. The search drains its
queue instead of stopping when the auxiliary sink is popped, so it labels
everything the residual graph reaches from the auxiliary source: 83.6% of the
nodes on a candidate-set problem carrying 0.08% of its pairs, 99.9% on the two
complete ones. The list the reset walks is the array it replaced, and walking it
is scattered writes where `std::fill` was three linear passes.

The prize was also small enough to have settled it first, which is the part
worth carrying forward. Three fills of 12,003 entries over 2,000 augmentations
is 7.2e7 writes, one to two percent of a 3.36 s solve. No change to the clearing
can return more than that.

An early exit at the sink does not recover it either. At the moment the sink
first tops the queue the labelled set is already complete on all three shapes,
so an early exit changes what is popped, not what is labelled. It would save
about a quarter of the pops, which is heap work and its own measurement.

What phase 3 keeps is the instrument. `dev_notes/phase3/c1_timing.R` runs two
families -- `b8_timing.R`'s three complete shapes through `assignment()`, and a
candidate-set flow problem built directly, which is the first thing in this
phase to put the restricted master's shape through the shared solver --
and `c1_ab.sh` runs a working-tree `flow_solve.cpp` against HEAD's on installed
builds. `dev_notes/phase3/findings.md` holds the tables; `design.md` is
corrected in place.

Suite after: 0 failed, 0 errors, 0 warnings, 3 skipped, 6702 passed.

## B0 has carried six differences since ed1ae7f

Found while establishing C1's control, and unrelated to it. `baseline.R
--compare` reports 6 differ at HEAD with a clean tree. Bisected: with
`src/flow/flow_solve.cpp` at `25a56dd` and everything else at HEAD, B0 is 0
differ, so the six are the clamp's.

All six are `full_match/*`, all in `value$potentials`, all one ulp. No match
vector, cost or status field moved, which is what the clamp should do: it
changes the arithmetic that produces the duals, and the duals are not unique.
The baseline was captured at `46460a53` and never re-captured.

Left as it is, because it is a decision about the instrument rather than a
repair: re-capturing makes B0 clean and gives up the record that the clamp moved
these six, and the alternative is a documented known-difference list of the kind
`differential.R` already carries. Until it is settled, 6 is the number that
means nothing changed.

## C2 is in, and C3's object came with it

A block can be expanded over a candidate set instead of over its whole grid, and
a block that is already expanded can be grown. That is the whole of section C2,
and it is what the restricted master was missing: the engine, the warm start and
the warm-start repair were all already there.

- `expand_block_subset()` emits arcs for the candidate pairs only, through the
  same two gates the full expansion uses, and records each arc's `(i, j)` the
  same way, so `flow_assign.cpp`, `map_assignment_duals()` and the Rcpp reader
  work unchanged on a restricted problem.
- `add_block_arcs()` grows an expanded block and moves `warm_flow` in step, each
  new arc entering at its lower bound. No new repair logic: an arc the incumbent
  potentials price below zero is pushed to its upper bound by the slackness pass
  already in `solve_min_cost_flow()`, and the augmentation loop repairs the
  conservation that breaks.
- `expand_blocks()` resumes from the blocks already expanded rather than
  clearing. Left as it was, subset-expanding one block and handing the problem
  to the solver would have cleared the ranges and re-expanded everything in
  full, with the subset arcs orphaned in the arc array -- wrong quietly rather
  than loudly.

The signature `expand_block_subset(prob, block, const CandidateSet&)` is C2's
own, so C3's object is in too: `src/flow/flow_candidates.h`, a per-row sorted
CSR with `contains()`, `add_pairs()` and the `edges_evaluated` counter. What C3
still owes is callers -- nothing prices yet, and section D has nothing to sweep.

Two things the spec said that measuring or reading changed:

- **The per-row `add_sorted(i, cols)` is one bulk `add_pairs(pairs)`.** A CSR
  insert shifts everything behind it, so a per-row insert repeated over one
  pricing round's rows is O(nrow * n_arcs), which is 50,000 rows against 1.8M
  arcs at the largest phase 0 shape. One rebuild pass is
  O(nrow + n_arcs + k log k), which is what `probe_csr_add` did.
- **Added arcs land at the end of their block, not at the end of the arc
  array.** Every reader maps arc `first_arc + k` to `rc[k]`, so appending behind
  a later block's arcs would break that mapping for both blocks. Later blocks'
  ranges move up instead, and `warm_flow` is inserted at the same offset. For
  the single-block problem section C runs, the two positions are the same.

**A candidate is not an arc.** A pair the cost source forbids stays in the
candidate set and gets no arc, which is what stops a pricer offering the same
forbidden pair every round, and it means `cand.n_arcs()` bounds a block's arc
count rather than equalling it. Both cpp_tests cases that add pairs assert the
gap.

The correctness claim is asserted where it can be: a restricted master's answer
equals the dense solve on the same costs with every non-candidate forbidden, and
a candidate set grown to the whole grid on a warm start reaches the cold dense
optimum, match vector included. That is condition 1 in miniature, on the shapes
`differential.R` will run at scale.

Suite after: 0 failed, 0 warnings, 3 skipped, 6702 passed in R, and 74413
assertions over 262 cases in `cpp_tests/`, up from 69265 over 251. B0: 1542
cases, 6 differ, and the six are the clamp's -- the same `full_match/*`
potentials, to the same ulp, as before this work.

## C4 is in, and the blocking it was specced with is not

`src/flow/flow_pricing.h`. `price_block(src, u, v, cand, keep_per_row, tol)`
walks every pair the candidate set omits and returns the `keep_per_row` most
negative reduced costs per row, the per-row minimum, and the counts
`edges_evaluated` is computed from. It is the pricer that works for any cost
function, and it is the oracle every pruned result in C6 and C7 gets asserted
against. Templated on the concrete source, not reached through `CostOracle`: the
master pays one virtual call per arc at expansion time and can afford it, this
loop runs over every omitted pair and cannot.

Only omitted pairs are priced. A candidate pair that became an arc is priced at
or above zero by the master's own optimality, and a candidate pair the source
forbids is not admissible, so neither can be a violator.

Three things measuring or reading changed:

- **The column blocking is gone.** The spec wanted the scan blocked column-wise
  to keep memory near-linear; a source is read one pair at a time, so nothing is
  stored in either order. Rewritten as a traffic argument and measured at five
  widths on four shapes, the widths are equal to the millisecond. The scan is
  row-outer and the width argument is gone. This is C1's shape a second time.
- **A dimension mismatch throws.** `scan_reduced_costs()` returns an empty scan
  on one, which is right for a checker. An empty pricing result reads as
  "nothing prices below zero", which is C8's signal to stop and call the answer
  optimal, so here it throws.
- **`row_min` is returned per row.** A tree that drops one row's violators is
  invisible in a global minimum, so the per-row vector is what C7 is held to.

The correctness claim is asserted against an independent implementation: thirty
random shapes with coarse costs so ties are common, a random fraction of pairs
forbidden and a random fraction already candidates, compared against the obvious
double loop with a `std::set` per row and no heap. Each row's kept reduced costs
are asserted to be that row's smallest, which survives a tie without naming which
of the tied columns was kept.

`cpp_tests`: 76265 assertions over 274 cases, up from 74413 over 262. B0 and the
R suite are untouched -- nothing outside the new header changed, and it has no
caller until C8.

### An unpinned timing on this machine is worth nothing

The same call on the same data in the same process times at 0.088 s or 0.183 s
depending on nothing the code does. Under `SetThreadAffinityMask` to one core and
`HIGH_PRIORITY_CLASS` every number above reproduces to the millisecond. C1's
numbers came from separate installed builds through `c1_ab.sh` with two runs
agreeing to a hundredth, so this is not a statement about them; it is a statement
about what D3's twenty-caliper measurement has to do.

## A pair is one question now, and a lazy solve reads it once

`src/core/lap_cost_source.h`. Not a phase 3 section -- it is the fix for what C4
measured, and it touches a type every solver reads.

`LazyCostMatrix::at()` called `allowed()`, and `allowed()` computes a distance
whenever `max_distance` is finite, so a caller asking `allowed()` then `at()`
computed the distance three times per admitted pair. That is `price_block()`,
`scan_reduced_costs()` since phase 1, `make_block_arc()`, all four `jv_core()`
loops and the lazy auction's dummy-cost scan.

- `LazyCostMatrix::admissible(i, j, cost)` runs the calipers, takes one distance
  and answers both questions. `at()` is composed from the same two private
  helpers rather than routed through it -- an out-parameter and a bool cost 12%
  on an unconstrained source, and `at()` alone is what the auction's bidding loop
  and the JV augmentation read through. `allowed()` still computes no distance
  when `max_distance` is infinite, so `hall_witness()` and `build_allowed()` are
  not charged for a cost they never read.
- `cost_if_allowed(src, i, j, cost)` is the one call a templated caller makes. A
  source opts in by exposing `admissible()`; everything else falls back to the
  two-call form the concept already guarantees. `PaddedCostView` forwards it and
  `CostOracle` carries it as a virtual with the fallback as its default body, so
  a padded or decorated lazy source keeps the path and an expansion makes half
  the virtual calls.

Worth, pinned, best of five, 2,000 x 10,000 at p = 8: a pricing round on a
`max_distance` problem goes 0.167 s to 0.121 s, and 0.088 s to 0.076 s
unconstrained. Reading a pair with `at()` alone is halved on a `max_distance`
source, 0.126 s to 0.064 s, which is the number that reaches the shipped lazy
solvers with none of them edited.

The 2x first read off the caliper comparison was not the right ceiling. A caliper
on one variable evaluates a distance only for the fifth of pairs it admits;
`max_distance` has to measure every pair to know which those are. What was
recoverable was the duplication, and that is the 1.38x above. Closing the rest is
C6's tree, which prunes subtrees out of the scan rather than making a pair
cheaper.

Judged by: `cpp_tests` 76265 assertions over 274 cases unchanged; R suite 0
failed, 0 warnings, 3 skipped, 6702 passed, unchanged; B0 1542 cases with the
same 6 clamp differences and nothing new; C0 26 pass, 0 fail, 7 known, 5 skipped,
unchanged, and running in 1.1 s against the 2.4 s on record.

## C5 is in, and the witness had to learn where the pairs are

`src/flow/flow_feasibility.h`. One round is: match over the restricted arc set,
and if that is row-perfect the block is feasible. Otherwise Hall's witness names
the deficient rows S and the columns N(S) they reach, one scan of the full source
over those rows keeps each of them the `width` cheapest admissible columns
outside N(S), and finding none of those anywhere is the infeasibility
certificate, re-checked against the full source before it is reported. The
prototype's doubling ladder is gone.

Three things measuring or reading changed:

- **The witness walks edges now.** Phase 1's `hall_witness()` reads every column
  of a row once per search phase, so asking it about a restricted arc set costs
  the grid the restriction exists to avoid. `for_each_allowed()` in
  `src/core/lap_neighbours.h` lets a source name an ascending superset of a
  row's admissible columns; `lap_hall.h`'s four column loops go through it, and
  a source that names nothing keeps the grid scan in the same column order.
  Measured on the same graph both ways: 142x at 250 x 1,250, 600x at
  1,000 x 5,000, same matching cardinality.
- **The re-seed takes columns outside N(S) only.** A matching of S is bounded by
  |N(S)| however many arcs into N(S) are added, so those are the only columns
  that can move the deficiency. It also collapses the section's steps 2 and 3
  into one scan: the scan looking for columns to seed with is the scan that
  establishes there are none.
- **The rebuild C3 parked costs 0.3% of a round.** C3 left the measurement for
  when C5 existed. At 2,000 x 10,000 the round is 0.0645 s, of which the witness
  is 0.0006 and the `add_pairs()` rebuild is 0.0002; the scan over the full
  source is the rest, at the same per-pair cost as a C4 pricing round. There is
  no second insertion path to write.

`src/flow/flow_topk.h` holds the one flat per-row heap the pricer and the
re-seed both keep their k smallest keys in.

Judged by: `cpp_tests` 77215 assertions over 285 cases, up from 76265 over 274;
R suite 0 failed, 0 warnings, 3 skipped, 6702 passed, unchanged; B0 1542 cases
with the same 6 clamp differences and nothing new; C0 26 pass, 0 fail, 7 known,
5 skipped in 1.1 s, unchanged.

## C8 is in, and the loop closes

`src/flow/flow_implicit.h`. `solve_implicit_assignment(src, prob, cand, opts)`
takes a compiled one-block assignment and the candidate set it may use, and runs
the roadmap's seven steps: expand the block over the candidates, solve the
restricted master, read its potentials as assignment duals, price the pairs the
master omits, add the ones that price below `-tol`, warm start, repeat until
none does. A master that comes back short of its required flow hands over to
C5's round instead, and an infeasible answer carries Hall's witness. C2 through
C5 each had their caller here and nowhere else.

Measured against the same source solved dense, pinned to one core, Euclidean at
p = 8 with `width` and `keep_per_row` at 5:

| shape | rounds | candidate pairs | of possible | loop | dense | speedup |
|---|---|---|---|---|---|---|
| 200 x 2,000 | 2 | 1,000 | 0.250% | 0.017 s | 0.363 s | 21.7x |
| 500 x 5,000 | 2 | 2,500 | 0.100% | 0.109 s | 4.867 s | 44.7x |
| 1,000 x 20,000 | 2 | 5,000 | 0.025% | 0.557 s | not run | |

Equal cost to 17 digits on both shapes the dense solve can run. Two rounds on
all three, because the seed the feasibility phase lays down is already optimal
and the pricing round proves it rather than repairing it.

### A warm-started master's duals are not the assignment's

The first run returned the dense solve's answer to the last digit and a
certificate reporting `dual_feasible = false`, at
`min_reduced_cost = -0.086` and `max_matched_slack = 0.086` -- the same number
twice, so the pair pricing below zero was a matched one.

The flow model gives a block arc the upper bound of one unit that the row's own
supply already implies, and min-cost flow optimality reads that bound: an arc at
its upper bound is optimal priced at or below zero. The assignment LP has no
such bound, and asks `u_i + v_j <= c_ij` on every admissible pair, matched pairs
included. Cold, the question does not arise -- a matched arc entered the flow on
a shortest path at reduced cost zero, and a row node is reachable in the
residual graph only through the column it is matched to, so it stays tight. A
warm start reaches the other state on purpose: the slackness repair pushes a
newly added arc, chosen by the pricer precisely because it prices below zero,
straight to its upper bound.

`tighten_matched_duals()` sets `u_i := c_i,match(i) - v_match(i)`, one cost read
per row. A matched arc prices at or below zero, so this only lowers `u_i`, and
lowering `u_i` raises every reduced cost in row i: a feasible dual point stays
feasible, the matched pairs become tight, and the dual objective meets the
primal. It is the row-side twin of the `v_j <= 0` clamp `map_assignment_duals()`
already applies for the sink arc's bound, and it stays in the loop rather than
moving beside the clamp, because only a warm start reaches the state it
addresses. `ImplicitRound::matched_slack` records the condition per round, and
`cpp_tests` asserts it is above tolerance on a warm-started run and at rounding
on a master solved in one round over every pair.

### The certificate is two scans, and neither is a sweep of the grid

Termination is C4's pricing round, which is the scan over the omitted pairs. The
other half is the pairs the master holds, and that walk costs the candidate set:
`CandidateGraph` gained `at()` and `admissible()` so the restricted problem is a
cost source, and `scan_reduced_costs()` reads a row through
`for_each_admissible()`, the cost-carrying twin of C5's `for_each_allowed()` and
built on the same range-or-grid dispatch. `certify_assignment()` gained an
overload taking a scan the caller already holds, and `merge_scans()` reads two
scans over disjoint pairs as one over their union, so the conclusion stays in
one body.

### What the loop does not save is the distances

`edges_evaluated` comes to exactly 2.00x the grid on all three Euclidean shapes:
the feasibility scan reads every pair once to seed, the pricing round reads every
omitted pair once to prove there is nothing left, and the certificate's walk of
the candidate set is the rest. A dense solve reads each pair once. What edge
generation saves at these shapes is the solve and the arcs, not the arithmetic.

So the surface's `edges_evaluated: 0.0031` is not something C4's block pricing
delivers, and nothing in C2 through C5 was going to. That is the number C6's
tree and C7's bound have to move, and it is the measurement they were told to
wait for.

### An infeasible caliper is answered with a witness where dense says partial

A `max_distance` of 2.0 on 8 standard normal covariates leaves six of 500 rows
with no admissible column. The loop returns `infeasible` with |S| = 6,
|N(S)| = 0 and the witness re-checked against the full source; the dense solve
returns `partial` with 494 of 500 units and a total cost for a matching of a
different size. They agree on the deficiency and one of them says why.

Judged by: `cpp_tests` 77865 assertions over 298 cases, up from 77215 over 285,
with forty random shapes run from an empty candidate set and asserted against
the dense solve of the same source; R suite 0 failed, 0 warnings, 3 skipped,
6702 passed, unchanged; B0 1542 cases with the same 6 clamp differences and
nothing new; C0 38 cases with the same 7 known differences, exit 0.

## Next action

Phase 3, section C9: the public surface. `"implicit"` is a fourth value in
`resolve_memory_mode()`'s vocabulary rather than a new argument, `certify = TRUE`
is its default, and `"auto"` does not select it until a dispatch rule with
measurements behind it exists.

It is the next thing rather than C6 because the loop currently has no caller
outside `cpp_tests`. C0 compares two paths in one session, which is how the
roadmap's condition 1 is answered on the shapes the package actually runs, and
it cannot see a path that R cannot reach. Forty random shapes against the dense
solve is what `cpp_tests` can say; the differential harness at scale is what the
section is done by.

C6 and C7 now have the measurement they were told to wait for, and it is not an
arc count: `edges_evaluated` is 2.00x the grid on every Euclidean shape the loop
was run on, because the feasibility scan and the pricing round each read every
pair once. The tree's job is that number, and the reasoning C1 was turned down
for still applies to the rest of what those two sections claim.

## Working tree

Clean apart from what this note is committed with.

Local `main` is at `fc500f7`, C8. `origin/main` is at `42217f3` and one behind.

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
- A lazy solve under `max_distance` evaluating each distance once instead of
  three times. Reading a pair is halved; on the shapes measured a whole pricing
  pass is 1.38x. Affects every `memory_mode = "lazy"` path and certification
  over a lazy source.

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

`dev_notes/phase3/`

| File | What |
|---|---|
| `design.md` | The spec phase 3 implements, C0 through D, corrected in place where findings disagree |
| `findings.md` | Everything outside phase 3's scope, every deviation from the spec, and the measurements that turned C1 and C4's blocking down |
| `differential.R` | C0. Runs one problem through two paths in the same session and compares them to each other. `--self-check` calibrates the comparator against `lazy` |
| `c1_timing.R` | Two families: `b8_timing.R`'s complete shapes through `assignment()`, and a candidate-set flow problem built directly, which is the restricted master's shape |
| `c1_ab.sh` | Runs `c1_timing.R` against installed builds of a working-tree `flow_solve.cpp` and HEAD's |
| `c4_timing.cpp` | What one pricing round costs, what the violator heap and a `max_distance` caliper cost inside it, and what each of the three ways of reading a pair costs. Pins to one core, which is what makes a timing on this machine repeatable |
| `c4_timing.log` | The current numbers from it |
| `c4_blocking_ab.log` | The column-blocking A/B that turned the blocking down |
| `c4_maxdist_before.log` | The same harness against `lap_lazy_types.h` at HEAD, which is the before column for the one-question change |
| `c5_timing.cpp` | What a feasibility round costs, split into the witness, the scan and the candidate-set rebuild, and the same witness walked over a graph's edges against over its grid |
| `c5_timing.log` | The current numbers from it |
| `c8_trace.cpp` | The loop end to end: the per-round table, what the candidate set retains, what a cost was computed for, and the same source solved dense beside it |
| `c8_trace.log` | The current numbers from it |

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
