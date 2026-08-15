# couplr roadmap: certified matching without building the assignment graph

Target: one optimization core that compiles every matching design into a single
internal flow model, solves it without evaluating the full implicit edge set,
and returns a certificate proving the answer optimal for the complete problem.
No new solvers. The nineteen existing ones move into the engine room.

## The claim, stated precisely

couplr 1.5.6 already solves without materializing the distance matrix.
`src/core/lap_lazy_types.h` computes `C(i,j)` on demand from the feature rows,
and `resolve_memory_mode()` in `R/matching_memory.R` switches to it against
probed free RAM. The class comment states the case itself: 100k x 100k dense is
~80GB, the underlying feature matrices are ~80MB.

So "matrix-free" is not the contribution, and a reviewer reading NEWS will find
that out. Today's lazy path still evaluates every cell, it only declines to
store them. Every solver scan is O(n_t n_c) arithmetic.

The contribution is the move from *solve the whole problem* to *solve a
certified sliver of it*:

> Exact, certified full and variable-ratio matching that solves a problem
> holding a vanishing fraction of the implicit arcs, with dual prices proving
> the answer optimal for the complete problem.

Phase 0 measured this and corrected an earlier draft of this section, which
claimed the win came from *evaluating* few distances. At eight covariates the
loop evaluates more distances than one full sweep and is still 7x faster than
lazy JV, because the problem it hands the solver holds 0.03% of the arcs. Few
distances is a low-dimension bonus; few arcs is the mechanism.

The number that carries the paper is arcs retained, not `edges_evaluated` and
not peak memory.

## Two claims the package makes and does not compute

Both filed with repro after re-reading the sources; line numbers below are
verified against the working tree, not carried over from the earlier draft.

1. **Optimality** (#28). `status = "optimal"` is a string literal, not a
   result: `R/lap_solve.R:240` (dense), `R/lap_solve.R:323` (lazy),
   `R/lap_solve.R:1150` (`assignment_duals`). Nothing checks it. #16 and #20
   are two instances where the literal is actually wrong, on a suboptimal
   pivot-capped solve and on an infeasible full match.
2. **The estimand** (#29). `R/matching_interop.R:137` supplies
   `estimand = result$info$estimand %||% "ATT"`. `info$estimand` is assigned in
   exactly one place in the package, `R/matching_subclass.R:218`, so the
   fallback fires for every other design and the matchit object claims ATT
   regardless. MatchIt and marginaleffects read that field to pick the target
   population.

A third claim in the earlier draft, that the Hall-deficient path falls back to
`greedy_matching(sub, strategy = "sorted")` silently, is **wrong and has been
removed.** `R/matching_core.R:99-117` warns explicitly that the result is a
greedy partial matching and is not optimal, and records
`method_used = "greedy_sorted"`, which reaches `info$solver`. Nothing to fix
there. Workstream A drops the `"heuristic_fallback"` status it was going to
add for this path.

`paper/paper.md:109` tells readers the duals certify optimality. That sentence
is true in substance: `assignment_duals()` returns duals with a zero duality
gap, feasible signs, and `v_j = 0` on unmatched columns, measured on square and
rectangular problems up to 500 x 2000. What is missing is any code that checks
it. Edge generation makes the check load-bearing, because the certificate
becomes the only reason a sparse answer can be trusted.

Separately, `cardinality_match()` is a balance-pruning heuristic under
Zubizarreta's name (#30), which section F resolves.

## What already exists

Read before rebuilding.

| Piece | Location | State |
|---|---|---|
| `at()`/`allowed()`/`nrow`/`ncol` cost-source concept | solvers templated on it | exists |
| `LazyCostMatrix`, implicit source, 5 metrics, calipers, `max_distance` | `src/core/lap_lazy_types.h` | exists |
| `PaddedCostView`, zero-copy rect-to-square decorator over any source | `src/core/lap_cost_view.h` | exists |
| `supports_raw_mask` trait for hot loops needing `.mask` | `src/core/lap_cost_source_traits.h` | exists |
| Duals from a solve | `src/solvers/solve_jv_duals.cpp`, `assignment_duals()` | exists |
| Native full matching | `src/solvers/solve_full_matching.cpp`, `full_match()` | exists |
| `memory_mode` user-facing enum | `R/matching_memory.R`, `"auto"`/`"dense"`/`"lazy"` | exists |
| k-best enumeration | `lap_solve_kbest()`, Murty and Lawler | exists |
| `variable_ratio_match()`, `optimal_subset_match()` | not in NAMESPACE | new |

Three consequences for the design, the first two correcting an earlier draft of
this document:

- **Edge generation is not another cost source.** The existing sources answer
  `at(i, j)` and the solver still walks every `j` for each `i`. Edge generation
  needs a restricted master problem over an explicit sparse arc list, an outer
  pricing loop, and a solver that accepts newly added arcs against a warm
  start. The cost-source concept is reused by the *pricing oracle*, which needs
  `c_ij` on demand and nothing else. The *solver* is new work.
- **JV duals only cover the one-to-one prototype.** `solve_jv_duals` returns
  duals for the assignment LP, which is enough to probe whether pricing prunes.
  Full and variable-ratio matching need node potentials from a general
  min-cost-flow solver, so the flow representation in section B has to land
  before any full-matching claim is made.
- Lazy is wired only for `method = "jv"`/`"auction"` with a built-in metric;
  every other caller passes `solver_supports_lazy = FALSE`. `memory_mode =
  "implicit"` is still a new value on an existing argument, so the user-facing
  surface stays small.

## Phase 0. Pricing-bound probe: DONE, verdict GO

Ran 2026-08-10 on beast. Full write-up and every number in
`dev_notes/pricing-probe/findings.md`; code and CSVs in the same directory
(gitignored). Five of six go/no-go criteria pass. The sixth fails and
investigating why changed what the contribution actually is.

### Result on the paper's own benchmark

`paper/bench_common.R` generates the problem behind the published 68.1 s
figure: n_total = 50,000, treated:control 1:2, eight covariates, Mahalanobis,
no caliper. That is the p = 8 regime where the tree bound prunes worst, so it
is the unfavourable comparison rather than a chosen one.

| n_total | couplr lazy JV | edge generation | speedup | rounds | arcs retained | cost |
|---|---|---|---|---|---|---|
| 20,000 | 10.50 s | 2.08 s | 5.05x | 3 | 0.040% of pairs | exact |
| 50,000 | 104.33 s | 14.99 s | 6.96x | 3 | 0.030% of pairs | exact |

Cost agreement is exact to 15 digits and reproduces
`paper/rjournal/data/scaling-lazy-results.csv` to the last digit.

**Baseline correction: lazy JV takes 104.33 s on beast, not 68.1 s.** The
published figure came from the Mac mini, which is roughly 1.5x faster on this
single-threaded workload. Do not compare beast numbers to the paper's table
without re-measuring.

### Scorecard

| Criterion | Target | Result | |
|---|---|---|---|
| Exact agreement | every case | 41/41 gates, incl. exact vs couplr at both sizes | PASS |
| Pricing rounds | < 5-10 | max 4, median 1 | PASS |
| Edges retained | < 1-5% | max 0.80%, median 0.05%, 0.03% on the benchmark | PASS |
| Distances evaluated | < 5-10% | 0.04% (p=1), 2.6% (p=2), 6.8% (p=4), 152-198% (p=8) | FAIL at p>=8 |
| Wall time by n ~ 10,000 | clear improvement | 5.05x at 20k, 6.96x at 50k, rising with n | PASS |
| Loose-caliper / clustered | no collapse | fine; only genuine infeasibility fails | PASS |

### What changed in the understanding

The roadmap assumed the win came from evaluating a vanishing fraction of the
distances. At p = 8 that is false and the loop is fast regardless: it evaluates
**152% of the pair count** in distances, more than a full sweep, and is still
7x faster, because the restricted master it solves holds 0.03% of the arcs.
Lazy JV recomputes distances repeatedly while augmenting through the full
implicit graph; edge generation pays about one and a half sweeps and then
solves a problem three thousand times smaller.

**The claim is about the solved problem being tiny and certified, not about
touching few distances.** At p <= 4 both effects hold; at p >= 8 only sparsity
does. Section C and the paper text both need this framing.

Loop wall time against a one-full-sweep floor (a real dense baseline measured
3-4x the floor where both could be run):

| p | 1000x5000 | 2000x10000 | 4000x20000 | 8000x40000 | 16000x80000 |
|---|---|---|---|---|---|
| 1 | 1.00x | 4.17x | 4.50x | 11.91x | 44.25x |
| 2 | 0.67x | 2.21x | 3.00x | 6.24x | 12.27x |
| 4 | 0.42x | 2.57x | 1.52x | 2.60x | 3.11x |
| 8 | 0.55x | 1.03x | 0.41x | 0.33x | 0.42x |

Below roughly n = 10,000 the loop loses at every p, because a complete solve
there already costs hundredths of a second. The crossover is real and moves
with p. Anything the paper claims about small problems has to say so.

### Requirements the probe added

1. **Complementary slackness on unmatched columns is a correctness
   requirement, not a detail.** A verifier testing only dual feasibility and
   matched-arc tightness passes a wrong answer on rectangular problems. See
   section A and #28.
2. **Feasibility and optimality are two separate phases.** Reaching a feasible
   restricted master is a different task from pricing to optimality. The probe
   doubles k until feasible, which is a placeholder.
3. **A real infeasibility certificate is needed.** The `overlap` case is
   genuinely infeasible at every k up to 320, and the loop discovers that by
   exhausting the ladder. Production needs the deficient row/column cut.
4. **Seeding must not scan.** Brute-force k-NN seeding was up to 91% of loop
   time; an O(nr * nc) seed puts a floor under the loop that no downstream
   pruning lifts. k-NN runs on the same tree with the same branch-and-bound.
5. **The whitened tree needs two bounding structures per node**, confirmed
   necessary: a whitened centre and radius for the distance bound, and an
   axis-aligned box in the original covariates for the caliper bound, because
   whitening destroys axis alignment.

## A. Certification layer

Independent of every architectural decision below, and it closes all three
uncomputed claims. Runs on the existing nineteen solvers as they stand.

**Contract.** Add to the matching and assignment entry points:

```r
cardinality = c("complete", "maximum", "fixed")
n_matches = NULL
unmatched_penalty = NULL
```

Every result records requested cardinality, achieved cardinality, unmatched
units with a reason code, primal objective, dual objective, duality gap, and a
computed solver status, replacing the literal at `R/lap_solve.R:240` and
`:323`. Status values beyond `"optimal"`: at least `"partial"`, plus something
for the pivot-cap and infeasible paths in #16 and #20.

**Verification.**

```r
verify_assignment(result, cost)
#> primal_feasible          TRUE
#> dual_feasible            TRUE
#> complementary_slackness  TRUE
#> duality_gap              0
#> certified_optimal        TRUE
```

Checks: primal feasibility, capacity and supply conservation, dual feasibility,
complementary slackness, objective equality, checked arithmetic, deterministic
cost scaling.

Complementary slackness must assert `v_j = 0` on unmatched columns explicitly.
Dual feasibility plus tightness on matched arcs is **not** sufficient for a
rectangular problem: the phase-0 prototype produced a perfect, dual-feasible,
matched-arc-tight solution whose dual bound equalled the true optimum while its
primal cost was 0.4% high, because one freed column kept `v_j < 0`. A verifier
without that assertion passes the wrong answer.

**Infeasibility witness.** When a complete assignment is impossible, return the
deficient rows and columns as a cut. `R/matching_core.R:25-27` already computes
the trivially deficient ones as `row_ok`/`col_ok`; the Hall-deficient subset is
the extension.

**Done when:** no reachable path reports a status it did not compute,
`verify_assignment()` is exported and documented, and the differential harness
under section I runs in CI.

## B. One internal flow model

A single C++ representation every design compiles into:

```cpp
FlowProblem {
    nodes
    supplies                 // signed, per node
    arcs                     // implicit, sparse or dense
    lower_capacity
    upper_capacity
    costs                    // via the cost-source concept
    side_constraints
    warm_start               // flows + node prices
}
```

Public functions become compilers:

| Function | Internal formulation |
|---|---|
| `pair_match()` | Unit-capacity bipartite flow |
| `optimal_subset_match()` | k-cardinality flow |
| `variable_ratio_match()` | Capacitated bipartite flow |
| `full_match()` | Lower/upper-capacitated circulation |
| Matching with replacement | Control capacities > 1 |
| Exact matching | Independent networks per stratum |
| Fine balance | Category nodes (see F) |
| `cardinality_match()` | Flow plus global balance constraints (see F) |

Full matching admitting a network-flow formulation is established (Hansen and
Klopfer 2006). The contribution is making it native, implicit and certified
inside couplr rather than the formulation itself.

**Done when:** every existing design routes through `FlowProblem`, the solver
returns node potentials for every design rather than only for the one-to-one
case, and the dense path produces byte-identical results to 1.5.6 on the full
test suite.

## C. Implicit edge generation

The centerpiece. Everything else is support or extension.

1. Give every treated unit its nearest k admissible controls.
2. Solve that sparse flow problem.
3. Take the dual prices `u_i`, `v_j`.
4. Search omitted pairs for negative reduced cost,
   `cbar_ij = c_ij - u_i - v_j`.
5. Add only violating edges.
6. Warm-start and re-solve.
7. Repeat until no omitted edge prices out.

At termination the duals certify the sparse solution optimal for the complete
implicit problem. Exact adaptive edge generation, not approximate k-nearest
matching.

**Three components, none of which is a decorator over an existing solver.**

- *Restricted master.* A sparse arc-list min-cost-flow or assignment solver
  that accepts arcs added between solves and resumes from a warm start. This is
  the new solver work.
- *Pricing oracle.* Needs `c_ij` on demand and nothing else, so it consumes the
  existing cost-source concept directly.
- *Outer loop.* Convergence test, violator selection policy, and the
  certificate.

Duals for the one-to-one case come from `solve_jv_duals`. Full and
variable-ratio matching need node potentials from the general flow solver, so
section B gates the full-matching claim even though the probe does not need it.

**Two pricing implementations.**

*Block pricing, generic costs.* Scan cost blocks without storing them. Remains
O(n_t n_c) arithmetic at near-linear memory. This is the fallback and it always
works.

*Metric-tree pricing, Euclidean and Mahalanobis.* The whole design lives or
dies on one bound. Skipping a ball-tree subtree S over controls requires

```
min_{j in S} c_ij  >=  u_i + max_{j in S} v_j
```

Four consequences:

- `max_{j in S} v_j` changes on every dual update, so the tree needs a
  bottom-up refresh per pricing round. One O(n_c) pass, cheap against the scan
  it replaces.
- Mahalanobis is Euclidean after whitening by the Cholesky factor of
  `inv_cov_`, which `LazyCostMatrix` already holds. One tree serves both
  metrics.
- Calipers are axis-aligned per-variable boxes in the *original* covariates,
  checked in `allowed()` before distance is computed. Whitening destroys that
  axis alignment, so each node needs a second bounding structure: an
  axis-aligned box in the original covariates alongside the whitened centre and
  radius. With both present, tight calipers are the strongest case for pruning,
  and they are where matching users actually work.
- **No exhaustive final scan for metric costs.** If the tree can prove
  `min_j cbar_ij >= 0` by pruning, that proof *is* the certificate; there is no
  separate O(n_t n_c) pass. A complete scan is needed only for arbitrary
  unstructured cost functions, and as a test oracle for the tree.

Whether the bound prunes on realistic propensity and Mahalanobis geometry is
phase 0, and nothing here should be built before that answer exists.

**Public surface.**

```r
full_match(
  treatment,
  covariates,
  ratio       = c(1, 5),
  memory_mode = "implicit",
  certify     = TRUE
)

result$certificate
#> optimal:          TRUE
#> primal_feasible:  TRUE
#> dual_feasible:    TRUE
#> duality_gap:      0
#> candidate_edges:  1842231
#> possible_edges:   8400000000
#> edges_evaluated:  0.0031
```

**Done when:** edge generation returns a certified-optimal solution identical
to the dense solve on every problem small enough to run both, and
`edges_evaluated` is reported on every implicit solve.

## D. Warm-started design paths

Once flows and prices persist, expose the frontier:

```r
match_path(data, vary = "caliper",       values = seq(0.1, 1, by = 0.05))
match_path(data, vary = "maximum_ratio", values = 1:10)
```

Reporting per point: matched sample size, total distance, covariate balance,
largest matched-set ratio, runtime, certificate.

Sweep direction is a correctness requirement, not a preference. Ascending
caliper adds arcs; ascending ratio adds capacity. Both keep the previous flow
feasible, leaving only reduced-cost violations on the new arcs to repair, which
is the same pricing loop as section C. One mechanism, two features. Sweeping
the other way removes arcs, breaks feasibility and needs a repair phase.

**Done when:** a path over 20 caliper values costs materially less than 20
independent solves, measured, and every point on the path carries a
certificate.

## E. Hybrid solver

Public method `"hybrid"`. A constant-factor story, which is why it comes after
C and D.

1. **Auction/SSAP initialization.** Feasible flow plus useful node prices.
2. **Clean-room relaxation.** Node-wise dual ascent from the published
   Bertsekas-Tseng method. Read Bertsekas and Tseng (1988) and the RELAX-IV
   report before writing any of it. Implement from the papers; do not read or
   adapt their source. Keep the boundary explicit in the commit history.
3. **Partial augment-relabel cost scaling.** When relaxation stagnates, switch
   state rather than restart.
4. **Network-simplex or SSAP cleanup.** Degenerate cases, exact complementary
   slackness.
5. **Online strategy selection** on observed behaviour: discharge count,
   augmenting-path length, saturated-arc fraction, price-update frequency,
   degeneracy, progress per edge scan.

RELAX-IV itself pairs relaxation with auction initialization because pure
relaxation struggles with long augmenting paths. This generalizes that.

**Done when:** `"hybrid"` matches or beats the best fixed method on every class
in the benchmark grid, and the selection trace is inspectable.

## F. Balance, split by complexity class

The previous roadmap posed this as HiGHS-or-rename. That was the wrong fork.
The two cases have different complexity and belong in different places.

**Polynomial, into the flow engine.** Fine balance and refined covariate
balance are flow-representable through category nodes (Rosenbaum, Ross and
Silber 2007; Pimentel, Kelz, Silber and Rosenbaum 2015). These get a real
certificate at polynomial cost, and they cover most of what people want from
balance-constrained matching.

**NP-hard, experimental.** Arbitrary linear moment constraints in the
Zubizarreta (2012) sense need branch and bound. Anytime exact solver:

- maximize matched cardinality first, minimize distance second;
- dualize global mean-balance constraints;
- solve each relaxation with the flow engine;
- branch on unit inclusion or violated balance conditions;
- prune on Lagrangian bounds;
- return incumbent, global bound and gap at every interruption.

```r
cardinality_match(..., exact = TRUE, time_limit = 300)

#> Matched units:       1842
#> Best possible:       1842
#> Optimality gap:      0
#> Certified optimal

#> Matched units:       18416
#> Global upper bound:  18431
#> Cardinality gap:     15 units (0.081%)
```

The current `R/matching_cardinality.R:91-117` runs a full match, then deletes
the worst `batch_fraction` of pairs on the single worst-balanced variable until
the threshold is met. It never re-adds a pair, never maximizes cardinality, and
considers one variable per iteration. It gets deprecated because something
better replaces it, not renamed.

**Done when:** fine balance ships certified in the flow engine. The B&B stays
behind `exact = TRUE` and is labelled experimental until verified against a MIP
solver on every problem small enough to run both.

## G. Streaming inputs

**vectra goes in Suggests, gated at the call site. Never Imports, never
LinkingTo.**

The precedent is couplr's own: `LinkingTo: RcppEigen` with no `Eigen::` types in
55 sources broke Lluis Revilla's install (`llrs/experDesign#60`), and commit
`7542cc0` removing it is the strongest paragraph in the JOSS reply. vectra
imports tidyselect, rlang, libgeos and parallel, and carries
`LinkingTo: libgeos`. A geometry library in the build path of a matching
package is the same shape of claim that was just deleted. vectra is also at
0.11.9; coupling couplr's headline correctness claim to a pre-1.0 release cycle
is an avoidable CRAN risk.

The idea is right, aimed at the wrong array. Under edge generation the
n_t x n_c matrix never exists, so there is nothing there to stream. What is
held in RAM permanently is `left_` and `right_` inside `LazyCostMatrix`: two
full `std::vector<double>` copies of the feature matrices. At 100k x 100 that is
the 80MB the class comment cites. At 50M controls x 20 covariates it is 8GB,
held alongside the R-side original. That ceiling is linear, which is what
vectra is for.

Two roles, both activating only when the unit table does not fit:

1. **Batched cost source.** Pull control feature blocks from a `.vtr` file
   instead of holding `right_`. A fifth implementation of the same
   `at()`/`allowed()` concept, so no solver changes.
2. **Out-of-core block pricing, for arbitrary cost functions only.** An earlier
   draft of this document called the exhaustive O(n_t n_c) certification pass
   unavoidable and built this role on it. That is wrong: for Euclidean and
   Mahalanobis costs the metric tree certifies by pruning and no full scan
   happens. Block pricing survives as the fallback for user-supplied
   unstructured cost functions, where no geometric bound exists, and as the
   test oracle for the tree. Both are pure streaming reductions with no state
   beyond a running minimum, which is a columnar engine's best case, and both
   are narrower than the role first claimed here.

`requireNamespace()` with an install hint at both sites. The core stays
installable with Rcpp and nothing else.

**Done when:** a problem whose unit table exceeds RAM completes and certifies,
and couplr still installs and passes its full suite with vectra absent.

## H. Estimand-aware output

No home-grown `estimate_effect()`. Post-matching estimation depends on the
estimand, weights, replacement, subclasses and outcome model, and MatchIt
documents that territory at length.

```r
design_summary(m)
```

Reporting: focal population and nominal estimand, whether focal units were
discarded and how that moves the target population, matching weights, effective
sample sizes, balance before and after, replacement and duplicate use,
recommended clustering variable, and an explicit warning when the design no
longer identifies a clean ATT, ATC or ATE. Most of this is computing what
`R/matching_interop.R:137` currently assumes.

Then make `match_data()`, `as_matchit()` and marginaleffects interoperability
reliable, with one vignette covering 1:1, ratio, replacement and full matching
end to end.

Sampling weights affect propensity estimation, balance, matching weights and
inference, so they are not one argument. Out of scope unless C through E finish
early.

**Done when:** `design_summary()` covers every design couplr produces, and the
vignette runs four workflows through to a marginaleffects estimate.

## I. Verification and dispatch transparency

**Differential harness.** One randomized generator over sizes, aspect ratios,
sparsity, tie density and cost type, run across every solver in the
`assignment()` switch plus brute force plus oracles. Existing brute-force
comparisons are per-file with ad-hoc method vectors across 13 test files, so
`paper/paper.md:122-125` describes a uniform check that does not yet exist as
one harness.

Coverage: brute-force enumeration of every small design, property-based
randomized testing, row and column permutations, cost shifting and scaling,
disconnected and infeasible graphs, zero and negative and enormous costs,
duplicated units, degenerate distances, cross-solver agreement among all
internal engines, and dense-versus-implicit agreement wherever both run.

**Oracles, never dependencies.** LEMON via rlemon (0.2.1, BSL-1.0, `Imports:
Rcpp` only, so zero added footprint), RELAX-IV, HiGHS. Compare feasibility,
achieved cardinality, objective, dual bound and status. Do not compare pair
identity; tied optima legitimately differ. Development-only comparisons live in
`~/dev/couplr-bench`, outside the package.

**Dispatch.** `R/lap_solve.R:168-192` has five branches and can select five of
the nineteen solvers. Its comment cites benchmarks the repo does not store.

- Cheap, phase 1: `explain_dispatch(cost)` reporting selected method,
  triggering condition, chosen representation and alternatives.
  `lap_probe_cost_matrix()` already computes every input. Store the reason in
  every result.
- Expensive, later: benchmark grid over square and rectangular problems, aspect
  ratios, forbidden-edge fractions, continuous and integer and binary and tied
  costs, random versus Mahalanobis versus propensity structure,
  caliper-induced sparsity, dense versus lazy versus implicit. Generate
  thresholds from a stored registry instead of hand-coded constants. This
  becomes the training data for section E's online selection.

Decide what the paper says about solvers `auto` cannot reach: either the grid
promotes them, or the text describes them as reference implementations
reachable by name.

## Assignment-stability diagnostics

Kept from the previous roadmap, and cheaper now: the certificate tier is the
same reduced-cost machinery as section C's pricing loop.

```r
st <- assignment_stability(result, cost, k = 100, tolerance = 0.01)
autoplot(st)
```

- **Exact tier**, small problems, via `lap_solve_kbest()`: best-to-second-best
  gap, count of tied optima, pair-selection frequency across the top k, units
  whose partner changes, forced pairs, cost and balance range, per-unit partner
  entropy.
- **Certificate tier**, any size: for each unit, the reduced cost of its best
  alternative bounds how much worse switching would be. Near-zero means
  ambiguous. O(nm) and no enumeration.

Frame as optimization ambiguity, not statistical uncertainty. The question it
answers: is this pair determined by the distances, or one of many equally good
choices?

## Schedule

One month was wrong. Section E alone is a clean-room Bertsekas-Tseng
implementation, and section C is a research-grade component. First publishable
result at roughly week 8.

| Phase | Weeks | Work | State |
|---|---|---|---|
| 0 | 1 | Pricing-bound probe, feasibility report, go/no-go | **DONE, GO** |
| 1 | 2 | A: certification layer, real status values, `verify_assignment()`, LEMON oracle, `explain_dispatch()` (#28) | **DONE** |
| 2 | 3-4 | B: flow model, design compiler, node potentials from the flow solver | **DONE** |
| 3 | 5-8 | C: restricted master, pricing loop, metric-tree pricing, infeasibility certificate. D: warm starts and `match_path()` | |
| 4 | 9-12 | E: auction-initialized relaxation, partial augment-relabel, online selection. I: benchmark grid | |
| 5 | 13-16 | F: fine balance in the flow engine. G: vectra-backed batched source. H: `design_summary()` (#29) | |
| 6 | later | F: anytime cardinality B&B (#30), experimental until verified | |

Four to six months. A CRAN release plus win-builder is a week of its own and is
not counted above.

Phase 1 was deliberately placed after phase 0 despite being independent of it.
It is the safe, useful work, and doing safe work first is how a project spends
three months before discovering its central idea does not hold. Had phase 0
failed, phases A, H and I would still have been worth shipping as couplr 1.6
with the rest deleted. It did not fail, so the full road is live.

## The paper

`paper/rjournal/` gets rewritten after phase 3 produces numbers, not alongside
generating them.

> **couplr: Certified optimal matching without building the assignment graph**

Two revisions from the working title. "Matrix-free" went because it shipped in
1.5.6 and a title claiming it invites a reviewer to check NEWS. "Without
evaluating the distance set" went because phase 0 disproved it at eight
covariates: the loop evaluates more distances than one full sweep there and
wins anyway. What is never built is the graph. Revert if either reads worse.

Contributions:

1. a common compilation layer for statistical matching designs;
2. a hybrid native min-cost-flow optimizer;
3. exact dual-certified adaptive edge generation;
4. warm-started matching frontiers;
5. optional exact global-balance cardinality matching.

The nineteen solvers become the engine room. RELAX becomes one component.
LEMON and RELAX-IV become validation targets. couplr stops competing with
MatchIt on convenience or optmatch on full matching and starts competing with
general optimization libraries, while exploiting matching structure they cannot
see.

Verify every reference against CrossRef before it goes in the manuscript. The
citations named in this document are from memory: Hansen and Klopfer (2006,
JCGS) on full matching via network flows; Bertsekas and Tseng (1988, Operations
Research) on relaxation methods; Zubizarreta (2012, JASA) on cardinality
matching by mixed integer programming; Rosenbaum, Ross and Silber (2007, JASA)
on fine balance; Pimentel, Kelz, Silber and Rosenbaum (2015, JASA) on refined
covariate balance.

This targets The R Journal or JSS. It does not address the JOSS rejection,
which turned on demonstrated research use and still turns on the east-west
divide study reaching preprint.

## Deferred

Additional solvers. Home-grown causal estimators. More matching-design
wrappers. Decorative plotting. Sampling weights unless phases 1 to 3 finish
early. Reformulating existing designs as LEMON flows, which would make a
Suggests package load-bearing for core features and contradicts the
from-scratch design.
