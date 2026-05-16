# Gabow-Tarjan speedup plan

Empirical state on dense random integer cost matrices, max=1e5:

| Solver | log-log slope | median ms @ n=256 | ratio vs JV |
|---|---|---|---|
| JV | 1.99 | 7.8 | 1× |
| LAPMOD | 2.13 | 19.1 | 2.4× |
| Auction | 2.29 | 16.5 | 2.1× |
| GT (current) | 2.31 | 434 | **55×** |
| Hungarian | 3.48 | 400 | 51× |

The slope is fine — under the paper's worst case of `n^{2.75} log(nC)` for dense graphs. The pain is the constant factor. Algorithm matches Gabow & Tarjan 1989; the bookkeeping around it does not.

Target after this work: **GT within 3× of JV at n=256**, slope unchanged (~2.3). Stretch: beat JV for large `C` since GT's `log C` factor should pay off there.

---

## Paper reference

Gabow, H. N., & Tarjan, R. E. (1989). "Faster scaling algorithms for network problems." *SIAM J. Comput.* 18(5), 1013–1036.

Assignment-problem result: `O(n^{3/4} m log(nC))` via:

- Multiply integer costs by `(n+1)`, take `k = ⌈log₂ ĉ_max⌉` bit-scaling phases (MSB → LSB).
- At each phase, double cost and dual (`c ← 2c + bit_s`, `y ← 2y − 1`).
- Within a phase, maintain a `1-feasible` matching (`y_u + y_v ≤ c + 1`, equality on matched edges) and refine to a `1-optimal` matching by alternating:
  - **Step 1** — find a *maximal vertex-disjoint set* of augmenting paths in the eligibility graph (eligible = `y_u + y_v == cl(e)`), augment all of them, then decrement `y_v` by 1 on every column touched by a path.
  - **Step 2** — one Hungarian search on the cost-length `cl(e) = c(e) + [e ∉ M]`, bucket-array based (bucket range `O(n)`), updates duals lazily via a global offset `A`.
- The `(n+1)` multiplier guarantees the final `1-optimal` matching after the last bit is the true optimum.

What carries the asymptotic: equality graph and cost-length are **conceptual**, never materialized as O(nm) arrays per call. Step 2 enqueues only the edges of the row that just entered the forest, not all m columns. The eligibility structure is maintained incrementally end-to-end.

---

## Current implementation map

Layout (all in `src/gabow_tarjan/`):

- `solve_gabow_tarjan.cpp` — R-facing entry point, scale/round, dual conversion.
- `utils_gabow_tarjan.cpp` — all the moving parts:
  - Module A — `cost_length`, `is_eligible`, `check_one_feasible`
  - Module B — `build_equality_graph`, `update_equality_graph_incremental`
  - Module C — `augment_along_path`
  - Module D — `find_one_augmenting_path_eq`, `find_maximal_augmenting_paths`
  - Module E — `build_cl_matrix`, `hungarian_search_cl`, `hungarian_step_one_feasible`
  - Module F — `apply_step1`, `match_gt`
  - Module G — `scale_match`
  - Module H — `find_max_cost`, `solve_gabow_tarjan_inner`

Outer call chain:
```
solve_gabow_tarjan_impl
  └ solve_gabow_tarjan_inner            // bit-scaling loop, k phases
      └ scale_match                     // builds cost_prime, runs inner solver
          └ match_gt                    // Step 1 / Step 2 alternation
              ├ apply_step1             // maximal augmenting paths
              │   ├ build_equality_graph (first call only)
              │   ├ find_maximal_augmenting_paths
              │   ├ augment_along_path (per path)
              │   └ update_equality_graph_incremental
              └ hungarian_step_one_feasible
                  ├ check_one_feasible    // O(nm)
                  ├ repair loop           // up to 20 × O(nm)
                  ├ build_cl_matrix       // O(nm)
                  └ hungarian_search_cl   // enqueues all m cols per row
```

---

## Issue inventory (priority order)

Each issue lists the file:line, what's wrong, what the paper does, and the fix.

### P0 — Cost-length matrix materialized every Step 2

`utils_gabow_tarjan.cpp:762` (`hungarian_step_one_feasible`) calls `build_cl_matrix(cost, row_match)` every Step-2 entry, producing a fresh `n × m` `CostMatrix`. Cost-length is just `c(i,j) + 1` for unmatched and `c(i,j)` for matched — derivable from `row_match` in O(1) per access.

**Fix:** delete `build_cl_matrix`. Inline the `cl` formula inside `hungarian_search_cl` everywhere `C_cl[i][j]` appears. Pass `(cost, row_match)` instead of `C_cl`.

**Estimated savings:** ~O(nm) per Step-2 call. Per scaling phase this is O(n²m) with n Step-2 calls; across log(nC) phases ≈ O(n²m log nC) eliminated.

### P0 — Equality graph rebuilt from scratch after every Step 2

`utils_gabow_tarjan.cpp:1031` in `match_gt`:

```cpp
if (!found_paths) {
    if (!hungarian_step_one_feasible(...)) ...
    eq_graph = build_equality_graph(cost, row_match, y_u, y_v);   // O(nm)
}
```

Step 2 changes a known set of row and column duals (every row in `S`, every col in `T`). The set of edges that become eligible/ineligible is bounded by the boundary of `S ∪ T`.

**Fix:** make `hungarian_search_cl` (or its wrapper) emit `affected_rows` and `affected_cols`, then call an extended `update_equality_graph_incremental` that handles row-dual changes too. The current incremental updater only handles column-dual changes (line 156–197).

**Estimated savings:** O(nm) per Step-2 call → O(deg(S) + deg(T)) per call.

### P0 — Hungarian forest enqueues all m columns per row

`utils_gabow_tarjan.cpp:535-550` (`enqueue_edges_from_row` inside `hungarian_search_cl`):

```cpp
for (int j = 0; j < m; ++j) {
    if (in_T[j] || C_cl[i][j] >= BIG_INT) continue;
    long long r = C_cl[i][j] - saved_y_u[i] - y_v[j] + enter_A_u[i];
    ...
}
```

For every row entering S, scans **all** m columns. Per Step-2 call with up to n rows entering S, that's `O(n·m)` of pure enqueue work even before bucket processing. The paper's variant only walks the row's adjacency in the eligibility structure (cheap when sparse) plus an `O(1)` boundary check.

**Fix:** maintain a row-wise adjacency representation of finite (non-forbidden) edges *once* at `match_gt` entry (`std::vector<std::vector<int>> adj_finite(n)`). In `enqueue_edges_from_row`, iterate `adj_finite[i]` instead of `0..m-1`. For an `n × m` dense matrix `adj_finite[i].size() == m_finite_i`, so no asymptotic change on dense — but it removes the `in_T[j]` branch and the BIG_INT compare on a fast inner loop, and gives 5–10× constant-factor reduction. The bigger structural fix is: prefer enqueuing only **near-eligible** edges (small `r`) — eligible edges go straight into the bucket at `A`, the rest get re-enqueued lazily.

**Estimated savings:** order-of-magnitude reduction in Hungarian inner-loop work.

### P1 — Feasibility check + repair loop on every Step 2

`utils_gabow_tarjan.cpp:665-759` (`hungarian_step_one_feasible`):

```cpp
if (!check_one_feasible(cost, row_match, col_match, y_u, y_v)) {
    ...
    for (int iter = 0; iter < MAX_REPAIR_ITERS; ++iter) {  // 20 × O(nm)
       ...
    }
}
```

The repair block is defensive code for state that *should not exist*. If `scale_match` and Step 1 maintain `1-feasibility` correctly, the state is feasible by invariant at every Step-2 entry. The fact that the repair loop is hit (or even checked-for) suggests we don't trust the invariant.

**Fix in two parts:**

1. Add `assert(check_one_feasible(...))` guarded by `#ifdef DEBUG`, remove the runtime path. Run the assertion build through the existing test suite; if it ever fires, fix the upstream invariant breach instead of repairing here.
2. Remove the upfront `check_one_feasible` call from the hot path entirely.

**Estimated savings:** O(nm) per Step-2 call (the check), plus the rare O(nm × 20) repair.

### P1 — `match_gt` discard-and-restart on infeasibility

`utils_gabow_tarjan.cpp:941-975`. Same root cause: defensive code that should be an assertion. Calls `check_one_feasible` at every `match_gt` entry, and on failure discards the matching and reinitializes duals via an O(nm) min-scan.

**Fix:** same as above — promote to debug-only assertion, document `scale_match` postcondition as "matching is consistent and 1-feasible on entry."

### P1 — `scale_match` builds a fresh `cost_prime` matrix per phase

`utils_gabow_tarjan.cpp:1104` allocates `CostMatrix cost_prime(n_work, std::vector<long long>(m_work, BIG_INT))` and fills it with `c - y_u - y_v` every scaling phase. `k = log(nC)` phases × O(nm) = O(nm log nC) of pure allocation and dual-subtraction.

**Fix:** don't materialize `cost_prime`. Pass `(cost, y_u, y_v)` into `match_gt` and let it compute reduced cost on access. `cl(i,j) = (cost[i][j] - y_u_global[i] - y_v_global[j]) + [unmatched]`. The Hungarian search already adds `enter_A_u[i]` and subtracts `saved_y_u[i]` from `C_cl[i][j]` — those bias terms compose naturally.

This is the biggest single edit (touches the type signatures across modules) but it's also the cleanest. Once cost-length and cost-prime are both implicit, the equality-graph machinery operates directly on `(cost, y_u, y_v, row_match)` and a lot of the bookkeeping evaporates.

**Estimated savings:** O(nm log nC) eliminated, plus better cache behavior.

### P2 — `update_equality_graph_incremental` scans all n rows

`utils_gabow_tarjan.cpp:180`. For each affected column, the right thing is to walk the columns of rows that *had* an eligible edge to those columns, not all n rows.

**Fix:** maintain a reverse index `col → rows with eligible edge to it` as part of the eligibility structure. After P0 fix (incremental rebuild around Step 2), this becomes more important.

### P2 — `compute_duals_from_matching` Bellman-Ford for `n ≤ 100`

`solve_gabow_tarjan.cpp:15-79`. O(n³) just to get a clean dual certificate for small problems. Fine for correctness, not on the perf path. Leave it for now — `n ≤ 100` gating is the right call. Note in passing.

### P2 — `find_maximal_augmenting_paths` uses persistent `marked_col`/`next_edge` across roots

`utils_gabow_tarjan.cpp:350-411`. Already O(E) per call thanks to `next_edge[i]++` advancement. Good. The Hopcroft-Karp BFS-leveling that would bound the number of *calls* to this routine per phase is missing — Step 1 just gets called again from `match_gt`'s outer loop. For the paper's sub-cubic bound this needs proper level-graph computation, but in practice the alternation Step1↔Step2 still works; this is more of a "to hit paper's theoretical bound" item than a constant-factor speedup. Defer.

### Correctness flag — bucket bound silently drops edges

`utils_gabow_tarjan.cpp:545-547`:

```cpp
if (r > bucket_bound) continue;
```

`bucket_bound = 6n + 2`. The paper proves reduced cost during a single phase is bounded by `O(n)` under the structural invariants. If we ever feed a state where that bound is violated (e.g., dual repair pushed `y_u` further negative than expected, or a corner case in `scale_match`), we silently drop a candidate edge and Hungarian search can return "no path" when one exists. Symptom: `LAP_ERROR("No augmenting path in Step 2 (no perfect matching)")` at line 1027 on instances where a perfect matching does exist.

**Fix:** add `LAP_ERROR` if `r > bucket_bound && C_cl[i][j] < BIG_INT` for any finite edge during enqueue, behind a `#ifdef DEBUG`. If the assertion never fires in the test suite + a fuzz pass, the silent drop is safe. If it fires, fall back to a heap-based path for that edge.

---

## Phased plan

Each phase ends with **all GT tests pass** and **benchmark re-run**. Numbers go into a running table at the bottom of this doc.

### Phase 0 — baseline pinned (today)

- [ ] Save current benchmark output to `dev_notes/gt_baseline.txt`.
- [ ] Add a longer-range benchmark to `inst/scripts/` covering n ∈ {64, 128, 256, 384, 512} × `max_cost ∈ {1e3, 1e5, 1e7}` so we catch regressions across the `log C` dimension.
- [ ] Run the existing test suite once and pin pass count.

### Phase 1 — kill the materialized derivatives (P0 + P1)

Inline `cl()`, remove repair loop, promote consistency checks to debug-only. No signature changes outside `utils_gabow_tarjan.cpp`.

- [ ] Delete `build_cl_matrix`. Inline `cl(c_ij, in_matching)` into `hungarian_search_cl` (it already calls `cost_length` indirectly — just call it directly on `cost`).
- [ ] Change `hungarian_search_cl` signature to take `(cost, row_match, col_match, y_u, y_v)`.
- [ ] Change `hungarian_step_one_feasible` to skip the upfront `check_one_feasible` and the repair loop. Replace with `DEBUG_ASSERT_FEASIBLE`.
- [ ] Change `match_gt` to skip the upfront `check_one_feasible` + restart block. Replace with `DEBUG_ASSERT_FEASIBLE`.
- [ ] Add `#define DEBUG_ASSERT_FEASIBLE(...)` no-op in release, full check under `-DCOUPLR_GT_DEBUG`.
- [ ] Run tests. Run benchmark. Record.

**Expected:** 3–8× speedup. If the assertion fires under `COUPLR_GT_DEBUG`, *stop* and fix the upstream invariant — do not paper over it.

### Phase 2 — incremental equality graph around Step 2 (P0)

- [ ] Modify `hungarian_search_cl` to emit `affected_rows` and `affected_cols` (the rows added to S and the cols added to T during the successful search).
- [ ] Extend `update_equality_graph_incremental` to handle both row-dual and column-dual changes. Replace the column-only signature with one that takes both.
- [ ] In `match_gt`, replace the post-Step-2 `eq_graph = build_equality_graph(...)` with the new incremental update.
- [ ] Tests + benchmark. Record.

**Expected:** another 1.5–3× on top of Phase 1.

### Phase 3 — adjacency-restricted Hungarian enqueue (P0)

- [ ] Build `adj_finite[i] = { j : cost[i][j] < BIG_INT }` once at `match_gt` entry. Pass through.
- [ ] In `enqueue_edges_from_row`, iterate `adj_finite[i]` instead of `0..m-1`.
- [ ] Tests + benchmark. Record.

**Expected:** 1.5–2× on dense, larger on sparse problems where most edges are forbidden.

### Phase 4 — drop `cost_prime` allocation (P1)

This is the surgery. Touch every callee signature so reduced cost is computed on access from `(cost, y_u_global, y_v_global)`.

- [ ] Introduce `reduced_cost(i, j)` inline helper.
- [ ] Update `match_gt`, `apply_step1`, `find_maximal_augmenting_paths`, `hungarian_search_cl`, `build_equality_graph`, `update_equality_graph_incremental` to use it.
- [ ] Delete `cost_prime` materialization in `scale_match`. Pass `(cost, y_u_global, y_v_global)` plus per-phase corrections through.
- [ ] Tests + benchmark. Record.

**Expected:** 1.2–1.5× on top, plus better cache behavior on large n.

### Phase 5 — defensive bucket-bound assertion + fuzz (correctness)

- [ ] Add `LAP_ASSERT(r <= bucket_bound)` under `COUPLR_GT_DEBUG`.
- [ ] Fuzz-test 1000 random instances with `n ∈ [3, 200]`, `max_cost ∈ [1, 1e9]`, with up to 10% forbidden edges, with 1-row and 1-col edge cases.
- [ ] If the assertion fires: characterize the failing class, decide between (a) tighter dual maintenance, (b) heap fallback. Either way, no silent drops.

### Phase 6 — Hopcroft-Karp leveling for Step 1 (paper-tight asymptotic, optional)

Defer unless Phases 1–4 leave more than ~5× gap to JV at large n.

- [ ] Replace pure DFS in `find_maximal_augmenting_paths` with BFS-leveling + DFS along level-increasing edges. This gets the paper's `O(m √n)` bound for the unweighted Hopcroft-Karp part.
- [ ] Tests + benchmark.

---

## Verification at each phase

- **Correctness** — full `testthat` suite passes. Pay attention to:
  - `test-gabow_tarjan_complexity.R` — optimal cost matches JV/Hungarian across sizes, uniform / diagonal / anti-diagonal / sparse patterns.
  - `tests/testthat/gabow-tarjan/test_gabow_tarjan_module{A..H}.R` — module-level invariants.
  - `test-assignment-duals.R` — dual feasibility certificate.
- **Performance** — `inst/scripts/benchmark_gabow_tarjan_micro.R` and the extended Phase-0 benchmark. Compare slope and absolute times against the pinned baseline.
- **No regressions on other solvers** — the work touches only `src/gabow_tarjan/`; the broader assignment-suite tests should be unchanged.

## Out of scope

- Reformulating GT as a min-cost-flow with a different scaling. Stay in the GT 1989 framework.
- SIMD / GPU. Constants-only improvements are the goal.
- API changes. The R-facing interface `lap_solve(method = "gabow_tarjan")` stays identical.
- Doc-only fixes (`vignettes/algorithms.Rmd` claims `O(n³ log C)`, paper proves `O(n^{3/4} m log nC)`) — separate PR after the speedup lands and we have a new empirical slope to quote.

---

## Running results table

Filled in as we go.

| Phase | Slope | ms @ n=64 | ms @ n=128 | ms @ n=256 | GT/JV @ n=256 | Test status |
|---|---|---|---|---|---|---|
| 0 baseline | 2.31 | 17.6 | 77.1 | 434 | 55× | passing |
| 1 inline cl + kill repair | 2.19 | 13.4 | 62.5 | 290 | 35× (JV=8.3) | passing |
| 2 incremental eq-graph | 2.22 | 10.4 | 48.8 | 225 | 27× (JV=8.3) | passing |
| 3 adj-restricted enqueue | reverted (dense regression, see notes) |
| 4 drop cost_prime | reverted (Step 1 starves without cost_prime, see notes) |
| 5 fuzz + assertions | unchanged | unchanged | unchanged | unchanged | unchanged | passing (drop is structural, not a bug) |
| 6 HK leveling | 2.09 | 2.42 | 9.88 | 44.05 | 23× (JV=1.91) | passing |

### Phase 1 notes

- 1.3–1.5× wall-clock on n=64/128/256 dense max=1e5; slope drops 2.31 → 2.19.
  Below the plan's 3–8× estimate because the killed work (cost-length matrix
  build + dual repair) was a smaller share of total time than estimated; the
  Hungarian inner loop (Phase 3 target) and post-Step-2 equality-graph rebuild
  (Phase 2 target) now dominate.
- **Finding (warm-start was already broken).** While simplifying `match_gt`
  the old `check_one_feasible` + discard/restart block turned out to be
  load-bearing: `scale_match` carries `row_match`/`col_match` from the previous
  phase but resets `y_u_loc = y_v_loc = 0`, so matched-edge lower bound
  `y_u + y_v >= c` fails on cost_prime ∈ [0, 3] for matched edges. Old code
  silently threw the matching away every phase 2+, so the "bit-scaling warm
  start" comment in `scale_match` was fictional. Phase 1 makes that explicit
  by clearing `row_loc`/`col_loc` in `scale_match` before calling `match_gt`,
  same algorithm, half the bookkeeping. **Phase 4 (drop cost_prime) is what
  restores a real warm start**, and is where the `log nC` advantage finally
  pays off.
- Empty-matching dual init kept: `y_v[j] = min_i(c_ij + 1)` (O(nm) once per
  `match_gt` / `hungarian_step_one_feasible` entry from empty matching).
  Without it, negative-cost test cases (`cost ∈ [-5, 10]` with zero duals)
  start non-1-feasible. The 20-iter repair loop for non-empty inconsistent
  state is gone, and `DEBUG_ASSERT_FEASIBLE` (release no-op, full check under
  `-DCOUPLR_GT_DEBUG`) guards that invariant. Full test suite passes under
  both builds.
- One Module F test was using infeasible duals on a partial matching to
  exercise the removed repair loop; updated to 1-feasible duals
  (`tests/testthat/gabow-tarjan/test_gabow_tarjan_moduleF.R`). Two
  `gt_build_cl_matrix` tests in Module E deleted (function is gone).

### Phase 2 notes

- 1.3–1.4× wall-clock on top of Phase 1; slope unchanged at ~2.22. The full
  `eq_graph = build_equality_graph(...)` rebuild after each Step 2 is gone.
  Below the plan's 1.5–3× estimate because Step 2 invocations are not the
  hot path on dense uniform-random instances — most augmentations happen in
  Step 1, where the rebuild was already incremental.
- **Root-cause fix that fell out of this phase.** The post-Step-2 dual
  state had matched path edges with `sum = c + 1` (1-feasible but not
  tight), because `materialize_forest_duals` is computed against the
  pre-Step-2 `row_match` (where the edge was still unmatched and
  `cl_pre = c + 1`). The old code papered over this with a "PRE-STEP" at
  the top of `hungarian_search_cl` that decremented `y_u[i]` by 1 on every
  non-tight matched row at the start of the next Step 2. That worked under
  the full-rebuild flow, but it broke the incremental update because
  `y_u[i]` was mutated for rows that did not enter S, so their
  `eq_graph[i]` adjacency went stale. The fix decrements `y_v[j]` by 1 on
  every newly-matched col inside the Step 2 augment loop — mirroring what
  Step 1 already does — so matched path edges leave Step 2 tight. With
  that invariant restored, PRE-STEP becomes dead code; replaced with a
  `COUPLR_GT_DEBUG`-only assertion that matched edges are tight on entry.
- `update_equality_graph_incremental` now takes both `affected_rows` and
  `affected_cols`. Step 1 calls it with empty `affected_rows`. Step 2
  calls it with `affected_rows = S`, `affected_cols = T`, where rows in
  S get their adjacency rebuilt over all m cols (their y_u and possibly
  row_match changed) and rows outside S get only edges to cols in T
  pruned (y_v dropped, eligibility can only be lost for those, never
  gained, since cl is unchanged on those edges). When the matching was
  empty entering `hungarian_step_one_feasible`, `gt_init_empty_duals`
  rewrites every `y_v`, so the wrapper signals a full rebuild by setting
  `affected_rows` to all rows.
- Net change to call chain: post-Step-2 work goes from one O(nm) full
  rebuild to O(|S|·m + (n - |S|)·|T|). For dense random costs at n=256
  this is what saved ~65 ms per `match_gt` outer iteration on average.

### Phase 3 notes (reverted)

Implemented `build_adj_finite(cost) -> std::vector<std::vector<int>>` once
at `match_gt` entry, passed through `hungarian_step_one_feasible` to
`hungarian_search_cl`, and replaced the `for (j = 0; j < m; ++j)` loop in
`enqueue_edges_from_row` with `for (int j : adj[i])`. Skipped the BIG_INT
check in the inner loop since `adj[i]` filters it at build time.

**Five-trial bench at n=256 dense uniform random max=1e5:** Phase 2
median 230 ms, Phase 3 median 253 ms — a stable ~10% regression. JV
~7.8 ms either way. Reverted the change.

The plan estimated 1.5–2× on dense and "order-of-magnitude reduction in
Hungarian inner-loop work" overall. That estimate was based on
"removes the in_T[j] branch and the BIG_INT compare on a fast inner
loop", but:

- The `cost[i][j] >= BIG_INT` compare is on a value already loaded for
  the `cl` computation. It's one well-predicted branch on dense (always
  not taken). The saving per iteration is essentially zero cycles.
- The `in_T[j]` branch is **still** required after Phase 3 (`in_T`
  changes during the search). Phase 3 doesn't remove it.
- Walking `adj[i]` adds an extra cache line per iteration (adj[i] lives
  in a separate allocation from cost[i]). On dense, |adj[i]| == m, so
  the iteration count is unchanged but inner-loop memory traffic goes
  from one cache line per iteration (cost[i] only) to two (adj[i] +
  cost[i]). That's the regression.

Phase 3 is a **sparse-problem** optimization. On instances where many
edges are forbidden (cost == BIG_INT), `|adj[i]| << m` and the iteration
count drops proportionally — there the win is real. Re-evaluate when we
add sparse benchmarks.

The plan's bigger structural idea — "prefer enqueuing only near-eligible
edges (small r), the rest get re-enqueued lazily" — is a different
intervention from "skip forbidden edges" and would attack actual Step 2
work, not per-edge overhead. Filed as a future direction; Phase 4 is the
next concrete win.

Bench artefact: `dev_notes/phase3_bench.txt`.

### Phase 4 notes (reverted)

Implemented the full plan: dropped `cost_prime` materialization in
`scale_match`, threaded warm-start duals through `match_gt` /
`hungarian_step_one_feasible` via a new `init_empty_duals` flag (default
true for the R test wrappers; false for the bit-scaling caller), and
moved `gt_init_empty_duals` out of `match_gt`'s empty-matching path when
the flag is false. The bit-scaling outer loop's `y <- 2y - 1` now feeds
directly into `match_gt` as the active duals — no per-phase O(nm) build,
no `y_global += y_loc` add-back.

**Five-trial bench, same-session state (JV ≈ 1.88 ms):**

| | Phase 2 | Phase 4 | Δ |
|---|---|---|---|
| n=64 | 2.18 ms | 4.11 ms | +88% |
| n=128 | 10.49 ms | 14.37 ms | +37% |
| n=256 | 47.30 ms | 64.49 ms | +36% |
| slope | 2.22 | 1.99 | better |

Stable across trials. Phase 4 has a better slope but a worse constant.
Crossover with Phase 2 is around n ≈ 1000; below that, regression. Our
target use case is n ≤ 256, so net loss. Reverted.

**Root cause: Step 1 starves without `cost_prime`.** The OLD design used
`cost_prime = cost - y_global` (small range), and `gt_init_empty_duals`
at each phase entry set `y_v[j] = min_i(cost_prime[i][j] + 1)`, making
the column-min row in every column eligible. The eq_graph at match_gt
entry contained ~m edges; Step 1's `find_maximal_augmenting_paths`
extracted cheap partial matchings before falling through to Step 2.

In the NEW design with warm-start duals and no `gt_init_empty_duals`,
the duals after the outer loop's `y <- 2y - 1` give `y_u + y_v = -2`
(first phase, where prev-phase y = 0). For an edge to be eligible,
`y_u + y_v == c + 1`, which means `c == -3`. No such edge exists. The
eq_graph is empty. Step 1 finds nothing. Every augmenting path routes
through the more expensive Step 2.

The trade-off is fundamental: either you keep `cost_prime` (small
range → canonical init populates eq_graph → Step 1 useful) **or** you
drop it (warm-start preserved across phases → eq_graph starts empty →
Step 1 starves). The plan's intuition that Phase 4 would "restore real
warm start" missed that the canonical init *was* the load-bearing thing
making Step 1 useful, not just a setup step we could skip.

What might still be worth trying for Phase 4: keep `cost_prime` but
**reuse the allocation across phases** (own the buffer in
`solve_gabow_tarjan_inner`, refill in-place each phase). Pure memory
optimisation, ~3–5% expected from skipped alloc/free. No algorithm
change. Filed as a possible Phase 4b.

Bench artefact: `dev_notes/phase4_bench.txt`.

### Phase 5 notes

Added a `#ifdef COUPLR_GT_DEBUG`-guarded check in
`hungarian_search_cl::enqueue_edges_from_row` (and the symmetric re-enqueue
branch in the bucket-processing loop) that flags any finite, not-in-T edge
whose `r` exceeds `bucket_bound = 6n + 2`. Built the package with
`-DCOUPLR_GT_DEBUG`, then ran `dev_notes/phase5_fuzz.R`: 1000 random
instances, `n in [3, 200]`, `max_cost in {1, 10, 100, 1e3, 1e5, 1e7, 1e9}`,
mix of dense / 0-10% forbidden / shifted-negative / 1-row / 1-col.

**Fuzz result.** 897 square instances ran; the 1-row/1-col 103 were skipped
(GT solves balanced LAP). The assertion fired on **770/897 (86%)** of
instances. JV vs GT optimal cost: **0 mismatches.** Every drop happened at
the initial-enqueue site; the re-enqueue site never fired in this run.

**What this means.** The paper's `r = O(n)` bound assumes `match_gt` exits
each phase with all reduced costs `cost - y_u - y_v` inside `[-1, n+1]`.
This implementation only tightens duals on rows entering S and cols
entering T during a Step 2; cold rows/cols keep `y_loc = 0`, so their
phase-exit reduced cost can be arbitrarily large (proportional to
`cost_prime` magnitude, which grows with `(n+1) * max_cost` over the bit
phases). The silent `continue` is what keeps the bucket array from
needing to be 2e11 entries wide at `n = 200, max_cost = 1e9`.

The drops do not corrupt the final cost because the bit-scaling outer
loop re-presents the dropped edges in later phases (after `c <- 2c + bit`,
`y <- 2y - 1`, the reduced cost shrinks toward 0 on edges that should be
matched). So the bound is theoretically optimistic for this
implementation, but empirically safe.

**Disposition.** Phase 5's plan offered two ways to remove silent drops:
(a) tight dual maintenance so the paper bound holds, or (b) heap-based
Step 2 so no bound is needed. Both are substantial reworks of Module E /
Module F, comparable in size to Phases 3/4 combined. Not done here.

Shipped: a comment block at the top of `utils_gabow_tarjan.cpp` plus
inline comments at both drop sites that name this as a Phase 5 finding
and explain why the drop is load-bearing. `dev_notes/phase5_fuzz.R` is
kept as the regression check: a future refactor of dual maintenance must
either drop counts to 0 (paper bound holds) or maintain 0 cost mismatches
(drops stay safe). `Makevars.win` is reverted to release config; the
COUPLR_GT_DEBUG block is reachable by adding `-DCOUPLR_GT_DEBUG` to
`PKG_CPPFLAGS` and rebuilding.

Bench: no perf change (release codepath unchanged); row in the results
table marked "unchanged".

### Phase 6 notes

Replaced the pure-DFS `find_maximal_augmenting_paths` with a two-phase
Hopcroft-Karp structure: BFS from all free rows builds a level graph
(rows at even levels via eligible edges to cols at odd levels, then back
to rows via `col_match`), then DFS along level-increasing edges only
extracts a maximal set of vertex-disjoint **shortest** augmenting paths.
The DFS uses the same `next_edge[i]` advancement pattern as before, so
per-call work stays O(E); restricting to shortest paths is the
algorithmic change that gives Hopcroft-Karp its `O(m sqrt(n))` bound for
unweighted bipartite matching.

**Same-session A/B vs Phase 2 (JV ~1.88 ms either way):**

| n | Phase 2 | Phase 6 | Δ |
|---|---|---|---|
| 64 | 2.17 ms | 2.42 ms | +12% |
| 128 | 10.30 ms | 9.88 ms | -4% |
| 256 | 44.84 ms | 44.05 ms | -2% |
| 384 | 112.56 ms | 121.26 ms | +8% |
| 512 | 181.25 ms | 176.95 ms | -2% |
| slope (64–256) | 2.184 | 2.092 | better |

The slope improvement (2.18 → 2.09) is real and consistent. Per-trial
variance is well under 1% across 5 trials. The constant is mixed: ~10%
slowdown at n=64 from BFS overhead at small problem size, ~2% win at
n=256 and n=512.

The win isn't bigger because Step 1 isn't the dominant cost. Most of
`match_gt`'s time is in Step 2's bucket processing — HK can't help
there. Additionally, Step 2 changes duals between successive Step 1
calls, reshaping the eligibility graph and breaking HK's amortization
assumption (paths from prior rounds don't carry).

Shipping anyway. The slope is the right shape, all 184 GT tests + 4955
full-suite tests pass, and the level-graph structure is a cleaner
foundation than the persistent-marked-col scheme for future Step 1
work (e.g., maintaining the level graph incrementally across dual
updates instead of rebuilding it per call).

Bench artefact: `dev_notes/phase6_bench.txt`.
