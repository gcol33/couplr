# couplr 1.7.0

## Breaking changes

* **`method = "auto"` no longer diverts on sparsity or aspect ratio.** Two of
  the five dispatch rules sent a matrix with more than half its entries
  forbidden to `"lapmod"`, and a matrix with at least three columns per row to
  `"sap"`. Measured across the regime grid in `paper/bench_regimes.R`, neither
  earned its place: `"sap"` was the quickest solver in none of the 32 cells
  where its rule fired, at a median of 5.75 times the cell's best and a worst
  of 13.4, and `"lapmod"` was quickest in 2 of 48 cells at 60 and 25 percent of
  the entries finite, and in 1 of 31 at 5 and 1 percent, the extreme sparsity
  its adjacency structure exists for. Jonker-Volgenant is at or below the
  best-known time in both regimes, so both properties now fall through to it.
  The dispatcher is three rules: enumerate an at most 8 by 8 problem, use
  `"hk01"` where the finite costs carry no scale, and otherwise `"jv"`. Both
  `"lapmod"` and `"sap"` remain reachable by name, which is what a caller with
  a problem outside the measured grid should use.
* **The memory guard estimates the solve, not the matrix.** `memory_mode =
  "auto"` compared a dense cost matrix's footprint against available RAM, at
  four times the raw cell bytes. A dense solve peaks well above the matrix it
  runs on: measured at 9.4, 7.2 and 8.6 times the raw bytes at 5,000, 10,000
  and 20,000 units, against the 4 the guard assumed, so a solve could be
  started on a machine it did not fit. `estimate_dense_solve_mb()` now supplies
  the figure the guard reads, at a multiplier taken from those measurements;
  `estimate_dense_matrix_mb()` keeps its own meaning and is no longer what
  decides the mode. The guard switches to `"lazy"` earlier than it did, and its
  warnings now name the solve rather than the matrix.

* **The `"orlin"` solver is now `"sap_dense"`.** The C++ behind it runs
  successive shortest paths: each augmentation is a Dijkstra search on reduced
  costs followed by a Johnson potential shift. It has no scaling phases and no
  auction warm-up, so it is not the Orlin-Ahuja (1992) algorithm its old name
  named, and its `alpha` and `auction_rounds` arguments were never read. The
  method now carries a name that describes what it does: shortest augmenting
  paths whose priority queue is a linear scan over the columns rather than a
  heap, which costs `O(n * m^2)` and suits a dense cost matrix. Calls passing
  `method = "orlin"` now raise an error listing the valid methods. Results,
  duals and timings are unchanged; only the name is.
* **`assignment()` documents `O(sqrt(V) * E * log(V * C))` for
  `"gabow_tarjan"`,** on a graph of `V` vertices and `E` edges, which for an
  `n` by `n` cost matrix is `O(n^2.5 * log(n * C))`. The previous `O(n^3 log C)`
  did not match the bound in the source it cites.

## Improvements

* **`verify_assignment()` decides its conditions in exact arithmetic and says
  so.** Every condition the certificate checks is the sign of
  `c_ij - u_i - v_j`, and a double is a rational number, so that sign has an
  exact answer; the check now evaluates it exactly instead of reading the sign
  of a rounded difference. The new `arithmetic` argument takes `"auto"`, the
  default, which reports the exact conclusion when the exact conditions hold
  and the tolerance conclusion otherwise, `"exact"`, which refuses to fall
  back, and `"double"`, which is the previous behaviour. The certificate
  carries `arithmetic`, `exact_certificate` and `all_rows_matched`, and its
  print method names the arithmetic the conclusion is in. The exact conditions
  imply the numerical ones at any non-negative `tol`, so `certified_optimal`
  under `"auto"` is what it was before.
* **`assignment()` documents the integer conversion `"gabow_tarjan"` performs.**
  The scale factor, the rounding rule, the instance whose optimum is claimed,
  the range a matrix is refused at, and the bound on how far the rounded
  instance's optimum can sit from the original one are all stated.

* **The edge-generation loop sizes its own seed.** Under
  `memory_mode = "implicit"` the first round used to give every row five
  columns whatever the problem was. Five is short enough that the loop bought
  the rest of what it needed a round at a time, and every one of those rounds
  costs a full pricing sweep over the pairs the master does not hold. The seed
  is now read off the number of columns, and a run reports the width it used as
  `$search$seed_width`.

  On the eight-covariate scaling problem the paper uses, the loop settles in two
  rounds instead of four to seven, and runs 1.9x to 2.4x faster at 5,000 to
  50,000 units. That turns the comparison with `memory_mode = "lazy"` around:
  the mode used to lose to it below 50,000 units (0.62x at 5,000, 0.87x at
  20,000) and now leads at every size measured, from 1.1x at 5,000 to 3.1x at
  50,000.

  The answer and the proof behind it are untouched. Across the seed widths
  measured -- 8 to 256 columns at four sizes, and 5 to 160 at four more -- every
  run returned the same total distance to the last digit and came back
  certified.

* `match_path()` reports the same `$search$seed_width`, and `width` still takes
  an explicit column count on both surfaces. Zero, the new default, asks for
  the sized seed.

## Bug fixes

* **A matched pair's reported distance is the one the solver priced it with.**
  Under `memory_mode = "lazy"` and `"implicit"` the distance column was
  recomputed in R from a second copy of the metric's formula, which agrees with
  the solver's own evaluation to rounding and not to the last bit. A
  `max_distance` set at a distance the package had reported could therefore
  exclude the pair it was read from: on a Mahalanobis problem whose widest
  matched arc is the one the matching depends on, a caliper at that value
  returned a complete matching on the dense path and none on the lazy and
  implicit paths. The reported distance now comes from the same routine the
  solve evaluated the pair with, and the formula is written once.

# couplr 1.6.1

## New features

* **`memory_mode = "implicit"` solves an assignment by generating the pairs it
  needs.** Available on `assignment()` and `match_couples()`, with `certify`
  beside it. Every row starts with its nearest admissible partners, that sparse
  problem is solved by the flow model, and the pairs left out are priced against
  the duals it returns: a pair enters on a negative reduced cost, and the loop
  repeats until none prices in. The duals then certify the sparse solution
  optimal for the complete problem, so the answer is the one a dense solve
  returns on every problem small enough to run both, reached without holding the
  complete problem. The result carries `u` and `v`, the certificate, and
  `search`: `candidate_edges`, `possible_edges`, `edges_evaluated`, `n_rounds`
  and a per-round record. An arc set admitting no complete matching comes back
  with Hall's witness, which says which units are short of partners.
  `memory_mode = "auto"` does not select it.

* **`match_path()` solves a matching per value of one argument as one sequence.**
  `vary = "max_distance"` sweeps the distance cut over `values`, which must
  ascend: each point resumes from the matching the point before it found, and
  raising the cut admits pairs while leaving that matching feasible, so a point
  costs a round of the edge-generation loop where an independent call costs a
  solve from cold. Over 20 values this is 2.83x, 2.61x, 3.71x and 4.54x against
  20 independent solves at 167 x 333, 667 x 1,333, 1,667 x 3,333 and
  6,667 x 13,333, with all 80 points optimal and every status and matched count
  equal to the independent solve's. A descending sweep withdraws pairs the
  matching may be standing on, so it is refused and told why.

  Returns a `couplr_path`: `$path`, a row per point carrying the matched count,
  the total distance and the matched sample's balance, and `$balance`, a row per
  point per variable, with the match vector, certificate, round record and Hall
  witness for each point beside them. `certify = TRUE` by default, and a point's
  certificate is what says its matching is the optimal one at that value.

* **`method = "push_relabel"` runs cost-scaling push-relabel.** The method value
  named an algorithm the solver did not run: what was dispatched was successive
  shortest paths with Johnson potentials, the same search as `method = "csflow"`,
  and three places in the repo said so (#32). It is now Goldberg-Tarjan's
  successive approximation (1990): a sequence of eps-optimal flows, each phase
  dividing eps, saturating the arcs the smaller eps no longer admits, and
  clearing the resulting excess with two local operations. A push moves excess
  along an arc of negative reduced cost; a relabel lowers the price of a node
  that holds excess and has no such arc, by exactly the amount that gives it one.
  Below `eps = 1/(n + 1)` an eps-optimal flow on integer costs is optimal, which
  is what ends the scaling; real costs are scaled to integers first, because
  without that the bound says nothing.

  The solver lands on the compiled `FlowProblem`, beside the existing
  shortest-path one, so every design the flow model compiles can reach it and
  the two solvers read the same network.

* **The min-cost flow solver can emit its per-step state, and the animation
  reads it** (#34). `solve_min_cost_flow()` takes an optional step sink and
  records, per augmentation, the distance labels, the shortest-path tree, the
  potentials after the shift, the path and the units moved. `trace_csflow()` is
  now a renderer over that record rather than a second implementation of the
  search in R, and the R residual-graph Dijkstra, Bellman-Ford and
  potential-update it used are gone. `solve_min_cost_flow_push_relabel()` has
  the same arrangement, per scaling phase.

  Two divergences between the two implementations had been found on 2026-08-15,
  both in the potential update, both masked by a defensive clamp. Reading the
  solver's own state is what removes the class.

* **`cardinality_match()` maximizes matched cardinality subject to balance
  constraints and reports an optimality gap** (#30). The matched sample now
  comes back beside the largest sample the stated constraints admit, in
  `result$cardinality`: `n_matched`, `best_possible`, `gap`, `gap_fraction`,
  `certified`, `stopped_on`, and the state of every constraint the match was
  asked to meet. Total distance remains the objective, ordered after
  cardinality.

  Fine and refined covariate balance are stated with the new `fine` and
  `refined` arguments and are represented in the matching network, so
  `max_std_diff = Inf` reaches a single min-cost flow solve that returns the
  largest balanced sample with a dual certificate at polynomial cost. Linear
  moment constraints -- a finite `max_std_diff`, or an explicit `moments` --
  are dualized and searched by branch and bound under `node_limit` and
  `time_limit`. `engine` names the solver directly, and refuses an argument the
  named engine would not read.

  `max_std_diff` now defaults to `Inf` rather than `0.1`, so a call states a
  moment constraint only when it asks for one, and a call that states balance
  through `fine` or `refined` alone is answered certified. A caller relying on
  the previous default reaches it with `max_std_diff = 0.1`. `time_limit`
  defaults to 30 seconds.

  `engine = "heuristic"` runs the pruning loop: a full match, then repeated
  deletion of the pairs carrying the worst variable's imbalance. It carries
  `info$pruning_iterations` and `info$pairs_removed`, which are now heuristic
  only, and reports `best_possible` and `gap` as `NA` with
  `certified = FALSE`, since the loop derives no bound.

  `left_id` and `right_id` are read through the same path as every other entry
  point, so the pairs a match with no `id` column produces are keyed on the
  values the rest of the package joins them by.

* **`assignment_duals()` gained `certify`, and reads a lazy cost
  specification.** It was computing a certificate's inputs without running the
  check. Under `certify = TRUE` it runs it and attaches the certificate, and
  `verify_assignment()` reads the duals off the result rather than solving
  again, so the option costs one pass over the admissible pairs. The default is
  `FALSE`, and the returned fields are unchanged without it.

  The lazy path could produce no duals at all, so certifying a lazy solve meant
  materializing the matrix that path exists to avoid. It takes a
  `lazy_cost_spec` now and returns the duals the dense path returns, a
  specification with more rows than columns is transposed rather than refused,
  and `verify_assignment()` no longer requires `duals` for one.

## Improvements

* The flow solver searches from one node holding an excess rather than from a
  super-source over all of them. Every design the flow model compiles gets it:
  dense `sap`, `csflow`, `push_relabel` and `cycle_cancel`, `full_match()`, the
  blocked and k:1 designs, and the implicit loop. Two orders of magnitude on the
  shapes measured. On a cost matrix with ties this can return a different
  matching among the equally optimal ones.

* A lazy solve under `max_distance` evaluates each distance once where it
  evaluated it three times. Reading a pair is halved, and a whole pricing pass
  is 1.38x on the shapes measured. Affects every `memory_mode = "lazy"` path and
  every certification over a lazy source.

* `method = "gabow_tarjan"` returns the optimum on rectangular problems, which
  1.6.0 carried as a known issue (#31, #33). The 1-optimality bound cancels its
  column terms only when both matchings cover every column, so a rectangular
  instance has to be completed, and it was completed by padding to
  `max(n, m)` square. The padding rows are copies of one node, so they are
  carried as a single row holding as many partners as there are dummies, which
  covers every column the same way and solves the instance at its own n by m
  shape: at n = 100, m = 100,000 the padded instance needed 80 GB for the cost
  matrix alone, and the collapsed one runs in 31 s at 221 MB and agrees with
  `jv` exactly. An unmatched row is reported as unmatched rather than as a
  padding column, and the multiplier separating the optimum from a 1-optimal
  matching falls from `max(n, m) + 1` to `2 * min(n, m) + 1`.

* `method = "gabow_tarjan"` takes its integer scale from the range the sentinel
  leaves. The conversion from doubles placed the largest magnitude at 1e6, so
  the quantum was the matrix maximum over 1e6, and it swamped the edges an
  optimum uses whenever those were far smaller than that maximum: a square 100
  by 100 instance of costs in [0, 1] carrying one entry at 1e5 came back 54
  percent above the optimum while reporting that it was optimal. Above a maximum
  of 1e6 no scaling was applied at all, so costs in [2e6, 2e6 + 1] lost every
  fractional part, and the shift counted a positive offset against the sentinel:
  at 1e14 + [0, 100] every finite cost read as forbidden, and a 10 by 10
  instance came back 442 high. The scale now satisfies
  `K * (hi - lo) <= BIG_INT / 8`, the shift is the low end of the range, and
  representability is checked in doubles before `llround()`, which had been
  running on the raw value and was undefined past `LLONG_MAX`.

* Every reader drops a pair the optimum was forced onto a forbidden edge. A cost
  at or above `BIG_COST` is what the rest of the package reads as no edge, and
  the 1:1 and precomputed-distance readers reported such a pair anyway, priced
  at 1e+308, with `status` then reading "optimal" off a matching that placed it.
  All four readers drop it now, and the two units come back unmatched. The
  predicate was open-coded in five places in two spellings, two of them missing
  the `is.finite()` half, which an NA distance could reach; the five call sites
  share one function.

* A matching's status is derived from what the design asked for. It came from
  unmatched left units, and a k:1 design places pairs while unmatched counts
  units, so a unit holding one of its two requested partners made the match
  report "optimal". Placed pairs are compared against requested pairs, the left
  unit count times the ratio, which reduces to the old rule at ratio 1 and
  covers the with-replacement design.

  The blocked path built its `info` twice, once per branch, and the copies
  disagreed on the requested-versus-actual method, on which fields exist, and on
  their order. Both branches build through one path now. `info$solver` is one
  entry per block that ran a solve, so a block's greedy fallback reaches the
  status and a blocked Hall-deficient match reports heuristic/greedy_sorted,
  which is what the same data reports unblocked. `block_summary` gains `n_left`,
  `n_right` and `n_matched` on the sequential branch, and loses `n_pairs`, that
  branch's own name for `n_matched`.

* Every reader in the matching layer joins on the id column. Columns were
  attached by row order in five places, and two of them scrambled what they
  attached: `match_data()` built weights and subclasses from a merge whose row
  order it then assumed, and the per-pair variable differences behind the
  cardinality prune did the same (#17, #24).

  That made the id contract worth stating. A duplicated id is rejected at
  extraction rather than left to pair the wrong units downstream (#35),
  `match_couples()` takes `left_id` and `right_id` where it had hardcoded the
  column name (#38), and a synthesised id warns and names the argument that
  sets one. `join_matched()` no longer re-parses the join key with
  `type.convert()`, so numeric-looking character ids join (#36).
  `match_data.matching_result()` emits one row per pair with the weight that
  pair carries, so `ratio > 1` and replacement come out with the shape MatchIt
  expects.

* The design's estimand is recorded, and `as_matchit()` reads it instead of
  labelling every non-subclass design `"ATT"` (#29). `augment()` is the
  `generics::augment()` generic rather than a second one beside it, and the
  `bal.tab` methods are registered on `cobalt::bal.tab()`, so they dispatch
  (#39). `forbidden` reaches the greedy single path, which had been ignoring it
  (#21).

* `verify_flow()`'s per-arc tolerance scales with the numbers behind the reduced
  cost. `cbar(a)` is computed as `cost(a) + pi(tail(a)) - pi(head(a))`, so its
  last bits are worth the largest of those three times the machine epsilon, and
  the comparison was made against an absolute `tol` of 1e-9. The lexicographic
  tier weights a balance design compiles to put the potentials in the millions,
  where one unit in the last place is around 1e-9, so an exactly optimal flow
  failed its own certificate on rounding: at n = m = 500 the search settled with
  a zero gap and still reported `certified = FALSE` (#43). Each arc is now
  compared against `tol * max(1, |cost(a)|, |pi(tail(a))|, |pi(head(a))|)`, the
  same reasoning the duality-gap check already applied to the objective, and the
  widest tolerance any comparison used is reported as `dual_tolerance`. The
  scale never falls below 1, so a problem of order 1 is checked against `tol`
  itself.

* `time_limit` reaches the flow solver. The cardinality search checked its
  budget between nodes, so a limit overshot by however long the solve in flight
  took, and nothing in the flow engine noticed a user interrupt either. The
  solver now asks between augmentations, at a cadence that cost nothing
  measurable on a 251,012-arc solve (median 1.080 s either way, over seven
  runs). A solve that runs out comes back with the new status `"interrupted"`,
  which `solver_status_values()` lists: its flow respects every arc bound and
  falls short of what the balances asked for, so it is neither an answer nor
  evidence that no answer exists. The node it belonged to goes back on the
  frontier unopened, which is what keeps the reported bound valid for the whole
  tree, and Ctrl+C raises an R interrupt condition from inside the solve rather
  than at the end of it.

* The flow solver takes a warm start from R. `FlowProblem` carried `warm_flow`
  and `warm_potential` and nothing on the R side reached them, so every solve
  the cardinality search made was cold, including the twenty per node that
  differ only in one multiplier step. The search now carries a solve's flow and
  potentials into the next one, and a node's into its children. On a
  branch-and-bound child, which differs from its parent by one arc bound, this
  is 6.9x at 5% density: 252 augmentations instead of 504.

  Which of the two starting points a solve uses is decided rather than assumed.
  Successive shortest paths pays for the b-flow its starting point leaves, so
  both are costed and the cheaper is taken. The check earns its pass on the
  designs that tie many pairs at one reduced cost: 71,156 of 251,012 arcs on a
  dense 500-by-500 with distances rounded to three decimals, where a repricing
  decides every tied arc at once and the previous flow stops being worth
  keeping.

* Euclidean and squared-Euclidean distances are summed one dimension at a time
  rather than through the Gram-matrix identity. The identity folds the same sum
  into one BLAS call, but subtracting two large nearly equal terms costs most of
  the mantissa whenever the coordinates are large next to the distance between
  them, and a blocked match on coordinates of order 1e15 returned different
  totals from its own parallel branch.

* `summary()` reports the share of focal units that kept a partner. It divided
  the pair count by the smaller side, which exceeds 1 under `ratio > 1` or
  `replace = TRUE`, where a unit holds several pairs.

* `print()` on a sensitivity result reports the largest Gamma actually tested
  below the critical one, instead of assuming the Gamma grid steps by 0.25.

* The many-forbidden-pairs warning counts the finite pairs instead of
  extrapolating the first row's count across every row.

## Internals

* One k-best partitioning engine serves both the Murty and the Lawler backend.
  Both had their own, and both dropped solutions: a child's subspace forces the
  prefix and forbids one column at the branch row, so the child's own children
  have to re-enter at that row carrying the forbidden set rather than start past
  it. An exhaustive differential test against brute force covers both backends,
  minimizing and maximizing, ties and forbidden edges.

* The auction reports an infeasible instance as one, instead of as a
  convergence failure (#18).

* The three shortest-augmenting-path traces share the tree construction and the
  dual lift; the two min-cost-flow traces share the edge lookup, which is now an
  index rather than a scan; and the two auto-transposing traces share the
  orientation helpers.

* Removed: eight morph helpers reachable only from their own tests, the stub
  trace registry (every name it covered has a real trace), a declared-never-
  defined C++ solver entry point, an exported-never-called Gabow-Tarjan path
  search, and the network-simplex thread arrays, which were maintained and never
  read.

* `CHANGELOG.md` is gone; `NEWS.md` is the changelog. The `matching_result` S3
  methods moved to `R/matching_methods.R`. `.onUnload()` unloads the DLL.

* `methods` is dropped from Imports. Nothing in the package imports from it.

# couplr 1.6.0

## New features

* **`verify_assignment()` checks an assignment against the optimality
  conditions and reports which of them hold.** Until now a result's
  `status` was the solver's word for it. `verify_assignment()` returns a
  checkable certificate instead: primal feasibility, dual feasibility, and
  complementary slackness on both matched arcs and unmatched columns.
  `certified_optimal` is `TRUE` only when every one of them holds.

  The check needs dual variables and does not trust them. Dual feasibility is
  tested over every admissible pair, so duals that certify nothing fail the
  check rather than pass it. Optimal duals are shared by all optimal solutions
  of a linear program, which is what makes it possible to certify a matching
  from one solver against duals from another, including the solvers that return
  no duals of their own.

  Both halves of complementary slackness are checked, and the second half is not
  optional. A verifier testing only tightness on matched arcs accepts a solution
  whose dual bound equals the true optimum while its primal cost sits above it,
  because a freed column carries `v_j < 0` (#28).

* **`assignment()` gained a `cardinality` argument**, with `"maximum"` and
  `"fixed"` alongside the previous behaviour, now named `"complete"`.
  `"maximum"` returns as many pairs as the admissible edges allow and the
  cheapest total among matchings of that size; `"fixed"` returns exactly
  `n_matches` pairs at minimum total cost. Both are solved exactly by the same
  solver as `"complete"`: dummy columns are appended, priced so that the
  solver's own optimum is the requested objective, and a row that took a dummy
  column comes back unmatched. `unmatched_penalty` replaces the lexicographic
  objective under `"maximum"` with a single one, where a pair costing more than
  the penalty is worth dropping.

* **`explain_dispatch()` reports which solver `method = "auto"` selects, and
  why**, without solving: the rule that fired, the property that triggered it,
  the rules tested first that did not fire, and the shape the solver will
  receive. The dispatch rules moved out of the branch chain in `assignment()`
  into one ordered table that both the dispatch and the report read, so the
  reported reason is the one that was acted on. Dispatch decisions themselves
  are unchanged.

* **`solver_status_values()` gives the closed set of values `status` can
  take**: `"optimal"`, `"partial"`, `"infeasible"`, `"eps_optimal"`,
  `"iteration_limit"`, `"heuristic"`. Each is documented in terms of what the
  solver terminated on. A status outside the set is now an error at the point a
  result is constructed rather than a string that reaches the caller.

* `match_couples()` and `full_match()` results carry a `status` element from the
  same vocabulary, so the matching layer reports what it achieved rather than
  leaving the caller to infer it from the number of unmatched units. It is
  computed before `return_unmatched = FALSE` and the `info` truncation remove
  the fields it is derived from, and it sits at the top level for that reason.

* Solve results carry a `dispatch` element recording how `method` was chosen,
  and `assignment()` results additionally carry `cardinality`, `n_matched` and
  `unmatched`.

## Bug fixes

* **The min-cost flow search no longer runs forever on a cost matrix with many
  tied entries.** Successive shortest paths terminates because it keeps the
  reduced cost of every residual arc at or above zero. The two directions of one
  arc are priced by expressions that are negatives of each other in exact
  arithmetic and not in floating point, so both can round a few ulps below zero
  at once, and that pair is a cycle of negative reduced cost for the search to
  circle. Ties make it reachable: a large block of equal costs prices a large
  part of the residual graph at zero, where a rounding error in either direction
  decides a comparison.

  It was reached from `match_couples()` with default arguments. A caliper
  problem with no complete matching is re-solved with forbidden entries at a
  finite sentinel, and a matrix that is mostly one sentinel value is exactly the
  tied input; `method = "auto"` sends a wide one to `"sap"`, which is the flow
  solver in assignment orientation. A 4 x 7 instance is enough to reproduce it.

  Where the invariant says a reduced cost cannot be negative, the search now
  reads a negative one as the rounding it is, which leaves the cold first search
  (whose costs may genuinely straddle zero) untouched. A search that outlives
  the bound on how often labels can improve now reports the negative-cost cycle
  it is circling instead of growing its queue until memory runs out.

* **`status` is computed from what the solver terminated on, instead of being
  a literal.** The generic solve paths assigned `status = "optimal"`
  unconditionally, so the field asserted optimality no matter how the solver
  stopped (#28). Three consequences of that are fixed:

  * `network_simplex` reported `"optimal"` when the pivot cap ended the loop
    with an improving arc still available. It now reports `"iteration_limit"`,
    which says the result is feasible and its optimality unproven (#16).

  * `network_simplex` decided infeasibility by looking for unmatched rows after
    the pivot loop, which conflated an input admitting no complete assignment
    with a basis that stopped carrying the flow. Hall's condition is now decided
    before the loop, from a maximum-cardinality matching on the allowed edges,
    and the two cases raise different errors (#16).

  * `full_match()` reported `"optimal"` for results that were not, and units in
    a group that lost its counterpart appeared in neither `groups` nor
    `unmatched` (#20). The status now comes from actual flow against required
    flow and distinguishes `"optimal"`, `"partial"` and `"infeasible"`;
    `unmatched` is the complement of the groups actually emitted, so every unit
    is in exactly one of the two; and `info$n_groups` counts the groups that
    were written rather than the ones the flow solver opened.

* **Constrained matching is now optimal instead of greedy.** When calipers,
  `max_distance` or explicit forbidden pairs left the admissible bipartite
  graph without a complete matching, `match_couples()` caught the solver's
  infeasibility error and returned `greedy_matching(strategy = "sorted")`
  instead. The result was a valid partial matching, so nothing failed and
  nothing warned, but it was not the optimal one and the reported `method`
  still named the optimal solver that had been asked for.

  Such a problem has a lexicographic optimum: the largest number of admissible
  pairs first, then the smallest total distance among matchings of that size.
  couplr now reaches it by replacing forbidden entries with a finite sentinel,
  chosen above `(k + 1)` times the spread of the admissible costs so that one
  sentinel edge outweighs any saving on the real edges, re-solving with the
  requested optimal solver, and dropping the pairs that came back on a
  sentinel edge.

  The difference is not cosmetic. On the `hospital_staff` example with
  `calipers = list(age = 3, experience_years = 2)` and `max_distance = 1.5`,
  which leaves 2,173 admissible pairs out of 60,000, the greedy fallback
  matched 180 of 200 treated units and the optimal solve matches 197.

  Greedy is still used as a last resort when the cost range is too wide for the
  sentinel arithmetic to stay exact in a double, and that case now warns
  explicitly that the result is not optimal. `method = "greedy"` is unaffected.

* `memory_mode = "lazy"` reports the same situation more accurately: recovering
  the partial matching needs the materialized cost matrix that lazy mode exists
  to avoid, so the warning now points at `memory_mode = "dense"` for the
  maximum-cardinality minimum-cost result rather than describing a greedy
  fallback that no longer exists.

## Testing

* New `test-constrained-optimality.R` checks the lexicographic contract against
  exhaustive enumeration over randomised instances with forbidden edges, rather
  than checking only that a matching of the right shape comes back.

* New `test-certificate.R` checks the certificate in both directions. It sweeps
  every solver in the registry over three shapes and three cost kinds, plus
  maximization, negative costs, forbidden edges and both rectangular
  orientations, and requires each answer to certify. It then requires the
  certificate to reject a permuted matching, a matching claiming a column twice,
  a matching using a forbidden pair, an inflated row potential, and a lowered
  potential on an unmatched column. A verifier that only ever returns `TRUE`
  proves nothing, so the rejection cases are what give the accepting cases their
  meaning.

  The sweep matters because `jv` had become the de facto oracle for roughly
  seventeen per-solver test files while being ground-truthed against brute force
  only at `n <= 6`. A certificate is checked against the cost matrix itself and
  does not rely on any solver being right.

* Solver and shape combinations known to return a suboptimal answer are held in
  one registry, `cert_known_suboptimal()`, and are both excluded from the sweep
  and separately asserted to still fail. Fixing one breaks that test, so the
  entry has to be removed rather than quietly masking a bug that came back.

## Known issues

* **`method = "gabow_tarjan"` returns suboptimal assignments on wide (`n < m`)
  problems**; square problems are unaffected (#31). The certification sweep
  found this on its first randomised run: 179 of 200 random wide problems came
  back above the optimum, and a 3 by 6 counterexample returns 20 against a
  brute-forced optimum of 8. The existing rectangular test asserted the number
  of matched rows and nothing about cost, which is why the suite had not caught
  it. `method = "auto"` never dispatches to this solver, so only an explicit
  request is affected.

# couplr 1.5.5

## Performance

* **`method = "auto"` no longer allocates the cost matrix several times over to
  pick a solver.** Selecting a solver needs three data-dependent facts: whether
  any entry is `NaN`, whether the finite entries are constant or binary, and
  what fraction of entries are non-finite. These were read with
  `any(is.nan())`, `range(finite = TRUE)` and
  `mean(is.na() | is.infinite())`, each of which allocates a temporary the size
  of the cost matrix, so choosing a solver cost several full-size allocations
  and several passes before any solving started. A single C++ pass
  (`lap_probe_cost_matrix()`) now returns all of them without allocating. The
  probe itself is 7 to 18 times faster than the code it replaces, measured from
  `n = 10` to `n = 5000`, and the gap between `method = "auto"` and naming the
  solver it selects falls from as much as 2x to a few percent. Dispatch
  decisions are unchanged: the new probe reproduces the previous solver choice
  on every case tested, including all-`Inf`, exactly-half-sparse, constant,
  binary-with-`NA` and integer inputs.

* **Integer cost matrices are no longer coerced during selection.** The probe
  reads `INTSXP` in place, so an integer matrix no longer pays for a full
  double copy on the way to the dispatcher.

## Documentation

* **`?assignment` no longer lists `"line_metric"` as a `method`.** It was never
  one of the accepted values, so `method = "line_metric"` failed `match.arg()`.
  One-dimensional problems are solved by `lap_solve_line_metric()`, which takes
  two point vectors rather than a cost matrix; the documentation now points
  there.

* **The `"auto"` selection rules are documented.** `?assignment` previously
  described `"auto"` only as "automatic selection based on problem
  characteristics". It now states the five rules and the order they are applied
  in, matching the dispatcher.

# couplr 1.5.4

## Bug Fixes

* **`memory_mode = "auto"` no longer under-reads available memory on Apple
  Silicon.** `get_free_ram_mb()` converted `vm_stat`'s page counts with a
  hardcoded 4096-byte page, but Apple Silicon pages are 16384 bytes, so every
  M-series Mac saw a quarter of the memory it actually had -- measured on a
  64 GB M4 Pro, 7.7 GB reported against 41.7 GB available. `"auto"` therefore
  switched to the lazy path, or warned about a dense allocation, at a quarter
  of the intended threshold. The page size is now read from `vm_stat`'s own
  header line, with `sysctl hw.pagesize` as a fallback, and the parsing is
  split into `vm_stat_page_size()` and `vm_stat_available_mb()` so it is
  testable without a macOS host.

* **The macOS memory figure now counts reclaimable pages.** Only the free list
  was counted, which macOS keeps nearly empty by design. Inactive and
  speculative pages are reclaimed on demand, so they are included, matching the
  `MemAvailable` semantics the Linux branch already used. The reported quantity
  is available rather than free memory, and the `memory_mode` warnings say so.

# couplr 1.5.3

## Bug Fixes

* **Fixed a silent integer overflow in the core cost-matrix type.**
  `lap::CostMatrix` computed its flat row-major index as `i * ncol + j` using
  plain 32-bit `int` arithmetic, which wraps (undefined behavior) once
  `nrow * ncol` exceeds `INT_MAX` (~46,341 square) -- a scale the package's
  own vignettes already walk through as a normal example. The same pattern
  was duplicated, uncaught, in several solvers' own raw index arithmetic
  (`auction`, `csa`, `cycle_cancel`, `munkres`, `ramshaw_tarjan`,
  `ssap_bucket`, `sap`/`ssp`, `network_simplex`) and in the Rcpp boundary's
  matrix conversion. All flat-index arithmetic is now computed in 64-bit,
  with a new arithmetic-only regression test covering the exact overflow
  point without allocating an overflow-sized matrix.

## New Features

* **`memory_mode = "lazy"`: compute costs on demand instead of materializing
  the full matrix.** `match_couples()`, `compute_distances()`, and
  `assignment()` now accept `memory_mode = c("auto", "dense", "lazy")`.
  `"lazy"` (with `method = "jv"` or `"auction"`) computes each pairwise
  distance from the underlying feature data as the solver needs it, rather
  than allocating an n_left x n_right matrix up front -- a 100k x 100k
  match needs ~80GB dense, but the underlying 100k x 100 feature data needs
  ~80MB. `"auto"` (the default) estimates the dense matrix's memory
  footprint against free system RAM and switches to lazy (with a warning
  giving concrete GB numbers) before a huge allocation, rather than silently
  crashing or thrashing; `"dense"` skips the check entirely. Supports all
  built-in distance metrics, per-variable calipers, and `max_distance`;
  `replace = TRUE`, `ratio > 1`, `method = "greedy"`, custom distance
  functions, and `full_match()` are not lazy-capable yet and error clearly
  rather than silently falling back to dense or producing a wrong answer.
  Ordinary-sized problems never probe RAM at all (a cheap cell-count
  short-circuit), so this adds no overhead for existing usage.

## Documentation

* **Corrected memory-usage claims for greedy matching.** The vignettes and
  `compute_distances()` docs said greedy matching "computes distances
  on-the-fly" and "avoids the full cost matrix", and that a caliper "creates
  a sparse matrix". None of that is true: `build_cost_matrix()` allocates the
  full n_left x n_right matrix for every `method`, including `"greedy"`, and
  couplr has no sparse matrix representation anywhere. Greedy and calipers
  change how that matrix is solved, not how much memory it takes; blocking is
  the only documented option that actually shrinks the allocation. Also fixed
  the `strategy = "pq"` docs, which called it "memory-efficient" when it
  holds the same candidate pairs as `"sorted"` and only `"row_best"` avoids
  that extra storage.

# couplr 1.5.2

## Performance

* **`method = "ssap_bucket"` is much faster on fine-grained fractional costs.**
  Dial's queue was built as a `std::vector<std::vector<int>>` grown to the
  largest *distance* in the shortest-path tree, so costs needing six decimals
  (scale `1e6`) allocated roughly 15 million bucket vectors per augmentation.
  It is now the textbook circular ring, sized by the largest
  *reduced edge cost* and holding intrusive lists over a pooled arena, which is
  what bounds Dial's memory to `O(maxC)` instead of `O(N * maxC)`. Measured over
  200 randomised solves: 36.0 s to 2.5 s at six decimals, 2.5 s to 0.23 s at
  five. The accepted inputs and returned optima are unchanged.

* **`lap_solve_batch()` honours the check-environment core limit.** With
  `n_threads = NULL` it sized its cluster from `parallel::detectCores()`, which
  reports the physical core count and ignores `_R_CHECK_LIMIT_CORES_`. It now
  uses two workers when that variable is set, and every available core
  otherwise. Both the matrix-list and grouped-data-frame paths read the same
  helper.

## Installation

* **Four unused packages dropped from `Suggests`:** `OpenImageR`, `reticulate`,
  `xml2`, and `farver` had no call site anywhere in the package, tests,
  vignettes, or scripts. `av` is kept: `pixel_morph_animate()` uses it for mp4
  output, guarded at call time.

# couplr 1.5.1

## Installation

* **`RcppEigen` is no longer required to build the package.** It was declared in
  `LinkingTo` but never used: the only Eigen reference in `src/` was the
  `#include <RcppEigen.h>` that `Rcpp::compileAttributes()` emits for each
  `LinkingTo` entry, and no solver instantiated an Eigen type. The declaration
  forced every source install to build RcppEigen first, which was reported as an
  install failure. `LinkingTo` is now `Rcpp` alone; the unused `testthat` entry
  and the `-DEIGEN_NO_DEBUG -DEIGEN_DONT_PARALLELIZE` compile flags are removed
  with it.

* **`htmlwidgets` moved from `Imports` to `Suggests`.** It is used only by
  `lap_animate()`, which now checks for it at call time and errors with an
  install hint if it is missing. This drops about 24 packages from a default
  install, including `knitr`, `rmarkdown`, `bslib`, `sass`, and `tinytex`.

## Documentation

* The README and `paper/paper.md` stated that the assignment is solved on
  RcppEigen. The solvers are hand-written C++ via Rcpp; both now say so.

# couplr 1.5.0

## Breaking changes

* **`greedy_couples()` is removed; greedy matching is now `match_couples(method =
  "greedy")`.** The two functions duplicated ~130 lines of identical scaffolding
  (validation, scaling, id extraction, blocking dispatch, metadata) over the same
  shared engine. They are now one front door: `match_couples()` gains a `method =
  "greedy"` value and a `strategy` argument ("row_best", "sorted", "pq"). Replace
  `greedy_couples(x, strategy = "sorted")` with `match_couples(x, method =
  "greedy", strategy = "sorted")`. The result object and `info$method == "greedy"`
  are unchanged.

## New features

* `pixel_morph()` and `pixel_morph_animate()` gain a `mode = "color_match"`
  option: pixels sharing a quantized colour are matched spatially and any
  remainder falls back to identity. A lighter-weight alternative to the default
  `"color_walk"` palette LAP.

## Bug fixes (statistical / causal-inference layer)

* **`sensitivity_analysis()` no longer scrambles matched pairs.** Outcomes
  were assembled with two independent `merge()` calls, each sorted by its
  own key, so the pair difference subtracted outcomes from mismatched
  pairs and every downstream quantity (Wilcoxon T+, Rosenbaum bounds,
  critical gamma) was computed on a scrambled pairing. Outcomes are now
  looked up by ID, preserving the row-wise pair correspondence (#4).

* **`subclass_match(estimand = "ATE")` weights corrected.** ATE subclass
  weights carried an extra factor of the stratum size, over-weighting large
  subclasses quadratically. A treated unit in subclass k now carries
  `(n_k / N) / n_t` as intended; ATT and ATC were already correct (#5).

* **`balance_diagnostics()` now applies stratum weights for full matching,
  CEM, and subclassification.** The weights were computed and discarded, so
  standardized differences were unweighted for the very estimators whose
  balance is achieved through weighting. Weighted mean, variance, and
  standardized difference are now used on both sides. Also: the variance
  ratio is now a true ratio of variances (matching the conventional 0.5-2
  bounds) rather than a ratio of standard deviations, and the unmatched
  right-unit count no longer goes negative under `ratio > 1` / `replace`
  (it counts distinct matched right units, not pair rows) (#6).

## Bug fixes (solvers)

* **`ssap_bucket` no longer silently rounds fractional costs to a wrong
  optimum.** The integer-scaling step tried only multipliers `{1, 10, 100,
  1000}` and, failing those, rounded at `1000` -- flipping which permutation
  was optimal on costs needing more than three decimals. It now searches
  ascending powers of ten with a fixed (scale-independent) integrality
  tolerance and refuses the problem, redirecting to `method = "jv"` or
  `"auction"`, when no bounded integer scaling is exact. The animation mirror
  `trace_ssap_bucket()` applies the same rule (#19).

* **`lap_solve_line_metric(maximize = TRUE)` now returns the true
  maximum-weight matching.** The DP always built the sorted (minimum-cost)
  pairing and merely negated the total; on a line the maximum-weight
  matching is the anti-monotone pairing. The DP now runs against the
  descending target ordering and returns that assignment and its true
  total (#8).

* **`gabow_tarjan` returns a perfect matching when the diagonal is
  forbidden.** The `C_max == 0` fast path assigned the diagonal without
  checking feasibility, returning an empty matching when the diagonal cells
  were forbidden but a perfect matching existed. It now finds a
  maximum-cardinality matching over the allowed edges via augmenting
  paths (#9).

## Bug fixes (front doors and input handling)

* **`compute_distances(auto_scale = TRUE)` now scales.** It read a
  nonexistent field (making `vars` `NULL`) and disabled scaling under the
  belief it had already happened. It now reads the selected variables and
  forwards the chosen scaling method to the cost builder (#7).

* **Pre-fitted propensity models predict on the supplied data.**
  `ps_match()` and `subclass_match()` called `predict()` without
  `newdata =`, so a `ps_model` fitted on a differently ordered or subset
  frame attached scores to the wrong rows. They now pass
  `newdata = data` (#10).

* **`match_couples(ratio > 1)` falls back to a partial match on
  infeasibility.** The `ratio > 1` path called the solver directly and hard
  errored when constraints forbade every edge of some unit, whereas the 1:1
  path returned a partial matching. Both paths now share the same
  partial-feasibility / greedy fallback (#10).

* **`lap_solve()` honors the `forbidden` sentinel for matrix input, and
  `lap_solve_batch()` preserves singleton-dimension orientation.** The
  matrix path silently ignored a non-`NA` `forbidden`; it now masks matching
  cells as forbidden. A 3-D array slice with a singleton row or column
  dimension was dropped to a vector and transposed; slices are now reshaped
  explicitly (#11).

## Robustness (C++ solver hardening)

Guarded the C++ solvers against silently wrong results and crashes at extreme
scale. None of these affect ordinary inputs; they add error paths and 64-bit
arithmetic where 32-bit overflow or a fixed tolerance could previously produce
a wrong "optimal" or a crash (#13):

* **Overflow / narrowing.** `ssap_bucket` errors clearly when cost magnitudes
  exceed what the integer-bucket solver can represent (rather than overflowing
  the sentinel or allocating an enormous bucket queue); the network-simplex
  iteration bound and `gabow_tarjan`'s bit-scaling range are computed and
  checked in 64-bit; `gabow_tarjan` also rejects costs that collide with its
  forbidden sentinel; the brute-force solver caps total enumeration work
  instead of running unbounded in the number of columns.
* **Large `n*m` indexing.** Flat cost/kernel indexing in
  `prepare_cost_matrix`, `solve_sinkhorn`, and the auction epsilon is done in
  64-bit; `network_simplex` and `lapmod` reject problems whose arc / entry
  counts would overflow a 32-bit index.
* **Tolerances and status.** `solve_munkres` scales its zero tolerance with the
  cost magnitude (a fixed `1e-12` could make a solvable large-cost matrix
  throw); `solve_csa` scales non-integer costs to integers before the
  epsilon-scaling auction, so its optimality guarantee (which assumes integer
  costs) also holds for real-valued inputs with near-tied assignments;
  `full_matching` now reports `infeasible` when the group capacity is below the
  number of units instead of silently dropping units as `optimal`;
  `solve_sinkhorn` reports the correct iteration count on non-convergence.
* **hk01 fallback.** The pure `solve_hk01` now falls back to the exact weighted
  solver (`solve_csflow`) when the zero-cost subgraph of a `{0,1}` matrix has no
  perfect matching, instead of erroring -- matching the Rcpp path that
  `assignment(method = "hk01")` already used.
* **Bounds.** The internal `morph_pixel_level` helpers assert their pixel /
  assignment buffer sizes, matching the exported wrappers.

* **`csa` shipped path now carries the fixes it was tested for.** The Rcpp
  entry point for `method = "csa"` ran a separate copy of the solver that never
  received the integer-scaling fix above, so `assignment(method = "csa")` could
  still return a suboptimal matching on fractional costs. It now delegates to
  the single pure `solve_csa` implementation exercised by the C++ tests. That
  implementation also gained square padding for rectangular problems, which it
  previously solved greedily (and suboptimally). The Rcpp `*_impl` wrappers now
  share one `rcpp_to_cost_matrix` / `lap_result_to_rcpp` conversion pair instead
  of per-file copies (#15).
* **Remaining solvers now ship the tested implementation.** Following `csa`, the
  Rcpp entry points for `sap`/`ssp`, `csflow`, `cycle_cancel`, `push_relabel`,
  `ssap_bucket`, `bruteforce`, `bottleneck`, `hk01`, `network_simplex`, and the
  three `auction` variants each ran a second copy of the algorithm that had
  drifted from the pure `lap::solve_*` exercised by the C++ tests. They now
  delegate to that single pure implementation, so the shipped path and the tested
  path are identical. Each pure copy was checked against brute force over
  randomised integer, fractional, rectangular, maximize, and forbidden-edge
  inputs before its wrapper was pointed at it. `network_simplex` thereby picks up
  the pure copy's `O(n^2)` pivot bound (the shipped copy used the slower
  `O(arcs * nodes)` bound).
* **`auction`, `auction_gs`, and `auction_scaled` now return the exact optimum.**
  The basic and Gauss-Seidel auctions used a single fixed epsilon, which leaves a
  duality-gap slack of up to `n * eps` and returned suboptimal matchings on
  closely-spaced costs (`assignment(method = "auction")` could disagree with
  `jv`); `auction_scaled` additionally threw on some feasible rectangular
  problems with forbidden edges under `maximize`. All three now run one shared
  epsilon-scaling core that scales epsilon down to a tiny final value, recovering
  the exact assignment. `lap_solve_auction_gs()` keeps its `bids` diagnostic.
* **`hk01` maximize.** The pure `solve_hk01` flipped `maximize` by negation,
  turning a `{0,1}` matrix into `{0,-1}`, which its palette check no longer
  recognised as binary -- so it threw on feasible binary maximization problems.
  It now flips via `cmax - c`, preserving the `{0,1}` palette so the fast path and
  the `solve_csflow` fallback engage.
* **Greedy matching wrappers** (`greedy_matching`, `_sorted`, `_row_best`, `_pq`)
  now delegate to the pure `lap::greedy_matching_*`. To keep the shipped
  behaviour identical, the pure copies gained the two tolerances the Rcpp copies
  had and they lacked: they skip the large-finite `BIG` sentinel the matching
  layer uses for forbidden edges (so a row whose only remaining options are
  forbidden is left unmatched rather than paired to a forbidden column), and they
  accept `n > m` by returning a partial matching instead of erroring. Verified
  byte-identical to the previous wrappers over 400 randomised cases spanning
  integer ties, `NA`/`BIG`-forbidden edges, and rectangular shapes. The three
  per-strategy Rcpp exports (`greedy_matching_sorted` / `_row_best` / `_pq`) were
  folded into the single `greedy_matching(strategy = ...)` dispatcher they
  duplicated; `match_couples(method = "greedy", strategy = ...)` is the
  user-facing verb.

## Tests

* Added a parameter-recovery and coverage suite for the statistical layer
  (`test-statistical-recovery.R`): sensitivity pair alignment, prefitted-PS
  row alignment, propensity-matching imbalance reduction, ATE subclass weight
  values, weighted-balance means, known-effect recovery across seeds, and
  nominal coverage of matched-pair confidence intervals (#14).
* Added a randomised ground-truth harness (`cpp_tests/tests/test_ground_truth.cpp`)
  that compares every optimal pure solver against brute-force enumeration over
  thousands of integer, fractional, rectangular, maximize, and forbidden-edge
  matrices (bottleneck against a brute-force minimax). This is the gate that
  decides whether a solver's Rcpp wrapper may delegate to the pure copy, and it
  is what surfaced the `auction` and `hk01` bugs above.

# couplr 1.4.1

## Bug fixes (solver stalls on constrained matching)

Fixes two solver paths that could stall indefinitely on `match_couples()`
inputs with `max_distance`, calipers, or other forbidden-edge constraints.
These stalls caused the M1mac and linux-arm64 additional CRAN checks for
1.4.0 to hit the 1.5-hour test timeout.

* **Forbidden-cell marker is now `Inf` instead of a large finite value.**
  `apply_max_distance()`, `apply_calipers()`, and `mark_forbidden_pairs()`
  previously wrote a large finite `BIG_COST` into forbidden cells. The
  Jonker-Volgenant and small-`n` SSP solvers treated `BIG_COST` as a
  regular expensive edge and could degenerate on sparse, near-square
  inputs instead of short-circuiting on infeasibility. Switched to `Inf`
  so the C++ solvers' non-finite check fires.

* **Auto-dispatch no longer routes sparse inputs through SSP for small `n`.**
  Previously `lap_solve()` with `method = "auto"` selected `"sap"`
  (`lap_solve_ssp`) for sparse matrices with `n <= 100`. SSP has its own
  worst-case stall on near-square, highly-sparse cost matrices. All
  sparse inputs now go through `lapmod` regardless of size.

* **`match_couples()` now drops fully-forbidden rows/columns before LAP.**
  `match_couples()` and `.couples_from_distance()` route through a new
  internal `.solve_with_partial_feasibility()` helper. It removes rows
  and columns with no allowed edges before the LAP call and falls back
  to `greedy_matching()` if the optimal solver still cannot find a
  perfect matching on the feasibility-pruned submatrix (Hall's-condition
  violation). Dropped rows/columns are returned as unmatched, preserving
  the partial-matching semantics that tests with tight `max_distance` /
  caliper constraints already expected.

## Other fixes

* **`jv_core`:** drop the same-pass reprocess in `AUGMENTING ROW REDUCTION`.
  The reprocess could revisit a freshly-reduced row in the same pass and
  delay convergence on degenerate inputs without changing the final
  assignment.

# couplr 1.4.0

## Animation coverage

* **`lap_animate()` now covers every method that `assignment()` accepts.**
  Ten new step-by-step traces ship: `auction_gs`, `ramshaw_tarjan`,
  `ssap_bucket`, `hk01`, `csflow`, `cycle_cancel`, `push_relabel`, `csa`,
  `orlin`, `network_simplex`. `animated_methods()` returns all 20 method
  strings.
* **Per-frame parity testing.** Every registered trace is exercised by a
  parametric `testthat` suite (`tests/testthat/test-trace-parity.R`) on a
  battery of small cost matrices including forbidden cells. Each frame's
  matching is validated for in-range entries, no double-bookings, and no
  use of forbidden edges; the final-frame total is compared to the C++
  oracle within tolerance.
* **Shared trace infrastructure.** New internal helpers
  `R/trace_helpers_frame.R` (`make_frame()`, `make_meta()`,
  `prepare_cost_work()`, `matching_total_cost()`, `validate_cost_input()`)
  and `R/trace_helpers_mcf.R` (min-cost-flow graph, residual edges,
  Dijkstra with Johnson potentials, Bellman-Ford, negative-cycle finder,
  push/extract). Used by all min-cost-flow traces.

## Bug fixes (correctness)

* **`prepare_cost_matrix.cpp`:** entries equal to `+Inf` were treated as
  regular very-large costs rather than forbidden, which made `cmax`
  become `Inf` and silently skipped the `maximize` flip. Result:
  `assignment(method = X, maximize = TRUE)` on matrices containing
  `Inf` returned the *minimizing* answer for any solver routing through
  `prepare_cost_matrix_impl` (`auction`, `auction_scaled`, `sap`,
  `csflow`, `hk01`, `bruteforce`). Now `NA` and any non-finite value are
  marked forbidden consistently.
* **`lap_solve_orlin` and `lap_solve_network_simplex_wrapper`:** the
  R-side wrapper used `work[is.na(work)] <- Inf` which missed the `-Inf`
  produced by negating `+Inf` in maximize mode, letting forbidden cells
  slip through as extreme-cost real edges. Fixed to
  `work[!is.finite(work)] <- Inf`.
* **`network_simplex` initial spanning tree:** the greedy initialiser in
  `ns_init.h` built a *partial* matching (any row that couldn't claim a
  fresh column was left unmatched) and connected unmatched columns to row
  0. The resulting starting basis violated flow conservation, and pivots
  could not recover a perfect matching even when one existed - e.g. on a
  5x5 cost matrix with two forbidden cells under `maximize`,
  `assignment(method = "network_simplex")` returned an infeasible result
  with one row unmatched. Fixed by adding an augmenting-path repair after
  the greedy pass: every still-unmatched row runs BFS for an augmenting
  path on the allowed-edge bipartite graph, extending the initial matching
  to a perfect matching whenever one exists.

# couplr 1.3.3

## Solver internals

* **Hungarian split into O(n^3) SAP + O(n^4) Munkres.** `method = "hungarian"`
  now uses the shortest-augmenting-path solver shared with JV; the original
  O(n^4) Munkres implementation remains available as `method = "munkres"`.
  At n = 2000 the new Hungarian runs orders of magnitude faster than 1.3.2.
* **LAPJV warm-start (column reduction + augmenting reduction) added to the
  JV core for square inputs.** Reduces JV / duals solve time at n >= 500.
* **CSA shares dual potentials across epsilon-scaling phases.** Removes the
  cold restart between phases that previously dominated CSA runtime at
  n >= 500.
* **Auction tie-breaker tweak cached in `auction` and `auction_gs`.**
  Cleaner inner loop; no behaviour change.
* **`solve_auction_scaled` collapsed into a thin wrapper over
  `scaled_params`** (~200 lines removed); behaviour identical.
* **Gabow-Tarjan**: bucket-array Step 2 reinstated per the 1989 paper
  (G&T's `r > bn` pruning is the algorithm, not a wart); added the 6n
  pruning heuristic from p.9.

## Documentation

* `paper/benchmark-table.csv` and `paper/scaling-results.csv` re-measured
  on the current development machine for n <= 2000 (per-method table) and
  n_total <= 2000 (cross-package table). Larger-n rows in both files are
  carried over from the previous machine and not directly comparable.

# couplr 1.3.2

## Test infrastructure

* Resubmission of 1.3.1 to address a win-builder r-devel pretest failure
  (exit code -1073741819 / access violation) in
  `test-lap-solve-batch-coverage.R`. Debian r-devel, local r-release, and
  local `R CMD check --as-cran` all pass; the crash did not reproduce off
  win-builder.
* Disabled testthat parallel execution (`Config/testthat/parallel: true`
  removed from DESCRIPTION) to eliminate cross-file worker-state leakage as
  a possible cause of the win-builder crash.
* Added a defensive `skip_on_cran()` at the top of
  `test-lap-solve-batch-coverage.R`. Equivalent coverage is exercised
  off-CRAN by `test-lap-solve-batch-coverage-2.R`,
  `test-lap-solve-batch-coverage-3.R`, `test-lap-solve-batch-extended.R`,
  `test-batch-coverage-final.R`, `test-batch-processing.R`, and
  `test-batch-kbest-extended.R`.

# couplr 1.3.1

## Behaviour changes

* **Mahalanobis distance now uses the pooled within-group covariance by
  default.** Previously the default was the overall-sample covariance of
  `rbind(left, right)`. The pooled within-group estimator
  `((n_L-1)*S_L + (n_R-1)*S_R) / (n_L+n_R-2)` is the convention used by
  `optmatch::match_on()` and aligns Mahalanobis behaviour across the matching
  packages a user is likely to compare against. Users who relied on the old
  default can recover it explicitly with
  `match_couples(..., sigma = cov(rbind(left[, vars], right[, vars])))`.
  The previous docstring already documented the default as "pooled
  covariance"; this release makes the code match the documentation.

---

# couplr 1.3.0

## New Features

### Optimal Full Matching

* **`full_match()` gains `method = "optimal"` (new default)** using a min-cost
  max-flow solver (Dijkstra + Johnson potentials) that finds the globally
  optimal group assignment minimizing total distance:
  - Standard lower bound transformation enforces `min_controls` per group
  - Automatic transposition when `n_left > n_right`
  - New C++ solver: `solve_full_matching.cpp` (self-contained MCMF)
  - `method = "greedy"` preserved for fast approximate matching

### Vignette Updates

* **Getting Started**: Added full matching section with `full_match()` example
* **Matching Workflows**: New "Full Matching (Variable-Ratio Groups)" section
  covering optimal vs greedy, constraints, weights, and comparison table
* **Comparison**: Updated feature table and all sections to reflect couplr's
  full matching support (previously listed as "No")

---

# couplr 1.2.0

## New Features

### Full Matching

* **New `full_match()` function** assigns every unit to a matched group
  with variable ratios (1:k or k:1):
  - Greedy group formation: match each left to nearest right, then assign
    remaining right units to nearest matched left
  - Caliper support: `caliper` (absolute) or `caliper_sd` (SD-based)
  - Control group size constraints: `min_controls`, `max_controls`
  - Weights inversely proportional to group size
  - Returns `full_matching_result` S3 class

### Coarsened Exact Matching (CEM)

* **New `cem_match()` function** implements coarsened exact matching:
  - Coarsens continuous variables into bins (Sturges, FD, Scott, or custom)
  - Exact matching on coarsened values with stratum-based weights
  - Support for categorical grouping variables via `grouping` parameter
  - Custom cutpoints per variable via `cutpoints` parameter
  - Returns `cem_result` S3 class with matched units and strata summary

### Subclassification

* **New `subclass_match()` function** divides units into propensity score
  strata:
  - Quantile-based stratification with configurable number of subclasses
  - Supports pre-computed PS, pre-fitted models, or formula interface
  - Target estimands: ATT, ATE, ATC with appropriate weighting
  - Returns `subclass_result` S3 class with subclass summary

### Output Layer & Ecosystem Integration

* **New `match_data()` generic** converts any couplr result to analysis-ready
  format with `treatment`, `weights`, `subclass`, and `distance` columns.
  Methods for all result types (matching, full, CEM, subclass).
* **New `as_matchit()` converter** creates `matchit`-class objects from couplr
  results, enabling interop with cobalt, marginaleffects, and other MatchIt
  ecosystem packages.
* **cobalt `bal.tab()` methods** for all couplr result types. Requires
  cobalt package (in Suggests).

### Mahalanobis Distance Improvements

* **Robust singularity check** using `rcond()` instead of fragile `det() == 0`
* **Custom `sigma` parameter** in `match_couples()`, `greedy_couples()`, and
  `compute_distance_matrix()` for user-supplied covariance matrices
* **Vectorized computation** replacing nested R for-loops for ~10x speedup

### S3 Generics

* `balance_diagnostics()` and `join_matched()` are now S3 generics with
  methods for all result types. Existing code is 100% backward-compatible.

### New Functions

* `full_match()` - Variable-ratio full matching
* `cem_match()` - Coarsened exact matching
* `subclass_match()` - Propensity score subclassification
* `match_data()` - Unified analysis-ready output
* `as_matchit()` - Convert to MatchIt format

---

# couplr 1.1.0

## New Features

### Ratio and Replacement Matching

* **k:1 ratio matching** via `ratio` parameter in `match_couples()` and
  `greedy_couples()`. Matches k control units to each treated unit by
  replicating the cost matrix, then deduplicates assignments.
* **With-replacement matching** via `replace` parameter. Each treated unit
  independently selects its nearest control, allowing controls to be reused
  across multiple treated units.

### Propensity Score Matching

* **New `ps_match()` function** wraps `match_couples()` with logistic regression:
  - Accepts a formula or pre-fitted `glm` object
  - Matches on the logit of propensity scores with a caliper
  - Default caliper: 0.2 SD of logit(PS) (Rosenbaum and Rubin recommendation)
  - Returns matching_result with PS model metadata

### Cardinality Matching

* **New `cardinality_match()` function** maximizes sample size subject to
  balance constraints:
  - Starts with a full optimal match, then iteratively prunes imbalanced pairs
  - Balance threshold via `max_std_diff` (default: 0.1 for excellent balance)
  - Configurable pruning speed with `batch_fraction`
  - Returns pruning diagnostics: iterations, pairs removed, final balance

### Sensitivity Analysis

* **New `sensitivity_analysis()` function** implements Rosenbaum bounds:
  - Tests sensitivity of matched comparisons to hidden bias
  - Uses Wilcoxon signed-rank statistic with upper/lower p-value bounds
  - Reports critical gamma (smallest gamma at which significance is lost)
  - S3 methods: `print()`, `summary()`, `plot()`

### Visualization

* **`autoplot()` methods** for ggplot2-based visualizations (requires ggplot2):
  - `autoplot.matching_result()`: histogram, density, or ecdf of distances
  - `autoplot.balance_diagnostics()`: love plot, histogram, or variance ratio plot
  - `autoplot.sensitivity_analysis()`: gamma vs p-value curve
* **Enhanced `summary.matching_result()`** now reports match rate and distance
  percentiles

### New Functions

* `ps_match()` - Propensity score matching with logit caliper
* `cardinality_match()` - Balance-constrained cardinality matching
* `sensitivity_analysis()` - Rosenbaum bounds sensitivity analysis

### Tests

* Added 58 new tests across 7 test files
* All 4916 tests passing across platforms

---

# couplr 1.0.7

## Bug Fixes

* Fixed undefined behavior (UB) in Gabow-Tarjan algorithm: replaced left bit-shift
  of potentially negative values with multiplication to avoid sanitizer errors
  on M1-SAN checks
* Fixed namespace conflict with `select()` in vignettes by using explicit
  `dplyr::select()` to prevent masking by MASS or other packages

---

# couplr 1.0.6

## Documentation

* Added Overview section to algorithms vignette with audience and prerequisites
* Fixed workflow diagram dark mode text handling in matching-workflows vignette
* Improved SVG theme-awareness for multi-line text labels
* Removed grid lines from matching-workflows plots for cleaner appearance
* Added threshold labels to balance comparison plot

---

# couplr 1.0.0

## Major New Features (2025-11-19 Update)

### Automatic Preprocessing and Scaling

The package now includes intelligent preprocessing to improve matching quality:

* **New `auto_scale` parameter** in `match_couples()` and `greedy_couples()` enables automatic preprocessing
* **Variable health checks** detect and handle problematic variables:
  - Constant columns (SD = 0) are automatically excluded with warnings
  - High missingness (>50%) triggers warnings
  - Extreme skewness (|skewness| > 2) is flagged
* **Smart scaling method selection** analyzes data and recommends:
  - "robust" scaling using median and MAD (resistant to outliers)
  - "standardize" for traditional mean-centering and SD scaling
  - "range" for min-max normalization
* New `preprocess_matching_vars()` function for manual preprocessing control
* Categorical variable encoding for binary and ordered factors

### Balance Diagnostics

Comprehensive tools to assess matching quality:

* **New `balance_diagnostics()` function** computes multiple balance metrics:
  - Standardized differences: (mean_left - mean_right) / pooled_sd
  - Variance ratios: SD_left / SD_right
  - Kolmogorov-Smirnov tests for distribution comparison
  - Overall balance metrics (mean, max, % large imbalance)
* **Quality thresholds** with interpretation:
  - |Std Diff| < 0.10: Excellent balance
  - |Std Diff| 0.10-0.25: Good balance
  - |Std Diff| 0.25-0.50: Acceptable balance
  - |Std Diff| > 0.50: Poor balance
* Per-block statistics with quality ratings when blocking is used
* `balance_table()` creates publication-ready formatted tables
* Informative print methods with interpretation guides

### Joined Matched Dataset Output

Create analysis-ready datasets directly from matching results:

* **New `join_matched()` function** automates data preparation:
  - Joins matched pairs with original left and right datasets
  - Eliminates manual data wrangling after matching
  - Select specific variables via `left_vars` and `right_vars` parameters
  - Customizable suffixes (default: `_left`, `_right`) for overlapping columns
  - Optional metadata: `pair_id`, `distance`, `block_id`
  - Works with both optimal and greedy matching
* **Broom-style `augment()` method** for tidymodels integration:
  - S3 method following broom package conventions
  - Sensible defaults for quick exploration
  - Supports all `join_matched()` parameters
* **Flexible output control**:
  - `include_distance` - Include/exclude matching distance
  - `include_pair_id` - Include/exclude sequential pair IDs
  - `include_block_id` - Include/exclude block identifiers
  - Custom ID column support via `left_id` and `right_id`
  - Clean column ordering: pair_id → IDs → distance → block → variables

### Precomputed and Reusable Distances

Performance optimization for exploring multiple matching strategies:

* **New `compute_distances()` function** precomputes and caches distance matrices:
  - Compute distances once, reuse across multiple matching operations
  - Store complete metadata: variables, distance metric, scaling method, timestamps
  - Preserve original datasets for seamless integration with `join_matched()`
  - Enable rapid exploration of different matching parameters
  - Performance improvement: ~60% faster when trying multiple matching strategies
* **Distance objects** (S3 class `distance_object`):
  - Self-contained: cost matrix, IDs, metadata, original data
  - Works with both `match_couples()` and `greedy_couples()`
  - Pass as first argument instead of datasets: `match_couples(dist_obj, max_distance = 5)`
  - Informative print and summary methods with distance statistics
* **Constraint modification** via `update_constraints()`:
  - Apply new `max_distance` or `calipers` without recomputing distances
  - Creates new distance object following copy-on-modify semantics
  - Experiment with different constraints efficiently
* **Backward compatible integration**:
  - Modified function signatures: `match_couples(left, right = NULL, vars = NULL, ...)`
  - Automatically detects distance objects vs. datasets
  - All existing code continues to work unchanged

### Parallel Processing

Speed up blocked matching with multi-core processing:

* **New `parallel` parameter** in `match_couples()` and `greedy_couples()`:
  - Enable with `parallel = TRUE` for automatic configuration
  - Specify plan with `parallel = "multisession"` or other future plan
  - Works with any number of blocks - automatically determines if beneficial
  - Gracefully falls back if future packages not installed
* **Powered by the `future` package**:
  - Cross-platform support (Windows, Unix/Mac, clusters)
  - Respects user-configured parallel backends
  - Automatic worker management
  - Clean restoration of original plan after execution
* **Performance**:
  - Best for 10+ blocks with 50+ units per block
  - Speedup scales with number of cores and complexity
  - Minimal overhead for small problems
* **Integration**:
  - Works with all blocking methods (exact, fuzzy, clustering)
  - Compatible with distance caching from Step 4
  - Supports all matching parameters (constraints, calipers, scaling)

### Fun Error Messages and Cost Checking

Like testthat, couplr makes errors light, memorable, and helpful with couple-themed messages:

* **New `check_costs` parameter** (default: `TRUE`) in `match_couples()` and `greedy_couples()`:
  - Automatically checks distance distributions before matching
  - Provides friendly, actionable warnings for common problems
  - Set to `FALSE` to skip checks in production code
* **Fun couple-themed error messages** throughout the package:
  - 💔 "No matches made - can't couple without candidates!"
  - 🔍 "Your constraints are too strict. Love can't bloom in a vacuum!"
  - ✨ Helpful suggestions: "Try increasing max_distance or relaxing calipers"
  - 💖 Success messages: "Excellent balance! These couples are well-matched!"
* **Automatic problem detection**:
  - **Too many zeros**: Warns about duplicates or identical values (>10% zero distances)
  - **Extreme costs**: Detects skewed distributions (99th percentile > 10x the 95th)
  - **Many forbidden pairs**: Warns when constraints eliminate >50% of valid pairs
  - **Constant distances**: Alerts when all distances are identical
  - **Constant variables**: Detects and excludes variables with no variation
* **New diagnostic function** `diagnose_distance_matrix()`:
  - Comprehensive analysis of cost distributions
  - Variable-specific problem detection
  - Actionable suggestions for fixes
  - Quality rating (good/fair/poor)
* **Emoji control**: Disable with `options(couplr.emoji = FALSE)` if preferred
* **Philosophy**: Errors should be less intimidating, more memorable, and provide clear guidance

### New Functions

* `preprocess_matching_vars()` - Main preprocessing orchestrator
* `balance_diagnostics()` - Comprehensive balance assessment
* `balance_table()` - Formatted balance tables for reporting
* `join_matched()` - Create analysis-ready datasets from matching results
* `augment.matching_result()` - Broom-style interface for joined data
* `compute_distances()` - Precompute and cache distance matrices
* `update_constraints()` - Modify constraints on distance objects
* `is_distance_object()` - Type checking for distance objects
* `diagnose_distance_matrix()` - Comprehensive distance diagnostics
* `check_cost_distribution()` - Check for distribution problems
* Added robust scaling method using median and MAD

### Documentation & Examples

* `examples/auto_scale_demo.R` - 5 preprocessing demonstrations
* `examples/balance_diagnostics_demo.R` - 6 balance diagnostic examples
* `examples/join_matched_demo.R` - 8 joined dataset demonstrations
* `examples/distance_cache_demo.R` - Distance caching and reuse examples
* `examples/parallel_matching_demo.R` - 7 parallel processing examples
* `examples/error_messages_demo.R` - 10 fun error message demonstrations
* Complete implementation documentation (claude/IMPLEMENTATION_STEP1.md through STEP6.md)
* All functions have full Roxygen documentation

### Tests

* Added 34+ new tests (10 for preprocessing, 11 for balance diagnostics, 13 for joined datasets, tests for distance caching)
* All tests passing with full backward compatibility

## Major Changes (Initial 1.0.0 Release)

### Package Renamed: lapr → couplr

The package has been renamed from **lapr** to **couplr** to better reflect its purpose as a general pairing and matching toolkit.

**couplr** = Optimal pairing and matching via linear assignment

### Clean 1.0.0 Release

First official stable release with clean, well-organized codebase.

## New Organization

### R Code
- Eliminated 3 redundant files
- Consistent `morph_*` naming prefix
- Two-layer API: `assignment()` (low-level) + `lap_solve()` (tidy)
- 10 well-organized files (down from 13)

### C++ Code  
- Modular subdirectory structure:
  - `src/core/` - Utilities and headers
  - `src/interface/` - Rcpp exports
  - `src/solvers/` - 14 LAP algorithms
  - `src/gabow_tarjan/` - Gabow-Tarjan solver
  - `src/morph/` - Image morphing

## Features

### Solvers
Hungarian, Jonker-Volgenant, Auction (3 variants), SAP/SSP, SSAP-Bucket, Cost-scaling, Cycle-cancel, Gabow-Tarjan, Hopcroft-Karp, Line-metric, Brute-force, Auto-select

### High-Level
✅ Tidy tibble interface
✅ Matrix & data frame inputs  
✅ Grouped data frames
✅ Batch solving + parallelization
✅ K-best solutions (Murty, Lawler)
✅ Rectangular matrices
✅ Forbidden assignments (NA/Inf)
✅ Maximize/minimize
✅ Pixel morphing visualization

## API

- `lap_solve()` - Main tidy interface
- `lap_solve_batch()` - Batch solving
- `lap_solve_kbest()` - K-best solutions
- `assignment()` - Low-level solver
- Utilities: `get_total_cost()`, `as_assignment_matrix()`, etc.
- Visualization: `pixel_morph()`, `pixel_morph_animate()`

---

*Development history under "lapr" available in git log before v1.0.0.*
