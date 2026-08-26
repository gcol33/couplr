# Round 1 dispositions

One decision per batch. Item numbers follow `review/round1.md`.
Default is accept. Rejections carry a load-bearing reason.

---

## Decisions only you can make

Four things gate the rest of the round.

**Answered 2026-08-25:** D1 route 2, D2 rename, D3 ship 1.6.2, D4 resolved by
reading the guidance.

### D1. What the certificate proves (drives 1.1, 1.5, and the title)

The verifier is double precision with a `1e-9` tolerance (`R/lap_certify.R:107`), so
the current blanket claim is not carried by the implementation. Three routes:

1. **Retitle only.** Change "provably optimal" to tolerance-qualified language
   everywhere. Cheapest. Leaves the strongest sentence in the paper weaker than the
   package could support.
2. **Exact verification for integer and rational costs, and retitle.** Recommended.
3. **Exact verification, keep the title.** Not defensible: double-cost problems
   still get a numerical certificate only, so an unqualified title still overstates
   on part of the input domain.

Why route 2. Verification is where exactness is cheap. Given integer costs, a
candidate `X` and duals `(u, v)`, checking `u_i + v_j <= C_ij`, complementary
slackness and objective equality is integer arithmetic with no tolerance at all.
The solver does not need exact arithmetic; only the checker does. The package
already scales reals to integers for `gabow_tarjan`, so the scaling path exists and
is the same path item 1.5 says is underspecified. One piece of work closes both:
an exact integer checker plus a stated scaling rule with an overflow guard. After
that the paper can say "proved, in exact arithmetic, for integer and rational
costs" and "verified to a stated tolerance" for doubles, and both sentences are
true.

The title still changes under route 2. The reviewer's suggestion works:
*couplr: Optimal Matching with Verifiable Certificates and Sparse Edge Generation*.

**Decided: route 2.** Exact verification for integer and rational costs, and a
retitle.

**Title decided 2026-08-25:**

> couplr: Optimal Matching with Verifiable Certificates and Sparse Edge Generation

It keeps the two distinguishing contributions and drops the overbroad claim. Applied
to `rjournal.Rmd`. The body sweep over "provably", "proof", "exact" and "certified"
landed with the rest of batch A on 2026-08-25; see batch A below.

### D2. The `orlin` solver

Not raised in the review. `R/trace_orlin.R:1-20` records that the production
`orlin` method is plain successive shortest paths, not Orlin–Ahuja. The paper
advertises nineteen algorithms and Figure 1 plots this one under its literature
name.

Options: implement the real scaling algorithm; rename the method and say what it is;
or remove the name. Renaming or removing a documented method is a breaking change
for a package already on CRAN, so it needs a deprecation path and a NEWS entry.

**Decided: rename, done.** The method is `"sap_dense"`. `"orlin"` is gone from the
`method` vector, so a call passing it fails through `match.arg()` with the valid
list rather than resolving through an alias. `NEWS.md` carries the reason under
Breaking changes. Five headers in the old solver directory (`orlin_scaling.h`,
`orlin_bidding.h`, `orlin_prices.h`, `orlin_ssp.h`, `orlin_types.h`) were included
by nothing and are deleted. The unused `alpha` and `auction_rounds` parameters are
gone from the C++, the Rcpp export and the R wrapper.

### D3. The release the paper is pinned to

`DESCRIPTION` says 1.6.2, the newest tag is `v1.6.1`, the bibliography and the
website say 1.5.5. A reviewer cannot install what the paper claims to have run.

**Decided: ship 1.6.2.** The paper then matches CRAN as claimed. Sequencing: the
rename is a breaking change, so it goes out in this release, and the release has to
clear `cran-check` and a blocking `check_win_devel()` before upload. Bibliography,
website and git tag are swept to 1.6.2 in batch I.

### D4. Where the expanded benchmarks live

§3 asks for a factorial grid, multiple instances per cell, memory measurements and
adversarial cases. The paper is at 20 pages, the hard limit. Check what the R Journal
allows as supplementary material for a package article before this is planned; do
not assume. If supplementary material is allowed, the full grid goes there and the
paper keeps a summary. If not, something in the current text is cut to make room.

**Resolved: supplementary material is allowed, so no cut list is needed.** The
submission guidance lists among the required files "Other supplementary files that
contain additional technical details or examples"
(<https://journal.r-project.org/submissions.html>, read 2026-08-25). The factorial
grid, the memory measurements and the adversarial implicit cases go there; the paper
keeps a summary within its 20 pages.

Two things that page does confirm, both matching the review: "Articles for the R
Journal should be no more than 20 pages" and "the abstract no more than 250 words".

The ten-minute rule is on the same page and is a real requirement: a submission
must reproduce in "a reasonable time (less than 10 minutes)", and the guidance
recommends supplying intermediate outputs for computationally intensive work. The
quick and full modes are therefore compliance rather than good practice. The default
paper reproduction reads the supplied intermediates and finishes inside the limit,
and the full benchmark suite may take longer, which the guidance allows. State this
directly in the round-2 reply.

---

## A. Exactness claim and its cascade — 1.1, 1.5

**ACCEPT. Done 2026-08-25.** See `dev_notes/review1/findings.md` for the design,
the measurements and the one thing the first draft of the prose got wrong.

What landed: `verify_assignment()` gains `arithmetic`, taking `"auto"` (the
default), `"exact"` and `"double"`. Every condition is the sign of
`c_ij - u_i - v_j`, and `src/core/lap_exact.h` decides that sign exactly for any
finite doubles, behind a rounding-error filter so the expansion runs on the
pairs near tightness rather than on all `nm`. The exact conclusion drops the
objective comparison in favour of `all_rows_matched`, which is what objective
equality reduces to once the other conditions hold exactly, and this is also
what 1.3 asks the article to say. The exact conditions imply the numerical ones
at any non-negative `tol`, so `certified_optimal` under `"auto"` is unchanged
and nothing in the suite moved: 8783 R assertions and 328 C++ test cases pass.

Availability was measured rather than asserted, ten instances per cell: integer
and uniform costs gave an exact certificate on every instance at three sizes,
both orientations and eleven solvers; computed Euclidean distances gave a
numerical one on every instance, missing exact tightness by a relative `2e-16`
to `5e-15`. The article states both.

1.5 is stated in `assignment()` and in the article, read out of
`solve_gabow_tarjan.cpp` rather than described from outside: the scale factor,
the rounding rule, the refusal condition and its limit, the instance whose
optimum is claimed, and the bound between the rounded instance's optimum and
the original's.

Batch C left one thing behind that this batch found: `cpp_tests` still carried
`test_orlin.cpp` including a deleted header, so the C++ harness did not build.
It is `test_sap_dense.cpp` now, and the test case exercising the removed `alpha`
and `auction_rounds` parameters is gone.

Still open in this cascade: the abstract's own wording and its 255-word length,
which batch I owns.

**The article is 21 pages after this batch, against a hard limit of 20.** The
additions are what the review asked for and cannot come back out. The page is
owed to batch I's abstract cut and to batch G moving the benchmark grids to
supplementary material. Re-measure after both; a page still over then makes
D4's cut list necessary after all.

The claim outruns the implementation and the reviewer is right that the
paper acknowledges this once, on page 3, then drops the qualification everywhere
else.

Blocked on D1. Once decided, sweep: title, abstract, contribution list, the
"Certifying a solution" section, the conclusion, `man/assignment.Rd`, roxygen in
`R/lap_solve.R` and `R/lap_certify.R`, and the website. Grep for "provably",
"proof", "exact" and "certified" and settle each occurrence against the vocabulary
chosen in D1. The paper must not use two of those words to mean the same thing.

For 1.5 specifically, the text must state the scaling factor, the rounding rule,
the resulting error bound, whether optimality is claimed for the scaled or the
original instance, the overflow guard, and the condition under which conversion is
refused. All of these are properties of code that already exists; read it rather
than describing it from the outside.

## B. The LP formalism — 1.2, 1.3

**ACCEPT both. Done 2026-08-26.** Cheap and uncontestable.

State the dual objective `max sum u_i + sum v_j` subject to `u_i + v_j <= C_ij`,
`v_j <= 0`. Split the current display so dual feasibility and complementary
slackness are separate, and give the unmatched-column condition `v_j = 0` its own
line, since the paper's own explanation of that half is one of its better passages.

On 1.3, call them four reported checks. Objective equality follows from the other
three in exact arithmetic; the software computes it as an independent numerical
cross-check, which is worth saying plainly.

## C. Complexity statements and solver honesty — 1.4, 1.7, plus D2 and the alias

**ACCEPT. Done 2026-08-25**, except the figure, which needs a rebuild.

**ACCEPT.** The reviewer found a real error and half of a second one.

`rjournal.Rmd:284` writes `O(sqrt(n) m log(n Cmax))` with `n` and `m` already bound
at `rjournal.Rmd:583` to treated and control counts. Restate the bound in the
source's own symbols, `O(sqrt(|V|) |E| log(|V| N))`, then substitute
`|V| = n + m`, `|E| = nm` explicitly. Note that the review's own display of this
bound renders as `sqrt(|V||E|)`, which is a paste artefact; the published form is
the one above. Fix `man/assignment.Rd:59` and `R/lap_solve.R:45` in the same pass,
where the bound currently appears as `O(n^3 log C)`.

For 1.7, `hk01` needs its construction stated: how a `{0,1}` cost assignment reduces
to maximum-cardinality matching on the zero-cost edges, and why completing the
matching arbitrarily afterwards is optimal. Read the C++ before writing the
sentence.

On the alias question the answer is straightforward and should just be given: the
public `method` argument accepts 20 names besides `"auto"`, `ssp` resolves to `sap`
at `R/lap_solve.R:212`, so there are 19 implementations. Whether it stays 19 depends
on D2.

## D. Formal statements for the central contribution — 1.6, 1.9

**ACCEPT. Done 2026-08-26.** This is the batch that most improves the paper.

The adaptive edge-generation loop is the contribution, and it currently reaches the
reader as narrative. Give it a proposition, a proof in exact arithmetic, the
tolerance-qualified version, and a finite-termination argument. State the worst case
honestly: the loop may add every edge. Give the per-sweep work as `O(nm)` and the
storage bound in final candidate edges. Say what happens when the restricted graph
starts infeasible, how violated edges are batched, and how ties break.

For 1.6, the sentinel argument needs its formula written out and a lemma showing
that swapping a sentinel edge for any combination of real edges improves the
lexicographic objective. Handle negative costs, maximisation by negation, and
sentinel overflow, all of which the current informal statement leaves open.

Costs pages. Blocked on D4.

## E. What the verifier is independent of — 1.8

**ACCEPT.** `R/lap_certify.R:228` calls `assignment_duals()` when duals are absent,
which the manual already documents at lines 49-50; the paper does not.

One paragraph: verifying externally supplied duals is independent of whatever
produced them, and verifying a bare `assignment()` result runs a second couplr solve
and then checks residuals. Reserve "third-party check" for the first case. This
costs nothing and removes a claim a reviewer would otherwise keep pulling at.

## F. Related work — 2.1 through 2.6

**ACCEPT. Done 2026-08-26.** The novelty boundary as drawn is too wide, and the narrower claim the
reviewer offers is both true and more convincing: existing approaches build a
prespecified sparse network, couplr expands a restricted graph until complete-graph
dual feasibility is verified.

Name column generation explicitly. The mechanism is textbook column generation and
saying so costs nothing while removing the largest single objection in §2. The
software contribution stands on its own.

Read every source before its sentence exists: Lübbecke and Desrosiers; Pimentel
et al. 2015 and rcbalance; Yu, Silber and Rosenbaum 2020; Yu et al. 2022;
Abeywickrama, Liang and Tan 2021; quickmatch. Use each concept as its authors
define it and cite the canonical source at first use. Do not paraphrase from
memory. `paper-retrieval` fetches them.

On 2.5, describe optmatch by what it provides: sparse distance structures, several
LEMON algorithms, and a documented `tol × subjects` deviation bound. Then state what
couplr adds. "Status only" is not accurate and should go.

On 2.4, widen the ecosystem discussion in prose to designmatch, rcbalance,
quickmatch, clue and Matching. Table 5 can stay focused; the text carries the map.

## G. Benchmark structure — 3.1 through 3.8

**ACCEPT the design criticisms. Scope by D4.**

3.2 is the one to fix regardless of page budget, because it changes what the
existing numbers mean. `scripts/bench_scaling.R:125` builds one instance per size
and times it repeatedly, so the five replicates estimate timing noise. Generate
several independent instances per cell with timing repetitions nested inside, report
medians and interquartile ranges across instances, and keep the raw per-run rows
rather than pre-aggregated medians.

3.3 is a wording fix and should land now: `scripts/bench_scaling.R:2` sets
treated:control at 1:2, so "dense square problem" at `rjournal.Rmd:1170` is wrong.
At the two largest sizes the grid runs a single replicate, so call those single runs
and drop the word median.

D4 is resolved, so these go to supplementary material and the paper keeps summaries.
3.1, 3.4, 3.6 and 3.7 need runs that do not exist: the factorial grid over aspect
ratio, density, cost type and distribution; implicit mode across seeds, dimensions,
clustered and adversarial clouds, and cases where most edges eventually enter; peak
resident memory for every mode and comparator; and a representation comparison
holding the solver fixed. All of these are worth having. The memory one is the most
load-bearing, because the paper argues memory is the limiting resource and reports
edge shares in its place, and because the dispatcher's factor-of-four heuristic is
currently unevidenced.

3.5 is correct and cheap: at the largest size the method stores 0.29 percent of
edges while Table 3 reports 1.42 complete-pair-counts of distance evaluations. The
abstract should say the complete graph is never built, not that most pair costs are
avoided.

3.8: report end-to-end wall time as primary in Table 4, with internal solve time as
a secondary diagnostic.

Run these on `~/dev/couplr-bench` on the Mac mini, the machine that produced the
current numbers, so old and new rows stay comparable. The bench scripts resume from
their CSVs, so move the existing CSVs aside for a full re-run. Load
`hardware-nodes` before launching anything detached.

## H. Worked examples and causal language — 3.9, 3.10, 3.11

**ACCEPT.**

3.10 first, because it is a factual question with an answer in the repository.
`data/lalonde-results.csv` records couplr at 304.042 and both comparators at
304.043. Find out which it is: display rounding, optmatch's documented tolerance, a
different pairing of equal total cost, or a difference in how the cost matrix is
rebuilt. Then say so, and report full precision if the paper claims agreement.

3.9: the caption at `rjournal.Rmd:1059` says the residual imbalance is one "that no
one-to-one matcher can resolve". That is too strong. A distance-minimising
one-to-one matcher without balance constraints does not resolve it; cardinality
matching, fine balance, or dropping hard units can. Weaken the claim to what was
shown, and state plainly that this example demonstrates objective agreement across
packages.

Then add the second example the reviewer asks for, using couplr's balance or
cardinality machinery to reach a design that is actually usable. The package has the
functionality and the paper currently never shows it improving balance. This is the
single best use of new page space.

3.11: "the matched mean hourly-rate difference is 3.53". State the Gamma
interpretation correctly, as a bound on how far treatment-assignment odds may differ
within a matched pair. Say the grid places the sensitivity value between 1.5 and
1.75 rather than claiming significance stops at 1.75, unless the function does a
finer search, in which case report what it does.

## I. Reproducibility, guidelines and release — 4.1 through 4.4, plus the audit table

**ACCEPT.** All confirmed, all mechanical.

4.1 is a genuine break. `scripts/bench_scaling.R:52-53` writes
`<repo_root>/paper/scaling-results.csv`; `rjournal.Rmd:55-62` reads `data/*.csv`
under `paper/rjournal`. The supplied scripts cannot regenerate what the manuscript
consumes. Build the single entry point the reviewer describes, running from the
submission directory, writing into `data/`, recording `sessionInfo()` with compiler
and BLAS details, and separating instance seeds from timing repetitions. Add the
quick and full modes so the ten-minute expectation is met by one of them.

While there: the scripts call `install.packages()` during setup. Check for the
packages and stop with a clear message instead.

4.2: `_Rpackages.txt` lists five and needs every package required to build the paper
or regenerate any supplied result, which at minimum adds MatchIt, optmatch, R.utils,
RhpcBLASctl, pkgload and cobalt. Derive the list from the scripts rather than by
hand.

4.3: blocked on D3.

4.4: the website claims the same pairing is returned on every run and every machine
and is independent of row order. With multiple optima that needs a specified
tie-breaking rule, and the paper itself discusses alternative optima. Either
document the tie-breaking rule and make the claim precise, or weaken it. The site
also still recommends greedy matching for large pools, which the lazy and implicit
modes supersede.

Abstract: 255 words, confirmed. Cut to about 225. The word budget freed by tightening
the two opening sentences the reviewer flags covers most of it.

`data-raw`: add it for the synthetic package datasets.

Run `rjtools::initial_check_article()` and a clean-machine build once the above
lands, not before.

---

## Nothing rejected

Every item is either accepted or blocked on a decision above. The review is accurate
on all fourteen claims that could be checked against the repository, and it missed
one problem that is worse than the one it asked about (D2).


---

## Batch C, what landed

Code:

- `orlin` renamed to `sap_dense` across `R/`, `src/`, `tests/`, `man/`, the
  vignette and the paper. `src/solvers/orlin_ahuja/` is now `src/solvers/sap_dense/`;
  `R/trace_orlin.R` is `R/trace_sap_dense.R`; `tests/testthat/test-assignment-orlin.R`
  is `test-assignment-sap-dense.R`.
- Five uncompiled headers deleted, unused `alpha` / `auction_rounds` removed.
- `Rcpp::compileAttributes()` and `roxygen2::roxygenise()` rerun; DLL rebuilt.
- Verified: `sap`, `ssp` and `sap_dense` return the same objective as `jv` on five
  random rectangular instances; `maximize` and the trace registry both work under
  the new name; `method = "orlin"` now errors.

Documentation:

- `assignment()` states the Gabow-Tarjan bound as `O(sqrt(V) * E * log(V * C))`,
  replacing the `O(n^3 log C)` that disagreed with both its source and the paper.
- `assignment()` documents `"ssp"` as a second spelling of `"sap"`.
- The vignette's Orlin-Ahuja section is gone, along with its worked example, its
  entry in the complexity table, its reference, and the claim that couplr ships an
  algorithm no other R package has. A `sap_dense` passage sits beside SAP and LAPMOD.

Manuscript:

- The Gabow-Tarjan bound is now stated in the source's own symbols, with the
  substitution to `n` and `m` written out.
- A paragraph gives the `hk01` construction: constant costs make every perfect
  matching optimal; on a `{0,1}` matrix Hopcroft-Karp runs on the zero-cost edges,
  a perfect matching there costs zero and is therefore optimal, and the weighted
  solver takes over when no such matching exists.
- The solver count says twenty names and nineteen implementations.
- Figure 1's palette and family map move the solver to the augmenting-path family
  under the label `SAP-D`.

Verified, all three closed:

- **Figure 1 rebuilt and inspected.** The solver moved to the JV / augmenting path
  panel, which required adding it to that panel's in-panel key or the curve would
  have been dropped from the plot. Its colour was `#B07AA1`, a pink from the
  Flow-based range, which would have kept it reading as a flow method; it is now
  `#08519C`, in the panel's blue family. The caption's counts are unaffected:
  nineteen solvers, seventeen on square integer costs, two special-purpose in Other.
- **`hk01` in Figure 1 runs one code path, the intended one.** Tested directly on
  the exact instances the figure times (`SEED = 42`, `matrix(sample(0:1, n*n,
  replace = TRUE))`, n from 10 to 5000): every one returns total cost 0, so the
  zero-cost subgraph had a perfect matching and Hopcroft-Karp answered. The
  fallback engages only when zero entries fall to roughly 3 percent, which matches
  the log(n)/n threshold for a perfect matching to exist. The caption is accurate as
  written and needs no change.
- **`lap_animate()` is fine.** It returns an htmlwidget, so the trace is at
  `$x$meta`, not `$meta`. Checked for `hungarian`, `csflow` and `sap_dense`: all
  three carry their algorithm name and description. There was no defect.

Test suite: full `devtools::test()` run, 0 failures, 0 errors.

---

## Batch B, what landed

Manuscript only, `paper/rjournal/rjournal.Rmd`. No code changed.

- Equation `duals` is the dual program: `max sum u_i + sum v_j` subject to
  `u_i + v_j <= C_ij` and `v_j <= 0`. It previously gave the constraints with the
  matched-edge equality folded in, and no objective.
- Complementary slackness is its own display, equation `cs`, with the
  unmatched-column condition `v_j = 0` written as an implication rather than
  recovered from prose below the display.
- The sign condition carries what `src/core/lap_certify.h:19-29` and
  `R/lap_certify.R:36-42` already say: the dual objective sums `v` over every
  column while an assignment pays only for the ones it uses, so at `n = m`, where
  no column can be left out, `v` is unrestricted in sign, and
  `verify_assignment()` imposes `v_j <= 0` only when `n < m`. Jonker-Volgenant's
  free-sign duals certify a square problem for that reason.
- "Certifying a solution" opens on three conditions, not four: primal
  feasibility, dual feasibility for `duals`, and both halves of `cs`. Objective
  equality is named a reported check, matching what the exact-arithmetic
  paragraph below it already concluded. The text says `verify_assignment()`
  reports four checks.
- The narrative restatement of slackness's two halves was the same content as
  equation `cs` and is gone; the passage points at the display and keeps the 0.4%
  prototyping failure. The edge-generation stopping rule no longer says "the four
  conditions of the certification section".

Knitted and read: 0 unresolved references, equations `(2)` and `(3)` render as
intended, page 3 of the PDF inspected. The article is 22 pages.

Left for batch I: `RJreferences.bib:5` still cites couplr as version 1.5.5, which
D3 sweeps to 1.6.2 with the website and the tag.

---

## Batch D, what landed

Manuscript only, `paper/rjournal/rjournal.Rmd`. No code changed; every statement
was read out of the implementation rather than described from outside.

**1.6, the sentinel.** "Reshaping the cost matrix" now states the padded
objective and proves the ordering. With `[l, h]` the range of the admissible
costs of the pruned submatrix and `k` the smaller of its two dimensions, every
padded assignment matches all `k` rows and costs `s*sigma + R` with
`R` in `[(k-s)l, (k-s)h]`. Lemma 1: if `sigma > k(|l| + |h|)` then an assignment
using more sentinel edges than another costs strictly more, proved in three
lines from those two facts. The paper gives the formula couplr uses,
`sigma = (k+1)(|l| + |h|) + 1` from `.cardinality_sentinel()`, and says it
satisfies the hypothesis whatever the signs of `l` and `h` and when the
admissible costs are all zero.

The four things 1.6 said the informal statement left open:

- *Negative costs.* The span is `|l| + |h|`, so the bound holds for any signs.
  The proof uses `(k-s')l >= -k|l|` and `-(k-s)h >= -k|h|`, which is where the
  signs enter.
- *Maximisation by negation.* Stated: the instance is negated and the sentinel
  with it, so the lemma applies to the negated problem, and the objective is
  recomputed over real pairs so no sentinel reaches a reported cost.
- *Overflow.* The lemma is over the reals and the solve is in doubles, so the
  paper gives the largest total the padded objective reaches,
  `(k+1)(|l| + |h|) + k*sigma`, and says what happens above `2^53`:
  `match_couples()` warns and returns a greedy partial matching, saying it is
  not optimal; `assignment(cardinality = "maximum")` errors and asks for
  rescaled costs or an `unmatched_penalty`. This is `PAD_PRECISION_LIMIT` and
  the NULL return of `.lex_tier_weights()`.
- *Non-integral costs.* Nothing in the lemma assumes integrality; `l` and `h`
  are a real range. No sentence claims this, because the statement shows it.

**1.9, the edge-generation loop.** The seven-step narrative is a six-step
algorithm over a candidate set `E_t`, carrying the parameters the code has: the
seed width `6*ceil(log2(m))` capped at `m` from `implicit_seed_width()`, the
pricing threshold, and the warm start. Then Proposition 1 with the exact
hypothesis the reviewer proposed, and a proof by weak duality: restricted
optimality makes the cost of `X` equal `sum u + sum v`, the hypothesis extends
dual feasibility to every admissible pair, so that sum bounds the complete
problem below and `X` attains it.

The rest of 1.9's list, all in the section:

- *Tolerance version.* Under `cbar >= -eps` the shifted potentials `u_i - eps`
  are dual feasible everywhere, which costs the dual objective `n*eps`, so the
  matching is within `n*eps` of the complete optimum: `2e-5` at the largest
  benchmark shape and the default `1e-9`. This is the statement the loop's
  certificate carries, which is why the numerical-certificate paragraph now
  points at it.
- *Finite termination.* Each pricing round adds at least one pair the master did
  not hold, so `|E_t|` strictly increases under the bound `nm`. `max_rounds`
  (60) is named a guard rather than the argument.
- *Worst case.* Stated as that bound: the loop may build the complete graph and
  pay for the rounds that got there, which is why `memory_mode = "auto"` does
  not select the mode.
- *Work and storage.* A complete sweep is `O(nm)` and stores nothing, so the
  memory held is the candidate set: `nw` from the seed plus what the rounds add,
  reported as `candidate_edges`.
- *Batching.* `keep_per_row`, five by default, so a round adds at most `5n`
  pairs and never fewer than one.
- *Ties and determinism.* Selection is by strict improvement on a row's worst
  kept reduced cost (`flow_topk.h`, `key < beg->first`) over a scan running
  columns ascending, so an exact tie keeps the lower column index, and violators
  enter in `(i, j)` order.
- *Infeasible restricted graph.* Its own paragraph, absorbing the Hall sentence
  that used to sit lower down: a short master has no dual solution to price
  with, Hall's condition names `S` and `N(S)`, only columns outside `N(S)` can
  move the deficiency so the re-seed takes those, and when there are none the
  witness is the infeasibility certificate.

**Page count unchanged at 22.** The additions were paid for by three trims the
new material made redundant: the seed-width sentence in the round-count
paragraph, the Hall sentence in the constraints paragraph, and (in batch B) the
narrative restatement of complementary slackness.

Not in this batch: naming column generation and citing Lübbecke and Desrosiers,
which is 2.1 and belongs to batch F.

Knitted and read: 0 unresolved references, Lemma 1 on page 7 and Proposition 1
on page 10 of the PDF, both inspected.

---

## Batch F, what landed

Manuscript and bibliography only. Every source below was read this session
before its sentence was written; none was paraphrased from memory.

**What was read.** Lübbecke and Desrosiers 2005 §2.1, from the authors' own
text: the master problem over `J`, the restricted master over `J' ⊆ J`, the
pricing step `arg min c_j - u'a_j`, and "If c* >= 0, no reduced cost coefficient
is negative and [the restricted solution] optimally solves the master problem as
well." Their subproblem is an optimisation over an implicitly given set; ours is
the enumeration of their equation (2), which is the one real departure and is
what the paper says. Pimentel et al. 2015 (abstract, Europe PMC) and the
rcbalance 1.8.8 reference manual: `build.dist.struct()` produces a
distance/sparsity object listing each treated unit's permissible controls, from
exact groups and a propensity or user caliper, solved as minimum-cost flow
through rlemon. Yu, Silber and Rosenbaum 2020 (abstract): an iterative form of
Glover's algorithm on a doubly convex bipartite graph picks an optimal
propensity-score caliper, "radically reducing the number of candidate matches;
then we optimally match in a large but much sparser graph". Yu and Rosenbaum
2022 (abstract): pairings are graded, matched at the best grade, "incorporating
progressively lower grade pairs only to the degree they are needed. In effect,
only sparse networks are built, stored and optimized." Abeywickrama, Liang and
Tan 2021 (paper): exact Kuhn-Munkres with edge costs computed incrementally
behind "an inexpensive lower-bound heuristic", which is application-specific.
quickmatch 0.2.3, its own Description: "near-optimal generalized full matching",
"more than an order of magnitude quicker than other methods".

**2.1, column generation is named.** The loop section opens on it, cited, with
the restricted master, the pricing formula and the stopping condition in the
survey's terms, and states the one departure: pair variables are enumerated
rather than produced by an oracle, a pair variable being an index rather than a
combinatorial object.

**2.2, the novelty claim is the narrower one.** The second limit in the
introduction no longer says the admissible pairs are generally materialised
before solving. It says sparse matching is a developed literature whose sparse
network is settled before the solve that uses it, names rcbalance,
Yu-Silber-Rosenbaum and Yu-Rosenbaum for how each settles it, notes that a
network too sparse drops pairings it should have considered, and states what is
missing as a rule read off the solution in hand plus a statement about the pairs
never built.

**2.3, incremental cost computation is separated.** A paragraph in the loop
section: Abeywickrama et al. defer exact costs behind an application-specific
lower bound and save arithmetic; the pricing here asks nothing of the cost
function beyond evaluating a pair and saves storage, evaluating every omitted
pair once per round. The metric-tree pricer already in the discussion is named
as where the two meet. Graded matching is separated in the same paragraph: a
grade is a property of the pairing fixed before the match, a reduced cost is a
property of the solution just returned, and the loop stops on a statement about
the complete graph rather than on the grades running out.

**2.4, the ecosystem map is wider.** rcbalance and quickmatch join the
introduction's package paragraph with what each does in its own terms. Table 5
stays focused, as the disposition allows. RBestMatch is not in the paper: it is
CRAN-archived (last 0.1.1, 2022-02-19) and the method it implements is cited
through Yu and Rosenbaum 2022, which is the primary source.

**2.5, optmatch is described by what it provides, and a claim of ours was
wrong.** The paper said the dual variables the optimality conditions need "are
not part of what the packages above return". That is false for optmatch 0.10.8.
A `fullmatch()` result carries `attr(m, "MCFSolutions")@nodes$price`, real node
prices that vary across nodes; the class has no Rd page and is not exported, and
`evaluate_primal()`, which would evaluate the primal against them, is not
exported either. Checked directly: `getNamespaceExports("optmatch")` holds
neither, and a `fullmatch(pr ~ cost, data = nuclearplants)` run returns 34 nodes
with prices from -1.2523 to 0. The introduction now says no package exports a
function returning the duals or checking the conditions, names optmatch as the
closest, and gives its documented guarantee in its own words: `tol` times the
number of subjects bounds how far the returned match may differ from an optimal
solution of the specified problem. `InfinitySparseMatrix` and the four LEMON
algorithms are named where optmatch is introduced. Table 5's certificate-row
note carries the same correction and is re-dated 2026-08-26.

**2.6 is not a writing item.** It asks couplr's empirical breadth to match
current matching-software articles. That is batch G, and nothing was written
here to stand in for it.

Bibliography: six entries added (LubbeckeDesrosiers2005, YuSilberRosenbaum2020,
YuRosenbaum2022, Abeywickrama2021, rcbalance, quickmatch). Pimentel2015 was
already present and is now cited in the introduction as well.

**The article is 23 pages.** Batch F cost one, and roughly half of it is the six
new bibliography entries, which cannot come out. Page 23 holds three references
and the address block.
