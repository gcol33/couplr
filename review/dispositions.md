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
is the rest of batch A and has not been done.

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

**ACCEPT.** The claim outruns the implementation and the reviewer is right that the
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

**ACCEPT both.** Cheap and uncontestable.

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

**ACCEPT.** This is the batch that most improves the paper.

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

**ACCEPT.** The novelty boundary as drawn is too wide, and the narrower claim the
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
