# Round 4 review

Referee-style audit of `paper/rjournal/rjournal.pdf` (21 pages, the 2026-09-02
build) and of the tree and certificate it describes. Verdict: **do not submit
yet**. Three blockers, one of them mathematical.

The round-3 work landed. The regimes section was re-derived rather than
re-timed, the numbers that used to be typed into the prose are now read from the
CSVs, and the benchmark re-run caught and corrected a stale-binary error that
would otherwise have carried into print. What stops the submission is a page
count, a release gate, and a bound that still does not do what its own header
says it does.

## Verified in the repository

Everything below was checked this session, against the working tree at
`3a083b1` plus the uncommitted paper rebuild.

| Claim | Status | Evidence |
|---|---|---|
| Page limit is 20 | Confirmed | journal.r-project.org/submissions.html: "Articles for the R Journal should be no more than 20 pages" |
| Submitted PDF is 21 pages | Confirmed | `pdfinfo paper/rjournal/rjournal.pdf`; the same 21-page file is inside `paper/couplr-rjournal-submission.zip` |
| Abstract within 250 words | Passes, 236 | word count over the `abstract:` block of `rjournal.Rmd` |
| Benchmarks measured on a verified binary | Confirmed | `dev_notes/bench-rerun-20260901-handoff.md`: every `.o` newer than the newest header and older than the 13:18:58 suite start |
| PDF carries the post-re-run numbers | Confirmed | rendered text gives 11.6 s at n = 20,000, 76 s at n = 50,000, median 1.07x, worst 10.41x |
| Staged data matches `paper/` | Confirmed | byte-compare of every `paper/rjournal/data/*.csv` against its `paper/` original |
| CRAN serves 1.6.1 | Confirmed | cran.r-project.org/package=couplr, published 2026-08-23; `DESCRIPTION` here is 1.7.0 |

## Blockers

### 1. The paper is 21 pages against a 20-page limit

The journal states "Articles for the R Journal should be no more than 20 pages".
`rjournal.pdf` is 21, and that file is what `couplr-rjournal-submission.zip`
carries. Round 3 closed at 20 pages, and this build adds 71 lines against 21
removed, most of it two new prose blocks: the tied-cost regime paragraph in the
dispatcher section and the rewritten memory-guard passage.

The page budget is float-bound, so trimming sentences will not recover the page
on its own. The two new blocks are both worth keeping on content; the space for
them has to come from float area or from the final section.

### 2. The ball-tree cost floor still sits above costs the source returns

`node_cost_floor()` is documented as "A lower bound on the cost of every column
the node holds", and `price_tree()` relies on that: a subtree whose bound does
not clear the threshold is skipped unread, and the comment at the top of
`flow_tree_pricing.h` argues that this is why "zero still means the restricted
answer is optimal for the complete problem".

The invariant it needs is not a bound on the exact mathematical distance. It is
a bound on the number `LazyCostMatrix::raw_distance()` returns, because that is
what the reduced costs the threshold is compared against are built from. Those
are two different quantities:

- the tree measures `||L' d||`, a sum of squares, computed from the Cholesky
  factor of `inv_cov` (`flow_balltree.h:72`, `whiten_point()`);
- the source measures `sqrt(d' A d)` by row sums over `inv_cov` directly
  (`lap_lazy_types.h:188-206`).

`bound_allowance()` charges for two things: `gamma_{n_vars + 3}` for the tree's
own arithmetic, and `||L L' - A||_F * ||L^-1||_F^2` for the algebraic mismatch
between the two evaluations. It charges nothing for the rounding error of the
source's own evaluation, and that is the cancellation-prone one: the row-sum
quadratic form loses relative accuracy as `d' A d` falls away from
`|d|' |A| |d|`, which is exactly the ill-conditioned direction the tree is asked
about.

The residual term does not cover it, and usually contributes nothing at all.
`||L L' - A||_F` measures whether `L L'` reconstructs the stored matrix, not
whether two evaluations at a given `d` agree. Over 2,000 ill-conditioned draws
(`n_vars = 3`, condition number 1e9 to 1e14) it came out **exactly zero in 42
percent of them**, leaving the whole allowance at `gamma_{n_vars + 3}`, about
6.7e-16 relative.

Reproduced in `dev_notes/review4/balltree_bound_invariant.R`, which mirrors
`cholesky_lower()`, `whiten_point()`, `bound_allowance()`, `node_ball_bounds()`,
`cost_lo_of()` and the centre-and-radius build, and models the downward
`nextafter` step as a full `2^-52` relative step so the modelled floor sits at or
below the real one:

    trials with a usable tree: 4000
    floor above the cheapest member (invariant violated): 25
    worst gap: 6.17e-17

That first run is at a metric scale where the violation is far below any
tolerance. It is not scale-invariant. `dev_notes/review4/balltree_bound_vs_tolerance.R`
repeats the search with `inv_cov` scaled the way a small-variance covariate set
scales it:

    trials 6000 | violations 13 | worst absolute gap 5.00773e-07
      floor           6.8324146820394738
      min source cost 6.8324141812669215
      allowance 6.66e-16   cond 4.28e+09   metric scale 8.26e+11

5.0e-7 is about 500 times the default certification tolerance of 1e-9
(`R/lap_certify.R:107`). A node holding a genuine violator can therefore price
above `-tol` and be skipped, `n_violators` reads zero, and the certificate
concludes optimality. The two extra `nextafter` steps in `node_cbar_lo()` are
about 1.8e-15 at this magnitude and do not absorb it.

This also reaches the feature `b71cd62` added. `max_suboptimality` and
`certified_reduced_cost_floor` are built from `proven_floor`, which `price_tree()`
sets from the bounds of the subtrees it skipped. The reported bound on how far
the answer can sit from the optimum is therefore computed from the same
quantity, so where the floor is too high the published suboptimality is too
small.

`d3365ae` fixed the case it names. The regime it names is not closed.

The fix is a term in `bound_allowance()` for the source's own evaluation, of the
shape `eps * (|d|' |A| |d|) / (d' A d)` bounded over the node, or else having the
source evaluate the distance through the same factor the tree uses, which
removes the mismatch instead of bounding it. The second is the smaller change to
reason about and makes the two evaluations one.

### 3. The article measures a version that is not on CRAN

CRAN serves couplr 1.6.1, published 2026-08-23. The article measures 1.7.0 and
documents `max_suboptimality`, which 1.6.1 does not have. The journal requires
"all code used open source and available via CRAN or BioConductor" at technical
review, and the abstract's closing sentence, "couplr is available on CRAN", is
read against the version the article describes.

This is the release gate round 3 already recorded. It stays open, and blocker 2
now sits in front of it, since the tree and the certificate are what 1.7.0 is
for.

## Should fix before submitting

### 4. The memory-guard paragraph rests on a filter it does not disclose

`rjournal.Rmd:123` is

    mem_sizes <- mem_sizes[mem_sizes >= 5000]

with no comment, in a chunk where the neighbouring additions are commented. The
prose then reports "Across 5,000, 10,000 and 20,000 units the solve peaked at
10.5, 7.2 and 8.8 times the cell bytes and the matrix at 1.8, 1.7 and 1.5 times
them. The matrix default bounds every one of those."

`memory-results.csv` also holds n = 2,000, with `status` "ok", and it ships with
the article. Dividing the two columns there gives a solve peak of 16.9 times the
cell bytes against the 10 the guard assumes, and a matrix peak of 8.0 times them
against the 4 it assumes. Both defaults are exceeded, and by more than the one
case the paragraph does disclose.

There is a good reason to exclude that row: at 7.1 MB of cells the peak is
dominated by fixed session overhead and the ratio stops meaning much. That
reason is not in the paper, and a referee who divides two columns of the shipped
CSV finds the excluded row before finding the reason. Either state the exclusion
and why, or report all four sizes and say what the smallest one measures.

The sentence that was added here is otherwise the right move: disclosing that
the multiplier sits under the peak at 5,000 units is more honest than the
formulation it replaced.

## Noted, not blocking

`node_cbar_lo()` (`flow_tree_pricing.h:59`) tests `!(floor_c < infinity)` and
returns `+inf`, which the caller reads as "no reachable column" and prunes. A
NaN floor takes that branch. Under a negated metric, `cost_lo_of()` reads
`b.d_hi`, which is NaN when the whitened separation is not finite. Covariate
finiteness is validated on the flow-model path, and I did not construct a case
that reaches this through the public API, so this is a defensive observation
rather than a demonstrated defect. Testing `std::isnan(floor_c)` separately and
treating it as a zero floor rather than an infinite one costs nothing.

## What is working

Worth recording at the same specificity as the findings, because three rounds of
blockers do not describe where this paper now is.

**The stale-binary catch.** `src/rcpp_interface.o` was ten hours older than the
two commits it was supposed to contain, and all of `src/flow/` compiles into it,
so relaunching the suite would have re-measured the old pricer under a clean
`3a083b1` stamp. It had already happened once: the previous 22 h suite linked an
object built 13 minutes before it started, and on a clean rebuild the unchanged
`lazy` path ran 5.4x faster, 340.4 s to 61.9 s at n = 50,000. Catching that
before print, re-running all eight stages on a verified binary, and recording
the object-versus-header timestamps in the handoff is the reason every number in
the paper is now attributable.

**The regimes section was re-derived, not re-timed.** `best_method` moved in 83
of 189 cells once the binary was correct, `csa` falling from 41 wins to 5. The
section now reports the tied-cost regime as a regime, median 3.92x and worst
10.41x against 1.06x over the rest, instead of pointing at the single worst cell.
That is a harder and more useful statement than the one it replaced.

**Prose numbers are now computed.** The LaLonde relative difference, the
tied-regime figures and the memory ratios are read from the CSVs at knit time
rather than typed in, so a re-measurement cannot silently desync the text from
the data. The head-to-head comparison added for the two removed dispatch rules
times `sap` and `lapmod` against the solver each rule actually named, joined on
the cell keys rather than on row order.

**The mechanical guideline items are clean.** 236 words against a 250-word
limit, A4 throughout, `_Rpackages.txt` present, scripts and data shipped
alongside, and the staged data byte-identical to what produced the figures.

## Order to work in

1. Blocker 2. It is the only one that is not bookkeeping, it decides whether the
   certificate claim survives in its present form, and its fix may change the
   pricing numbers the article quotes.
2. Item 4, which is a chunk edit and two sentences.
3. Blocker 1, once 2 and 4 have settled the text, since both can move the page.
4. Blocker 3, the release, last.

---

# Dispositions

Worked in the order the review set. Code and paper changes are in this commit;
`dev_notes/review4/handoff.md` carries the detail and the re-run scope.

### 2. The cost floor sits above costs the source returns [FIXED]

Confirmed and repaired. `bound_allowance()` charged for the tree's arithmetic
and for the algebraic gap between the two evaluations, and nothing for the
source's own row-sum rounding, which is the cancellation-prone one. The
algebraic term is also exactly zero in 42 percent of poorly conditioned draws,
because `L L'` reconstructs the stored `A` bit for bit, so what looked like a
conditioning-aware allowance usually contributed nothing.

The tree now carries `|sym(inv_cov)|` and charges
`gamma_{n_vars + 3} * m' |A| m` over the node's box, applied on the squared
distance and to both sides of the ball. Every cost-level question routes through
one `node_bounds_for_source()`, so the geometric bound cannot be read alone. A
NaN floor is reported as no bound rather than as an unreachable node, which also
closes the item noted as defensive.

The regression test was shown to discriminate: with the new term forced to zero
it fails at `0.0245673944 <= 0.0245673839`, and passes with it. C++ suite
754,994 assertions in 337 cases; R suite FAIL 0, PASS 8900.

**This creates a new gate.** The safer bound descends into a few more subtrees,
so `edges_evaluated` rises by 0.005 to 0.06 percent on the article's own
instances. `candidate_edges`, `n_rounds`, `total_cost` and `certified_optimal`
are byte-identical, so the abstract's 0.29 percent and every correctness claim
stand. The article quotes `edges_evaluated` at `rjournal.Rmd:1117`, `1168` and
`1219`, so `implicit`, `implicit_grid`, `path`, `memory`, `scaling` and
`scaling_lazy` need re-measuring, about 2h22m. `regimes` and `figure` never
reach the tree and stand.

### 1. Twenty-one pages against a twenty-page limit [FIXED]

Now 20. The budget is float-bound in the body but text-bound at the tail: with
no float on the last two pages, the address block was what would not fit.
Recovered by compressing the summary's opening, replacing its quantitative
recap with a four-line version, dropping the future-work paragraph that
restated limitation 1, tightening three limitation bullets, and cutting the
capability caption to what is needed to read the table, with its sourcing moved
to the supplement. Figure heights are unchanged at their committed values.

### 4. The memory-guard filter [FIXED]

`rjournal.Rmd:123` still reads the ratios from 5,000 units up, and now says so,
with the reason and the direction of the excluded row: at 2,000 the matrix holds
7 MB of cells against a session floor near 146 MB, so the peak measures the
session, and both multipliers are exceeded there.

### 3. The CRAN gate [NOT ADDRESSED]

Out of scope by instruction. CRAN still serves 1.6.1.

### Found in this round, not in the review

**The competitor table was wrong in two rows.** MatchIt's sparse-cost entry read
`no`; `MatchIt:::matchit2optimal()` calls `optmatch::as.InfinitySparseMatrix()`
on its distance object before solving. The "rectangular cost matrix as solver
entry point" row read `full match` for both alternatives, which is evasive:
MatchIt documents `distance` as accepting a matrix and optmatch takes one
through its distance constructor. The row is split into one recording that all
three accept a user matrix and one recording what a pair match is solved as.

**`full_match()` was not Hansen-Klopfer full matching [FIXED].** It fixed the
group centres to the globally smaller side, so every group was 1:k; genuine full
matching mixes 1:k and k:1 sets in one solution. On a 3 by 3 instance it returned
10 against `optmatch::fullmatch()`'s 0 and reported `status = "optimal"`, and the
ratio is unbounded in the spread. The article cited @HansenKlopfer2006 for a
formulation the code did not implement.

The blocker named earlier in this file, `min_controls > 1`, dissolved on a second
reading. `min_controls` is the least number of **right** units a group may hold,
and a many-to-one group holds exactly one, so a lower bound above one forbids
that shape by arithmetic: the one-centre design is already the whole feasible set
there and was never wrong. Only `min_controls = 1`, the default, was.

At that value the network now carries a lower bound of one on both sides with
unit capacity on the pair arcs, so the arcs a solve places are an edge cover of
the admissible pairs. A cheapest cover is inclusion-minimal, a minimal cover is a
disjoint union of stars, and such a union covering every unit is exactly a full
matching, so minimising distance over covers minimises it over full matchings.
The flow value is not fixed in advance, since the arc count depends on how many
groups form, so the source injects the unit count and a bypass arc absorbs the
slack. The reading step prunes any arc whose two ends are both met elsewhere,
which a zero-cost arc can otherwise leave in a non-minimal cover, and takes the
components as groups.

The counterexample now returns 0 with two groups, and 80 random instances agree
with `optmatch::fullmatch()` exactly. Three `test_flow_compile.cpp` cases were
asserting the old structure at `min_controls = 1`; each was read before being
touched, and they were retargeted to `min_controls = 2`, where the orientation
they test still applies. Two new cases cover the counterexample at the network
level and the boundary between the two designs. C++ 755,071 assertions in 339
cases, R FAIL 0 PASS 8900.

No benchmark stage calls `full_match()`, so this changes no measured number.
