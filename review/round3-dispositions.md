# Round 3 dispositions

Source: `review/round3.md`. Every claim below was checked in the repository
before it was accepted or declined. Code work is commit `4c5970d`.

## Blockers

### 1. The numerical certificate accepted an uncovered row [FIXED]

Confirmed and reproduced on the installed 1.7.0: `verify_assignment(c(0L,0L),
matrix(1,2,2), duals = list(u = c(0,0), v = c(0,0)))` returned
`certified_optimal = TRUE` under `"auto"` and `"double"`, with zero of two rows
matched against a true optimum of 2.

Taken further than the reviewer's patch. Adding `all_rows_matched` to the
`numerical` conjunction fixes the symptom and leaves `primal_feasible` meaning
something the paper's equation (1) does not: the row cover is part of primal
feasibility, not a fourth condition beside it. `primal_feasible` is now both
halves, reported separately as `structurally_valid_matching` and
`all_rows_matched`. `primal_objective` is still reported for a structurally
valid partial matching, since an unmatched row costs nothing and the sum stays
meaningful; it is withheld only where the matching is not a matching.

`tests/testthat/test-certify-row-cover.R`, 8 tests and 75 assertions: all three
`arithmetic` values, square, wide and tall-after-transposition, dense and lazy
sources, one row missing and every row missing, positive, zero and negative
costs, and a guard that a complete matching still certifies.

The flow verifier was checked for the same defect and does not have it. Flow
conservation is the row cover expressed in the flow model, so an unserved row
fails `n_conservation_violations` before any conclusion is drawn.

### 2. The tree ships but the article described the block scan [FIXED]

Confirmed. `flow_balltree.h`, `flow_tree_pricing.h` and `flow_tree_nearest.h`
are routed from `flow_row_search.h:96`, `ball_tree_pays()` returns `true`
unconditionally for Mahalanobis, and `bench_implicit.R` passes
`distance = "mahalanobis"`. The tree landed in `6580679` on 2026-08-18 and is
present at `a63b760`.

Root cause: `NEWS.md` carried no entry for it under any spelling, so the article,
rewritten a week later against NEWS and the manual, described the pricing it
replaced. NEWS entry written, routing documented on the `memory_mode` surfaces,
and three passages in the article corrected: the "evaluated once per round"
claim, the "block scan over the complete pair set" limitation, and the
future-work paragraph that proposed the tree as an extension. What remains
future is general flow-arc pricing and a bound tighter than a ball.

A fourth passage was found while fixing these and was not in the review: the
sweep paragraph still said the source "is read one pair at a time". It is merged
into the pricing paragraph.

### 3. The prune had no soundness argument [FIXED]

Confirmed. No `nextafter`/`nexttoward` anywhere under `src/flow/` or `src/core/`,
and the prune at `flow_tree_pricing.h` compared a plain-double bound.

One correction to the review's premise, which narrows the work rather than
widening it. The review reads the Mahalanobis gap as the tree bounding a rounded
Cholesky factor while the source evaluates the quadratic form "from the inverse
covariance", implying two independently derived matrices. The tree factors the
**same** `inv_cov` the source reads: `build_ball_tree()` calls
`cholesky_lower(src.inv_cov(), ...)`. So there is no inversion error to bound,
only rounding between two algebraically identical evaluations plus the residual
of that one factorization, and the residual is computable rather than merely
estimable.

Implemented as a one-sided bound rather than an error term reported beside a
two-sided one. Every bound the tree reports is lowered before any consumer sees
it, so `node_cost_floor()` is safe by construction and no call site can forget:

- geometric rounding as `gamma_{n_vars + 3}`, covering a sum of squares, its
  square root, and the centre and radius built the same way;
- for Mahalanobis, `||L L' - A||_F / ||A||_F` measured at build time against the
  matrix the factor came from;
- radii read high and centre distances low, caliper windows widened by one
  representable step each side, the metric transform and the two reduced-cost
  subtractions stepped down with `nextafter`.

A subtree that does not clear its threshold is descended into, so an allowance
set too high costs evaluations and one set too low would cost an edge. Adversarial
coverage added to `test_flow_tree_pricing.cpp`: coordinate scales from 1e-150 to
1e150, translations to 1e12, and covariances driven toward singular, each
comparing tree pricing against exhaustive pricing over the full violator set.
The C++ suite passes 197,002 assertions in 334 cases.

## Version and CRAN

The release is being folded into 1.7.0 rather than a 1.7.1, which is available
because 1.7.0 never reached CRAN. CRAN serves 1.6.1.

This dissolves the provenance wording the review asked for rather than requiring
it. The rerun's `logs/ENVIRONMENT.txt` records `commit 4c5970d` and
`couplr 1.7.0`, read from the DESCRIPTION at that commit, so the article no
longer has to explain a benchmark run whose DESCRIPTION said 1.6.2. The only
modified source paths are `R/RcppExports.R` and `src/RcppExports.cpp`, which
vanish under `--ignore-cr-at-eol` and are line endings, as in the previous run.

**The package still has to reach CRAN before submission.** The R Journal's
submission page states the requirement, and it is checked editorially.

## Mathematics

- Square duals: rewritten on redundancy rather than binding. When `n = m` the
  column inequalities may be replaced by equalities without changing the
  feasible set, and the equality formulation leaves `v` unrestricted.
- The column-generation bound: the article now names the four quantities that
  weaken Proposition 1 and defers the accounting to the supplement, which treats
  the master's gap, the pricing threshold, the sign condition on the column
  potentials and the tree allowance separately, and says which are subtracted
  and which are checked.
- Returning the suboptimality rather than the tolerance: **not done.** It is a
  new field on a public object and a new contract, and it is not needed for any
  claim the article makes. Tracked rather than rushed into the release.

## Multiple optima and overbroad claims

- "All three return the same assignment" now says they solve the same problem and
  returned the same pairing where pairing was compared.
- The testing paragraph no longer defines correctness as identical pairing.
  Correctness is a certified objective the two agree on; pair identity is
  reported where it holds.
- "Every solver returns the same optimal value" now says every solver targets the
  same objective, that scaling methods are exact for the instance the scaling
  produces, and that `verify_assignment()` decides optimality for the matrix as
  supplied.
- The warm-start claim is scoped to where the interface exposes it.

## Found while fixing, not in the review

**`method = "csa"` returns a grossly suboptimal assignment on heavy-tailed costs
and reports `status = "optimal"`.** Filed as #51 with a reproduction: at n = 60
on lognormal costs it disagreed with the optimum in 40 of 40 replicates, worst
case returning 77329.983 against an optimum of 0.0013427418. `gabow_tarjan`
behaves the same way but documents its integer conversion; `csa` documents no
caveat at all.

`verify_assignment()` returns `FALSE` on every one of them, which is the
certification layer doing exactly what it exists for, and is worth stating that
way rather than only as a defect.

Scope checked before it was allowed to worry anyone: the dispatcher never selects
`csa`, and in the regime grid `csa` is fastest in 2 of 189 cells, neither of them
a cost regime where its scaling loses resolution, and never on any
`heavy_tailed` cell. The article's dispatcher analysis is uncontaminated.

## Layout

Both float problems resolved under the reflow and verified in the rebuilt PDF:
the dispatcher sentence now sits whole on one page, and Figure 1's caption and
the subsection heading that introduces it are both on page 15 with the heading
above.

Table alt text: unchanged, and the reply should say why rather than leave it
silent. Every `fig.alt` in `rjtools`' own `sample-article`, `skeleton` and
`paper-with-errors` templates sits on a figure chunk and none on a `kable`
chunk; knitr has no table-alt option, and a `kable` table is real table markup in
HTML rather than an image.

Figure 1's abbreviations now map to public method name, family and intended
regime in the supplement's solver glossary.

## Supplement

55 pages to 13, A4 throughout. The four raw dumps carried 2,264 of about 2,300
table rows and are replaced by summaries, worst cases and explicit pointers to
the CSVs that ship beside the article. Kept: the lemma and its proof, the
outcome-analysis caveats, every protocol paragraph, the memory and
representation tables. Added: the solver glossary, the tolerance derivation, the
integer-conversion statement, and the bound the pricer prunes with.

## Page budget

Back to 20 pages, the limit the R Journal states. The additions were paid for by
moving the tolerance derivation, the integer conversion and the tree's bound to
the supplement, merging the two pricing paragraphs, and returning the two figure
heights to their committed values. The font increases in both figures are kept,
which is where the legibility gain was; the in-panel key spacing was retuned for
the restored height and checked in the rendered PDF.
