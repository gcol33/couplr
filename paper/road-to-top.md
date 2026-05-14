# Road to a top-tier JOSS paper

The paper currently sits in the upper third of what JOSS publishes: clean,
real engineering, honest writing. To move it into top-10-of-the-year
territory it needs one structural addition (a head-to-head comparison
against the packages it would replace) and a handful of focused edits.
This document tracks the work.

## Items, ranked by leverage

### 1. Head-to-head benchmark and case study on Lalonde NSW data [BIGGEST LEVERAGE]

**Why.** The current benchmark figure compares the 18 internal solvers to
each other. The comparison readers actually want is *"is this better than
the package I already use?"* — for matching, that's `MatchIt` /
`optmatch`. Without that figure the paper reads as a tool description; with
it the paper reads as a replacement claim. Combining the head-to-head with
a real-data case study (instead of synthetic `hospital_staff`) doubles the
return: the same exercise gives an empirical comparison and a credible
worked example.

**What.** Replicate the canonical Lalonde National Supported Work
observational-matching benchmark (in `MatchIt::lalonde`, used in dozens of
matching-methods papers). For couplr, MatchIt (`method = "optimal"` via
`optmatch`), and optmatch direct, report:

- standardized mean differences on the seven covariates after matching;
- wallclock matching time (median of 5 replicates);
- final ATT estimate on `re78` with a simple matched-pair regression.

Show as a small results table plus a balance-profile figure (love plot,
all three packages on the same axes). Add a short prose paragraph in a new
"Worked example" subsection of the paper showing the workflow on Lalonde.

**Concrete plan.**
- `paper/bench_lalonde.R` runs the three packages, writes
  `paper/lalonde-results.csv` and `paper/figures/lalonde_balance.png`.
- New "Worked example" subsection between Quickstart and Research Impact,
  ~12-15 lines including code.
- Update Statement of Need / Research Impact to reference the result.

**Status.** [x]

### 2. Sharpen the Statement of Need opening

**Why.** The current opening — *"Matching is a common way to make treated
and control groups more comparable"* — is the textbook sentence. Top-tier
papers open with the consequence, not the definition.

**What.** Open with the friction the package removes: that a serious
matching workflow today threads three packages by hand. The current
*second* paragraph already says this; promote it.

**Status.** [x]

### 3. Trim the "18 solvers from scratch" redundancy

**Why.** The phrase *"implemented from scratch in C++ via Rcpp and
RcppEigen, with no dependency on external R assignment libraries"* (or near
variants) appears three times: Summary, State of the Field, Software
Design. That's a design fact, not a user benefit, and it crowds out the
actual user benefit (auto-dispatch picking the right algorithm for free).

**What.** State the "from scratch in C++" point once, in Software Design.
Replace the other two mentions with a sentence about the user-facing
payoff of the dispatcher.

**Status.** [x]

### 4. Refocus the benchmark figure on the dispatcher win

**Why.** Panel (a) is five small-multiple solver-family panels; panel (b)
is auto-vs-Hungarian. A reader scanning the PDF takes away "lots of
algorithms exist." The story we *want* the figure to tell is "the auto
dispatcher beats every fixed choice across the size regime."

**What.** Promote panel (b) (the dispatcher comparison) to the primary
panel, with panel (a) either shrunk to a supplementary inset or restricted
to one curve per algorithm family (the family-best) so the dispatcher's
advantage is visible at a glance.

**Status.** Subsumed by Item 1. The new Lalonde figure now carries the
dispatcher-payoff story on a real matching problem, which is a stronger
narrative than restructuring the synthetic-benchmark figure. Figure 1
remains as the algorithm-internal benchmark; Figure 2 (Lalonde) is the
headline. [done]

### 5. `pixel_morph` — demote from abstract, keep as vignette

**Why.** Image alignment via LAP is genuinely cool, but the paper mentions
it twice in two sentences and never shows it. Earning a place in the
Summary requires a figure; the paper doesn't have one and adding one
risks scope drift.

**What.** Drop `pixel_morph` and `pixel_morph_animate` from the Summary
function list and from the "two layers" framing. Keep one line in
Software Design noting that the same solvers also support image alignment
via the package's vignettes, with a pointer.

**Status.** [x]

### 6. Rewrite the Research Impact Statement

**Why.** *"Useful for experimental designs, observational studies, and
allocation problems"* reads as scope, not impact. The Lalonde benchmark
result lets us replace generic claims with concrete numbers.

**What.** Cite the Lalonde balance / wallclock result, name any downstream
users or integrations, mention CI matrix and CRAN download count. Length
unchanged.

**Status.** [x]

### 7. Mention CI matrix and test coverage in-text

**Why.** Reviewers expect to see CI badges or test coverage referenced in
the paper, not just on the README. The current text only says "continuous
integration across Windows, macOS, and Linux" without specifying
coverage.

**What.** One sentence in Research Impact stating CI matrix, test count,
and coverage percentage (pull from `codecov.yml` / actions).

**Status.** [x]

### 8. Mention `sensitivity_analysis` Rosenbaum-bounds citation in State of the Field

**Why.** The Stuart (2010) and Rosenbaum (2002) references are currently
introduced only in the Quickstart caveat paragraph. State of the Field
discusses balance support without crediting either.

**What.** One-clause addition naming Rosenbaum bounds where balance
diagnostics are listed.

**Status.** [x]

## Execution order

1. Kick off `bench_lalonde.R` in the background (slowest item).
2. Items 2, 3, 5, 7, 8: cheap prose edits while the benchmark runs.
3. Item 6: requires the new benchmark data — do after item 1 finishes.
4. Item 1 prose (worked example): also after the data lands.
5. Final `build.ps1` rebuild.
