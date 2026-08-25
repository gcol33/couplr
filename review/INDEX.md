# Round 1 triage

Source: `review/round1.md`. Reviewed artefact: `paper/rjournal/rjournal.pdf`, 20 pages.
Verdict: major revision before submission.

## Counts

| Block | Items | Kind |
|---|---:|---|
| §1 Mathematics and correctness | 9 | 1 author decision, 8 writing/implementation |
| §2 Novelty and related work | 6 | reading, then writing |
| §3 Benchmarks and evidence | 11 | 6 need new runs, 5 writing |
| §4 R Journal guidelines | 4 + 17-row audit table | mechanical, 3 Fails |
| §5 Section-by-section | restates §1–§4 | no new items |
| §6 Priority order | restates | no new items |

30 numbered items. Three hard guideline failures: abstract length, `_Rpackages.txt`,
reproduction wiring.

## Claims I checked against the source

Everything in this table was verified in the repository this session.

| Review item | Status | Evidence |
|---|---|---|
| Abstract exceeds 250 words | Confirmed, 255 | `rjournal.Rmd` YAML abstract block |
| Gabow–Tarjan bound reuses `n`, `m` | Confirmed | `rjournal.Rmd:284` gives `O(sqrt(n) m log(n Cmax))`; `rjournal.Rmd:583` defines `n` treated, `m` controls |
| Manual disagrees with the paper on the same bound | Confirmed | `man/assignment.Rd:59` and `R/lap_solve.R:45` give `O(n^3 log C)` |
| Verifier can trigger a second solve | Confirmed | `R/lap_certify.R:228` calls `assignment_duals(cost, ...)`; documented at `lap_certify.R:49-50` |
| Tolerance default `1e-9` | Confirmed | `R/lap_certify.R:107`, `R/flow_model.R:354`, `R/lap_implicit.R:42` |
| "dense square problem" is rectangular | Confirmed, 1:2 | `scripts/bench_scaling.R:2,62` |
| Replicates time one instance per size | Confirmed | `scripts/bench_scaling.R:125` builds one `d` per `n_total`, reps loop inside |
| Objective totals differ | Confirmed | `data/lalonde-results.csv`: couplr 304.042, MatchIt 304.043, optmatch 304.043 |
| Scripts do not write what the Rmd reads | Confirmed | script writes `<repo>/paper/scaling-results.csv`; Rmd reads `data/scaling-results.csv` |
| Scripts install packages unprompted | Confirmed | `scripts/bench_scaling.R` calls `install.packages()` inside its setup block |
| `_Rpackages.txt` lists five | Confirmed | file contains couplr, ggplot2, knitr, rmarkdown, rjtools |
| Version mismatch | Confirmed | `DESCRIPTION` 1.6.2; newest git tag `v1.6.1` |
| `ssp` is an alias | Confirmed | `R/lap_solve.R:212`, `if (method == "ssp") method <- "sap"` |
| "nineteen solvers" | Correct as an implementation count | the public `method` vector holds 20 names besides `"auto"`; `ssp` aliases `sap`, leaving 19 implementations |

## Read before resolving

### The solver count has a second problem the review did not reach [RESOLVED]

`R/trace_orlin.R:1-20` records, in the repository's own words, that
`src/solvers/orlin_ahuja/orlin_solve.cpp` is misnamed: it runs plain successive
shortest paths with Dijkstra, no scaling phases and no auction warm-up, and its
`alpha` and `auction_rounds` parameters are declared and unused. The true
Orlin–Ahuja algorithm is not implemented.

The review asks whether "19 solvers" counts aliases. The honest answer is that one
of the nineteen carries the name of an algorithm it does not implement, and is
algorithmically a third copy of the shortest-augmenting-path solver. A paper that
advertises nineteen algorithms cannot ship with that unresolved. This is in scope,
in this repository, and it needs a decision before anything else in §5 is written.

Resolved by D2: the method is `"sap_dense"`, the dead headers and unused parameters
are gone, and the paper states twenty names against nineteen implementations. See
`dispositions.md`, batch C.

### Items nobody can close with a wording change

These need a decision or a new computation. None is a prose fix.

- **1.1** exactness claim. Governs the title, abstract, contribution list,
  conclusion and the package manual at once.
- **1.5** integer scaling. Shares machinery with 1.1; the same code answers both.
- **1.9** the edge-generation proposition. The paper's central contribution is
  currently prose. Turning it into a stated proposition with a proof costs pages.
- **3.1, 3.2, 3.4, 3.6, 3.7, 3.8** benchmark structure. Every one requires runs
  that do not exist yet.
- **3.9** the LaLonde example. Adding a balance-constrained second example is new
  analysis and new text.
- **4.3** which release the paper is pinned to.

### The page budget is the binding constraint

The paper is at exactly 20 pages, the R Journal hard limit. The review asks for a
dual program, two lemmas, a proposition with proof, a wider related-work section,
a factorial benchmark, memory measurements and a second worked example. All of it
is additive. Nothing lands until something is cut or moved.

The submission guidance accepts "Other supplementary files that contain additional
technical details or examples", so the expanded benchmark grids go beside the paper
and the 20 pages carry summaries. The additions in §1.6, §1.9 and §2 are prose and
still have to fit.

The same page caps reproduction at "a reasonable time (less than 10 minutes)" and
recommends supplying intermediate outputs for computationally intensive work. That
is what the quick and full modes in §4.1 are for: the default reproduction reads the
supplied intermediates and finishes inside the limit, and the full benchmark suite
may run longer. This is a requirement, not a courtesy, so the round-2 reply states
compliance rather than querying it.

### Cascades

One change propagates to many places. Sweep the whole document each time.

- The exactness decision (1.1) rewrites the title, abstract, contribution list,
  §"Certifying a solution", the conclusion, `man/assignment.Rd`, the roxygen in
  `R/lap_solve.R` and `R/lap_certify.R`, and the package website.
- The version decision (4.3) touches `DESCRIPTION`, the bibliography entry, the
  paper's stated version, the website, the git tag and the CRAN release.
- The Gabow–Tarjan bound (1.4) appears in `rjournal.Rmd:284`, `man/assignment.Rd:59`
  and `R/lap_solve.R:45`, currently in two mutually inconsistent forms.
- The solver-count decision touches the abstract, three body passages
  (`rjournal.Rmd:701, 770, 918`), the Figure 1 caption and its alt text, and
  Table 5 at `rjournal.Rmd:1304`.

### Terminology to fix once and use everywhere

- "proof" / "provably" / "exact" / "certified": pick one meaning per word and hold
  it. The review's split into `exact_certificate` and `numerical_certificate(tol)`
  is a good starting vocabulary.
- `n` and `m`: the paper uses them for matrix sides. Complexity bounds quoted from
  the literature use them for vertices and edges. Rename one of the two uses.
- "sparse": the review is right that the paper's evidence supports sparse *storage*.
  Say which is meant at every occurrence.

### Collegial framing

§2.5 asks for a more accurate account of optmatch, and it is a fair request.
optmatch documents `InfinitySparseMatrix`, sparse storage, several LEMON algorithms
and a `tol × subjects` deviation bound. Describe what each package provides and
report both side by side. The distinction to draw is that optmatch offers a solver
tolerance guarantee and sparse distance structures, and couplr returns dual
variables and a separate verification object. The same applies to rcbalance,
quickmatch, designmatch and Matching when §2.4 is written.

### Do not write these from memory

§2 asks the paper to engage five bodies of work: column generation
(Lübbecke and Desrosiers), large sparse optimal matching (Pimentel et al. 2015,
rcbalance), graded and administrative-scale matching (Yu, Silber and Rosenbaum
2020; Yu et al. 2022), incremental edge-cost computation (Abeywickrama, Liang and
Tan 2021), and quickmatch. None has been read in this session. Pull each primary
source before its sentence is drafted, and use each concept as its authors define
it. Getting a definition loosely right is the class of comment that reopens on
round 2 and spreads.
