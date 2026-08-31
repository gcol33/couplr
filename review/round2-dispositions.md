# Round 2 dispositions

Source: `review/round2.md`. All items accepted. Every claim below was verified in
the repository this session.

## Message 1: correctness

### 1. Version inconsistency [RESOLVED]

The premise that the paper might need re-benchmarking under 1.7.0 does not hold:
`git diff --name-only a63b760..HEAD` touches `DESCRIPTION`, `NEWS.md`,
`cran-comments.md` and paper artefacts only, and nothing under `R/` or `src/`.
The benchmarked commit *is* the 1.7.0 code; only the version string differed.

Beyond that, the promised full-suite re-measurement turned out to exist. The bench
clone on the Mac mini held run `20260830-152628` at `a63b760` with every
measurement stage exiting 0 (`logs/SUITE.done`, 2026-08-30 21:15), never synced
back. Its only failing stage was `figure`, which wrote `benchmark-table.csv`
successfully and then died on a `lalonde-per-covariate.csv` absent from that clone.
The clone's two `RcppExports` modifications vanish under `--ignore-cr-at-eol`, so
they are line endings, not code.

Adopted that run. Consequences:

- Every arm now comes from one run at one commit. The carry-over note is gone.
- `logs/ENVIRONMENT.txt` records `couplr 1.7.0` and states the basis: the harness
  read 1.6.2 from the DESCRIPTION at `a63b760`, and no commit between there and
  the release touches `R/` or `src/`. The supplementary prints this block verbatim.
- Numbers moved only by noise. The implicit arm's costs, edge counts and round
  counts are unchanged; `elapsed_s` at 50,000 went 20.153 -> 20.171 s. The
  abstract's 0.29 percent is unchanged.
- `v1.7.0` tagged and released on GitHub. CRAN submission left to the author.

The carry-over caveat the note used to carry was checked before it was dropped and
was sound: only `bench_regimes.R` calls `method = "auto"`, and `bench_memory.R`
passes `memory_mode` and `method = "jv"` explicitly.

### 2. Abstract outruns the evidence [RESOLVED]

Confirmed. `data/implicit-results.csv` has no dense arm at `n_total = 50000`, and
`data/implicit-equivalence.csv` carries `identical_pairing` at 500, 2,000, 5,000
and 10,000 only. The body was already correctly scoped ("On the four sizes where
the dense solve can also be run"); only the abstract overreached.

Took the reviewer's cleaner claim: "holds 0.29 percent of the pairs and returns a
certified optimum." Certification is carried at that size (`certified_optimal`
TRUE, `duality_gap` 0). Abstract now 231 words.

### 3a. Unembedded fonts [RESOLVED]

Confirmed: `pdffonts` reported three Helvetica objects at IDs 396, 397 and 413
with `emb no`, from the default `pdf()` device. The setup chunk now sets
`dev = if (knitr::is_latex_output()) "cairo_pdf" else "png"`, conditional so the
HTML build keeps its raster device. `pdffonts` now reports no unembedded font.

### 3b. Alt text [NO CHANGE NEEDED]

The R Journal instructions say "Figures and tables should have alt-text in chunk
specifications." Both figure chunks already carry `fig.alt`. Tables do not, and
this matches the journal's own tooling: every `fig.alt` in `rjtools`'
`sample-article`, `skeleton` and `paper-with-errors` templates sits on a figure
chunk, none on a `kable` chunk, and knitr has no table-alt option. A `kable` table
is also real table markup in HTML rather than an image. Stated in the reply rather
than bolting an inert option onto the table chunks.

### 4. "the pruning is itself the certificate" [ACCEPTED]

Rewritten: "Where such a bound prunes, the prune certifies that its subtree holds
no violating edge, and the prunes together with the leaves the scan examines
certify the pricing result."

## Message 2: structure

Reorganised to the reviewer's outline. 11 top-level sections became 7. No prose
was rewritten except where noted; the moves are hierarchy and order.

| New | Built from |
|---|---|
| 1 Introduction | unchanged, plus a two-sentence roadmap |
| 2 The core assignment engine | old 2 (assignment problem, certifying a solution, why more than one algorithm) + old 8 (the solver layer) as 2.4 |
| 3 Matching with couplr | old 3 (workflow) + old 4 (reshaping, designs, interoperability) |
| 4 Common optimization architecture | old 5, 6, 7 + memory modes from old 9, under a lead-in stating the causal chain |
| 5 Implementation and verification | old 9 lead-in and dispatch, with Verification split into independent verification and testing/reproducibility |
| 6 Empirical evaluation | old 10, renamed and reordered by contribution: edge generation and memory, warm-started paths, solver portfolio and dispatch, comparison with alternatives |
| 7 Summary and discussion | unchanged, plus an empirical synthesis before the limitations |

Item-by-item:

- **1 too many peer sections** and **2 solver story split**: addressed by the table
  above. The solver layer is now 2.4, immediately after the portfolio it exposes.
- **3 "Software design" catch-all**: dispatch stayed with implementation, memory
  modes moved to the architecture section beside the edge representations they
  belong with, and verification split in two.
- **4 section 10 misnamed and misordered**: renamed "Empirical evaluation" and
  reordered so certified edge generation leads. The 0.29 percent result is now met
  on page 13 rather than page 16.
- **5 evidence far from mechanisms**: forward references added at the end of
  "Matrix-level interface and extensions", "Certified implicit edge generation"
  and "Warm-started matching paths". Named rather than numbered, because
  `\@ref()` does not resolve inside a `kable` caption in this template and the
  article builds to both PDF and HTML.
- **6 discussion lacks synthesis**: added before the limitations, reading all four
  numbers off the CSVs through inline R rather than hardcoding them.

### Page budget

The additions cost a page: the paper went 20 -> 22, and the limit is 20. Recovered
without dropping any scientific material:

- figure heights 5.0 -> 4.2 and 3.1 -> 2.6;
- caption text that duplicated the body immediately around it, in the solver
  benchmark figure, the capability table and the warm-started path table;
- four `###` headings the reviewer's own outline lists as single items;
- float separations 12pt -> 7pt;
- tightened the roadmap, the reproducibility paragraph and the synthesis.

Back to 20 pages, with the bibliography ending on page 20.

### Caught by the restructure

Reordering renumbered the tables. The memory-mode table's caption carried a
hardcoded "Table 3" pointing at the scaling table, which the reorder made Table 5.
Rephrased to "the scaling table below", which cannot go stale. It was the only
hardcoded float number in the article; there are no prose cross-references to
section numbers.
