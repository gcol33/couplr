---
document: paper/rjournal/rjournal.Rmd (couplr, R Journal submission)
round: 1
received: 2026-08-25
reviewed_pdf: rjournal(1).pdf, 20 pages
verdict: major revision before submission
---

I reviewed all 20 pages of rjournal(1).pdf, inspected the public article source and reproduction scripts, checked the current package documentation, and compared the manuscript with the relevant matching and optimization literature available as of 25 August 2026.

## Overall verdict

**Major revision before submission.**

The package and central contribution are strong enough for *The R Journal*. The manuscript is unusually good at explaining software architecture, solver dispatch, dual variables, testing, and limitations. The adaptive edge-generation result is genuinely useful.

However, I would not submit this version yet. In its present form:

* The title and several central claims overstate what the floating-point certificate proves.
* A Gabow–Tarjan complexity bound is written incorrectly.
* The novelty discussion omits directly relevant sparse and incremental matching literature.
* The performance evidence is too narrow for some of the dispatcher and scalability claims.
* The publicly available reproduction materials appear not to regenerate the files consumed by the manuscript.
* At least two explicit R Journal requirements are not met: the abstract exceeds 250 words, and the reproduction package list is incomplete.
* The paper, CRAN package, bibliography, and website report inconsistent versions.

If submitted today, I would expect either a technical return before review or a major-revision verdict.

## The strongest parts

The paper has substantial merits:

* The unification of assignment and statistical matching through one internal flow representation is compelling.
* Returning potentials and exposing separate verification functions is a meaningful usability contribution.
* The explanation of the missing "unmatched-column" half of complementary slackness is excellent.
* The adaptive restricted-graph solve followed by pricing of omitted edges is mathematically sound in exact arithmetic.
* The object model, solver registry, testing strategy, dependency discussion, and memory modes are unusually transparent.
* The manuscript clearly admits important benchmarking limitations instead of concealing them.
* The layout is clean: no clipping, overprinting, broken equations, or unreadable tables. Figure 1 is dense but still legible.
* The source uses `rjtools`, evaluates examples, and supplies useful alternative text for both figures.

Those strengths make this a revision problem, not a fundamentally unsuitable paper.

# 1. Critical mathematical and correctness issues

## 1.1 "Provably optimal" is stronger than the implemented certificate

This is the most serious issue.

The title is:

> *couplr: Provably Optimal Matching with Sparse Edge Generation*

The manuscript repeatedly calls the verifier a "proof," "exact," and "provably optimal." But the documented verifier uses double-precision quantities and accepts inequalities within a tolerance, by default `1e-9`. The current [CRAN manual](https://cran.r-project.org/web/packages/couplr/couplr.pdf) states that dual feasibility accepts reduced costs down to `-tol`, and that the duality-gap threshold is scaled by objective magnitude.

That is an excellent **numerical a posteriori certificate**, but it is not an exact mathematical proof. For example, a truly negative reduced cost of magnitude below the permitted tolerance can be accepted. The manuscript acknowledges this on page 3—"a verdict therefore names the resolution"—but the title, abstract, contribution list, conclusion, and package manual do not consistently preserve that qualification.

You have two defensible options:

1. Keep double precision and describe the result as:

   * "verified to the reported numerical tolerance,"
   * "numerically certified optimal," or
   * "an a posteriori primal–dual optimality check."

2. Retain "provably optimal" only after adding a rigorous route such as:

   * exact verification for integer/rational costs;
   * interval arithmetic with outward rounding;
   * or a computed global error bound proving that numerical residuals cannot change the optimum.

My preferred title under the existing implementation is:

> **couplr: Optimal Matching with Verifiable Certificates and Sparse Edge Generation**

That is still strong and more precisely describes the contribution.

At minimum, define two distinct concepts in the paper and API:

* `exact_certificate`: a mathematical certificate in exact arithmetic;
* `numerical_certificate(tol)`: all residuals satisfy stated floating-point thresholds.

## 1.2 The dual problem is incomplete

Pages 2–3 give the dual constraints but never state the dual objective. The complete dual should be written explicitly:

\[
\max_{u,v}\ \sum_i u_i+\sum_j v_j
\]

subject to

\[
u_i+v_j\le C_{ij},\qquad v_j\le 0.
\]

Without the objective, the later phrases "dual objective" and "duality gap" appear without a formally stated dual program.

Also, the displayed equation currently combines dual feasibility with the matched-edge equality. The latter is complementary slackness, not a dual constraint. Separate them:

* Dual feasibility: \(u_i+v_j\le C_{ij}\), \(v_j\le0\).
* Complementary slackness:

  * \(X_{ij}=1\Rightarrow u_i+v_j=C_{ij}\);
  * an unmatched column implies \(v_j=0\).

## 1.3 "Four conditions" are useful checks, but not independent conditions

The paper lists:

1. primal feasibility;
2. dual feasibility;
3. complementary slackness;
4. objective equality.

In exact LP theory, objective equality follows from feasibility plus complementary slackness. Keeping all four in the software is valuable as a numerical cross-check, but the text should say "four reported checks," not imply four logically independent optimality conditions.

## 1.4 The Gabow–Tarjan complexity expression is incorrect

Page 4 gives approximately

\[
O(\sqrt n\,m\log(nC_{\max})),
\]

but the paper has already defined \(n\) and \(m\) as the two sides of the rectangular cost matrix. That is not the meaning of \(n\) and \(m\) in the original bound.

The [Gabow–Tarjan paper](https://doi.org/10.1137/0218069) gives

\[
O\!\left(\sqrt{|V||E|}\log(|V|N)\right),
\]

where \(|V|\) is the number of graph vertices, \(|E|\) the number of graph edges, and \(N\) the largest integer cost magnitude. For a complete \(n_L\times n_R\) bipartite graph, substitute \(|V|=n_L+n_R\) and \(|E|=n_Ln_R\).

This should be corrected in both the paper and package manual. The current package manual also gives a different `O(n^3 log C)` statement, so the documentation is internally inconsistent.

## 1.5 Integer scaling is underspecified

Page 8 states that real costs are scaled to integers before Gabow–Tarjan or the flow certificate is used. But it does not define:

* the scaling factor;
* the rounding rule;
* the maximum rounding error;
* whether optimality is for the scaled or original objective;
* how ties introduced by scaling are handled;
* overflow protection;
* or when integer conversion is refused.

If a real-valued assignment is replaced by an integer approximation, an exact optimum for the scaled problem need not be an exact optimum for the original real-valued problem.

You need either:

* a theorem giving sufficient scaling precision to preserve the optimum;
* a reported error/gap bound relative to the original costs;
* or a clear statement that the result is exact for the scaled instance only.

## 1.6 The sentinel argument needs a formal lemma

The maximum-cardinality-then-minimum-cost construction is plausible, but "greater than \((k+1)\) times the spread" is not sufficiently formal when:

* admissible costs may be negative;
* maximization is implemented by negation;
* costs may be very large or non-integral;
* and finite sentinels can overflow.

State the transformed objective and prove that replacing one sentinel edge with any possible combination of real edges always improves the lexicographic objective. Specify the actual formula, including any cost shift.

## 1.7 The binary-cost Hopcroft–Karp claim needs explanation

Hopcroft–Karp directly solves maximum-cardinality unweighted matching. A `{0,1}` minimum-cost assignment is not automatically identical to an arbitrary unweighted maximum-cardinality matching.

Explain how `hk01` minimizes the number of one-cost edges—perhaps by first maximizing zero-cost matches and then completing the matching—or provide a proposition and test oracle. "Hopcroft–Karp style" is currently too vague for a solver advertised as optimal.

## 1.8 The verifier's independence is slightly overstated

Architecturally, `verify_assignment()` is separate from the solver, which is good. But the package manual states that if the result lacks duals, the verifier obtains them by calling `assignment_duals()`, requiring a second solve with the same package.

Thus:

* verification of supplied external duals is independent of the producing solver;
* default verification of an ordinary `assignment()` result is not an independent implementation—it is a second package solve followed by residual checking.

Clarify this distinction. "Third-party check" should be reserved for the case where the primal assignment or duals come from an external implementation.

## 1.9 The exactness theorem for edge generation should be stated formally

The central idea deserves more than prose. Add pseudocode and a proposition:

> If the restricted master has a primal-feasible solution and optimal duals \((u,v)\), and every omitted edge has nonnegative reduced cost, then the restricted solution is optimal for the complete assignment problem.

Then give:

* exact-arithmetic proof;
* finite-termination argument;
* numerical version with tolerance;
* worst-case bound: the algorithm may add every edge;
* work bound: each complete pricing sweep is \(O(nm)\);
* storage bound in terms of final candidate edges;
* handling of an initially infeasible restricted graph;
* batching rule for violated edges;
* tie policy and deterministic behavior.

This would turn the paper's central contribution into a reviewable algorithm rather than an implementation narrative.

# 2. Novelty and missing related work

The method is valuable, but the novelty boundary is currently too broad.

## 2.1 The edge-generation loop is column generation

Starting with a restricted set of edge variables, solving the restricted master, pricing omitted variables by reduced cost, and adding negative-reduced-cost variables is classical column generation.

The manuscript should explicitly identify this connection and cite, for example, [Lübbecke and Desrosiers](https://doi.org/10.1287/opre.1050.0234). The software contribution can still be novel:

* an R-facing assignment/matching implementation;
* automatic dual pricing of pair edges;
* integration with matching designs and diagnostics;
* exposed numerical certificates;
* warm-started constraint paths.

But presenting the mechanism without naming column generation invites reviewers to regard the novelty discussion as incomplete.

## 2.2 Sparse optimal matching already has substantial literature

The statement that admissible pairs are generally materialized before solving is too sweeping.

Directly relevant work includes:

* [Pimentel et al. 2015](https://doi.org/10.1080/01621459.2014.997879), which develops large sparse optimal matching with refined balance.
* The current [`rcbalance`](https://cran.r-project.org/package=rcbalance) package, explicitly described as "large, sparse optimal matching."
* [Yu, Silber, and Rosenbaum 2020](https://doi.org/10.1214/19-STS699), which creates a radically sparser matching graph for very large administrative datasets.
* [Yu et al. 2022](https://doi.org/10.1080/10618600.2022.2058001), which makes the choice of sparse matching network central to graded matching.
* [`quickmatch`](https://cran.r-project.org/web/packages/quickmatch/quickmatch.pdf), a current package designed for fast near-optimal generalized full matching on large datasets.

These are not equivalent to couplr's adaptive reduced-cost pricing. That distinction should become the novelty claim:

> Existing sparse matching approaches build a prespecified sparse network through calipers, grades, balance structure, or locality. couplr adaptively expands a restricted graph until complete-graph dual feasibility is verified.

That is narrower and more convincing.

## 2.3 Incremental cost computation is also directly relevant

[Abeywickrama, Liang, and Tan 2021](https://doi.org/10.14778/3450980.3450983) presents an exact bipartite matching method that computes expensive edge costs incrementally using lower bounds and tests it on multiple real-world datasets.

This is particularly relevant to the `lazy` and `implicit` modes. The distinction is:

* Abeywickrama et al.: application-specific lower bounds avoid computing many exact costs.
* couplr: general dual pricing avoids storing most pairs, but currently still scans/evaluates all pairs during pricing.

That comparison would sharpen the paper considerably. It also supports the discussion section's proposed future metric lower-bound pruning.

## 2.4 Comparison with current packages is too narrow

Table 5 compares only couplr, MatchIt, and optmatch. Yet the manuscript claims contributions in cardinality matching, fine/refined balance, sparse matching, and solver selection.

At minimum, discuss:

* [`designmatch` 0.5.5](https://cran.r-project.org/package=designmatch);
* `rcbalance`;
* `quickmatch`;
* `clue`;
* possibly `RBestMatch`, clearly marked as archived;
* and `Matching`.

The table can remain focused, but the text needs a fuller ecosystem map.

## 2.5 optmatch deserves more nuanced treatment

It is fair to say optmatch does not export a user-facing dual certificate. But the current [optmatch manual](https://cran.r-project.org/web/packages/optmatch/optmatch.pdf) documents:

* `InfinitySparseMatrix`;
* sparse storage of permissible pairings;
* multiple LEMON algorithms;
* and a quantitative `tol × number of subjects` bound on deviation caused by internal rounding.

Therefore, characterize the distinction as:

* optmatch offers a solver tolerance guarantee and sparse distance structures;
* couplr exports primal–dual residuals, dual variables, and a separate verification object.

That is stronger and more accurate than "status only."

## 2.6 Position against current software papers

The 2025 JSS paper on [`dame-flame`](https://doi.org/10.18637/jss.v113.i02) is methodologically different, but it illustrates the current standard for matching-software articles: explicit assumptions, comparisons, multiple use cases, and substantial empirical evaluation.

Recent R Journal algorithm-package papers such as [ASML](https://journal.r-project.org/articles/RJ-2025-045/) also use explicit competitor tables and standardized benchmark scenarios. couplr's architecture explanation is stronger than many such papers, but its empirical breadth is weaker.

# 3. Benchmark and empirical-evidence review

## 3.1 Figure 1 validates only one dispatcher rule

Figure 1 uses square dense matrices with independent integer costs uniform on `[1,10000]`. This supports using JV on that regime, but not the rules for:

* sparsity;
* strong rectangularity;
* binary or constant costs;
* degeneracy;
* clustered metric costs;
* floating-point costs;
* forbidden-edge patterns.

The manuscript acknowledges this, but still exposes those rules as package defaults. Before publication, add a factorial benchmark varying:

* left/right aspect ratio;
* finite-edge density;
* cost type: integer, double, binary;
* cost distribution: uniform, clustered, tied, heavy-tailed;
* metric versus arbitrary cost;
* number and structure of forbidden components.

Ramshaw–Tarjan should be tested on strongly rectangular inputs, not only the square matrices used for the main figure.

## 3.2 Replicates measure timing noise, not instance variability

The scripts generate one instance per size and time the same instance repeatedly. Five repetitions therefore estimate execution-time variability, not performance variation across problem instances.

Use multiple independently generated matrices per cell, with timing repetitions nested within instances. Report medians and interquartile ranges across instances. Preserve raw per-run data rather than only pre-aggregated medians.

## 3.3 Table 2 is an end-to-end package comparison, not a solver comparison

The paper correctly acknowledges that:

* couplr sends the problem to a JV assignment solver;
* optmatch uses a general minimum-cost flow formulation;
* MatchIt delegates to optmatch.

Therefore the result demonstrates workflow performance for this particular one-to-one task, not that couplr has a faster implementation of an equivalent solver.

Also correct the phrase "dense square problem": the benchmark has a 1:2 treated/control ratio, so it is rectangular.

At the two largest sizes, a single successful timing is not a "median" in a meaningful statistical sense. Call it a single run and report uncertainty only where replication exists.

## 3.4 The implicit benchmark is too favorable and too narrow

All reported implicit runs:

* come from one eight-dimensional synthetic distribution;
* take exactly two rounds;
* build a rapidly shrinking share of edges;
* and use one seed per size.

That is an excellent demonstration, but not enough to characterize typical or worst-case behavior. Add:

* multiple random seeds;
* more dimensions;
* clustered and adversarial point clouds;
* near-tied distances;
* caliper-boundary cases;
* sparse/disconnected admissibility structures;
* instances where the nearest-neighbor seed is far from sufficient;
* instances where most or all edges eventually enter.

Report distributions of:

* rounds;
* candidate edges;
* pair evaluations;
* peak memory;
* wall time;
* and certificate residuals.

## 3.5 "Sparse" refers to storage, not necessarily computation

At the largest size, the method stores only 0.29% of possible edges, which is impressive. But Table 3 reports 1.42 complete-pair-count distance evaluations; at smaller sizes it reaches 2.00.

Thus the current algorithm is primarily:

* subquadratic in stored graph size on these examples;
* but still quadratic per pricing sweep in distance work.

The abstract should not imply that most pair costs are avoided. It avoids building the complete graph, not scanning its logical edge set.

## 3.6 Memory claims require memory measurements

The paper argues that memory is the limiting resource, but reports edge shares rather than actual memory.

Measure peak resident memory for:

* dense;
* lazy;
* implicit;
* optmatch;
* MatchIt.

Include memory used by:

* R objects;
* C++ workspaces;
* temporary conversion copies;
* garbage-collection lag;
* restricted master structures.

This is especially important because the automatic memory dispatcher uses a factor-of-four footprint heuristic.

## 3.7 Table 3 compares mode-plus-solver systems

Dense and lazy use JV, while implicit uses restricted minimum-cost flow. The paper acknowledges this, but the timing comparison can still be misread as a pure storage-mode comparison.

Report separately:

* representation comparison with a common solver where possible;
* full mode comparison as experienced by the user.

## 3.8 Table 4 should report end-to-end wall time

The caliper path table sums solver-internal per-point time. User-observed runtime may also include:

* feasibility checks;
* balance computations;
* object conversion;
* path construction;
* R-level work.

End-to-end wall time should be primary; internal solve time can remain a secondary diagnostic.

## 3.9 LaLonde example does not demonstrate a usable causal match

The post-match Black-race SMD is 1.053. That match is not satisfactory for causal inference, even if it is the minimum-distance one-to-one assignment.

The sentence:

> "a residual imbalance that no one-to-one matcher can resolve"

is too strong. A distance-minimizing one-to-one matcher without balance constraints fails to resolve it. Cardinality matching, exact/fine balance, or excluding difficult units can produce a different estimand and better balance.

This example currently proves objective agreement among packages, not match quality. That is a legitimate purpose, but say so explicitly.

Better still, add a second example using couplr's distinctive balance/cardinality functionality to obtain an actually defensible design.

## 3.10 The objective totals do not literally agree

The paper reports:

* couplr: 304.042;
* MatchIt/optmatch: 304.043.

It then says "They agree on the optimum." Explain whether this difference is:

* display rounding;
* optmatch's documented tolerance;
* a different pairing within numerical tolerance;
* or a cost-matrix reconstruction difference.

If exact equivalence is claimed, report full-precision objectives and the permitted tolerance.

## 3.11 The synthetic outcome example needs more careful causal language

The text initially says "association," which is appropriate, but then states "Full-time nurses earn 3.53 more." Prefer:

> "The matched mean hourly-rate difference is 3.53."

The simple paired t-test does not automatically account for all uncertainty introduced by matching or design selection. Since the dataset is synthetic, this should be framed as an API demonstration rather than substantive causal evidence.

For Rosenbaum bounds, \(\Gamma=1.5\) means treatment-assignment odds may differ by a factor of up to 1.5 within a matched pair due to unobserved differences. It is not literally one identified confounder "raising one pair member's odds by half."

And the analysis does not establish that significance "stops at 1.75"; the displayed grid only places the sensitivity value between 1.5 and 1.75 unless the function performs a finer search.

# 4. R Journal guideline audit

The current official requirements are in the [submission guidance](https://journal.r-project.org/submissions.html) and [package-paper guidelines](https://journal.r-project.org/R_package_guidelines.html).

| Requirement                                                |                      Status | Finding                                                                                                                                                        |
| ---------------------------------------------------------- | --------------------------: | -------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| No more than 20 pages                                      |                        Pass | Exactly 20 pages—the hard limit.                                                                                                                               |
| Abstract no more than 250 words                            |                    **Fail** | The source abstract contains 255 words. Reduce it safely to about 220–230.                                                                                     |
| Use `rjtools`                                              |                        Pass | The source uses `rjtools::rjournal_article`.                                                                                                                   |
| Title in title case; sections in sentence case             |                        Pass | Appears compliant.                                                                                                                                             |
| Plain-text title and abstract                              |                        Pass | No problematic markup.                                                                                                                                         |
| Evaluated R output, not manually typed                     |                Pass/partial | Article examples are evaluated. Benchmark tables read precomputed CSVs, which is allowed if regeneration scripts work.                                         |
| Figure accessibility text                                  |                        Pass | Both figures have substantial `fig.alt` text in the [Rmd source](https://raw.githubusercontent.com/gcol33/couplr/refs/heads/main/paper/rjournal/rjournal.Rmd). |
| PDF/HTML build sources supplied                            |                 Likely pass | Wrappers, Rmd, bibliography and style files are present publicly. Actual archive build was not executed.                                                       |
| Package on CRAN                                            |                     Partial | couplr is on CRAN, but the version used in the paper is not the current CRAN version.                                                                          |
| Fully reproducible results                                 | **Fail in public snapshot** | Script paths and output locations do not align with the Rmd inputs.                                                                                            |
| Complete `_Rpackages.txt`                                  |                    **Fail** | The [file lists only five packages](https://github.com/gcol33/couplr/blob/main/paper/rjournal/_Rpackages.txt).                                                 |
| Reproduction in under 10 minutes                           |                     Partial | Rendering from precomputed CSVs is probably fast; full benchmarks include 300-second timeouts. A documented quick/full workflow is required.                   |
| Code and data available in open formats                    |                Pass/partial | CSV outputs and R scripts are present; reproducibility orchestration is the problem.                                                                           |
| Motivating letter                                          |                 Likely pass | A letter is present publicly; I did not assess editorial persuasiveness in detail.                                                                             |
| Package tests, documentation, vignette, VCS, bug mechanism |                Largely pass | Tests, vignettes, GitHub, CRAN documentation and issues are present.                                                                                           |
| `data-raw` generation code for package datasets            |      **Recommended change** | The repository root does not expose a `data-raw` directory despite containing synthetic package data.                                                          |
| Avoid output-specific LaTeX where possible                 |                 Minor issue | The Rmd uses `\usepackage{placeins}` and `\FloatBarrier`. Probably acceptable, but check both HTML and PDF builds.                                             |

## 4.1 Reproduction scripts appear structurally broken

The public [scaling script](https://raw.githubusercontent.com/gcol33/couplr/refs/heads/main/paper/rjournal/scripts/bench_scaling.R):

* says to run `Rscript paper/bench_scaling.R`;
* requires the working directory to be the package root or a directory named `paper`;
* sources `paper/bench_common.R`;
* writes `paper/scaling-results.csv`;
* and loads the development package using `pkgload::load_all()`.

But in the article bundle:

* the script resides in `paper/rjournal/scripts/`;
* the shared script resides there too;
* and the Rmd reads `paper/rjournal/data/scaling-results.csv`.

Therefore the supplied script does not appear able to regenerate the CSV consumed by the manuscript when run from the submission folder.

Create a single top-level reproduction entry point such as:

```r
Rscript reproduce.R --mode quick
Rscript reproduce.R --mode full
```

It should:

* run from the submission directory;
* use paths relative to that directory;
* verify package versions;
* write outputs directly into `data/`;
* avoid silently installing packages;
* record `sessionInfo()`, compiler and BLAS details;
* distinguish instance seeds from timing repetitions;
* and verify regenerated CSV schemas before rendering.

## 4.2 `_Rpackages.txt` is incomplete

It lists only:

* couplr;
* ggplot2;
* knitr;
* rmarkdown;
* rjtools.

The benchmark scripts use or require at least:

* MatchIt;
* optmatch;
* R.utils;
* RhpcBLASctl;
* pkgload;
* cobalt;
* microbenchmark or other benchmark dependencies used by the remaining scripts.

List every package needed either to build the paper or regenerate any supplied result.

## 4.3 Versioning is inconsistent

The attached paper says results were produced with couplr 1.6.2.

But:

* the current [CRAN manual](https://cran.r-project.org/web/packages/couplr/couplr.pdf) is version 1.6.1, published 23 August 2026;
* the paper bibliography cites version 1.5.5;
* the [package website](https://gillescolling.com/couplr/) displays 1.5.5.

This prevents a reviewer from reproducing the manuscript from CRAN as stated.

Choose one immutable release and align:

* manuscript;
* CRAN;
* bibliography;
* supplementary data;
* scripts;
* website;
* Git tag;
* and ideally a Zenodo DOI.

If the paper requires 1.6.2, release it before submission or archive that exact source and say explicitly that the benchmarks use the archived development release.

## 4.4 Package-site claims also need revision

The website says the same pairing is returned "every run and every machine" and is independent of row order. With multiple optimal assignments, that claim requires a formally specified cross-platform tie-breaking rule. The paper itself discusses alternative optima, so the website language is stronger than the manuscript supports.

The website also still advises greedy matching when pools become too large, despite the newer lazy/implicit modes. Refresh it before submission.

# 5. Recommended section-by-section revisions

## Title and abstract

* Replace "provably optimal" or qualify it.

* Replace the opening "an answer arrives without a proof" with:

  > "Most R matching interfaces return solver status but not an independently inspectable primal–dual certificate."

* Replace the broad graph-materialization claim with:

  > "Mainstream general-purpose workflows often materialize a dense or prespecified sparse pair representation. couplr instead expands a restricted graph adaptively by reduced-cost pricing."

* State that the reported largest-case result is one benchmark instance, not a general scaling law.

* Reduce the abstract from 255 to under 250 words.

## Introduction

Add subsections or paragraphs on:

* classical column generation;
* rcbalance and large sparse matching;
* graded matching and large administrative-data matching;
* incremental edge-cost computation;
* quickmatch;
* the exact distinction between prespecified sparse networks and adaptive dual pricing.

## Mathematical section

* State the complete primal relaxation and dual program.
* Separate feasibility from complementary slackness.
* Add the edge-generation proposition and proof.
* Define numerical certification.
* Correct the Gabow–Tarjan complexity.
* Formalize sentinel construction and integer scaling.

## Software section

* Explain whether "19 solvers" counts aliases. The public API lists both `sap` and `ssp`; readers need to know whether these are separate algorithms or names for one implementation.
* Document deterministic tie handling.
* Explain how `hk01` optimizes binary costs.
* Clarify that a verifier may obtain duals through a second internal solve.
* State the worst-case behavior of implicit mode.

## Performance section

Add:

* multiple independent instances per benchmark cell;
* sparse and rectangular solver experiments;
* actual peak memory;
* adversarial implicit-mode cases;
* full-precision objective comparisons;
* end-to-end path timing;
* a statistically usable matching example;
* at least `designmatch`, `rcbalance`, and `quickmatch` in the related-work comparison.

# 6. Prioritized revision order

1. **Decide the exactness claim.** Either implement rigorous exact certification or change "proof/provably/exact" to tolerance-qualified language everywhere.
2. **Repair the mathematics.** Full dual, correct Gabow–Tarjan bound, scaling definition, sentinel lemma, edge-generation theorem.
3. **Repair reproducibility.** One runnable entry point, correct paths, complete package list, immutable version.
4. **Expand related work.** Column generation, sparse matching, graded matching, incremental cost computation, current R packages.
5. **Strengthen benchmarks.** Multiple instances, rectangular/sparse/degenerate cases, memory measurements and adverse implicit cases.
6. **Replace or supplement the LaLonde example.** Demonstrate that couplr can improve balance, not merely minimize a distance while leaving SMD 1.053.
7. **Run the formal checks.** `rjtools::initial_check_article()`, clean-machine build, spelling, PDF and HTML accessibility, and a timed reproduction test.
8. **Synchronize the release and website.**

## Final recommendation

The manuscript has a publishable core, and its software-engineering exposition is already better than average for an R package paper. The adaptive pricing, verifier API, common flow representation, and warm-started paths form a coherent contribution.

The necessary revision is nevertheless substantive. The principal risk is not that the software lacks value; it is that the paper currently describes a numerically checked and classically motivated optimization framework using language that implies a stronger proof and a broader novelty claim than the evidence supports. Tightening those claims, fixing the reproduction package, and broadening the benchmark/literature comparison would make this a credible and competitive R Journal submission.
