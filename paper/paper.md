---
title: "couplr: Optimal pairing and matching via linear assignment"
tags:
  - R
  - matching
  - assignment problem
  - causal inference
  - operations research
authors:
  - name: Gilles Colling
    orcid: 0000-0003-3070-6066
    corresponding: true
    affiliation: 1
affiliations:
  - name: Department of Botany and Biodiversity Research, University of Vienna, Austria
    index: 1
date: 15 May 2026
repository: https://github.com/gcol33/couplr
bibliography: paper.bib
header-includes:
  - \rightlinenumbers
---

# Summary

`couplr` [@couplr] is an R package for optimal pairing, matching, and
assignment. It solves linear assignment problems: given two sets of objects
and a cost for each possible pair, choose the pairs with minimum total cost.
That operation appears in causal inference, experimental design, task
allocation, sample pairing, and image alignment.

The package provides two entry points. A high-level data-frame interface
prepares data, builds distances, enforces constraints, and returns
analysis-ready matched output for optimal, greedy, propensity-score, full,
coarsened-exact, subclassification, and cardinality matching, with balance
diagnostics and Rosenbaum sensitivity bounds [@Rosenbaum2002]. A low-level
interface exposes the solvers directly when the user already has a cost
matrix. An automatic dispatcher routes each problem to one of 18 internal
solvers based on shape, sparsity, cost type, and size, so the same package
supports statistical matching workflows and general-purpose assignment
tasks.

# Statement of need

A serious matching workflow in R today threads three packages by hand: one
for the assignment solver, one for preprocessing and constraint construction,
one for balance diagnostics. Any non-default choice (exact blocking,
calipers on multiple variables, pairwise distances on observed covariates,
variable-ratio full matching, replacement matching, direct control over the
solver) pulls the user into a different corner of that toolchain.

`couplr` packages the workflow around the linear assignment problem. A
causal-inference user starts with a data frame, observed covariates, and a
treatment indicator; an operations-research user starts with a cost matrix
already in hand. The same solver core serves both.

# State of the field

`MatchIt` [@MatchIt], `optmatch` [@optmatch], `Matching` [@Matching], and
`designmatch` [@designmatch] handle preprocessing and matching for causal
inference; `cobalt` supplies cross-package balance diagnostics [@cobalt].
For the assignment problem itself, R users reach for `clue` (Hungarian via
`solve_LSAP`) or `lpSolve` (general LP) [@clue; @lpSolve]. The
causal-inference packages assume a treated/control framing and route through
a single fixed back-end (`optmatch` for `MatchIt`, network flow for
`optmatch`, genetic search for `Matching`, integer programming via
commercial solvers for `designmatch`); the solver-focused packages expose
one algorithm each without the preprocessing, constraint, or diagnostic
machinery a matching workflow needs.

`couplr` combines both. It treats covariate distance as the primary entry
point rather than a side option behind a propensity score, and ships 18
assignment solvers spanning classical dense assignment, sparse and
rectangular variants, k-best [@Murty1968], bottleneck, min-cost flow, and
entropy-regularized transport, with an automatic dispatcher.

# Software design

The package has three layers. The first validates inputs and converts data
frames into cost matrices, handling scaling, missing-data checks, blocking,
calipers, maximum-distance rules, and user weights. The second solves the
linear assignment problem: given a cost matrix
$C \in \mathbb{R}^{n \times m}$ with entries $C_{ij}$ for row
$i \in \{1, \ldots, n\}$ and column $j \in \{1, \ldots, m\}$, find a binary
assignment matrix $X \in \{0,1\}^{n \times m}$ ($X_{ij} = 1$ iff row $i$ is
matched to column $j$) solving

$$
\min_{X} \; \sum_{i,j} C_{ij} X_{ij}
\quad \text{subject to} \quad
\sum_{j} X_{ij} \le 1 \ \forall i, \quad
\sum_{i} X_{ij} \le 1 \ \forall j.
$$

The bipartite structure makes the LP relaxation integral (Birkhoff–von
Neumann), so row potentials $u_i \in \mathbb{R}$ ($i = 1, \ldots, n$) and
column potentials $v_j \in \mathbb{R}$ ($j = 1, \ldots, m$) satisfying

$$
u_i + v_j \le C_{ij} \quad \forall (i,j),
\qquad
u_i + v_j = C_{ij} \quad \text{whenever } X_{ij} = 1
$$

certify optimality; `assignment_duals()` returns $(u, v)$. Worst-case time
is $O(n^3)$ for Hungarian and Jonker–Volgenant,
$O(n^3 \log(n\,C_{\max}))$ for cost scaling, and
$O(n^{3/4} m \log(n\,C_{\max}))$ for Gabow–Tarjan bit scaling, where
$C_{\max} = \max_{i,j} |C_{ij}|$ is the largest cost magnitude; the
dispatcher selects among solvers using $n$, $m$, sparsity, and
rectangularity. All 18 solvers
are implemented from scratch in C++ via Rcpp and RcppEigen, so the package
adds no external solver dependency: the Hungarian method [@Kuhn1955],
Jonker–Volgenant shortest augmenting paths [@JonkerVolgenant1987], auction
algorithms [@Bertsekas1988], cost scaling [@GoldbergKennedy1995],
Gabow–Tarjan scaling [@GabowTarjan1989], push-relabel min-cost flow ideas
[@GoldbergTarjan1988], network simplex, Ramshaw–Tarjan rectangular
assignment [@RamshawTarjan2012], and related specialized solvers. The third layer returns structured result objects with
pairs, costs, diagnostics, weights, subclass information, and conversion
methods for other R matching tools.

Distances cover Euclidean, Manhattan, Mahalanobis (pooled within-group
covariance by default), Chebyshev, and squared-Euclidean metrics, or a
user-supplied function; propensity-score distances use a separate entry
point. Three constraint mechanisms reshape the cost matrix before it reaches
the solver: a global `max_distance` ceiling, per-variable calipers, and
forbidden pairs encoded as large finite costs so the matrix stays usable
across all solvers. Blocking either reuses a factor variable or constructs
blocks via $k$-means or hierarchical clustering, and the solver is called
once per block.

The dispatcher in `lap_solve(method = "auto")` chooses by matrix shape,
sparsity, cost type, and size: dense square problems use Jonker–Volgenant
or cost scaling; sparse matrices use sparse augmenting-path solvers;
rectangular problems avoid padding through rectangular assignment methods.
Figure \ref{fig:benchmark} shows median solve time across problem sizes for
all 18 solvers; benchmarks are reproducible via `paper/make-figure.R`.

![(a) Median wall-clock solve time versus problem size $n$ for all 18
assignment solvers in `couplr`, arranged as five small-multiple panels
grouped by algorithm family (JV / augmenting-path, Auction, Cost-scaling,
Flow-based, and Other). Within each panel, individual solvers share a
single family colour and are distinguished by line style; all panels share
log--log axes. (b) The package's `auto` dispatcher ($\blacksquare$) against
the classical Hungarian baseline ($\bullet$); annotated points mark the
speed-up at diagnostic $n$. Each point is the median of 5 replicates on a
single core; integer cost matrices with entries in $[1, 10{,}000]$.
Reproducible from `paper/make-figure.R`.
\label{fig:benchmark}](figures/benchmark.png){width=100%}

`balance_diagnostics()` reports standardized mean differences, variance
ratios, and Kolmogorov–Smirnov statistics, summarised through
`balance_table()`; `sensitivity_analysis()` reports the critical Rosenbaum
$\Gamma$ at which inference would no longer be significant [@Rosenbaum2002].
Blocked designs surface per-block diagnostics so imbalance is not hidden by
averaging across strata.

# Quickstart

The package ships with `hospital_staff`, a small example dataset for a
treated/control workflow that runs without external data.

```r
library(couplr)

data(hospital_staff)
treated <- transform(hospital_staff$nurses_extended,   id = nurse_id)
control <- transform(hospital_staff$controls_extended, id = nurse_id)

# Optimal one-to-one matching on three pre-treatment covariates, with
# automatic scaling and solver selection.
m <- match_couples(
  left  = treated,
  right = control,
  vars  = c("age", "experience_years", "certification_level"),
  auto_scale = TRUE
)

# Balance on matching variables; analysis-ready paired output.
bal <- balance_diagnostics(m, treated, control,
  vars = c("age", "experience_years", "certification_level"))
balance_table(bal)
matched <- join_matched(m, treated, control)
```

On this dataset the matched pairs achieve absolute standardized differences
below $0.15$ on all three matching covariates. The same `match_couples()`
call accepts `calipers`, `max_distance`, `block_id`, `method`, `replace`,
and `ratio` for harder problems. Identification also requires conditional
ignorability, positivity, and SUTVA; the matched output is designed to feed
into an outcome regression for a doubly robust estimate [@Stuart2010].

# Comparison against established alternatives

We compare `couplr` against `MatchIt` and `optmatch` on runtime and feature
coverage. The matching task is fixed: 1-to-1 optimal matching on a
Mahalanobis distance with pooled within-group covariance, the convention
`optmatch::match_on()` uses by default and `couplr` adopts as of 1.3.1. On
the canonical Lalonde NSW dataset [@LaLonde1986; @DehejiaWahba1999] (185
treated, 429 control, eight covariates) all three packages produce identical
absolute standardized mean differences to three decimal places, reducing
the maximum $|\text{SMD}|$ from $1.757$ to $1.053$ on `race_black` (a
residual no 1-to-1 matcher can resolve here) and the remaining seven
covariates to $\le 0.124$ [@Stuart2010]; per-covariate values are in
`paper/lalonde-per-covariate.csv`.

Table \ref{tab:scaling} reports median wall-clock time on synthetic
problems with the same eight-covariate structure, at five sizes from
$n = 500$ to $n = 20{,}000$ with treated:control = 1:2. `couplr` is
$1.5$–$1.6\times$ faster than `optmatch` and $2.0$–$2.5\times$ faster than
`MatchIt` where all three complete; at $n = 20{,}000$ `couplr` finishes in
$3.5$ minutes, `optmatch` in $11$ minutes ($3.1\times$ slower), and
`MatchIt::matchit(method = "optimal")` aborts inside the `optmatch` backend
with an integer-size overflow. `couplr` reaches large dense problems
through its dispatcher, which routes to Jonker–Volgenant
[@JonkerVolgenant1987], auction with scaling [@Bertsekas1988], and
cost-scaling [@GoldbergKennedy1995] algorithms.

Table: 1-to-1 optimal Mahalanobis matching, median wall-clock by problem
size. Treated:control = 1:2; eight covariates; pooled within-group
covariance; single core, single-threaded BLAS. Median of 5 / 5 / 3 / 3 / 1
replicates respectively for the rows; `optmatch_max_problem_size` set to
`Inf` for $n \ge 10{,}000$. `int overflow` marks an integer-size overflow
(\texttt{result would exceed 2\textasciicircum 31-1 bytes}) inside the
`optmatch` backend reached through `MatchIt`. Reproducible from
`paper/bench_scaling.R` and `paper/bench_scaling_alternatives.R`.
\label{tab:scaling}

| Problem size ($n_t + n_c$) | `couplr` |  `optmatch`  |   `MatchIt`  |
| :------------------------- | -------: | -----------: | -----------: |
| $167 + 333$                |   80 ms  |       120 ms |       190 ms |
| $667 + 1{,}333$            |  1.37 s  |       2.09 s |       3.40 s |
| $1{,}667 + 3{,}333$        |  10.1 s  |       16.4 s |       23.8 s |
| $3{,}333 + 6{,}667$        |  53.7 s  |       79.3 s |        110 s |
| $6{,}667 + 13{,}333$       |   210 s  |        657 s | int overflow |

Table \ref{tab:capability} summarises feature coverage. `couplr` exposes 18
solvers through a single dispatcher, supports k-best, bottleneck, and
Rosenbaum sensitivity bounds in the core package, and accepts both data
frames and user-supplied cost matrices.

Table: Differentiating feature coverage; rows where all three packages
support the feature (covariate distance, propensity-score matching, exact
blocking, cost-matrix entry point) are omitted. \emph{partial} indicates the
feature is achievable but not via a first-class user-facing API.
\label{tab:capability}

| Feature                                | `couplr` | `MatchIt` | `optmatch` |
| :------------------------------------- | :------: | :-------: | :--------: |
| Per-variable calipers (named vector)   | yes      | yes       | partial    |
| Rectangular $n_t \neq n_c$, no padding | yes      | partial   | partial    |
| Sparse-cost support                    | yes      | no        | yes        |
| k-best assignments (Murty)             | yes      | no        | no         |
| Bottleneck (minimax) assignment        | yes      | no        | no         |
| Rosenbaum sensitivity bounds           | yes      | no        | no         |

<!-- The AI usage disclosure below is a required JOSS section and is excluded
from the 750–1750 word target. -->

# AI usage disclosure

The package was developed using a modern AI-assisted developer stack. The
author designed the package architecture, made the algorithm-selection choices,
and wrote the implementation in a TUI-first command-line workflow, using a self-hosted
Qwen3-Coder-Next-REAP-48B-A3B model (mixture-of-experts coder, ~3B active per
token, 4-bit MLX quantization, running on a single Apple M4 Pro) for code
completion, refactoring, and boilerplate generation through Anthropic's Claude Code CLI,
with requests routed to the local model and no cloud inference.

# Acknowledgements

No external funding supported this work.

# References
