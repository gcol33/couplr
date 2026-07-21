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
date: 20 July 2026
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
matrix. An automatic dispatcher routes each problem to one of 19 internal
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
point rather than a side option behind a propensity score, and ships 19
assignment solvers spanning classical dense assignment, sparse and
rectangular variants, k-best [@Murty1968], bottleneck, min-cost flow, and
entropy-regularized transport, with an automatic dispatcher. To our
knowledge, `couplr` provides the first publicly available open-source
implementation of the Gabow–Tarjan bit-scaling assignment algorithm
[@GabowTarjan1989] in any language; the algorithm's
$O(\sqrt{n}\, m \log(n\,C_{\max}))$ bound has stood since 1989 without a
library counterpart.

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
$O(\sqrt{n}\, m \log(n\,C_{\max}))$ for Gabow–Tarjan bit scaling, where
$C_{\max} = \max_{i,j} |C_{ij}|$ is the largest cost magnitude; the
dispatcher selects among solvers using $n$, $m$, sparsity, and
rectangularity. All 19 solvers
are implemented from scratch in C++ via Rcpp and RcppEigen, so the package
adds no external solver dependency: the Hungarian method [@Kuhn1955],
Jonker–Volgenant shortest augmenting paths [@JonkerVolgenant1987], auction
algorithms [@Bertsekas1988], cost scaling [@GoldbergKennedy1995],
Gabow–Tarjan scaling [@GabowTarjan1989], push-relabel min-cost flow ideas
[@GoldbergTarjan1988], network simplex, Ramshaw–Tarjan rectangular
assignment [@RamshawTarjan2012], and related specialized solvers. Each optimal solver
is checked against brute-force enumeration over randomised integer,
fractional, rectangular, maximizing, and forbidden-edge instances, so the
shipped path and the verified path are the same implementation. The third
layer returns structured result objects with pairs, costs, diagnostics,
weights, subclass information, and conversion methods for other R matching
tools.

Distances cover Euclidean, Manhattan, Mahalanobis (pooled within-group
covariance by default), Chebyshev, and squared-Euclidean metrics, or a
user-supplied function; propensity-score distances use a separate entry
point. Three constraint mechanisms reshape the cost matrix before it reaches
the solver: a global `max_distance` ceiling, per-variable calipers, and
forbidden pairs marked as non-finite entries, which every solver recognises
as a missing edge and uses to short-circuit on infeasibility. Blocking either reuses a factor variable or constructs
blocks via $k$-means or hierarchical clustering, and the solver is called
once per block.

The dispatcher in `lap_solve(method = "auto")` chooses by shape, sparsity,
cost type, and size: dense problems use Jonker–Volgenant; matrices over half
forbidden use its sparse variant `lapmod`; strongly rectangular problems
avoid padding through shortest augmenting paths; binary and very small
problems use specialized solvers. Other solvers are named explicitly.
Figure \ref{fig:benchmark} shows median solve time across problem sizes for
all 19 solvers.

![(a) Median wall-clock solve time versus problem size $n$ for all 19
assignment solvers in `couplr`, arranged as five small-multiple panels
grouped by algorithm family (JV / augmenting-path, Auction, Cost-scaling,
Flow-based, and Other). Within each panel, individual solvers share a
single family colour and are distinguished by line style; all panels share
log--log axes. Median of 5 replicates on a single core of an Apple M4 Pro;
integer cost matrices with entries in $[1, 10{,}000]$. (b) Absolute standardized mean
differences on the LaLonde NSW data (185 treated, 429 control, eight
covariates) before and after 1-to-1 optimal Mahalanobis matching. After
matching, `couplr`, `MatchIt`, and `optmatch` produce identical $|SMD|$ to
three decimal places, so a single matched point per covariate is shown.
Seven of the eight covariates fall below the 0.1 balance threshold (dashed
line); the `race_Black` indicator is reduced from $1.757$ to $1.053$, a residual no
1-to-1 matcher can resolve here. Reproducible from `paper/make-figure.R`
and `paper/bench_lalonde.R`.
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
call accepts `calipers`, `max_distance`, `block_id`, `method`, `strategy`,
`replace`, and `ratio` for harder problems. Identification also requires conditional
ignorability, positivity, and SUTVA; the matched output is designed to feed
into an outcome regression for a doubly robust estimate [@Stuart2010].

# Comparison against established alternatives

We compare `couplr` against `MatchIt` and `optmatch` on runtime and feature
coverage. The matching task is fixed: 1-to-1 optimal matching on a
Mahalanobis distance with pooled within-group covariance, the convention
`optmatch::match_on()` uses by default and `couplr` adopts as of 1.3.1. On
the canonical Lalonde NSW dataset [@LaLonde1986; @DehejiaWahba1999] (185
treated, 429 control, eight covariates) all three packages produce identical
absolute standardized mean differences to three decimal places
(Figure \ref{fig:benchmark}b), with the remaining seven covariates at
$\le 0.128$ [@Stuart2010]; per-covariate values are in
`paper/lalonde-per-covariate.csv`.

Table \ref{tab:scaling} reports median wall-clock time on synthetic
problems with the same eight-covariate structure, at six sizes from
$n = 500$ to $n = 50{,}000$ with treated:control = 1:2. The margin widens
with problem size: `couplr` is $9\times$ faster than `optmatch` at
$n = 500$ and $24\times$ faster at $n = 20{,}000$, and $11\times$ to
$24\times$ faster than `MatchIt` up to $n = 10{,}000$, the largest size
`MatchIt` completes. At $n = 20{,}000$ `couplr` finishes in $11.6$ seconds
against $4.7$ minutes for `optmatch`, while
`MatchIt::matchit(method = "optimal")` aborts inside the `optmatch` backend
with an integer-size overflow. At $n = 50{,}000$ `couplr` completes in
$83$ seconds; both alternatives exceed the $300$-second per-replicate cap.
`couplr` reaches large dense problems through its dispatcher, which routes
them to Jonker–Volgenant [@JonkerVolgenant1987].

Table: 1-to-1 optimal Mahalanobis matching, median wall-clock by problem
size. Treated:control = 1:2; eight covariates; pooled within-group
covariance; single core, single-threaded BLAS, on an Apple M4 Pro.
Median of 5 / 5 / 3 / 3 / 1 / 1 replicates respectively for the rows;
`optmatch_max_problem_size` set to `Inf` for $n \ge 10{,}000$.
`int overflow` marks an integer-size overflow
(\texttt{result would exceed 2\textasciicircum 31-1 bytes}) inside the
`optmatch` backend reached through `MatchIt`; `timeout` marks a replicate
exceeding the $300$-second cap. Reproducible from
`paper/bench_scaling.R` and `paper/bench_scaling_alternatives.R`.
\label{tab:scaling}

| Problem size ($n_t + n_c$) | `couplr` |  `optmatch`  |   `MatchIt`  |
| :------------------------- | -------: | -----------: | -----------: |
| $167 + 333$                |   11 ms  |        99 ms |       117 ms |
| $667 + 1{,}333$            |  146 ms  |       1.70 s |       2.11 s |
| $1{,}667 + 3{,}333$        |  749 ms  |       13.1 s |       15.9 s |
| $3{,}333 + 6{,}667$        |  3.07 s  |       60.2 s |       73.3 s |
| $6{,}667 + 13{,}333$       |  11.6 s  |        279 s | int overflow |
| $16{,}667 + 33{,}333$      |  83.1 s  |      timeout |      timeout |

Table \ref{tab:capability} summarises feature coverage. `couplr` exposes 19
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
