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
  - name: University of Vienna, Austria
    index: 1
date: 08 May 2026
repository: https://github.com/gcol33/couplr
bibliography: paper.bib
header-includes:
  - \rightlinenumbers
---

# Summary

`couplr` [@couplr] is an R package for optimal pairing, matching, and assignment.
It solves linear assignment problems: given two sets of objects and a cost for
each possible pair, choose the pairs with minimum total cost. That simple
operation appears in causal inference, experimental design, task allocation,
sample pairing, and image alignment.

The package provides two entry points. High-level matching functions such as
`match_couples()`, `ps_match()`, `full_match()`, `cem_match()`, and
`subclass_match()` prepare data, build distances, enforce constraints, and
return analysis-ready matched data. Low-level functions such as `lap_solve()`,
`lap_solve_batch()`, `lap_solve_kbest()`, and `assignment_duals()` expose a
collection of assignment solvers directly for users who already have a cost
matrix. This lets the same package support both statistical matching workflows
and general-purpose assignment tasks.

# Statement of need

Matching is a common way to make treated and control groups more comparable
before downstream analysis. In practice, researchers often need more than a
single nearest-neighbor match on a propensity score. They may need exact
blocking by site, calipers on multiple variables, pairwise distances on observed
covariates, variable-ratio full matching, replacement matching, or direct
control over the assignment algorithm. Those choices are scattered across R
packages and usually require users to move between data preparation, distance
calculation, solver calls, and balance checks by hand.

`couplr` packages this workflow around the linear assignment problem. Users can
match directly on observed covariates, add blocking and distance constraints,
inspect standardized differences and distributional diagnostics, then join the
matched groups for analysis. The lower-level interface remains available when
the matching problem has already been reduced to a cost matrix, which matters
for operations research and scientific computing use cases where rows and
columns are not treated and control units.

This split is important. A causal-inference user should not need to know which
solver handles rectangular matrices well, and an operations researcher should
not need the package to impose a statistical matching vocabulary. `couplr`
keeps those layers separate while sharing the same tested assignment core.

# State of the field

Several R packages address parts of this problem. `MatchIt` provides a widely
used interface for preprocessing and matching in causal inference, with strong
support for propensity score workflows and downstream balance assessment
[@MatchIt]. `optmatch` implements optimal and full matching through network flow
formulations [@optmatch]. `cobalt` supplies balance diagnostics that work across
matching packages [@cobalt]. For the linear assignment problem itself, R users
often reach for packages such as `clue`, `lpSolve`, or domain-specific wrappers
around one or two solvers [@clue; @lpSolve].

`couplr` is designed for the gap between these layers. It provides a
model-light interface for direct covariate matching, exports compatibility
methods for `MatchIt` and `cobalt`, and includes a broad collection of native
C++ solvers under a single R API. The solver set covers classical dense
assignment, sparse and rectangular variants, k-best assignment, bottleneck
assignment, min-cost flow formulations, and entropy-regularized transport. This
lets users start with a statistical matching task and still keep access to the
algorithmic machinery when the problem size or structure changes.

# Software design

The package has three main layers. The first layer validates inputs and converts
data frames into cost matrices. It handles scaling, missing data checks,
blocking, calipers, maximum-distance rules, and user-supplied weights. The
second layer solves the assignment problem. It includes implementations of the
Hungarian method [@Kuhn1955], Jonker-Volgenant shortest augmenting paths
[@JonkerVolgenant1987], auction algorithms [@Bertsekas1988], cost scaling
[@GoldbergKennedy1995], Gabow-Tarjan scaling [@GabowTarjan1989],
push-relabel min-cost flow ideas [@GoldbergTarjan1988], network simplex,
Ramshaw-Tarjan rectangular assignment [@RamshawTarjan2012], and related
specialized solvers. The third layer returns structured result objects with
pairs, costs, diagnostics, weights, subclass information, and conversion methods
for other R matching tools.

The input layer supports several distance choices and constraint types.
Distances are computed from numeric covariate columns using Euclidean,
Manhattan, Mahalanobis, Chebyshev, or squared-Euclidean metrics, or from a
user-supplied function; propensity-score distances are available through a
separate entry point. Variables can be standardized to mean zero and unit
variance, rescaled to the unit interval, or robust-scaled using the median and
median absolute deviation. Per-variable weights are folded into the distance by
multiplying each column by the square root of its weight, so that squared
differences accumulate with the intended weighting. Three constraint mechanisms
reshape the cost matrix before it reaches the solver: a global `max_distance`
ceiling, per-variable calipers expressed as absolute-difference thresholds, and
explicit forbidden pairs supplied as index pairs. Forbidden cells receive a
large finite cost rather than infinity, which keeps the matrix usable across
all solvers, including those that assume finite arithmetic. Blocking is built
on top of these mechanisms: stratified matching either reuses an existing
factor variable or constructs blocks via $k$-means or hierarchical clustering,
and the solver is then called once per block.

The automatic solver selection in `lap_solve(method = "auto")` uses matrix
shape, sparsity, cost type, and problem size to choose a method. Dense square
problems can use fast dense solvers such as Jonker-Volgenant or cost scaling;
sparse matrices can use sparse augmenting-path solvers; rectangular problems can
avoid padding through rectangular assignment methods. Integer scaling solvers
are exposed where their assumptions are met. For example, the package documents
Gabow-Tarjan as an integer scaling method with theoretical dense square
complexity $O(n^3 \log C)$, or $O(n^3 \log(nC))$ under the common bound that
includes problem size and maximum cost.
Figure \ref{fig:benchmark} shows observed median solve time across problem sizes
for all 18 solvers; benchmarks are reproducible via `paper/make-figure.R`.

![(a) Median wall-clock solve time versus problem size $n$ for all 17
assignment solvers in `couplr`, arranged as five small-multiple panels grouped
by algorithm family (JV / augmenting-path, Auction, Cost-scaling, Flow-based,
and Other; panel headers abbreviated for space). Within each panel, individual solvers share a single family colour
and are distinguished by line style; all panels share log--log axes. The
*Other* panel collects single-solver families and special cases: Hungarian
(classical baseline), Ramshaw--Tarjan (rectangular), HK-01 (binary $0/1$
costs, its target regime), and Bruteforce (only evaluated at
$n \in \{4, 6, 8\}$). (b) The package's `auto` dispatcher ($\blacksquare$)
against the classical Hungarian baseline ($\bullet$); annotated points mark
the speed-up at diagnostic $n$. Each point is the median of 5 replicates on a
single core; integer cost matrices with entries in $[1, 10{,}000]$.
Reproducible from `paper/make-figure.R`.
\label{fig:benchmark}](figures/benchmark.png){width=100%}

The matching layer returns ordinary R objects with print, summary, plot, and
join methods. This keeps the output usable in base R, tidyverse pipelines, and
causal-inference workflows that expect matched data, weights, or subclasses.

# Research impact statement

`couplr` is already released on CRAN and has continuous integration across
Windows, macOS, and Linux. Its documentation includes vignettes for getting
started, matching workflows, algorithm selection, troubleshooting, and
pixel-level morphing examples. The package is useful for experimental designs
where samples must be paired before measurement, observational studies that
need transparent covariate matching, and allocation problems where each object
must be assigned once under costs and constraints.

The research contribution is practical: `couplr` brings several assignment
methods into one R package and connects them to preprocessing, constraints,
balance diagnostics, sensitivity analysis, and joined output. The result is a
shorter path from a scientific matching question to a reproducible set of
pairs.

# AI usage disclosure

The package was developed using a modern AI-assisted developer stack. The
author designed the package architecture, made the algorithm-selection choices,
and wrote the implementation, using a self-hosted
Qwen3-Coder-Next-REAP-48B-A3B model (48B-parameter mixture-of-experts coder,
~3B active per token, Router-weighted Expert Activation Pruning, 4-bit MLX
quantization, running on-device on a single Apple M4 Pro with no cloud
inference) for code completion and routine generation, integrated through
Anthropic's Claude Code CLI configured to route to the local model.

# Acknowledgements

No external funding supported this work.

# References
