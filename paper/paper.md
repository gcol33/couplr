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

![(A) Median solve time versus problem size $n$ for all 16 general-purpose
assignment solvers in `couplr`, coloured by algorithm family and labelled
directly. Cycle-cancel and Orlin are capped at $n = 100$ due to super-cubic
scaling. (B) Automatic solver selection (`method = "auto"`) versus the
classical Hungarian baseline. `auto` dispatches to fast modern solvers and is
$8.6\times$ faster at $n = 200$ and $21\times$ faster at $n = 500$. Log-log
scale; integer cost matrices with entries in $[1, 10{,}000]$.
\label{fig:benchmark}](figures/benchmark.png)

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

Generative AI tools were used during development and paper preparation. OpenAI
Codex assisted with code review, refactoring, test scaffolding, benchmarking
scripts, and drafting this manuscript. The author reviewed, edited, and
validated AI-assisted outputs, ran the package tests and benchmarks, and made
the package design and submission decisions. The author remains responsible for
the correctness, originality, licensing, and compliance of the submitted
software and paper.

# Acknowledgements

No external funding supported this work.

# References
