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
date: 2 November 2026
repository: https://github.com/gcol33/couplr
bibliography: paper.bib
header-includes:
  - \usepackage{lineno}
  - \rightlinenumbers
---

# Summary

Researchers often need to pair similar observations. For example, to study
a treatment, they may match each treated person with an untreated person
of similar age and health and compare their outcomes.
Matching every observation in the smaller group to a different observation
in the larger group, so that the total dissimilarity of the pairs is as
small as possible, is known as the linear assignment problem.
`couplr` [@couplr] is an R package that solves this problem and provides
a certificate showing that no permitted matching has a smaller total
dissimilarity. Users can restrict which pairs are permitted, for example
by limiting the age difference within a pair. The certificate comes from
the dual of the assignment problem, a related optimisation problem that
assigns a price to each observation. These prices give a lower bound on
the total dissimilarity of any permitted matching, and the returned
matching attains that bound. For large datasets, the distances between all
possible pairs may not fit in memory, so `couplr` can store a subset of
candidate pairs and compute the remaining distances only when needed.
Because the prices are checked against every permitted pair, including
pairs that were never stored, the certificate still covers the complete
problem. In a benchmark with 50,000 observations, `couplr` stored
1.60 million of 555.56 million possible pairs and returned a certified
optimal matching. The package also reports how closely the matched groups
resemble each other.

# Statement of need

The memory needed for optimal matching grows with the number of possible
pairs. With $n$ observations in one group and $m$ in the other, there are
$nm$ pairwise distances: matching 10,000 observations to 20,000 potential
partners involves 200 million of them. A common way to save memory is to
store only the pairs of nearby observations. If this selection is fixed
before optimisation begins, only the stored pairs can appear in the
result, and the best matching among them can have a larger total
dissimilarity than the best matching among all pairs.

A second difficulty is knowing whether a returned matching is optimal. A
matching computation returns a matching and a solver status. The status
describes how the computation ended, and it gives the user nothing to
check the matching against.

`couplr` addresses both difficulties with the prices from the dual
problem. During optimisation, the prices identify the pairs outside the
stored subset that could still lower the total dissimilarity, so the
other pairs never need to be stored. Afterwards, the same prices let users
verify that the matching is optimal over all permitted pairs.
The package is intended for researchers who build matched samples from
observational data, and for users who need a verified solution to an
assignment problem stated as a cost matrix.

# State of the field

Existing R packages support both statistical matching and direct
assignment. `MatchIt` [@MatchIt] provides matching and subclassification,
including optimal pair and full matching through `optmatch` and genetic
matching through `Matching`. `optmatch` [@optmatch] supports network-flow
matching and sparse distance structures; `Matching` [@Matching] provides
multivariate and propensity-score matching; `designmatch` [@designmatch]
supports explicit balance constraints. `cobalt` [@cobalt] provides
diagnostics across packages. For supplied cost matrices,
`clue::solve_LSAP()` solves linear assignment and `lpSolve` supports
linear and integer programming [@clue; @lpSolve].

These packages obtain the pairwise distances in one of two ways.
`clue::solve_LSAP()` and `lpSolve` take a complete cost matrix. `optmatch`
can also store only the pairs within a distance limit, in which case the
pairs outside the limit are excluded from the matching problem. It solves
the problem after rounding the distances, keeps the node prices of its
network solver with the result, and documents how far the rounding can
place its total above the optimum. `couplr` differs in two respects.
First, pairs left out of storage remain part of the problem: the solver
computes their distances when needed, adds the pairs that could lower the
total, and stops when no such pair remains. Second, a matching from any
program can be checked against the unrounded costs with an exported
function, without repeating the optimisation. Both capabilities need the
solver's dual values and the
distance calculations to be available during optimisation, so `couplr`
implements the distance code, the solvers and the verification together.

# Software design

Figure \ref{fig:workflow} outlines the matching workflow, from data and
restrictions to matched pairs and diagnostics.

![Matching with reduced storage and explicit verification. (I) Data, distances, and restrictions define the problem. (II) Three cost representations serve the same matching objective. In implicit mode, omitted-pair checks expand the candidate set until verification succeeds or the search stops without certification. (III) Assignment certificates and balance diagnostics assess optimality and covariate balance, respectively.\label{fig:workflow}](figures/matching-concept.pdf){width=80%}

Users first specify variables, a distance measure, and restrictions
on which pairs are permitted. Calipers limit differences in individual
variables, maximum distances limit overall dissimilarity, and blocking
partitions the observations into separate assignments.

For one-to-one assignment, let $i = 1, \ldots, n$ index the observations
in the smaller group and $j = 1, \ldots, m$ those in the larger group, so
that $n \le m$. A pair $(i,j)$ is admissible if the restrictions permit
it; $E$ denotes the set of admissible pairs and $C_{ij}$ the cost of pair
$(i,j)$, a finite number that measures how different its two observations
are. The solver seeks a binary matrix $X$, with $X_{ij}=1$ when $i$ is
matched to $j$, that solves

$$
\min_X \sum_{(i,j)\in E} C_{ij}X_{ij},\qquad
\sum_{j:(i,j)\in E}X_{ij}=1\ \forall i,\qquad
\sum_{i:(i,j)\in E}X_{ij}\le1\ \forall j.
$$

When the restrictions leave some observation in the smaller group without
a partner, a maximum-cardinality matching identifies how many can be
matched; it is not a solution of the problem above.

Verification rests on the dual of this problem, which assigns a potential
$u_i$ to each observation in the smaller group and $v_j$ to each
observation in the larger group. These potentials are the prices of the
Summary. When they satisfy $u_i+v_j\le C_{ij}$ for every admissible pair,
and $v_j\le 0$ for every $j$ when $n<m$, their sum is a lower bound on
the total cost of any admissible matching. A matching $X$ attains this
bound, and is therefore optimal, when in addition

$$
u_i+v_j=C_{ij}\ \text{whenever } X_{ij}=1,\qquad
v_j=0\ \text{for every unmatched } j.
$$

Given a matching, its costs and potentials, `verify_assignment()` checks
each of these conditions together with the assignment constraints, and
`assignment_duals()` supplies potentials with a matching. Each condition
is the sign of a difference of floating-point numbers, which rounding can
change, so the check evaluates that sign exactly with floating-point
expansions [@Shewchuk1997]. The certificate states whether optimality was
established exactly for the supplied costs or numerically within a stated
tolerance.

The same potentials allow the costs to be stored in three ways with
different memory and computation trade-offs. Dense mode stores the full
cost matrix. Lazy mode computes each distance from the data when the
solver requests it, which saves storage and repeats distance evaluations.
Implicit mode stores a subset of candidate pairs and solves the assignment
on that subset. It then computes $C_{ij}-u_i-v_j$ for the admissible pairs
that were not stored. A negative value marks a pair that could lower the
total cost, so implicit mode adds such pairs and solves again. When no
negative value remains, the potentials satisfy the conditions above for
every admissible pair and the matching is optimal for the complete
problem; otherwise the search stops without certification, and the
certificate records which of the two happened.

Jonker--Volgenant [@JonkerVolgenant1987] is the general default
solver. Alternative solvers, k-best [@Murty1968] and bottleneck
assignments, and additional statistical matching designs are documented
with the package.

The returned pairs can then be assessed as a statistical design.
Diagnostics report standardized mean differences, variance ratios, and
Kolmogorov--Smirnov statistics; Rosenbaum bounds assess sensitivity to
unmeasured confounding for supported paired analyses [@Rosenbaum2002].
These checks complement assignment verification because a minimum-distance
matching need not balance every covariate. Conversion methods and
registered `cobalt` methods pass the matched sample to other diagnostic
tools. Outcome estimation remains a separate step that must reflect the
chosen design [@Stuart2010].

# Research impact statement

Implicit mode used about a tenth of the memory of dense mode and returned
certified matchings. With 20,000 observations, it reduced peak resident memory above the loaded-session baseline from
1,534.8 MB in dense mode to 143.4 MB, while elapsed time increased from
4.17 to 4.85 seconds. Lazy mode reduced memory further, to
25.6 MB, but required 7.18 seconds (Figure \ref{fig:memory}; individual
runs). In a second experiment across six sizes from 500 to 50,000 observations,
every implicit assignment was certified and agreed in objective value with
lazy mode. At the largest size, implicit mode stored 1.60 million of
555.56 million possible pairs.

![Peak memory and elapsed time by storage mode. (A) Peak resident memory above the loaded-session baseline, on a logarithmic scale. (B) Memory and elapsed time at 20,000 observations, on linear scales. Each measurement is an individual run with approximately one treated unit per two controls.\label{fig:memory}](figures/memory-tradeoff.pdf)

The storage advantage depends on the geometry of the data. The following
results are medians across five instances with 10,000 observations and
eight covariates. The heavy-tailed case retained
1.17% of possible pairs but took about as long as lazy mode. In the
contested case, where observations compete for nearby partners, the
candidate graph expanded to 73.3% of possible pairs and distance
evaluations reached 10.3 times the number of possible pairs. Implicit
mode nevertheless completed in 139 seconds compared with 423 seconds for
lazy mode. The stored fraction therefore does not by itself predict the
runtime.

Exact certificates confirmed optimality on all 120 common-cost validation
matrices, covering integer, uniform, and Euclidean costs in square and
rectangular problems. Recorded objective values agreed with `clue` and
SciPy on all instances and with `lpSolve` on the 60 smaller square
instances tested. In the accompanying synthetic hospital-staff example,
implicit mode matches all 200 treated units using 10,800 of 60,000
possible pairs, while tighter restrictions leave at most 197 units
matchable.

Certification added between 12% and 25% to the runtime of the dense
matching workflow across the six sizes tested. For 20,000 observations, dense `couplr` completed one-to-one
Mahalanobis matching in 3.78 seconds, or 4.61 seconds with certification,
compared with 288.50 seconds for `optmatch` (Table \ref{tab:scaling}).
These timings include distance construction and output preparation;
`MatchIt` delegates the optimization to `optmatch`. The benchmark uses
pooled within-group covariance and materializes distances in all
workflows.

Table: Complete matching workflow times in seconds, including distance
construction. Medians across independent instances, using the fastest
repetition per instance. The second timing column adds solution verification.
\label{tab:scaling}

| Total observations | `couplr` (s) | With certificate (s) | `optmatch` (s) | `MatchIt` (s) |
|-------------------:|---------:|-----------------:|-----------:|----------:|
| 500                | 0.0033   | 0.0037           | 0.0965     | 0.1161    |
| 2,000              | 0.0331   | 0.0405           | 1.6336     | 2.0696    |
| 5,000              | 0.2089   | 0.2528           | 12.0799    | 14.9241   |
| 10,000             | 0.9299   | 1.1649           | 58.7787    | 71.3423   |
| 20,000             | 3.7817   | 4.6087           | 288.4989   | overflow  |
| 50,000             | 25.0925  | 31.2177          | timeout    | timeout   |

Treated:control is approximately 1:2, with eight covariates and three to
five independent instances per size. Runs use one core and single-threaded BLAS on an Apple M4 Pro, R 4.5.3,
`couplr` 1.9.0, `optmatch` 0.10.8, and `MatchIt` 4.7.2.
The optmatch size limit is disabled. `overflow` denotes a size-limit error
reached through MatchIt; `timeout` denotes a feasibility probe exceeding
600 seconds.

An optimal assignment can still reveal a limitation of the statistical
design. In the LaLonde job-training example [@LaLonde1986;
@DehejiaWahba1999], matching all 185 treated units to distinct controls
from 429 available candidates leaves an absolute standardized mean
difference of 1.053 for the indicator for Black participants. The
treated sample includes 156 Black participants, but only 87 Black
controls are available. This shortage makes balance on the indicator
unattainable under complete one-to-one matching.
Balance agrees to three decimal places across `couplr`, `MatchIt`, and
`optmatch`, despite a small difference in assignment cost
(Figure \ref{fig:balance}).

![Covariate balance in the LaLonde data. (A) Absolute standardized mean differences before and after complete one-to-one matching; the shaded band and dashed line mark values up to 0.1. Matched values agree to three decimal places across the three packages. (B) Numbers of Black treated participants and Black controls.\label{fig:balance}](figures/lalonde-balance.pdf)

Scripts, individual measurements, environment records, and extended
examples accompany the paper under `paper/bench`. The scaling table is
drawn from `results/scaling-results.csv`.

# AI usage disclosure

The author designed the architecture, selected the algorithms, and
implemented the software with AI assistance. A self-hosted
Qwen3-Coder-Next-REAP-48B-A3B model, accessed through the Claude Code CLI,
assisted the author's development through code completion, refactoring,
and boilerplate generation. Large language models accessed through u:ai,
the University of Vienna's AI service, supported grammar and structure
checking and manuscript revision. The author reviewed and edited the
suggested wording and code and takes full responsibility for the code,
results, and conclusions.

# Acknowledgements

No external funding supported this work.

# References
