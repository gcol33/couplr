# couplr benchmarks and validation

The scripts in this directory produce couplr's performance and validation results, which are recorded in [results](results). The extended example below shows the package on a small synthetic data set.

## Extended matching example

[example_hospital_staff.R](example_hospital_staff.R) runs the synthetic `hospital_staff` example, its balance diagnostics, restricted matching and a sequence of maximum distances.

```r
library(couplr)
data(hospital_staff)
treated <- transform(hospital_staff$nurses_extended, id = nurse_id)
control <- transform(hospital_staff$controls_extended, id = nurse_id)
covars <- c("age", "experience_years", "certification_level")

m <- match_couples(treated, control, vars = covars, auto_scale = TRUE,
                  memory_mode = "implicit", certify = TRUE)
bal <- balance_diagnostics(m, treated, control, vars = covars)
balance_table(bal)
```

All 200 treated units are matched, with a total distance of 59.95. The search stores 10,800 of 60,000 possible pairs. Absolute standardised mean differences are 0.116 for age, 0.074 for experience and 0.021 for certification level.

### Calipers and infeasibility

A caliper limits the permitted difference in a matching variable. This example allows differences of at most 3 years in age and 2 years in experience, together with a maximum total distance of 1.5:

```r
m_cal <- match_couples(
  treated, control, vars = covars, auto_scale = TRUE,
  calipers = list(age = 3, experience_years = 2),
  max_distance = 1.5
)
nrow(m_cal$pairs)
#> [1] 197
```

These restrictions leave 2,173 admissible pairs. A complete matching is infeasible; the largest matching covers 197 treated units. In implicit mode, Hall's condition identifies infeasibility: a set of rows has fewer distinct admissible columns than rows. The returned set provides evidence that those rows cannot all receive different partners.

The 197-pair result answers a maximum-cardinality question. It is not a feasible solution of the complete-assignment problem, which requires every treated unit to receive a partner, and it does not carry an optimality certificate for that problem. An infeasibility witness and an optimality certificate establish different conclusions.

### Matching paths

`match_path()` solves an increasing sequence of caliper values. Widening a caliper preserves the feasibility of the previous matching, which supplies a starting solution for the next value. Each result has its own certificate. Decreasing sequences are refused because they can remove pairs used in the current matching.

The following path varies the maximum distance alone. It applies no age or experience calipers:

```r
p <- match_path(
  treated, control, vars = covars, auto_scale = TRUE,
  vary = "max_distance", values = c(1.0, 1.2, 1.5, 2.0, 3.0)
)
p$path[, c("max_distance", "n_matched", "total_distance", "certified")]
#>   max_distance n_matched total_distance certified
#> 1          1.0       200           62.6 TRUE
#> 2          1.2       200           61.5 TRUE
#> 3          1.5       200           59.9 TRUE
#> 4          2.0       200           59.9 TRUE
#> 5          3.0       200           59.9 TRUE
```

Total distance stops changing at 1.5. In the performance benchmarks, a sweep of 20 caliper values on problems with 500 to 20,000 units was 2.0 to 2.2 times faster in wall-clock time than 20 independent solves. Every point was certified and agreed with its independent solve in status, matched-unit count and total distance. See [bench_path.R](bench_path.R), [path-results.csv](results/path-results.csv) and [path-points.csv](results/path-points.csv).

## External validation

I compared matchings on 120 random cost matrices: integer costs from 1 to 10,000, uniform costs on the unit interval, and Euclidean distances between point clouds. Each cost type was evaluated at dimensions 100 by 100, 400 by 400, 1,000 by 1,000 and 200 by 600, with 10 instances per combination. lpSolve was evaluated on the square instances up to 400 by 400, giving 60 instances.

For each instance, I evaluated every matching on the same cost matrix and checked it with `verify_assignment()` using couplr's potentials. The validation script also evaluates the LaLonde example with a common Mahalanobis cost matrix.

- [bench_external.R](bench_external.R): instance construction, external solvers and certificate checks.
- [external-runs.csv](results/external-runs.csv): individual results, keyed by cost type, dimensions, seed and tool.
- [external-results.csv](results/external-results.csv): results grouped by tool and cost type.
- [external-lalonde.csv](results/external-lalonde.csv): real-data matching costs and certificates.
- [external-ENVIRONMENT.txt](results/external-ENVIRONMENT.txt): execution environment.

The current external-validation environment record dated 21 October 2026 lists Apple ARM macOS, R 4.5.3, couplr 1.9.0, clue 0.3.68, lpSolve 5.6.23, optmatch 0.10.8, MatchIt 4.7.2 and SciPy 1.17.1. The performance environment record lists the same R and matching-package versions on an Apple M4 Pro. These records accompany the stored results; reruns should retain their own environment records.

### LaLonde interpretation and solver settings

The LaLonde comparison matches 185 treated units to 429 available controls without replacement. On the common cost matrix, the totals in `external-lalonde.csv` are 304.042249060069 for couplr and 304.043164044937 for both MatchIt and optmatch. The calls use `matchit(..., method = "optimal", distance = "mahalanobis", ratio = 1)` and `pairmatch(..., controls = 1)` with no explicit override of the optimization tolerance. These are comparisons at the recorded package defaults, not an experiment isolating the effect of solver tolerance.

Only the couplr pairing passes the exact certificate on this common matrix. That numerical distinction does not imply a better statistical design: absolute standardized mean differences agree to three decimal places and the Black-race indicator remains at 1.053. The data contain 156 Black treated participants and 87 Black controls. This shortage prevents a balanced match on that indicator when all treated units must be retained with distinct controls. The paper uses this example to separate optimization accuracy from covariate balance, not to claim a substantive balance advantage.

## Assignment certificates

For a rectangular assignment with $n<m$, every row must be matched once and every column at most once. With finite admissible costs $C_{ij}$ on edges $E$, the dual maximizes $\sum_i u_i+\sum_j v_j$ subject to $u_i+v_j\le C_{ij}$ on $E$ and $v_j\le0$. The bipartite assignment polytope is integral. A primal-feasible assignment and dual-feasible potentials establish optimality when matched edges are tight and unused columns have $v_j=0$. For a square problem, column constraints can be equalities and both sets of potentials are unrestricted.

`verify_assignment()` checks primal feasibility, dual feasibility, and complementary slackness. It evaluates reduced-cost signs with floating-point expansions. If rounded potentials pass numerical checks but fail exact conditions, it attempts to recover exact potentials from the matching through shortest-path calculations in expansion arithmetic. The certificate identifies whether its conclusion is exact or uses a stated tolerance; exactness is relative to the supplied costs, including rounding during distance construction. Without supplied potentials, verification obtains them through another solve with `assignment_duals()`.

See [the certificate implementation and documentation](../../R/lap_certify.R) and the Shewchuk (1997) reference in [the paper bibliography](../joss/paper.bib). Agreement of recorded objective values alone is not an exact certificate. In particular, `implicit-results.csv` records `certified_optimal` and the duality gap but does not separately record the certificate's arithmetic mode; the paper describes those runs as certified without asserting that this CSV establishes exact arithmetic for every run.

## Solver selection and diagnostics

For implicit matching, the initial candidate count per row is six times the base-two logarithm of the number of columns, rounded up before multiplying by six. If there are $m$ columns, this is $w=6\lceil\log_2 m\rceil$, where $w$ is the requested number of nearest admissible columns and the ceiling symbols mean rounding up to the next integer.

`assignment(method = "auto")` uses exhaustive enumeration for matrices up to 8 by 8, a Hopcroft–Karp routine for constant or binary finite costs, and Jonker–Volgenant otherwise. Users can name an alternative solver. In the solver-selection benchmarks, automatic selection was a median of 5.1 and at most 18 times slower than the fastest solver on heavily tied costs, and up to 4.3 times slower on some very sparse matrices.

The solver-selection comparisons are recorded by [bench_dispatch_validation.R](bench_dispatch_validation.R) and [bench_regimes.R](bench_regimes.R); `dispatch-validation-*-results.csv` and `regime-results.csv` contain their summaries.

The [package documentation](https://gillescolling.com/couplr/) describes the additional matching designs. `balance_diagnostics()` reports standardised mean differences, variance ratios and Kolmogorov–Smirnov statistics. `sensitivity_analysis()` computes Rosenbaum bounds for sensitivity to unmeasured confounding. `as_matchit()` converts results for MatchIt workflows, and registered cobalt methods support balance assessment.

## Performance data

This directory contains the scripts for the memory, scaling and candidate-structure experiments, with outputs in [results](results). The performance comparisons cover complete workflows from input data to matched pairs. optmatch uses minimum-cost flow; couplr uses Jonker–Volgenant for the dense one-to-one comparisons.

Memory figures report peak resident memory above the baseline of a fresh R session loading the same packages. Timing runs use one processor core and a single-threaded Basic Linear Algebra Subprograms (BLAS) library. The shared timing helper first runs each workflow in a forked feasibility probe with a 600-second limit. Successful probes are followed by timing repetitions in the parent process, without a separate enforced timeout. This helper requires a Unix platform. The 22.171-second implicit-mode and 61.597-second lazy-mode results at 50,000 units in `implicit-results.csv` are individual runs.

### Scaling table protocol

The main comparison uses one-to-one Mahalanobis matching with pooled within-group covariance, eight covariates, and approximately one treated unit per two controls. All workflows materialize distances. The optmatch size limit is disabled. Each instance is represented by its fastest timing repetition; the paper reports the median of those instance-level values. `scaling-results.csv` also records the quartiles and range across instances.

| Total observations | Independent instances | Timing repetitions per instance |
|-------------------:|----------------------:|--------------------------------:|
| 500 | 5 | 5 |
| 2,000 | 5 | 5 |
| 5,000 | 5 | 3 |
| 10,000 | 3 | 3 |
| 20,000 | 3 | 1 |
| 50,000 | 3 | 1 |

These are the planned counts; a failed feasibility probe contributes an error or timeout rather than a timing. The table's `overflow` entry records `result would exceed 2^31-1 bytes` reached through MatchIt. A `timeout` records an untimed forked feasibility probe exceeding 600 seconds; it is not a measured 600-second solve. Successful probes precede timing repetitions in the parent process. The parent repetitions have no separately enforced timeout. MatchIt delegates this matching method to optmatch, so those columns are related workflows rather than independent solver families. Direct assignment solvers are compared for correctness in the external-validation experiment.

### Candidate-search performance across problem structures

`bench_implicit_grid.R` and `results/implicit-grid-results.csv` vary point-cloud geometry, dimension, sample size, and calipers. The grid reports stored candidate pairs, evaluated distances, elapsed time, and certification separately. For the heavy-tailed cloud with 10,000 observations, eight covariates, and no caliper, implicit mode retains a median 1.1699% of possible pairs while performing distance evaluations equivalent to 2.009 complete pair sets. Its median speedup over lazy mode is 1.00. The result demonstrates compact storage without a timing advantage on that case. The contested cloud provides a stronger stress test: at the same size and dimension, it retains a median 73.318% of possible pairs, evaluates the equivalent of 10.343 complete pair sets, and takes 139.39 seconds against 422.505 seconds for lazy mode. These summaries use five instances. Candidate storage can therefore approach the full graph even while implicit mode remains faster than lazy mode. The grid complements the main scaling comparison rather than supporting a universal speedup claim.

## Running the scripts

`Rscript paper/bench/run_benchmarks.R` runs the benchmarks the JOSS paper reports, in order, on one machine; name stages to run a subset (`Rscript paper/bench/run_benchmarks.R memory implicit`) and add `--fresh` to re-measure instead of resuming from the CSVs in `results/`. The scripts work from any directory: inside this repository they time the source tree, elsewhere the installed package, and they write only to `results/` and `logs/` beside themselves. `Rscript paper/bench/example_hospital_staff.R` runs the extended example.

