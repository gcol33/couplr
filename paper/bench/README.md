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

The environment record dated 30 September 2026 lists Linux, R 4.3.3, couplr 1.8.0, clue 0.3.68, lpSolve 5.6.23, optmatch 0.10.8, MatchIt 4.8.1 and SciPy 1.18.1. The performance timings come from separate runs on an Apple M4 Pro with R 4.5.3, couplr 1.8.0, optmatch 0.10.8 and MatchIt 4.7.2.

## Solver selection and diagnostics

For implicit matching, the initial candidate count per row is six times the base-two logarithm of the number of columns, rounded up before multiplying by six. If there are $m$ columns, this is $w=6\lceil\log_2 m\rceil$, where $w$ is the requested number of nearest admissible columns and the ceiling symbols mean rounding up to the next integer.

`assignment(method = "auto")` uses exhaustive enumeration for matrices up to 8 by 8, a Hopcroft–Karp routine for constant or binary finite costs, and Jonker–Volgenant otherwise. Users can name an alternative solver. In the solver-selection benchmarks, automatic selection was a median of 5.1 and at most 18 times slower than the fastest solver on heavily tied costs, and up to 4.3 times slower on some very sparse matrices.

The solver-selection comparisons are recorded by [bench_dispatch_validation.R](bench_dispatch_validation.R) and [bench_regimes.R](bench_regimes.R); `dispatch-validation-*-results.csv` and `regime-results.csv` contain their summaries.

The [package documentation](https://gillescolling.com/couplr/) describes the additional matching designs. `balance_diagnostics()` reports standardised mean differences, variance ratios and Kolmogorov–Smirnov statistics. `sensitivity_analysis()` computes Rosenbaum bounds for sensitivity to unmeasured confounding. `as_matchit()` converts results for MatchIt workflows, and registered cobalt methods support balance assessment.

## Performance data

This directory contains the scripts for the memory, scaling and candidate-structure experiments, with outputs in [results](results). The performance comparisons cover complete workflows from input data to matched pairs. optmatch uses minimum-cost flow; couplr uses Jonker–Volgenant for the dense one-to-one comparisons.

Memory figures report peak resident memory above the baseline of a fresh R session loading the same packages. Timing runs use one processor core and a single-threaded Basic Linear Algebra Subprograms (BLAS) library. The scripts record a 600-second limit for each timing run. The 22.1-second implicit-mode and 60.6-second lazy-mode results at 50,000 units are individual runs.

## Running the scripts

Run the scripts from the package root. `sh paper/bench/run_bench_suite.sh` runs the timing benchmarks in order on one machine. Each script resumes from its CSVs in `results/`, and `FRESH=1` re-measures the selected stages. `Rscript paper/bench/bench_external.R` runs the external validation, and `Rscript paper/bench/example_hospital_staff.R` runs the extended example.

