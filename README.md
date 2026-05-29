# couplr

*matching two groups into pairs*

[![CRAN status](https://www.r-pkg.org/badges/version/couplr)](https://CRAN.R-project.org/package=couplr)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/couplr)](https://cran.r-project.org/package=couplr)
[![Monthly downloads](https://cranlogs.r-pkg.org/badges/couplr)](https://cran.r-project.org/package=couplr)
[![R-CMD-check](https://github.com/gcol33/couplr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gcol33/couplr/actions/workflows/R-CMD-check.yaml)
[![Coverage](https://codecov.io/gh/gcol33/couplr/graph/badge.svg?flag=r)](https://app.codecov.io/gh/gcol33/couplr)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

**Optimal one-to-one matching by linear assignment, solved exactly in C++.**

Hand it two groups. `couplr` returns the pairing that minimizes total covariate
distance across the whole sample, solved by linear assignment (Jonker-Volgenant,
Hungarian, Auction, cost-scaling) on `RcppEigen`. The common tool, greedy nearest
neighbour, locks in each pair as it goes and gives back whatever the ordering
produced. This finds the global minimum, and the same answer every run.

```r
library(couplr)

# match treatment and control on covariates, in a single call
result <- match_couples(treated, control, vars = c("age", "income"), auto_scale = TRUE)

# analysis-ready paired data
join_matched(result, treated, control)
```

## Optimal, not greedy

`MatchIt`, the most used matching package in R, pairs units greedily on an estimated
propensity score: it takes them in order, grabs the nearest free control for each, and
the result depends on that order. `couplr` matches directly on the covariates and solves
the assignment exactly, so total distance is the global minimum and the pairing is the
same every time.

```r
match_couples(treated, control, vars = c("age", "income"), auto_scale = TRUE)   # optimal, deterministic
greedy_couples(treated, control, vars = c("age", "income"), strategy = "pq")    # greedy, for large pools
```

For large control pools, `greedy_couples()` trades the exact guarantee for speed, with
three strategies (`sorted`, `row_best`, `pq`) and the same preprocessing and constraints.

## What's in the box

- **`match_couples()`**: optimal one-to-one matching with automatic scaling
  (robust / standardize / range), distance constraints (`max_distance`, `calipers`),
  blocking, and `ratio` / `replace` matching.
- **`greedy_couples()`**: fast approximate matching for large datasets, three strategies.
- **`full_match()` / `cem_match()` / `subclass_match()` / `cardinality_match()`**:
  variable-ratio full matching, coarsened exact matching, propensity subclassification,
  and balance-constrained matching.
- **`ps_match()`**: propensity score matching with a logit caliper.
- **`balance_diagnostics()` / `sensitivity_analysis()`**: standardized differences,
  variance ratios, KS tests, and Rosenbaum bounds for hidden bias.
- **`lap_solve()`**: tidy interface to the assignment backend, 20 solvers with
  `method = "auto"`, plus `lap_solve_batch()` and `lap_solve_kbest()` (Murty's algorithm).

## The assignment backend

`lap_solve()` exposes the solver layer directly. It takes a cost matrix, handles
rectangular shapes and forbidden edges (`NA` / `Inf`), and picks an algorithm from the
problem when `method = "auto"`:

```r
cost <- matrix(c(4, 2, 8, 4, 3, 7, 3, 1, 6), nrow = 3, byrow = TRUE)

lap_solve(cost)                      # auto-selected solver
lap_solve(cost, method = "hungarian")
lap_solve_kbest(cost, k = 3)         # the three best assignments
```

The solvers span the classics and the scaling algorithms: Jonker-Volgenant, Hungarian,
Kuhn-Munkres, Bertsekas auction (with epsilon-scaling variants), Goldberg-Kennedy
cost-scaling, Gabow-Tarjan bit-scaling, push-relabel, network simplex, and Sinkhorn
entropy-regularized transport.

## `match_couples` or `greedy_couples`?

|  | `match_couples()` | `greedy_couples()` |
|---|---|---|
| Result | Globally optimal | Approximate |
| Deterministic? | Yes | Yes |
| Cost | `O(n^3)` | `O(n^2)` or better |
| Best for | `n < 5000` | large control pools |
| Constraints, blocking? | Yes | Yes |

Start with `match_couples()`. Switch to `greedy_couples()` when the optimal solve runs too long.

## Fits the matching ecosystem

`couplr` results convert to `matchit`-class with `as_matchit()`, so `cobalt` balance tables
and `marginaleffects` estimates work without rewiring your analysis. `match_data()` returns
treatment, weights, and subclass columns in one analysis-ready frame, and `autoplot()`
methods cover matching results, balance, and sensitivity.

## Installation

```r
install.packages("couplr")            # CRAN

install.packages("pak")               # development version
pak::pak("gcol33/couplr")
```

## Documentation

- [Getting Started](https://gillescolling.com/couplr/articles/getting-started.html)
- [Matching Workflows](https://gillescolling.com/couplr/articles/matching-workflows.html)
- [Algorithms](https://gillescolling.com/couplr/articles/algorithms.html)
- [Comparison](https://gillescolling.com/couplr/articles/comparison.html)
- [Pixel Morphing](https://gillescolling.com/couplr/articles/pixel-morphing.html)

## Support

> "Software is like sex: it's better when it's free." — Linus Torvalds

I'm a PhD student who builds R packages in my free time because I believe good tools
should be free and open. I started these projects for my own work and figured others
might find them useful too.

If this package saved you some time, buying me a coffee is a nice way to say thanks.
It helps with my coffee addiction.

[![Buy Me A Coffee](https://img.shields.io/badge/-Buy%20me%20a%20coffee-FFDD00?logo=buymeacoffee&logoColor=black)](https://buymeacoffee.com/gcol33)

## License

MIT (see the LICENSE file)

## Citation

```bibtex
@software{couplr,
  author = {Colling, Gilles},
  title  = {couplr: Optimal Matching via Linear Assignment},
  year   = {2026},
  url    = {https://github.com/gcol33/couplr}
}
```
