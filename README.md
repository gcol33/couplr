# couplr

[![CRAN status](https://www.r-pkg.org/badges/version/couplr)](https://CRAN.R-project.org/package=couplr)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/couplr)](https://cran.r-project.org/package=couplr)
[![Monthly downloads](https://cranlogs.r-pkg.org/badges/couplr)](https://cran.r-project.org/package=couplr)
[![R-CMD-check](https://github.com/gcol33/couplr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gcol33/couplr/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

**couplr** is an R package for solving Linear Assignment Problems (LAP) with production-ready matching workflows. It provides optimal one-to-one matching between two groups, with automatic preprocessing, balance diagnostics, and analysis-ready output.

## Why couplr?

Existing R packages for optimal matching (MatchIt, optmatch) focus on propensity score workflows for causal inference. **couplr** takes a different approach:

- **Direct covariate matching**: Match on observed variables without propensity score estimation
- **Algorithm choice**: 20 LAP solvers let you pick the right algorithm for your problem size and structure
- **Production workflows**: Preprocessing, diagnostics, and joined output in a single pipeline
- **Beyond causal inference**: Pair samples for experiments, assign workers to tasks, align images pixel-by-pixel

If you need propensity score matching with replacement or variable ratios, use MatchIt. If you need optimal one-to-one assignment with full control over the distance metric and algorithm, use couplr.

## Installation

```r
# install.packages("pak")
pak::pak("gcol33/couplr")
```

## Quick Start

```r
library(couplr)

# Match treatment and control groups on covariates
result <- match_couples(
 treated, control,
 vars = c("age", "income", "education"),
 auto_scale = TRUE
)

# Check covariate balance
balance_diagnostics(result, treated, control, vars = c("age", "income", "education"))

# Get analysis-ready dataset
matched_data <- join_matched(result, treated, control)
```

## Features

- **20 optimal LAP algorithms**: Hungarian, Jonker-Volgenant, Auction, Gabow-Tarjan, Network Simplex, and more
- **3 greedy algorithms**: Fast approximate matching for large datasets
- **Automatic preprocessing**: Variable scaling, health checks, categorical encoding
- **Balance diagnostics**: Standardized differences, variance ratios, KS statistics
- **Blocking support**: Exact matching and stratification
- **Distance caching**: Precompute distances for rapid experimentation
- **Parallel processing**: Multi-core matching via the future framework

## Usage

### Optimal Matching

```r
result <- match_couples(
 left = treated,
 right = control,
 vars = c("age", "income"),
 auto_scale = TRUE,
 max_distance = 0.5
)
```

### Greedy Matching (Large Datasets)

```r
result <- greedy_couples(
 left = treated,
 right = control,
 vars = c("age", "income"),
 strategy = "row_best"
)
```

### Low-Level LAP Solving

```r
cost <- matrix(c(4, 2, 8, 4, 3, 7, 3, 1, 6), nrow = 3, byrow = TRUE)
result <- lap_solve(cost)
```

## Documentation

- `vignette("getting-started", package = "couplr")`
- `vignette("algorithms", package = "couplr")`
- `vignette("matching-workflows", package = "couplr")`
- `vignette("pixel-morphing", package = "couplr")`

## Citation

```bibtex
@software{couplr,
 author = {Colling, Gilles},
 title = {couplr: Optimal Matching via Linear Assignment},
 year = {2025},
 url = {https://github.com/gcol33/couplr}
}
```

## License

MIT
