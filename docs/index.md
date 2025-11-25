# couplr

[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen)](https://github.com/gcol33/couplr)
[![License:
MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

**couplr** is an R package for solving Linear Assignment Problems (LAP)
with production-ready matching workflows. It provides optimal one-to-one
matching between two groups, with automatic preprocessing, balance
diagnostics, and analysis-ready output.

## Installation

``` r

# Install from GitHub
remotes::install_github("gcol33/couplr")
```

## Quick Start

``` r

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

- **12 optimal LAP algorithms**: Hungarian, Jonker-Volgenant, Auction,
  Gabow-Tarjan, and more
- **3 greedy algorithms**: Fast approximate matching for large datasets
- **Automatic preprocessing**: Variable scaling, health checks,
  categorical encoding
- **Balance diagnostics**: Standardized differences, variance ratios, KS
  statistics
- **Blocking support**: Exact matching and stratification
- **Distance caching**: Precompute distances for rapid experimentation
- **Parallel processing**: Multi-core matching via the future framework

## Usage

### Optimal Matching

``` r

result <- match_couples(
 left = treated,
 right = control,
 vars = c("age", "income"),
 auto_scale = TRUE,
 max_distance = 0.5
)
```

### Greedy Matching (Large Datasets)

``` r

result <- greedy_couples(
 left = treated,
 right = control,
 vars = c("age", "income"),
 strategy = "row_best"
)
```

### Low-Level LAP Solving

``` r

cost <- matrix(c(4, 2, 8, 4, 3, 7, 3, 1, 6), nrow = 3, byrow = TRUE)
result <- lap_solve(cost)
```

## Documentation

- [`vignette("getting-started", package = "couplr")`](https://gcol33.github.io/couplr/articles/getting-started.md)
- [`vignette("algorithms", package = "couplr")`](https://gcol33.github.io/couplr/articles/algorithms.md)
- [`vignette("matching-workflows", package = "couplr")`](https://gcol33.github.io/couplr/articles/matching-workflows.md)
- [`vignette("pixel-morphing", package = "couplr")`](https://gcol33.github.io/couplr/articles/pixel-morphing.md)

## Citation

``` bibtex
@software{couplr,
 author = {Colling, Gilles},
 title = {couplr: Optimal Matching via Linear Assignment},
 year = {2025},
 url = {https://github.com/gcol33/couplr}
}
```

## License

MIT
