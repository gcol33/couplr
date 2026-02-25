# couplr

[![CRAN
status](https://www.r-pkg.org/badges/version/couplr)](https://CRAN.R-project.org/package=couplr)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/couplr)](https://cran.r-project.org/package=couplr)
[![Monthly
downloads](https://cranlogs.r-pkg.org/badges/couplr)](https://cran.r-project.org/package=couplr)
[![R-CMD-check](https://github.com/gcol33/couplr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gcol33/couplr/actions/workflows/R-CMD-check.yaml)
[![Coverage](https://codecov.io/gh/gcol33/couplr/graph/badge.svg?flag=r)](https://app.codecov.io/gh/gcol33/couplr)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

**Optimal Pairing and Matching via Linear Assignment**

The `couplr` package provides high-level functions for **optimal
one-to-one matching** between two groups. Whether you need to pair
treatment and control units, assign workers to tasks, or align images
pixel-by-pixel, `couplr` offers fast, deterministic solutions with
automatic preprocessing and balance diagnostics.

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

## Statement of Need

Optimal matching is central to experimental design, causal inference,
and resource allocation. Existing R packages (MatchIt, optmatch) focus
on propensity score workflows, requiring users to estimate scores before
matching. This adds complexity and can obscure the direct relationship
between covariates and match quality.

This package addresses **direct covariate matching**: selecting optimal
pairs based on observed variables without intermediate modeling. It
provides:

- 20 LAP algorithms for different problem sizes and structures,
- automatic preprocessing with variable health checks,
- balance diagnostics for assessing match quality,
- analysis-ready joined output.

These features make the package useful in domains like:

- causal inference (matching treated/control units),
- experimental design (pairing samples for within-pair comparisons),
- resource allocation (assigning workers to tasks),
- image processing (pixel-level morphing and alignment).

## Features

### High-Level Matching Functions

- **[`match_couples()`](https://gillescolling.com/couplr/reference/match_couples.md)**:
  Optimal one-to-one matching
  - Automatic preprocessing with variable health checks
  - Multiple scaling methods: robust (MAD), standardize (SD), range
  - Distance constraints via `max_distance` and `calipers`
  - Blocking support for stratified matching
- **[`greedy_couples()`](https://gillescolling.com/couplr/reference/greedy_couples.md)**:
  Fast approximate matching
  - Three strategies: `sorted`, `row_best`, `pq` (priority queue)
  - 10-100x faster than optimal for large datasets
  - Same preprocessing and constraint options

### Balance Diagnostics

- **[`balance_diagnostics()`](https://gillescolling.com/couplr/reference/balance_diagnostics.md)**:
  Comprehensive balance assessment
  - Standardized differences, variance ratios, KS tests
  - Quality thresholds: \<0.1 excellent, 0.1-0.25 good, 0.25-0.5
    acceptable
  - Per-block statistics when blocking is used
  - Publication-ready tables via
    [`balance_table()`](https://gillescolling.com/couplr/reference/balance_table.md)

### Low-Level LAP Solving

- **[`lap_solve()`](https://gillescolling.com/couplr/reference/lap_solve.md)**:
  Tidy interface for LAP algorithms
  - 20 solvers: Hungarian, Jonker-Volgenant, Auction, Network Simplex,
    etc.
  - Automatic method selection via `method = "auto"`
  - Supports rectangular matrices and forbidden assignments
- **[`lap_solve_batch()`](https://gillescolling.com/couplr/reference/lap_solve_batch.md)**:
  Batch solving for multiple matrices
- **[`lap_solve_kbest()`](https://gillescolling.com/couplr/reference/lap_solve_kbest.md)**:
  K-best solutions via Murty’s algorithm

## Installation

``` r

# Install from CRAN
install.packages("couplr")

# Or install development version from GitHub
# install.packages("pak")
pak::pak("gcol33/couplr")
```

## Usage Examples

### Optimal Matching (`match_couples`)

``` r

library(couplr)

# Basic matching with automatic scaling
result <- match_couples(
  treated, control,
  vars = c("age", "income"),
  auto_scale = TRUE
)

# With distance constraint
result <- match_couples(
  treated, control,
  vars = c("age", "income"),
  auto_scale = TRUE,
  max_distance = 0.5
)

# With blocking (exact matching on site)
result <- match_couples(
  treated, control,
  vars = c("age", "income"),
  block_by = "site",
  auto_scale = TRUE
)

# Check what was matched
result$pairs
```

### Greedy Matching (`greedy_couples`)

``` r

# Fast matching for large datasets
result <- greedy_couples(
  treated, control,
  vars = c("age", "income"),
  strategy = "row_best",
  auto_scale = TRUE
)

# Priority queue strategy (often best quality)
result <- greedy_couples(
  treated, control,
  vars = c("age", "income"),
  strategy = "pq"
)
```

### Low-Level LAP Solving

``` r

# Solve a cost matrix
cost <- matrix(c(4, 2, 8, 4, 3, 7, 3, 1, 6), nrow = 3, byrow = TRUE)
result <- lap_solve(cost)
result$assignment
result$total_cost

# Choose a specific algorithm
result <- lap_solve(cost, method = "hungarian")

# K-best solutions
results <- lap_solve_kbest(cost, k = 3)
```

## Choosing Between `match_couples` and `greedy_couples`

| Feature | [`match_couples()`](https://gillescolling.com/couplr/reference/match_couples.md) | [`greedy_couples()`](https://gillescolling.com/couplr/reference/greedy_couples.md) |
|----|----|----|
| **Optimality** | Guaranteed optimal | Approximate |
| **Speed** | O(n^3) | O(n^2) or better |
| **Best for** | n \< 5000 | n \> 5000 |
| **Supports constraints?** | Yes | Yes |
| **Supports blocking?** | Yes | Yes |

**Tip**: Start with
[`match_couples()`](https://gillescolling.com/couplr/reference/match_couples.md).
Switch to
[`greedy_couples()`](https://gillescolling.com/couplr/reference/greedy_couples.md)
if runtime is too long.

## Advanced Features

### Distance Caching

Precompute distances for rapid experimentation:

``` r

# Compute once
dist_obj <- compute_distances(treated, control, vars = c("age", "income"))

# Reuse with different constraints
result1 <- match_couples(dist_obj, max_distance = 0.3)
result2 <- match_couples(dist_obj, max_distance = 0.5)
```

### Parallel Processing

Speed up blocked matching with multi-core processing:

``` r

result <- match_couples(
  treated, control,
  vars = c("age", "income"),
  block_by = "site",
  parallel = TRUE
)
```

### Pixel Morphing

Align images pixel-by-pixel using optimal assignment:

``` r

morph <- pixel_morph(image_a, image_b)
pixel_morph_animate(morph, "output.gif")
```

## Documentation

- [Getting
  Started](https://gillescolling.com/couplr/articles/getting-started.html)
- [Algorithm
  Details](https://gillescolling.com/couplr/articles/algorithms.html)
- [Matching
  Workflows](https://gillescolling.com/couplr/articles/matching-workflows.html)
- [Pixel
  Morphing](https://gillescolling.com/couplr/articles/pixel-morphing.html)

## Support

> “Software is like sex: it’s better when it’s free.” — Linus Torvalds

I’m a PhD student who builds R packages in my free time because I
believe good tools should be free and open. I started these projects for
my own work and figured others might find them useful too.

If this package saved you some time, buying me a coffee is a nice way to
say thanks. It helps with my coffee addiction.

[![Buy Me A
Coffee](https://img.shields.io/badge/-Buy%20me%20a%20coffee-FFDD00?logo=buymeacoffee&logoColor=black)](https://buymeacoffee.com/gcol33)

## License

MIT (see the LICENSE file)

## Citation

``` bibtex
@software{couplr,
  author = {Colling, Gilles},
  title = {couplr: Optimal Matching via Linear Assignment},
  year = {2026},
  url = {https://github.com/gcol33/couplr}
}
```
