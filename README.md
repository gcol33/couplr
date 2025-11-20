# couplr

> Optimal Pairing and Matching via Linear Assignment

[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen)](https://github.com/gcol33/couplr)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

**couplr** is an R package for solving Linear Assignment Problems (LAP) with production-ready matching workflows. It combines state-of-the-art optimization algorithms with modern statistical matching tools for observational studies, treatment effect estimation, and optimal pairing problems.

## Features

### 🎯 12 Optimal LAP Algorithms
- **Classic solvers**: Hungarian, Jonker-Volgenant
- **Auction variants**: Standard, Gauss-Seidel, scaled
- **Advanced methods**: Gabow-Tarjan, cost-scaling, cycle canceling, SSAP bucket
- **Specialized**: SAP (shortest augmenting path), CS-flow, HK01 (binary costs), brute-force
- **Automatic selection**: Smart method selection based on problem characteristics

### ⚡ Greedy Matching (Separate)
- **Fast approximate algorithms**: Sorted, row-best, priority queue (10-100x faster)
- **Separate function**: Use `greedy_couples()` instead of `match_couples()`
- **Trade-off**: Speed vs optimality - no guarantee of minimum total distance

### 🔬 Production-Ready Matching Workflows
- **Automatic preprocessing**: Variable health checks, smart scaling, categorical encoding
- **Balance diagnostics**: Standardized differences, variance ratios, KS tests
- **Blocking/stratification**: Exact matching and k-means clustering
- **Distance caching**: Precompute distances for rapid experimentation
- **Parallel processing**: Multi-core block matching via the `future` framework
- **Joined datasets**: Analysis-ready merged output with broom-style interface

### 🎨 Visualization
- **Pixel morphing**: Optimal transport visualization for images
- **Multiple modes**: Exact, color-walk, and recursive tiling strategies

## Installation

```r
# Install from GitHub
# install.packages("devtools")
devtools::install_github("gcol33/couplr")
```

## Quick Start

### Basic LAP Solving

```r
library(couplr)

# Create a cost matrix
cost <- matrix(c(
  4, 2, 8,
  4, 3, 7,
  3, 1, 6
), nrow = 3, byrow = TRUE)

# Solve the assignment problem
result <- lap_solve(cost)
result
#> # A tibble: 3 × 4
#>     row   col  cost method
#>   <int> <int> <dbl> <chr>
#> 1     1     2     2 jv
#> 2     2     3     7 jv
#> 3     3     1     3 jv
```

### Tidy Workflow: Optimal vs Greedy

```r
library(couplr)
library(dplyr)

# Optimal matching with pipe workflow
matched_data <- treatment_df |>
  match_couples(
    control_df,
    vars = c("age", "income", "education"),
    auto_scale = TRUE,
    max_distance = 0.5
  ) |>
  join_matched(treatment_df, control_df)

# Greedy matching for large datasets (10-100x faster)
matched_data_fast <- treatment_df |>
  greedy_couples(
    control_df,
    vars = c("age", "income", "education"),
    strategy = "row_best",
    auto_scale = TRUE
  ) |>
  join_matched(treatment_df, control_df)

# Assess balance quality
treatment_df |>
  match_couples(control_df, vars = c("age", "income")) |>
  balance_diagnostics(treatment_df, control_df, vars = c("age", "income"))
```

### Optimal Matching with Preprocessing

```r
# Match treatment and control groups
result <- match_couples(
  left = treatment_df,
  right = control_df,
  vars = c("age", "income", "education"),
  auto_scale = TRUE,           # Automatic preprocessing
  scale = "robust",             # MAD-based scaling
  max_distance = 0.5,           # Caliper constraint
  return_diagnostics = TRUE
)

# Assess balance quality
balance <- balance_diagnostics(result, treatment_df, control_df,
                               vars = c("age", "income", "education"))
print(balance)
#> Balance Diagnostics
#> ══════════════════════════════════════
#> Overall Balance:
#>   Mean Absolute Std Diff: 0.08 (Excellent)
#>   Max Absolute Std Diff:  0.12
#>   % Large Imbalance (>0.1): 33.3%
#>
#> Variable Statistics:
#> # A tibble: 3 × 8
#>   variable  mean_left mean_right std_diff variance_ratio quality    n_left n_right
#>   <chr>         <dbl>      <dbl>    <dbl>          <dbl> <chr>       <int>   <int>
#> 1 age            45.2       44.8     0.05           0.98 Excellent     100     100
#> 2 income      50000.     48500.      0.12           1.05 Good          100     100
#> 3 education      14.2       14.1     0.03           0.95 Excellent     100     100

# Get analysis-ready dataset
matched_data <- join_matched(result, treatment_df, control_df)
head(matched_data)
```

### Fast Greedy Matching for Large Data

```r
# Use greedy algorithm for speed (10-100x faster)
result <- greedy_couples(
  left = large_treatment_df,
  right = large_control_df,
  vars = c("age", "income"),
  strategy = "row_best",  # or "sorted" or "pq"
  auto_scale = TRUE
)
```

### Matching with Blocking

```r
# Create blocks for exact matching on site
blocks <- matchmaker(
  treatment_df, control_df,
  block_type = "group",
  block_by = "site"
)

# Match within blocks (optionally in parallel)
result <- match_couples(
  treatment_df, control_df,
  vars = c("age", "income"),
  block_id = blocks$block_id,
  parallel = TRUE
)
```

### Batch & Group Processing (Built-in Parallelization!)

**Process multiple matching problems simultaneously** - perfect for multi-site studies, stratified analyses, or Monte Carlo simulations:

```r
library(couplr)
library(dplyr)

# Example 1: Multi-site clinical trial (automatic parallel processing)
# Match patients within each hospital site simultaneously
result <- treatment_df |>
  match_couples(
    control_df,
    vars = c("age", "severity", "comorbidities"),
    block_by = "hospital_site",   # Exact match within hospitals
    parallel = TRUE,               # Enable multi-core processing
    auto_scale = TRUE
  )

# Example 2: Grouped data frame workflow (dplyr integration)
# Solve 100+ matching problems in parallel automatically
multi_site_data <- bind_rows(
  mutate(treatment_df, group = site_id),
  mutate(control_df, group = site_id)
) |>
  group_by(group) |>                # Group by any variable
  lap_solve_batch(                   # Batch processing built-in
    cost_matrix,
    parallel = TRUE                  # Leverages future framework
  )

# Example 3: Monte Carlo simulation with batch solving
# Efficiently solve 1000 bootstrap samples
bootstrap_results <- replicate(1000, {
  sample_treatment <- treatment_df[sample(nrow(treatment_df), replace = TRUE), ]
  sample_control <- control_df[sample(nrow(control_df), replace = TRUE), ]

  match_couples(sample_treatment, sample_control,
                vars = covariates,
                parallel = TRUE)
}, simplify = FALSE)

# Performance benefit: 10+ sites with 50+ units each?
# Sequential: ~10 minutes → Parallel (8 cores): ~2 minutes
```

**Why use batch processing in couplr?**
- ⚡ **Automatic parallelization**: Just add `parallel = TRUE`
- 🎯 **Smart blocking**: Match within groups while maintaining optimal solutions
- 🔄 **Works everywhere**: Cross-platform via `future` framework (Windows, Unix, clusters)
- 📊 **Scales linearly**: More cores = proportionally faster results
- 🧩 **Seamless integration**: Works with blocking, distance caching, and all constraints

### Distance Caching for Experimentation

```r
# Compute distances once with full control over distance metric
dist_obj <- compute_distances(
  treatment_df, control_df,
  vars = c("age", "income", "education"),
  distance = "euclidean",       # Options: "euclidean", "manhattan", "mahalanobis"
  scale = "robust"               # Options: "robust", "standardize", "range", "none"
)

# Rapidly experiment with different matching constraints (no distance recomputation!)
result1 <- match_couples(dist_obj, max_distance = 0.3)     # Tight caliper
result2 <- match_couples(dist_obj, max_distance = 0.5)     # Relaxed caliper
result3 <- match_couples(dist_obj, min_distance = 0.1)     # Avoid too-similar pairs
result4 <- greedy_couples(dist_obj, strategy = "sorted")   # Fast approximate
result5 <- greedy_couples(dist_obj, strategy = "row_best") # Faster greedy

# Works with all constraint combinations
result6 <- match_couples(dist_obj,
                        max_distance = 0.5,
                        min_distance = 0.05,     # Minimum distance threshold
                        calipers = c(age = 5))   # Per-variable calipers
```

## Use Cases

### Treatment Effect Estimation
```r
# 1. Match treated and control units
result <- match_couples(treated, control, vars = covariates, auto_scale = TRUE)

# 2. Assess balance
balance <- balance_diagnostics(result, treated, control, vars = covariates)

# 3. Get matched dataset for analysis
data <- join_matched(result, treated, control)

# 4. Estimate treatment effect
lm(outcome ~ treatment + covariates, data = data)
```

### Ecological Studies
```r
# Match vegetation plots across time periods with spatial constraints
# Ensure plots are matched only if they're within 500m (Euclidean distance)
result <- match_couples(
  plots_2010, plots_2020,
  vars = c("temperature", "precipitation", "elevation", "latitude", "longitude"),
  distance = "euclidean",        # Use Euclidean distance
  auto_scale = TRUE,              # Auto-scale different units
  max_distance = 500,             # Spatial caliper: max 500m distance
  return_diagnostics = TRUE
)

# Alternative: Match across seasons with temporal restrictions
result <- match_couples(
  summer_plots, winter_plots,
  vars = c("biomass", "species_count", "soil_moisture"),
  distance = "manhattan",         # Or use Manhattan distance
  max_distance = 0.5,             # Restrict to similar plots only
  block_by = "site_id"            # Exact match within same site
)
```

### Particle Tracking
```r
# Track particles across video frames
result <- lap_solve(distance_matrix, method = "jv")
```

## Algorithm Selection

### Optimal LAP Solvers

The package automatically selects the best optimal algorithm with `method = "auto"`:

| Problem Type | Recommended Method | Complexity |
|-------------|-------------------|-----------|
| Very small (n ≤ 8) | `bruteforce` | O(n!) |
| Small (n ≤ 50) | `hungarian` | O(n³) |
| Medium (50 < n ≤ 75) | `jv` | O(n²log n) |
| Large (n > 75) | `auction_scaled` | O(n²) |
| Binary costs | `hk01` | O(√n · m) |
| Sparse (>50% NA) | `sap` | O(n²) |

```r
# Optimal matching (default uses method = "auto")
lap_solve(cost)                        # Automatic selection
lap_solve(cost, method = "hungarian")  # Force specific algorithm
match_couples(left, right, vars)       # Always optimal
```

### Greedy Algorithms (Separate Function)

For very large problems where approximate solutions are acceptable:

```r
# Greedy matching (separate function)
greedy_couples(left, right, vars, strategy = "row_best")  # Default: fast
greedy_couples(left, right, vars, strategy = "sorted")    # Better quality
greedy_couples(left, right, vars, strategy = "pq")        # Memory efficient
```

**Trade-off**: 10-100x faster but no optimality guarantee

## Documentation

- **Getting Started**: `vignette("getting-started", package = "couplr")`
- **Algorithms**: `vignette("algorithms", package = "couplr")`
- **Pixel Morphing**: `vignette("pixel-morphing", package = "couplr")`
- **Function Reference**: `?match_couples`, `?lap_solve`, `?balance_diagnostics`

## Examples

The package includes comprehensive example scripts:
- `examples/auto_scale_demo.R` - Preprocessing demonstrations
- `examples/balance_diagnostics_demo.R` - Balance assessment
- `examples/join_matched_demo.R` - Creating analysis datasets
- `examples/distance_cache_demo.R` - Distance caching workflow
- `examples/parallel_matching_demo.R` - Parallel processing
- `examples/error_messages_demo.R` - Fun error messages

## Performance

Benchmarks on a standard laptop (n = 1000):

```r
# Optimal matching
bench::mark(
  optimal = match_couples(left, right, vars, method = "optimal"),
  greedy  = greedy_couples(left, right, vars, strategy = "row_best")
)
#> # A tibble: 2 × 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 optimal      850ms    865ms      1.16     45.2MB     0
#> 2 greedy        12ms     14ms     71.4       8.1MB     0
```

## Citation

If you use couplr in your research, please cite:

```
@software{couplr2025,
  author = {Colling, Gilles},
  title = {couplr: Optimal Pairing and Matching via Linear Assignment},
  year = {2025},
  url = {https://github.com/gcol33/couplr},
  version = {1.0.0}
}
```

## Contributing

Contributions are welcome! Please see the [GitHub repository](https://github.com/gcol33/couplr) for:
- Bug reports and feature requests: [Issues](https://github.com/gcol33/couplr/issues)
- Development guidelines: See `CLAUDE.md` in the repository

## License

MIT License. See [LICENSE](LICENSE) file for details.

## Related Packages

- **MatchIt**: Propensity score matching and weighting
- **optmatch**: Optimal matching for observational studies
- **clue**: Solve linear assignment problems
- **RcppHungarian**: Hungarian algorithm implementation
- **lpSolve**: Linear programming solver

**couplr** distinguishes itself by combining:
- Modern tidy interfaces with tibble outputs
- Production-ready matching workflows with preprocessing and diagnostics
- Multiple state-of-the-art LAP algorithms with automatic selection
- Performance optimizations for large-scale problems
- Comprehensive balance assessment tools

---

**Package formerly known as:** lapr (renamed to couplr in v1.0.0)
