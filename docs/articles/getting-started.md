# Quick Start

## What couplr Does

couplr creates matched pairs from two groups of observations. Given a
“left” group (e.g., treatment) and a “right” group (e.g., control), it
finds the best one-to-one pairing based on similarity across variables
you specify.

**Common use cases:**

- Matching treated patients to similar controls in observational studies
- Pairing survey respondents for comparison
- Creating balanced samples for causal inference

### Documentation Roadmap

| Vignette | Focus | Audience |
|----|----|----|
| **Quick Start** (this) | Basic matching with [`match_couples()`](https://gillescolling.com/couplr/reference/match_couples.md) | Everyone |
| [Matching Workflows](https://gillescolling.com/couplr/articles/matching-workflows.md) | Full pipeline: preprocessing, blocking, diagnostics | Researchers |
| [Algorithms](https://gillescolling.com/couplr/articles/algorithms.md) | Mathematical foundations, solver selection | Technical users |
| [Comparison](https://gillescolling.com/couplr/articles/comparison.md) | vs MatchIt, optmatch, designmatch | Package evaluators |

**Start here**, then proceed to whichever vignette matches your use
case.

------------------------------------------------------------------------

## Your First Match

The simplest workflow uses
[`match_couples()`](https://gillescolling.com/couplr/reference/match_couples.md):

``` r

library(couplr)
library(dplyr)

# Create example data: treatment and control groups
set.seed(123)
treatment <- tibble(
  id = 1:50,
  age = rnorm(50, mean = 45, sd = 10),
  income = rnorm(50, mean = 55000, sd = 12000)
)

control <- tibble(
  id = 1:80,
  age = rnorm(80, mean = 50, sd = 12),
  income = rnorm(80, mean = 48000, sd = 15000)
)

# Match on age and income
result <- match_couples(
  left = treatment,
  right = control,
  vars = c("age", "income"),
  auto_scale = TRUE
)
#> Auto-selected scaling method: standardize

# View matched pairs
head(result$pairs)
#> # A tibble: 6 × 5
#>   left_id right_id distance .age_diff .income_diff
#>   <chr>   <chr>       <dbl>     <dbl>        <dbl>
#> 1 1       46          0.779    -4.23        9144. 
#> 2 2       11          0.257    -0.398       3441. 
#> 3 3       52          0.594     1.36        7840. 
#> 4 4       66          0.809    -7.87       -5112. 
#> 5 5       65          0.530     1.30        6977. 
#> 6 6       36          0.130    -1.43          62.2
```

**What happened:**

1.  couplr calculated how similar each treatment unit is to each control
    unit
2.  It found the optimal one-to-one pairing that minimizes total
    distance
3.  Each treatment unit gets matched to exactly one control unit

### Understanding the Output

``` r

# Quick overview with summary()
summary(result)
#> Matching Result Summary
#> =======================
#> 
#> Method: lap
#> Pairs matched: 50 
#> Unmatched: 0 left, 30 right
#> 
#> Distance Statistics:
#>   Total: 19.6423 
#>   Mean: 0.3928 
#>   Min: 0.0615 
#>   Q1: 0.1845 
#>   Median: 0.2967 
#>   Q3: 0.5039 
#>   Max: 1.2184 
#>   SD: 0.2863

# Or access specific info
result$info$n_matched
#> [1] 50
```

The `result$pairs` table contains:

- `left_id`: Row number from the treatment group
- `right_id`: Row number from the control group
- `distance`: How different the matched units are (lower = more similar)

------------------------------------------------------------------------

## Why Scaling Matters

Without scaling, variables with larger values dominate the matching.
Income (measured in thousands) would overwhelm age (measured in
decades):

``` r

# BAD: Without scaling, income dominates
result_unscaled <- match_couples(
  treatment, control,
  vars = c("age", "income"),
  auto_scale = FALSE
)

# GOOD: With scaling, both variables contribute equally
result_scaled <- match_couples(
  treatment, control,
  vars = c("age", "income"),
  auto_scale = TRUE
)
#> Auto-selected scaling method: standardize

# Compare mean distances
cat("Unscaled mean distance:", round(mean(result_unscaled$pairs$distance), 1), "\n")
#> Unscaled mean distance: 2769.1
cat("Scaled mean distance:", round(mean(result_scaled$pairs$distance), 3), "\n")
#> Scaled mean distance: 0.393
```

**Rule of thumb:** Always use `auto_scale = TRUE` unless you have a
specific reason not to.

------------------------------------------------------------------------

## Checking Match Quality

After matching, verify that treatment and control groups are now
balanced:

``` r

# Get the matched observations
matched_treatment <- treatment[result$pairs$left_id, ]
matched_control <- control[result$pairs$right_id, ]

# Compare means before and after matching
cat("BEFORE matching:\n")
#> BEFORE matching:
cat("  Age difference:", round(mean(treatment$age) - mean(control$age), 1), "years\n")
#>   Age difference: -3.4 years
cat("  Income difference: $", round(mean(treatment$income) - mean(control$income), 0), "\n\n")
#>   Income difference: $ 9266

cat("AFTER matching:\n")
#> AFTER matching:
cat("  Age difference:", round(mean(matched_treatment$age) - mean(matched_control$age), 1), "years\n")
#>   Age difference: -1.3 years
cat("  Income difference: $", round(mean(matched_treatment$income) - mean(matched_control$income), 0), "\n")
#>   Income difference: $ 2667
```

For formal balance assessment, use
[`balance_diagnostics()`](https://gillescolling.com/couplr/reference/balance_diagnostics.md)
(covered in [Matching
Workflows](https://gillescolling.com/couplr/articles/matching-workflows.md)).

### Visualizing Match Quality

Use [`plot()`](https://rdrr.io/r/graphics/plot.default.html) to see the
distribution of match distances:

``` r

plot(result)
```

![Histogram showing distribution of match distances, with most matches
having low distances near
zero](getting-started_files/figure-html/plot-result-1.svg)

The histogram shows how similar matched pairs are. A distribution
concentrated near zero indicates good matches.

------------------------------------------------------------------------

## Large Datasets: Use Greedy Matching

For datasets larger than a few thousand observations, optimal matching
becomes slow. Use
[`greedy_couples()`](https://gillescolling.com/couplr/reference/greedy_couples.md)
instead; it’s 10-100x faster with nearly identical results:

``` r

# Create larger datasets
set.seed(456)
large_treatment <- tibble(
  id = 1:2000,
  age = rnorm(2000, 45, 10),
  income = rnorm(2000, 55000, 12000)
)

large_control <- tibble(
  id = 1:3000,
  age = rnorm(3000, 50, 12),
  income = rnorm(3000, 48000, 15000)
)

# Fast greedy matching
result_greedy <- greedy_couples(
  large_treatment, large_control,
  vars = c("age", "income"),
  auto_scale = TRUE,
  strategy = "row_best"  # fastest strategy
)
#> Auto-selected scaling method: standardize

cat("Matched", result_greedy$info$n_matched, "pairs\n")
#> Matched 2000 pairs
cat("Mean distance:", round(mean(result_greedy$pairs$distance), 3), "\n")
#> Mean distance: 0.201
```

**When to use which:**

| Dataset size | Recommended function |
|----|----|
| \< 1,000 per group | [`match_couples()`](https://gillescolling.com/couplr/reference/match_couples.md) |
| 1,000 - 5,000 | Either works; greedy is faster |
| \> 5,000 | [`greedy_couples()`](https://gillescolling.com/couplr/reference/greedy_couples.md) |

------------------------------------------------------------------------

## Setting a Maximum Distance (Caliper)

Sometimes you want to reject poor matches rather than force bad
pairings. Use `max_distance` to set a caliper:

``` r

# Allow any match
result_loose <- match_couples(
  treatment, control,
  vars = c("age", "income"),
  auto_scale = TRUE
)
#> Auto-selected scaling method: standardize

# Only allow close matches
result_strict <- match_couples(
  treatment, control,
  vars = c("age", "income"),
  auto_scale = TRUE,
  max_distance = 0.5  # reject pairs more different than this
)
#> Auto-selected scaling method: standardize

cat("Without caliper:", result_loose$info$n_matched, "pairs\n")
#> Without caliper: 50 pairs
cat("With caliper:", result_strict$info$n_matched, "pairs\n")
#> With caliper: 40 pairs
```

Stricter calipers mean fewer but better matches.

------------------------------------------------------------------------

## Matching Within Groups (Blocking)

When you have natural groups in your data (e.g., hospitals, regions,
study sites), you can match within each group separately. This ensures
exact balance on the grouping variable.

First, create blocks with
[`matchmaker()`](https://gillescolling.com/couplr/reference/matchmaker.md),
then pass the result to
[`match_couples()`](https://gillescolling.com/couplr/reference/match_couples.md):

``` r

# Data from multiple hospital sites
set.seed(321)
treated <- tibble(
  id = 1:60,
  site = rep(c("Hospital A", "Hospital B", "Hospital C"), each = 20),
  age = rnorm(60, 55, 10),
  severity = rnorm(60, 5, 2)
)

controls <- tibble(
  id = 1:90,
  site = rep(c("Hospital A", "Hospital B", "Hospital C"), each = 30),
  age = rnorm(90, 52, 12),
  severity = rnorm(90, 4.5, 2.5)
)

# Step 1: Create blocks by hospital site
blocks <- matchmaker(
  left = treated,
  right = controls,
  block_type = "group",
  block_by = "site"
)

# Step 2: Match within each block
result_blocked <- match_couples(
  left = blocks$left,
  right = blocks$right,
  vars = c("age", "severity"),
  block_id = "block_id",
  auto_scale = TRUE
)
#> Auto-selected scaling method: standardize

# Verify: matches stay within their block
result_blocked$pairs |> count(block_id)
#> # A tibble: 3 × 2
#>   block_id       n
#>   <chr>      <int>
#> 1 Hospital A    20
#> 2 Hospital B    20
#> 3 Hospital C    20
```

Blocking guarantees that Hospital A patients are only matched to
Hospital A controls, etc.

------------------------------------------------------------------------

## Complete Example

Here’s a realistic workflow from start to finish:

``` r

# 1. Prepare your data
set.seed(789)
patients_treated <- tibble(
  patient_id = paste0("T", 1:100),
  age = rnorm(100, 62, 8),
  bmi = rnorm(100, 28, 4),
  smoker = sample(0:1, 100, replace = TRUE, prob = c(0.6, 0.4))
)

patients_control <- tibble(
  patient_id = paste0("C", 1:200),
  age = rnorm(200, 58, 10),
  bmi = rnorm(200, 26, 5),
  smoker = sample(0:1, 200, replace = TRUE, prob = c(0.7, 0.3))
)

# 2. Match on clinical variables
matched <- match_couples(
  left = patients_treated,
  right = patients_control,
  vars = c("age", "bmi", "smoker"),
  auto_scale = TRUE
)
#> Auto-selected scaling method: standardize

# 3. Check how many matched
cat("Treated patients:", nrow(patients_treated), "\n")
#> Treated patients: 100
cat("Successfully matched:", matched$info$n_matched, "\n")
#> Successfully matched: 100
cat("Match rate:", round(100 * matched$info$n_matched / nrow(patients_treated), 1), "%\n")
#> Match rate: 100 %

# 4. Extract matched samples for analysis
treated_matched <- patients_treated[matched$pairs$left_id, ]
control_matched <- patients_control[matched$pairs$right_id, ]

# 5. Verify balance
cat("\nBalance check (difference in means):\n")
#> 
#> Balance check (difference in means):
cat("  Age:", round(mean(treated_matched$age) - mean(control_matched$age), 2), "\n")
#>   Age: NA
cat("  BMI:", round(mean(treated_matched$bmi) - mean(control_matched$bmi), 2), "\n")
#>   BMI: NA
cat("  Smoker %:", round(100*(mean(treated_matched$smoker) - mean(control_matched$smoker)), 1), "\n")
#>   Smoker %: NA
```

------------------------------------------------------------------------

## Next Steps

You now know the basics of matching with couplr. Here’s where to go
next:

**For production research workflows:**

- [Matching
  Workflows](https://gillescolling.com/couplr/articles/matching-workflows.md)
  covers preprocessing, blocking, formal balance diagnostics, and
  publication-ready output

**For understanding algorithm choices:**

- [Algorithms](https://gillescolling.com/couplr/articles/algorithms.md)
  explains when different solvers are faster or more appropriate

**For comparing with other packages:**

- [Comparison](https://gillescolling.com/couplr/articles/comparison.md)
  shows how couplr differs from MatchIt, optmatch, and designmatch

------------------------------------------------------------------------

## Additional: Direct Assignment Problem Solving

If you need to solve assignment problems directly (not matching
workflows), couplr also provides lower-level functions.

### lap_solve(): Matrix-Based Assignment

Given a cost matrix where entry (i,j) is the cost of assigning row i to
column j:

``` r

# Cost matrix: 3 workers × 3 tasks
cost <- matrix(c(
  4, 2, 5,
  3, 3, 6,
  7, 5, 4
), nrow = 3, byrow = TRUE)

result <- lap_solve(cost)
print(result)
#> Assignment Result
#> =================
#> 
#> # A tibble: 3 × 3
#>   source target  cost
#>    <int>  <int> <dbl>
#> 1      1      2     2
#> 2      2      1     3
#> 3      3      3     4
#> 
#> Total cost: 9 
#> Method: bruteforce
```

Row 1 is assigned to column 2 (cost 2), row 2 to column 1 (cost 3), row
3 to column 3 (cost 4). Total cost: 9.

### Forbidden Assignments

Use `NA` or `Inf` for impossible assignments:

``` r

cost_forbidden <- matrix(c(
  4, 2, NA,   # Row 1 cannot go to column 3
  Inf, 3, 6,  # Row 2 cannot go to column 1
  7, 5, 4
), nrow = 3, byrow = TRUE)

lap_solve(cost_forbidden)
#> Assignment Result
#> =================
#> 
#> # A tibble: 3 × 3
#>   source target  cost
#>    <int>  <int> <dbl>
#> 1      1      1     4
#> 2      2      2     3
#> 3      3      3     4
#> 
#> Total cost: 11 
#> Method: bruteforce
```

### Maximization

For preference or profit maximization:

``` r

preferences <- matrix(c(
  8, 5, 3,
  4, 7, 6,
  2, 4, 9
), nrow = 3, byrow = TRUE)

lap_solve(preferences, maximize = TRUE)
#> Assignment Result
#> =================
#> 
#> # A tibble: 3 × 3
#>   source target  cost
#>    <int>  <int> <dbl>
#> 1      1      1     8
#> 2      2      2     7
#> 3      3      3     9
#> 
#> Total cost: 24 
#> Method: bruteforce
```

### Grouped Data

Solve multiple assignment problems at once using grouped data frames:

``` r

# Weekly nurse-shift scheduling: solve each day separately
schedule <- tibble(
  day = rep(c("Mon", "Tue", "Wed"), each = 9),
  nurse = rep(rep(1:3, each = 3), 3),
  shift = rep(1:3, 9),
  cost = c(4,2,5, 3,3,6, 7,5,4,   # Monday costs
           5,3,4, 2,4,5, 6,4,3,   # Tuesday costs
           3,4,5, 4,2,6, 5,5,4)   # Wednesday costs
)

# Solve all three days at once
schedule |>
  group_by(day) |>
  lap_solve(nurse, shift, cost)
#> # A tibble: 9 × 4
#>   day   source target  cost
#>   <chr>  <int>  <int> <dbl>
#> 1 Mon        1      2     2
#> 2 Mon        2      1     3
#> 3 Mon        3      3     4
#> 4 Tue        1      2     3
#> 5 Tue        2      1     2
#> 6 Tue        3      3     3
#> 7 Wed        1      1     3
#> 8 Wed        2      2     2
#> 9 Wed        3      3     4
```

This solves each day’s assignment problem independently and returns all
results in one tidy table.

### K-Best Solutions

Find multiple near-optimal solutions:

``` r

cost <- matrix(c(1, 2, 3, 4, 3, 2, 5, 4, 1), nrow = 3, byrow = TRUE)

kbest <- lap_solve_kbest(cost, k = 3)
print(kbest)
#> K-Best Assignment Results
#> =========================
#> 
#> Number of solutions: 3 
#> 
#> Solution costs:
#>   Rank 1: 5.0000
#>   Rank 2: 7.0000
#>   Rank 3: 7.0000
#> 
#> Assignments:
#> # A tibble: 9 × 6
#>    rank solution_id source target  cost total_cost
#>   <int>       <int>  <int>  <int> <dbl>      <dbl>
#> 1     1           1      1      1     1          5
#> 2     1           1      2      2     3          5
#> 3     1           1      3      3     1          5
#> 4     2           2      1      2     2          7
#> 5     2           2      2      1     4          7
#> 6     2           2      3      3     1          7
#> 7     3           3      1      1     1          7
#> 8     3           3      2      3     2          7
#> 9     3           3      3      2     4          7
```

------------------------------------------------------------------------

## See Also

- [`?match_couples`](https://gillescolling.com/couplr/reference/match_couples.md) -
  Optimal matching function reference
- [`?greedy_couples`](https://gillescolling.com/couplr/reference/greedy_couples.md) -
  Fast approximate matching
- [`?balance_diagnostics`](https://gillescolling.com/couplr/reference/balance_diagnostics.md) -
  Formal balance assessment
- [`?lap_solve`](https://gillescolling.com/couplr/reference/lap_solve.md) -
  Direct assignment problem solving
