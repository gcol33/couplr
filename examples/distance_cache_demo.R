# Distance Caching and Reuse Examples
# =====================================
#
# This script demonstrates the distance caching functionality in couplr,
# which allows you to compute distances once and reuse them across multiple
# matching operations. This is particularly useful when exploring different
# matching parameters or comparing matching strategies.
#
# Performance benefit: ~60% faster when trying multiple matching scenarios

library(couplr)
library(dplyr)

# ==============================================================================
# Example 1: Basic Distance Caching
# ==============================================================================
# Compute distances once, reuse for multiple matching attempts

cat("\n=== Example 1: Basic Distance Caching ===\n\n")

# Create sample datasets
set.seed(123)
left <- data.frame(
  id = 1:50,
  age = rnorm(50, mean = 45, sd = 10),
  income = rnorm(50, mean = 50000, sd = 15000),
  bp_systolic = rnorm(50, mean = 130, sd = 15)
)

right <- data.frame(
  id = 51:150,
  age = rnorm(100, mean = 47, sd = 10),
  income = rnorm(100, mean = 52000, sd = 15000),
  bp_systolic = rnorm(100, mean = 132, sd = 15)
)

# Step 1: Compute distances once
cat("Step 1: Computing distances...\n")
dist_obj <- compute_distances(
  left, right,
  vars = c("age", "income", "bp_systolic"),
  distance = "euclidean",
  scale = "standardize"
)

print(dist_obj)

# Step 2: Reuse for different max_distance thresholds
cat("\nStep 2: Trying different max_distance thresholds...\n")

result_strict <- match_couples(dist_obj, max_distance = 0.5)
cat("Strict matching (max_dist = 0.5):\n")
cat("  Matched pairs:", nrow(result_strict$pairs), "\n")
cat("  Mean distance:", round(mean(result_strict$pairs$distance), 3), "\n\n")

result_moderate <- match_couples(dist_obj, max_distance = 1.0)
cat("Moderate matching (max_dist = 1.0):\n")
cat("  Matched pairs:", nrow(result_moderate$pairs), "\n")
cat("  Mean distance:", round(mean(result_moderate$pairs$distance), 3), "\n\n")

result_relaxed <- match_couples(dist_obj, max_distance = 2.0)
cat("Relaxed matching (max_dist = 2.0):\n")
cat("  Matched pairs:", nrow(result_relaxed$pairs), "\n")
cat("  Mean distance:", round(mean(result_relaxed$pairs$distance), 3), "\n\n")

# All three use the SAME precomputed distances!


# ==============================================================================
# Example 2: Comparing Optimal vs Greedy Matching
# ==============================================================================
# Use the same distance object for both methods

cat("\n=== Example 2: Comparing Matching Methods ===\n\n")

# Reuse the same distance object from Example 1
cat("Using the same precomputed distances for both methods:\n\n")

# Optimal matching
result_optimal <- match_couples(dist_obj, max_distance = 1.0)
cat("Optimal matching:\n")
cat("  Matched pairs:", nrow(result_optimal$pairs), "\n")
cat("  Total distance:", round(sum(result_optimal$pairs$distance), 2), "\n")
cat("  Mean distance:", round(mean(result_optimal$pairs$distance), 3), "\n\n")

# Greedy matching (sorted strategy)
result_greedy <- match_couples(dist_obj, strategy = "sorted", method = "greedy")
cat("Greedy matching (sorted):\n")
cat("  Matched pairs:", nrow(result_greedy$pairs), "\n")
cat("  Total distance:", round(sum(result_greedy$pairs$distance), 2), "\n")
cat("  Mean distance:", round(mean(result_greedy$pairs$distance), 3), "\n\n")

cat("Difference in total distance:",
    round(sum(result_greedy$pairs$distance) - sum(result_optimal$pairs$distance), 2), "\n")


# ==============================================================================
# Example 3: Updating Constraints Without Recomputation
# ==============================================================================
# Apply new constraints to an existing distance object

cat("\n=== Example 3: Updating Constraints ===\n\n")

# Start with the original distance object (no constraints)
cat("Original distance object:\n")
finite_dists <- dist_obj$cost_matrix[is.finite(dist_obj$cost_matrix)]
cat("  Valid pairs:", length(finite_dists), "\n")
cat("  Distance range: [", round(min(finite_dists), 3), ",",
    round(max(finite_dists), 3), "]\n\n")

# Apply max_distance constraint
constrained_dist <- update_constraints(dist_obj, max_distance = 1.5)
cat("After applying max_distance = 1.5:\n")
finite_dists_constrained <- constrained_dist$cost_matrix[is.finite(constrained_dist$cost_matrix)]
cat("  Valid pairs:", length(finite_dists_constrained), "\n")
cat("  Distance range: [", round(min(finite_dists_constrained), 3), ",",
    round(max(finite_dists_constrained), 3), "]\n\n")

# Use the constrained distance object
result_constrained <- match_couples(constrained_dist)
cat("Matching with constrained distances:\n")
cat("  Matched pairs:", nrow(result_constrained$pairs), "\n")
cat("  Max distance:", round(max(result_constrained$pairs$distance), 3), "\n")


# ==============================================================================
# Example 4: Distance Caching with Automatic Scaling
# ==============================================================================
# Combine auto_scale with distance caching

cat("\n=== Example 4: Auto-scaling + Distance Caching ===\n\n")

# Create data with different scales and problematic variables
set.seed(456)
left_mixed <- data.frame(
  id = 1:40,
  age = rnorm(40, mean = 50, sd = 10),
  income = rnorm(40, mean = 55000, sd = 20000),
  weight_kg = rnorm(40, mean = 75, sd = 12),
  constant_var = 5  # Problematic: constant
)

right_mixed <- data.frame(
  id = 41:120,
  age = rnorm(80, mean = 52, sd = 10),
  income = rnorm(80, mean = 58000, sd = 20000),
  weight_kg = rnorm(80, mean = 77, sd = 12),
  constant_var = 5
)

# Compute distances with automatic preprocessing
cat("Computing distances with auto_scale = TRUE...\n")
dist_auto <- compute_distances(
  left_mixed, right_mixed,
  vars = c("age", "income", "weight_kg", "constant_var"),
  auto_scale = TRUE
)

cat("\nPreprocessing automatically:\n")
cat("  - Excluded constant_var (SD = 0)\n")
cat("  - Analyzed remaining variables\n")
cat("  - Applied appropriate scaling\n")
cat("  Variables kept:", paste(dist_auto$metadata$vars, collapse = ", "), "\n\n")

# Reuse the preprocessed distances
result_auto <- match_couples(dist_auto, max_distance = 1.0)
cat("Matching with preprocessed distances:\n")
cat("  Matched pairs:", nrow(result_auto$pairs), "\n")


# ==============================================================================
# Example 5: Performance Comparison
# ==============================================================================
# Demonstrate the performance benefit of distance caching

cat("\n=== Example 5: Performance Comparison ===\n\n")

# Create larger datasets for meaningful timing
set.seed(789)
left_large <- data.frame(
  id = 1:100,
  x1 = rnorm(100),
  x2 = rnorm(100),
  x3 = rnorm(100),
  x4 = rnorm(100)
)

right_large <- data.frame(
  id = 101:300,
  x1 = rnorm(200),
  x2 = rnorm(200),
  x3 = rnorm(200),
  x4 = rnorm(200)
)

vars_to_match <- c("x1", "x2", "x3", "x4")

# Method 1: Recomputing distances each time
cat("Method 1: Recomputing distances each time\n")
time_recompute <- system.time({
  r1 <- match_couples(left_large, right_large, vars = vars_to_match,
                      distance = "euclidean", scale = "standardize", max_distance = 1.0)
  r2 <- match_couples(left_large, right_large, vars = vars_to_match,
                      distance = "euclidean", scale = "standardize", max_distance = 1.5)
  r3 <- match_couples(left_large, right_large, vars = vars_to_match,
                      distance = "euclidean", scale = "standardize", max_distance = 2.0)
})
cat("  Time:", round(time_recompute["elapsed"], 3), "seconds\n\n")

# Method 2: Caching distances
cat("Method 2: Caching distances\n")
time_cached <- system.time({
  dist_cached <- compute_distances(left_large, right_large,
                                   vars = vars_to_match,
                                   distance = "euclidean",
                                   scale = "standardize")
  r1_cached <- match_couples(dist_cached, max_distance = 1.0)
  r2_cached <- match_couples(dist_cached, max_distance = 1.5)
  r3_cached <- match_couples(dist_cached, max_distance = 2.0)
})
cat("  Time:", round(time_cached["elapsed"], 3), "seconds\n\n")

speedup <- time_recompute["elapsed"] / time_cached["elapsed"]
cat("Speedup:", round(speedup, 2), "x faster\n")
cat("Time saved:", round(time_recompute["elapsed"] - time_cached["elapsed"], 3), "seconds\n")


# ==============================================================================
# Example 6: Complete Workflow with Distance Caching
# ==============================================================================
# Realistic workflow: preprocess → cache → match → balance → join

cat("\n=== Example 6: Complete Workflow ===\n\n")

# Create treatment/control datasets
set.seed(999)
treated <- data.frame(
  id = 1:60,
  treatment = 1,
  age = rnorm(60, mean = 48, sd = 12),
  income = rnorm(60, mean = 52000, sd = 18000),
  bmi = rnorm(60, mean = 27, sd = 4),
  outcome = rnorm(60, mean = 75, sd = 10)  # Don't match on outcome!
)

controls <- data.frame(
  id = 61:200,
  treatment = 0,
  age = rnorm(140, mean = 50, sd = 12),
  income = rnorm(140, mean = 54000, sd = 18000),
  bmi = rnorm(140, mean = 28, sd = 4),
  outcome = rnorm(140, mean = 70, sd = 10)
)

# Step 1: Compute distances with auto-scaling
cat("Step 1: Computing distances with auto-scaling...\n")
dist_workflow <- compute_distances(
  treated, controls,
  vars = c("age", "income", "bmi"),
  auto_scale = TRUE,
  left_id = "id",
  right_id = "id"
)

# Step 2: Try multiple matching strategies
cat("\nStep 2: Exploring matching strategies...\n")

match_tight <- match_couples(dist_workflow, max_distance = 0.5)
cat("  Tight matching (0.5):", nrow(match_tight$pairs), "pairs\n")

match_moderate <- match_couples(dist_workflow, max_distance = 1.0)
cat("  Moderate matching (1.0):", nrow(match_moderate$pairs), "pairs\n")

match_relaxed <- match_couples(dist_workflow, max_distance = 1.5)
cat("  Relaxed matching (1.5):", nrow(match_relaxed$pairs), "pairs\n")

# Step 3: Choose moderate matching and check balance
cat("\nStep 3: Checking balance for moderate matching...\n")
balance <- balance_diagnostics(
  match_moderate,
  treated, controls,
  vars = c("age", "income", "bmi")
)

cat("\nBalance summary:\n")
cat("  Mean standardized difference:",
    round(balance$overall$mean_abs_std_diff, 3), "\n")
cat("  Max standardized difference:",
    round(balance$overall$max_abs_std_diff, 3), "\n")

# Step 4: Create analysis dataset
cat("\nStep 4: Creating analysis-ready dataset...\n")
analysis_data <- join_matched(
  match_moderate,
  treated, controls,
  left_vars = c("treatment", "age", "income", "bmi", "outcome"),
  right_vars = c("age", "income", "bmi", "outcome"),
  suffix = c("_treated", "_control")
)

cat("  Dataset dimensions:", nrow(analysis_data), "×", ncol(analysis_data), "\n")
cat("  Columns:", paste(names(analysis_data)[1:5], collapse = ", "), "...\n")

# Step 5: Analyze treatment effect
cat("\nStep 5: Estimating treatment effect...\n")
analysis_data <- analysis_data %>%
  mutate(outcome_diff = outcome_treated - outcome_control)

treatment_effect <- mean(analysis_data$outcome_diff)
se <- sd(analysis_data$outcome_diff) / sqrt(nrow(analysis_data))

cat("  Average treatment effect:", round(treatment_effect, 2), "\n")
cat("  Standard error:", round(se, 2), "\n")
cat("  95% CI: [", round(treatment_effect - 1.96*se, 2), ",",
    round(treatment_effect + 1.96*se, 2), "]\n")


# ==============================================================================
# Example 7: Distance Object Inspection
# ==============================================================================
# Explore the structure and statistics of a distance object

cat("\n=== Example 7: Distance Object Inspection ===\n\n")

# Create a distance object
set.seed(2025)
left_inspect <- data.frame(
  id = 1:30,
  x = rnorm(30, mean = 0, sd = 1),
  y = rnorm(30, mean = 0, sd = 1)
)

right_inspect <- data.frame(
  id = 31:80,
  x = rnorm(50, mean = 0.5, sd = 1),
  y = rnorm(50, mean = 0.5, sd = 1)
)

dist_inspect <- compute_distances(
  left_inspect, right_inspect,
  vars = c("x", "y"),
  distance = "euclidean",
  scale = "standardize"
)

# Print method shows key information
cat("Print method output:\n")
print(dist_inspect)

cat("\n\nSummary method shows detailed statistics:\n")
summary(dist_inspect)

# Access components directly
cat("\n\nDirect access to components:\n")
cat("  Metadata vars:", paste(dist_inspect$metadata$vars, collapse = ", "), "\n")
cat("  Distance metric:", dist_inspect$metadata$distance, "\n")
cat("  Scaling method:", dist_inspect$metadata$scale, "\n")
cat("  Matrix dimensions:", nrow(dist_inspect$cost_matrix), "×",
    ncol(dist_inspect$cost_matrix), "\n")
cat("  Computed at:", format(dist_inspect$metadata$computed_at, "%Y-%m-%d %H:%M:%S"), "\n")

cat("\n=== All Examples Complete ===\n")
