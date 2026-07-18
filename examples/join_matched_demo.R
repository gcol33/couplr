# ==============================================================================
# join_matched() - Comprehensive Examples
# ==============================================================================
#
# This script demonstrates how to use join_matched() to create analysis-ready
# datasets from matching results. join_matched() automates the tedious process
# of joining matched pairs with original data, making it easy to proceed
# directly to analysis.
#
# ==============================================================================

library(couplr)
library(dplyr)

# ==============================================================================
# Example 1: Basic Treatment Effect Analysis
# ==============================================================================

cat("\n=== Example 1: Basic Treatment Effect Analysis ===\n\n")

# Create treatment and control groups
set.seed(123)
treated <- data.frame(
  id = 1:100,
  treatment = 1,
  age = rnorm(100, 50, 10),
  income = rlnorm(100, 10, 0.5),
  education = sample(c("HS", "College", "Grad"), 100, replace = TRUE),
  outcome = rnorm(100, 100, 15)  # Outcome of interest
)

controls <- data.frame(
  id = 101:250,
  treatment = 0,
  age = rnorm(150, 52, 10),
  income = rlnorm(150, 10, 0.5),
  education = sample(c("HS", "College", "Grad"), 150, replace = TRUE),
  outcome = rnorm(150, 95, 15)
)

# Match treated units to controls
result <- match_couples(
  treated, controls,
  vars = c("age", "income"),
  scale = "standardize"
)

cat("Matched", result$info$n_matched, "pairs\n")

# Join matched pairs with original data
matched_data <- join_matched(
  result,
  treated, controls,
  left_vars = c("treatment", "age", "income", "outcome"),
  right_vars = c("age", "income", "outcome"),
  suffix = c("_treated", "_control")
)

print(head(matched_data))

# Calculate treatment effect
matched_data <- matched_data %>%
  mutate(
    outcome_diff = outcome_treated - outcome_control,
    age_diff = age_treated - age_control,
    income_diff = income_treated - income_control
  )

cat("\nAverage treatment effect:", mean(matched_data$outcome_diff), "\n")
cat("Balance on age (mean difference):", mean(matched_data$age_diff), "\n")
cat("Balance on income (mean difference):", mean(matched_data$income_diff), "\n")


# ==============================================================================
# Example 2: Custom Variable Selection
# ==============================================================================

cat("\n\n=== Example 2: Custom Variable Selection ===\n\n")

# Sometimes you only need specific variables for analysis
# Use left_vars and right_vars to select them

minimal_data <- join_matched(
  result,
  treated, controls,
  left_vars = c("treatment", "outcome"),  # Only outcome from treated
  right_vars = "outcome",                 # Only outcome from control
  suffix = c("_T", "_C")
)

print(head(minimal_data))

# This gives a cleaner dataset for simple comparisons
cat("\nMean outcome (treated):", mean(minimal_data$outcome_T), "\n")
cat("Mean outcome (control):", mean(minimal_data$outcome_C), "\n")
cat("Difference:", mean(minimal_data$outcome_T - minimal_data$outcome_C), "\n")


# ==============================================================================
# Example 3: Matched Analysis with Blocking
# ==============================================================================

cat("\n\n=== Example 3: Matched Analysis with Blocking ===\n\n")

# Often we want exact matching on some variables (like education)
# and continuous matching on others (like age, income)

# Create blocks by education level
blocks <- matchmaker(
  treated, controls,
  block_type = "group",
  block_by = "education"
)

cat("Created", nrow(blocks$block_summary), "blocks:\n")
print(blocks$block_summary)

# Match within blocks
result_blocked <- match_couples(
  blocks$left, blocks$right,
  vars = c("age", "income"),
  scale = "standardize"
)

# Join with block information
matched_blocked <- join_matched(
  result_blocked,
  blocks$left, blocks$right,
  left_vars = c("treatment", "education", "age", "income", "outcome"),
  right_vars = c("education", "age", "income", "outcome"),
  suffix = c("_T", "_C"),
  include_block_id = TRUE
)

print(head(matched_blocked))

# Analyze by block
block_effects <- matched_blocked %>%
  mutate(effect = outcome_T - outcome_C) %>%
  group_by(block_id) %>%
  summarise(
    n_pairs = n(),
    mean_effect = mean(effect),
    se_effect = sd(effect) / sqrt(n()),
    .groups = "drop"
  )

cat("\nTreatment effects by education level:\n")
print(block_effects)


# ==============================================================================
# Example 4: Minimal Output for Compact Analysis
# ==============================================================================

cat("\n\n=== Example 4: Minimal Output for Compact Analysis ===\n\n")

# Sometimes you don't need all the metadata
# Remove distance, pair_id, and block_id for cleaner output

compact_data <- join_matched(
  result,
  treated, controls,
  left_vars = c("outcome"),
  right_vars = c("outcome"),
  suffix = c("_T", "_C"),
  include_distance = FALSE,
  include_pair_id = FALSE
)

print(head(compact_data))

# This is perfect for quick t-tests or simple comparisons
cat("\nPaired t-test:\n")
test_result <- t.test(compact_data$outcome_T, compact_data$outcome_C, paired = TRUE)
print(test_result)


# ==============================================================================
# Example 5: Using augment() (broom-style interface)
# ==============================================================================

cat("\n\n=== Example 5: Using augment() (broom-style) ===\n\n")

# If you're familiar with the broom package, you can use augment()
# It's just a wrapper around join_matched() with sensible defaults

augmented <- augment(result, treated, controls)

cat("augment() gives the same result as join_matched():\n")
print(head(augmented))

# You can still pass custom arguments
augmented_custom <- augment(
  result, treated, controls,
  suffix = c("_treated", "_control"),
  include_distance = FALSE
)

print(head(augmented_custom))


# ==============================================================================
# Example 6: Greedy Matching with join_matched()
# ==============================================================================

cat("\n\n=== Example 6: Greedy Matching ===\n\n")

# join_matched() works with any matching method, including greedy

# Try greedy matching for faster results
result_greedy <- match_couples(
  treated, controls,
  vars = c("age", "income"),
  strategy = "sorted",
  scale = "standardize"
, method = "greedy")

cat("Greedy matched", result_greedy$info$n_matched, "pairs\n")
cat("Total distance:", result_greedy$info$total_distance, "\n")

# Join just like before
greedy_data <- join_matched(
  result_greedy,
  treated, controls,
  suffix = c("_T", "_C")
)

print(head(greedy_data))

# Compare results
cat("\nGreedy vs Optimal:\n")
cat("Greedy mean distance:", mean(greedy_data$distance), "\n")
cat("Optimal mean distance:", mean(matched_data$distance), "\n")


# ==============================================================================
# Example 7: Custom ID Columns
# ==============================================================================

cat("\n\n=== Example 7: Custom ID Columns ===\n\n")

# Sometimes your datasets don't use "id" as the identifier

patients <- data.frame(
  patient_id = paste0("P", 1:50),
  treatment = 1,
  age = rnorm(50, 50, 10),
  baseline_score = rnorm(50, 100, 15)
)

controls_pool <- data.frame(
  control_id = paste0("C", 1:100),
  treatment = 0,
  age = rnorm(100, 52, 10),
  baseline_score = rnorm(100, 100, 15)
)

# Match with custom ID columns
result_custom <- match_couples(
  patients, controls_pool,
  vars = c("age", "baseline_score"),
  left_id = "patient_id",
  right_id = "control_id"
)

# join_matched needs to know the ID column names
matched_custom <- join_matched(
  result_custom,
  patients, controls_pool,
  left_id = "patient_id",
  right_id = "control_id",
  suffix = c("_patient", "_control")
)

print(head(matched_custom))


# ==============================================================================
# Example 8: Complete Workflow with Diagnostics
# ==============================================================================

cat("\n\n=== Example 8: Complete Workflow with Balance Check ===\n\n")

# A realistic workflow: match, check balance, create analysis dataset

# 1. Match
result_final <- match_couples(
  treated, controls,
  vars = c("age", "income"),
  scale = "standardize",
  auto_scale = TRUE
)

# 2. Check balance
balance <- balance_diagnostics(
  result_final,
  treated, controls,
  vars = c("age", "income")
)

cat("Balance assessment:\n")
print(balance)

# 3. If balance is good, create analysis dataset
if (balance$overall$mean_abs_std_diff < 0.1) {
  cat("\nBalance is excellent! Proceeding to analysis...\n")

  analysis_data <- join_matched(
    result_final,
    treated, controls,
    left_vars = c("treatment", "age", "income", "education", "outcome"),
    right_vars = c("age", "income", "education", "outcome"),
    suffix = c("_T", "_C")
  )

  # Run analysis
  model <- lm(
    I(outcome_T - outcome_C) ~ age_T + income_T + education_T,
    data = analysis_data
  )

  cat("\nRegression of treatment effect on covariates:\n")
  print(summary(model))

} else {
  cat("\nBalance is not ideal. Consider different matching parameters.\n")
}


# ==============================================================================
# Summary
# ==============================================================================

cat("\n\n=== Summary ===\n\n")
cat("join_matched() provides a convenient way to:\n")
cat("  1. Combine matched pairs with original data\n")
cat("  2. Select which variables to include\n")
cat("  3. Apply consistent naming with suffixes\n")
cat("  4. Remove unnecessary columns (distance, pair_id, block_id)\n")
cat("  5. Create analysis-ready datasets in one step\n\n")
cat("Key parameters:\n")
cat("  - left_vars / right_vars: Select variables to include\n")
cat("  - suffix: Control naming (default: c('_left', '_right'))\n")
cat("  - include_distance / include_pair_id: Toggle metadata\n")
cat("  - left_id / right_id: Specify custom ID columns\n\n")
cat("Use augment() for a broom-style interface with sensible defaults.\n")
