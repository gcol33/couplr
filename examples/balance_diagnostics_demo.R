# ==============================================================================
# Balance Diagnostics Demo
# ==============================================================================
# Demonstrates how to assess matching quality using balance diagnostics

library(couplr)

# ==============================================================================
# Example 1: Basic Balance Diagnostics
# ==============================================================================

cat("\n=== Example 1: Basic Balance Diagnostics ===\n\n")

set.seed(123)

# Create treatment and control groups with some differences
left <- data.frame(
  id = 1:50,
  treatment = 1,
  age = rnorm(50, 45, 10),
  income = rnorm(50, 55000, 15000),
  education_years = sample(12:20, 50, replace = TRUE)
)

right <- data.frame(
  id = 51:200,
  treatment = 0,
  age = rnorm(150, 48, 10),         # Slightly older
  income = rnorm(150, 52000, 15000), # Slightly less income
  education_years = sample(12:20, 150, replace = TRUE)
)

cat("Sample sizes:\n")
cat("  Treatment (left):", nrow(left), "\n")
cat("  Control (right):", nrow(right), "\n\n")

# Before matching: Check initial balance
cat("Before matching - Summary statistics:\n")
cat("Age:\n")
cat("  Treatment mean:", round(mean(left$age), 2), "\n")
cat("  Control mean:", round(mean(right$age), 2), "\n")
cat("Income:\n")
cat("  Treatment mean:", round(mean(left$income), 0), "\n")
cat("  Control mean:", round(mean(right$income), 0), "\n\n")

# Perform matching
result <- match_couples(
  left, right,
  vars = c("age", "income", "education_years"),
  scale = "standardize"
)

cat("Matching completed:\n")
cat("  Matched pairs:", result$info$n_matched, "\n")
cat("  Total distance:", round(result$info$total_distance, 2), "\n\n")

# Get balance diagnostics
balance <- balance_diagnostics(
  result,
  left, right,
  vars = c("age", "income", "education_years")
)

# Print full diagnostics
print(balance)

# Get just the balance table
cat("\n=== Balance Table ===\n")
print(balance_table(balance))


# ==============================================================================
# Example 2: Comparing Optimal vs Greedy Matching
# ==============================================================================

cat("\n\n=== Example 2: Optimal vs Greedy Matching ===\n\n")

set.seed(456)

left2 <- data.frame(
  id = 1:30,
  age = rnorm(30, 50, 12),
  bp_systolic = rnorm(30, 130, 15)
)

right2 <- data.frame(
  id = 31:80,
  age = rnorm(50, 52, 12),
  bp_systolic = rnorm(50, 128, 15)
)

# Optimal matching
result_opt <- match_couples(left2, right2, vars = c("age", "bp_systolic"))
balance_opt <- balance_diagnostics(result_opt, left2, right2, vars = c("age", "bp_systolic"))

# Greedy matching
result_greedy <- match_couples(left2, right2, vars = c("age", "bp_systolic"), method = "greedy")
balance_greedy <- balance_diagnostics(result_greedy, left2, right2, vars = c("age", "bp_systolic"))

cat("Optimal Matching Balance:\n")
cat("  Mean |Std Diff|:", round(balance_opt$overall$mean_abs_std_diff, 3), "\n")
cat("  Max |Std Diff|:", round(balance_opt$overall$max_abs_std_diff, 3), "\n")
cat("  Total distance:", round(result_opt$info$total_distance, 2), "\n\n")

cat("Greedy Matching Balance:\n")
cat("  Mean |Std Diff|:", round(balance_greedy$overall$mean_abs_std_diff, 3), "\n")
cat("  Max |Std Diff|:", round(balance_greedy$overall$max_abs_std_diff, 3), "\n")
cat("  Total distance:", round(result_greedy$info$total_distance, 2), "\n\n")

cat("Comparison:\n")
if (balance_opt$overall$mean_abs_std_diff < balance_greedy$overall$mean_abs_std_diff) {
  cat("  Optimal matching achieved better balance\n")
} else {
  cat("  Greedy matching achieved comparable or better balance\n")
}


# ==============================================================================
# Example 3: Balance with Blocking
# ==============================================================================

cat("\n\n=== Example 3: Balance with Blocking ===\n\n")

set.seed(789)

left3 <- data.frame(
  id = 1:60,
  site = rep(c("Hospital_A", "Hospital_B", "Hospital_C"), each = 20),
  age = rnorm(60, 55, 10),
  bmi = rnorm(60, 27, 4)
)

right3 <- data.frame(
  id = 61:180,
  site = rep(c("Hospital_A", "Hospital_B", "Hospital_C"), each = 40),
  age = rnorm(120, 55, 10),
  bmi = rnorm(120, 27, 4)
)

# Create blocks by site
blocks <- matchmaker(left3, right3, block_type = "group", block_by = "site")

cat("Created blocks by site:\n")
cat("  Number of blocks:", length(unique(blocks$left$block_id)), "\n\n")

# Match within blocks
result_blocked <- match_couples(
  blocks$left, blocks$right,
  vars = c("age", "bmi")
)

# Get balance diagnostics
balance_blocked <- balance_diagnostics(
  result_blocked,
  blocks$left, blocks$right,
  vars = c("age", "bmi")
)

# Print with block-level statistics
print(balance_blocked)


# ==============================================================================
# Example 4: Detecting Poor Balance
# ==============================================================================

cat("\n\n=== Example 4: Detecting Poor Balance ===\n\n")

set.seed(321)

# Create groups with substantial differences
left_young <- data.frame(
  id = 1:25,
  age = rnorm(25, 35, 5),      # Much younger
  income = rnorm(25, 45000, 8000)
)

right_old <- data.frame(
  id = 26:75,
  age = rnorm(50, 60, 5),      # Much older
  income = rnorm(50, 65000, 12000)  # Higher income
)

cat("Initial group differences:\n")
cat("  Left age mean:", round(mean(left_young$age), 1), "\n")
cat("  Right age mean:", round(mean(right_old$age), 1), "\n")
cat("  Difference:", round(mean(right_old$age) - mean(left_young$age), 1), "years\n\n")

# Try to match despite large differences
result_poor <- match_couples(
  left_young, right_old,
  vars = c("age", "income"),
  scale = "standardize"
)

balance_poor <- balance_diagnostics(
  result_poor,
  left_young, right_old,
  vars = c("age", "income")
)

cat("Balance diagnostics for difficult matching:\n")
print(balance_poor)

# Check if balance is acceptable
if (balance_poor$overall$mean_abs_std_diff > 0.25) {
  cat("\nWARNING: Poor balance detected!\n")
  cat("The matched groups still differ substantially.\n")
  cat("Consider:\n")
  cat("  - Relaxing inclusion criteria\n")
  cat("  - Using ratio matching (multiple controls per treatment)\n")
  cat("  - Adding more matching variables\n")
  cat("  - Using propensity score matching instead\n")
}


# ==============================================================================
# Example 5: Extracting Specific Balance Metrics
# ==============================================================================

cat("\n\n=== Example 5: Extracting Specific Metrics ===\n\n")

set.seed(654)

left5 <- data.frame(
  id = 1:40,
  var1 = rnorm(40, 100, 15),
  var2 = rnorm(40, 50, 8),
  var3 = rnorm(40, 10, 2)
)

right5 <- data.frame(
  id = 41:120,
  var1 = rnorm(80, 102, 15),
  var2 = rnorm(80, 49, 8),
  var3 = rnorm(80, 10, 2)
)

result5 <- match_couples(left5, right5, vars = c("var1", "var2", "var3"))
balance5 <- balance_diagnostics(result5, left5, right5, vars = c("var1", "var2", "var3"))

cat("Accessing specific balance metrics:\n\n")

# Overall metrics
cat("Overall Balance Quality:\n")
cat("  Mean absolute standardized difference:",
    round(balance5$overall$mean_abs_std_diff, 3), "\n")
cat("  Maximum absolute standardized difference:",
    round(balance5$overall$max_abs_std_diff, 3), "\n")
cat("  Percentage of vars with |Std Diff| > 0.25:",
    round(balance5$overall$pct_large_imbalance, 1), "%\n\n")

# Per-variable metrics
cat("Per-variable Standardized Differences:\n")
for (i in 1:nrow(balance5$var_stats)) {
  var_name <- balance5$var_stats$variable[i]
  std_diff <- balance5$var_stats$std_diff[i]

  quality <- if (abs(std_diff) < 0.1) {
    "Excellent"
  } else if (abs(std_diff) < 0.25) {
    "Good"
  } else {
    "Needs attention"
  }

  cat(sprintf("  %s: %.3f (%s)\n", var_name, std_diff, quality))
}

cat("\n")

# Variance ratios
cat("Variance Ratios (Left SD / Right SD):\n")
cat("(Values close to 1.0 indicate similar spread)\n")
for (i in 1:nrow(balance5$var_stats)) {
  var_name <- balance5$var_stats$variable[i]
  var_ratio <- balance5$var_stats$var_ratio[i]
  cat(sprintf("  %s: %.2f\n", var_name, var_ratio))
}


# ==============================================================================
# Example 6: Using Balance Table for Reporting
# ==============================================================================

cat("\n\n=== Example 6: Balance Table for Publication ===\n\n")

# Get formatted balance table
balance_tbl <- balance_table(balance5, digits = 2)

cat("Formatted balance table (suitable for reports):\n")
print(balance_tbl)

cat("\n")
cat("This table can be exported to CSV or used in reports:\n")
cat("  write.csv(balance_table(balance), 'balance_results.csv', row.names = FALSE)\n")


# ==============================================================================
# Summary
# ==============================================================================

cat("\n\n=== Summary ===\n\n")
cat("Balance diagnostics help you:\n")
cat("  1. Assess the quality of your matches\n")
cat("  2. Identify problematic variables with poor balance\n")
cat("  3. Compare different matching strategies\n")
cat("  4. Report matching quality in publications\n")
cat("  5. Detect when groups remain too different after matching\n\n")

cat("Key thresholds for standardized differences:\n")
cat("  |Std Diff| < 0.10: Excellent balance\n")
cat("  |Std Diff| 0.10-0.25: Good balance\n")
cat("  |Std Diff| 0.25-0.50: Acceptable balance\n")
cat("  |Std Diff| > 0.50: Poor balance (investigate)\n\n")

cat("Next steps:\n")
cat("  - Use balance diagnostics to validate your matches\n")
cat("  - Compare balance across different matching strategies\n")
cat("  - Report balance metrics in your analysis\n")
cat("  - Investigate and address any large imbalances\n")
