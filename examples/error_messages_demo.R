# Error Messages and Cost Checking Demo
# =======================================
#
# This script demonstrates couplr's fun, helpful error messages and automatic
# cost distribution checking. Like testthat, couplr makes errors light and
# memorable with couple-themed messages and optional emojis!
#
# Features:
# - 💔 Fun couple-themed error messages
# - 📊 Automatic cost distribution checking
# - ⚠️ Helpful warnings with actionable suggestions
# - ✨ Optional emoji support

library(couplr)
library(dplyr)

cat("\n=== Welcome to couplr's Error Messages Demo! ===\n")
cat("Making errors fun since 2025 ", couplr:::couplr_emoji("heart"), "\n\n")

# ==============================================================================
# Example 1: Basic Error Messages
# ==============================================================================

cat("=== Example 1: Basic Friendly Errors ===\n\n")

# Error 1: Missing dataset
cat("1.1 Missing right dataset:\n")
left <- data.frame(id = 1:10, x = rnorm(10))
try(match_couples(left, vars = "x"), silent = FALSE)
cat("\n")

# Error 2: Missing vars
cat("1.2 Missing vars parameter:\n")
right <- data.frame(id = 11:20, x = rnorm(10))
try(match_couples(left, right), silent = FALSE)
cat("\n")

# Error 3: Missing variable in data
cat("1.3 Variable not found in data:\n")
try(match_couples(left, right, vars = c("x", "nonexistent")), silent = FALSE)
cat("\n")


# ==============================================================================
# Example 2: Cost Distribution Warnings - Many Zeros
# ==============================================================================

cat("\n=== Example 2: Duplicate Detection ===\n\n")

# Create data with many identical values
set.seed(123)
left_dups <- data.frame(
  id = 1:50,
  age = rep(c(25, 30, 35, 40, 45), each = 10),  # Only 5 unique values
  score = rep(c(70, 80, 90), length.out = 50)    # Only 3 unique values
)

right_dups <- data.frame(
  id = 51:100,
  age = rep(c(25, 30, 35, 40, 45), each = 10),
  score = rep(c(70, 80, 90), length.out = 50)
)

cat("Matching with many duplicates (check_costs = TRUE by default):\n")
result_dups <- match_couples(
  left_dups, right_dups,
  vars = c("age", "score"),
  scale = "standardize"
)

cat("\nMatched ", nrow(result_dups$pairs), " pairs despite duplicates.\n")
cat("Notice the helpful warning about zero distances!\n")


# ==============================================================================
# Example 3: Cost Distribution Warnings - Extreme Values
# ==============================================================================

cat("\n\n=== Example 3: Skewed Distribution Detection ===\n\n")

# Create data with extreme outliers
set.seed(456)
left_skewed <- data.frame(
  id = 1:40,
  income = c(rnorm(38, mean = 50000, sd = 10000), 5000000, 8000000),  # 2 outliers!
  age = rnorm(40, mean = 45, sd = 10)
)

right_skewed <- data.frame(
  id = 41:80,
  income = rnorm(40, mean = 50000, sd = 10000),
  age = rnorm(40, mean = 45, sd = 10)
)

cat("Matching with extreme outliers:\n")
result_skewed <- match_couples(
  left_skewed, right_skewed,
  vars = c("income", "age"),
  scale = FALSE  # No scaling to trigger the warning
)

cat("\nCouplr detected the skewed distribution and suggested scaling!\n")
cat("Let's try again with scaling:\n\n")

result_scaled <- match_couples(
  left_skewed, right_skewed,
  vars = c("income", "age"),
  scale = "robust",  # Use robust scaling
  check_costs = FALSE  # Already know about the issue
)

cat("Much better! Robust scaling handled the outliers.\n")


# ==============================================================================
# Example 4: Too Many Forbidden Pairs
# ==============================================================================

cat("\n\n=== Example 4: Overly Strict Constraints ===\n\n")

# Create data with strict constraints
set.seed(789)
left_strict <- data.frame(
  id = 1:30,
  age = rnorm(30, mean = 50, sd = 15),
  income = rnorm(30, mean = 60000, sd = 20000)
)

right_strict <- data.frame(
  id = 31:60,
  age = rnorm(30, mean = 50, sd = 15),
  income = rnorm(30, mean = 60000, sd = 20000)
)

cat("Matching with very strict max_distance:\n")
result_strict <- match_couples(
  left_strict, right_strict,
  vars = c("age", "income"),
  scale = "standardize",
  max_distance = 0.1  # Very strict!
)

cat("\nOnly ", nrow(result_strict$pairs), " matches! Couplr warned that constraints are too strict.\n")

cat("\nLet's relax the constraint:\n\n")
result_relaxed <- match_couples(
  left_strict, right_strict,
  vars = c("age", "income"),
  scale = "standardize",
  max_distance = 2.0,  # More reasonable
  check_costs = FALSE
)

cat("Much better! ", nrow(result_relaxed$pairs), " matches with relaxed constraints.\n")


# ==============================================================================
# Example 5: Constant Variables
# ==============================================================================

cat("\n\n=== Example 5: Constant Variable Detection ===\n\n")

# Create data with constant variable
set.seed(999)
left_constant <- data.frame(
  id = 1:25,
  age = rnorm(25, mean = 40, sd = 8),
  gender = "Male",  # Constant!
  score = rnorm(25, mean = 75, sd = 10)
)

right_constant <- data.frame(
  id = 26:50,
  age = rnorm(25, mean = 40, sd = 8),
  gender = "Male",  # Also constant!
  score = rnorm(25, mean = 75, sd = 10)
)

cat("Matching with constant variable (using auto_scale):\n")
result_constant <- match_couples(
  left_constant, right_constant,
  vars = c("age", "gender", "score"),
  auto_scale = TRUE  # Will detect and exclude constant var
)

cat("\nAuto-scaling detected and excluded the constant variable!\n")
cat("Matched on: ", paste(result_constant$info$vars_used, collapse = ", "), "\n")


# ==============================================================================
# Example 6: No Valid Pairs Error
# ==============================================================================

cat("\n\n=== Example 6: Impossible Matching Scenario ===\n\n")

# Create completely non-overlapping data with strict constraints
left_no_overlap <- data.frame(
  id = 1:20,
  value = 1:20  # Values 1-20
)

right_no_overlap <- data.frame(
  id = 21:40,
  value = 100:119  # Values 100-119 (no overlap!)
)

cat("Trying to match with no overlap and strict caliper:\n")
try({
  match_couples(
    left_no_overlap, right_no_overlap,
    vars = "value",
    calipers = list(value = 5)  # Only allow differences of 5
  )
}, silent = FALSE)

cat("\nCouplr explains why no matches are possible with fun messaging!\n")


# ==============================================================================
# Example 7: Diagnose Distance Matrix
# ==============================================================================

cat("\n\n=== Example 7: Distance Matrix Diagnostics ===\n\n")

# Create various problematic scenarios
set.seed(2025)
diag_left <- data.frame(
  id = 1:50,
  var1 = rnorm(50, mean = 100, sd = 5),
  var2 = c(rep(42, 48), 1000, 2000),  # Mostly constant with outliers
  var3 = rnorm(50, mean = 50, sd = 15)
)

diag_right <- data.frame(
  id = 51:100,
  var1 = rnorm(50, mean = 100, sd = 5),
  var2 = rep(42, 50),  # Completely constant
  var3 = rnorm(50, mean = 50, sd = 15)
)

# Compute distances and diagnose
cat("Computing distances for diagnosis:\n")
dist_obj <- compute_distances(
  diag_left, diag_right,
  vars = c("var1", "var2", "var3"),
  scale = FALSE
)

cat("\nDiagnosing distance matrix:\n")
diagnosis <- diagnose_distance_matrix(
  dist_obj$cost_matrix,
  diag_left, diag_right,
  vars = c("var1", "var2", "var3"),
  warn = TRUE
)

cat("\nDiagnosis summary:\n")
cat("  Quality:", diagnosis$quality, "\n")
cat("  Problem variables:", paste(diagnosis$problem_variables, collapse = ", "), "\n")
cat("  Suggestions:\n")
for (sug in diagnosis$suggestions) {
  cat("    - ", sug, "\n")
}


# ==============================================================================
# Example 8: Disabling Cost Checks
# ==============================================================================

cat("\n\n=== Example 8: Disabling Cost Checks ===\n\n")

cat("Sometimes you want to skip the checks (e.g., in production):\n\n")

# Use check_costs = FALSE to skip all warnings
result_silent <- match_couples(
  left_skewed, right_skewed,  # The data with outliers from Example 3
  vars = c("income", "age"),
  scale = FALSE,
  check_costs = FALSE  # No warnings!
)

cat("Matched without any cost warnings (check_costs = FALSE)\n")
cat("Pairs:", nrow(result_silent$pairs), "\n")


# ==============================================================================
# Example 9: Emoji Control
# ==============================================================================

cat("\n\n=== Example 9: Controlling Emoji Display ===\n\n")

cat("By default, emoji are shown in interactive sessions.\n")
cat("Current emoji setting:", getOption("couplr.emoji", TRUE), "\n\n")

cat("You can disable emoji if needed:\n")
options(couplr.emoji = FALSE)

try(match_couples(left, vars = "x"), silent = FALSE)

cat("\nRe-enable emoji:\n")
options(couplr.emoji = TRUE)

try(match_couples(left, vars = "x"), silent = FALSE)


# ==============================================================================
# Example 10: Balance Diagnostics with Fun Messages
# ==============================================================================

cat("\n\n=== Example 10: Balance Quality Messages ===\n\n")

# Create well-balanced match
set.seed(111)
treated <- data.frame(
  id = 1:60,
  age = rnorm(60, mean = 50, sd = 10),
  income = rnorm(60, mean = 55000, sd = 12000)
)

controls <- data.frame(
  id = 61:180,
  age = rnorm(120, mean = 50.5, sd = 10),  # Very similar!
  income = rnorm(120, mean = 55500, sd = 12000)
)

cat("Matching with good overlap:\n")
result_balanced <- match_couples(
  treated, controls,
  vars = c("age", "income"),
  scale = "standardize",
  max_distance = 0.5,
  check_costs = FALSE
)

cat("\nChecking balance:\n")
balance <- balance_diagnostics(
  result_balanced,
  treated, controls,
  vars = c("age", "income")
)

# Check if we get a success message for good balance
if (balance$overall$mean_abs_std_diff < 0.1) {
  cat(couplr:::couplr_emoji("success"),
      "Excellent balance! These couples are well-matched!\n")
}

cat("\nMean absolute std diff:",
    round(balance$overall$mean_abs_std_diff, 3), "\n")


# ==============================================================================
# Summary
# ==============================================================================

cat("\n\n=== Summary: Couplr's Fun Error Philosophy ===\n\n")

cat(couplr:::couplr_emoji("heart"), "Why fun error messages?\n")
cat("  1. Less intimidating for new users\n")
cat("  2. More memorable (couple-themed puns!)\n")
cat("  3. Clearer guidance on what went wrong\n")
cat("  4. Actionable suggestions to fix problems\n\n")

cat(couplr:::couplr_emoji("sparkles"), "Key features:\n")
cat("  - Automatic cost distribution checking\n")
cat("  - Detect duplicates, outliers, and constant variables\n")
cat("  - Warn about overly strict constraints\n")
cat("  - Suggest fixes with concrete examples\n")
cat("  - Optional emoji for fun (disable with options(couplr.emoji = FALSE))\n\n")

cat(couplr:::couplr_emoji("check"), "Best practices:\n")
cat("  - Leave check_costs = TRUE (default) during development\n")
cat("  - Set check_costs = FALSE in production if needed\n")
cat("  - Use auto_scale = TRUE to automatically handle problem variables\n")
cat("  - Read the warnings - they contain helpful suggestions!\n\n")

cat(couplr:::couplr_emoji("heart"), "Happy matching! May all your couples be well-paired!\n")

cat("\n=== Demo Complete ===\n")
