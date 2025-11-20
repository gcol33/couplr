# Parallel Matching Examples
# ===========================
#
# This script demonstrates parallel processing for blocked matching using the
# 'future' package. Parallel processing is particularly beneficial when matching
# within many blocks (e.g., matching by region, time period, or other grouping
# variables).
#
# Requirements: Install future and future.apply packages
#   install.packages(c("future", "future.apply"))

library(couplr)
library(dplyr)

# Check if parallel packages are available
if (!requireNamespace("future", quietly = TRUE) ||
    !requireNamespace("future.apply", quietly = TRUE)) {
  stop(
    "This demo requires 'future' and 'future.apply' packages.\n",
    "Install with: install.packages(c('future', 'future.apply'))"
  )
}

# ==============================================================================
# Example 1: Basic Parallel Matching with Blocks
# ==============================================================================

cat("\n=== Example 1: Basic Parallel Matching ===\n\n")

# Create data with multiple blocks (e.g., regions)
set.seed(123)
n_blocks <- 10
n_per_block <- 50

left <- do.call(rbind, lapply(1:n_blocks, function(b) {
  data.frame(
    id = paste0("L", b, "_", 1:n_per_block),
    region = paste0("Region_", b),
    age = rnorm(n_per_block, mean = 45, sd = 10),
    income = rnorm(n_per_block, mean = 50000, sd = 15000),
    bp_systolic = rnorm(n_per_block, mean = 130, sd = 15)
  )
}))

right <- do.call(rbind, lapply(1:n_blocks, function(b) {
  data.frame(
    id = paste0("R", b, "_", 1:(n_per_block * 2)),
    region = paste0("Region_", b),
    age = rnorm(n_per_block * 2, mean = 47, sd = 10),
    income = rnorm(n_per_block * 2, mean = 52000, sd = 15000),
    bp_systolic = rnorm(n_per_block * 2, mean = 132, sd = 15)
  )
}))

cat("Dataset:", nrow(left), "left units,", nrow(right), "right units\n")
cat("Blocks:", n_blocks, "regions\n\n")

# Sequential matching (default)
cat("Sequential matching:\n")
time_seq <- system.time({
  result_seq <- match_couples(
    left, right,
    vars = c("age", "income", "bp_systolic"),
    block_id = "region",
    scale = "standardize"
  )
})
cat("  Time:", round(time_seq["elapsed"], 2), "seconds\n")
cat("  Matched:", nrow(result_seq$pairs), "pairs\n\n")

# Parallel matching
cat("Parallel matching:\n")
time_par <- system.time({
  result_par <- match_couples(
    left, right,
    vars = c("age", "income", "bp_systolic"),
    block_id = "region",
    scale = "standardize",
    parallel = TRUE  # Enable parallel processing
  )
})
cat("  Time:", round(time_par["elapsed"], 2), "seconds\n")
cat("  Matched:", nrow(result_par$pairs), "pairs\n")
cat("  Speedup:", round(time_seq["elapsed"] / time_par["elapsed"], 2), "x\n\n")

# Results should be identical
cat("Results identical:", identical(
  result_seq$pairs %>% arrange(left_id, right_id),
  result_par$pairs %>% arrange(left_id, right_id)
), "\n")


# ==============================================================================
# Example 2: Custom Parallel Plan
# ==============================================================================

cat("\n=== Example 2: Custom Parallel Plan ===\n\n")

# You can also set up your own future plan for more control
library(future)

# Option 1: Explicitly set the plan before matching
plan(multisession, workers = 4)

result_custom <- match_couples(
  left, right,
  vars = c("age", "income", "bp_systolic"),
  block_id = "region",
  scale = "standardize",
  parallel = TRUE  # Will use the plan you set
)

cat("Matched with custom plan:", nrow(result_custom$pairs), "pairs\n")

# Reset to sequential
plan(sequential)


# ==============================================================================
# Example 3: Greedy Matching with Parallel Processing
# ==============================================================================

cat("\n=== Example 3: Greedy Matching in Parallel ===\n\n")

# Greedy matching also supports parallel processing
cat("Sequential greedy matching:\n")
time_greedy_seq <- system.time({
  result_greedy_seq <- greedy_couples(
    left, right,
    vars = c("age", "income", "bp_systolic"),
    block_id = "region",
    scale = "standardize",
    strategy = "sorted"
  )
})
cat("  Time:", round(time_greedy_seq["elapsed"], 2), "seconds\n")
cat("  Matched:", nrow(result_greedy_seq$pairs), "pairs\n\n")

cat("Parallel greedy matching:\n")
time_greedy_par <- system.time({
  result_greedy_par <- greedy_couples(
    left, right,
    vars = c("age", "income", "bp_systolic"),
    block_id = "region",
    scale = "standardize",
    strategy = "sorted",
    parallel = TRUE
  )
})
cat("  Time:", round(time_greedy_par["elapsed"], 2), "seconds\n")
cat("  Matched:", nrow(result_greedy_par$pairs), "pairs\n")
cat("  Speedup:", round(time_greedy_seq["elapsed"] / time_greedy_par["elapsed"], 2), "x\n")


# ==============================================================================
# Example 4: When Parallel Processing Helps Most
# ==============================================================================

cat("\n=== Example 4: When Parallel Processing Helps ===\n\n")

# Parallel processing is most beneficial when:
# 1. Many blocks (10+)
# 2. Moderate to large blocks (50+ units per block)
# 3. Complex distance calculations

# Small example (minimal benefit)
small_left <- data.frame(
  id = 1:10,
  block = rep(c("A", "B"), each = 5),
  x = rnorm(10)
)
small_right <- data.frame(
  id = 11:20,
  block = rep(c("A", "B"), each = 5),
  x = rnorm(10)
)

cat("Small dataset (2 blocks, 5 units each):\n")
time_small_seq <- system.time({
  match_couples(small_left, small_right, vars = "x", block_id = "block")
})
time_small_par <- system.time({
  match_couples(small_left, small_right, vars = "x", block_id = "block", parallel = TRUE)
})
cat("  Sequential:", round(time_small_seq["elapsed"], 3), "s\n")
cat("  Parallel:", round(time_small_par["elapsed"], 3), "s\n")
cat("  Note: Parallel overhead may outweigh benefits for small problems\n\n")

# Recommendation
cat("Recommendation:\n")
cat("  - Use parallel=TRUE for 10+ blocks with 50+ units per block\n")
cat("  - For smaller problems, sequential matching is often faster\n")
cat("  - The benefit scales with the number and size of blocks\n")


# ==============================================================================
# Example 5: Parallel Matching with Distance Caching
# ==============================================================================

cat("\n=== Example 5: Combining Parallel + Distance Caching ===\n\n")

# You can combine parallel processing with distance caching
# for maximum performance when exploring parameters

# Compute distances (this step is still sequential)
cat("Step 1: Computing distances (sequential)...\n")
dist_obj <- compute_distances(
  left, right,
  vars = c("age", "income", "bp_systolic"),
  scale = "standardize"
)

# Now matching with different parameters can use distance cache
# AND parallel processing if the distance object contains block info
cat("\nStep 2: Matching with different thresholds (using cached distances)...\n")

result1 <- match_couples(dist_obj, max_distance = 0.5)
result2 <- match_couples(dist_obj, max_distance = 1.0)
result3 <- match_couples(dist_obj, max_distance = 1.5)

cat("  Threshold 0.5:", nrow(result1$pairs), "pairs\n")
cat("  Threshold 1.0:", nrow(result2$pairs), "pairs\n")
cat("  Threshold 1.5:", nrow(result3$pairs), "pairs\n")


# ==============================================================================
# Example 6: Platform-Specific Plans
# ==============================================================================

cat("\n=== Example 6: Platform-Specific Parallel Plans ===\n\n")

# Different platforms support different parallelization strategies:

# Windows: Use multisession (separate R processes)
if (.Platform$OS.type == "windows") {
  cat("On Windows: Using multisession plan\n")
  plan(multisession, workers = 2)
}

# Unix/Mac: Can use multicore (forked processes, more efficient)
if (.Platform$OS.type == "unix") {
  cat("On Unix/Mac: Using multicore plan\n")
  plan(multicore, workers = 2)
}

# Cluster: For distributed computing
# plan(cluster, workers = c("node1", "node2", "node3"))

result_platform <- match_couples(
  left, right,
  vars = c("age", "income"),
  block_id = "region",
  parallel = TRUE
)

cat("Matched:", nrow(result_platform$pairs), "pairs\n")

# Always reset plan when done
plan(sequential)


# ==============================================================================
# Example 7: Monitoring Parallel Progress
# ==============================================================================

cat("\n=== Example 7: Monitoring Progress ===\n\n")

# The future package supports progress reporting
if (requireNamespace("progressr", quietly = TRUE)) {
  cat("Using progressr for progress reporting:\n")

  library(progressr)

  with_progress({
    result_progress <- match_couples(
      left, right,
      vars = c("age", "income", "bp_systolic"),
      block_id = "region",
      parallel = TRUE
    )
  })

  cat("Matched:", nrow(result_progress$pairs), "pairs\n")
} else {
  cat("Install 'progressr' package for progress reporting:\n")
  cat("  install.packages('progressr')\n")
}


# ==============================================================================
# Performance Tips
# ==============================================================================

cat("\n=== Performance Tips ===\n\n")
cat("1. Parallel processing works best with:\n")
cat("   - Many blocks (10+)\n")
cat("   - Moderate to large block sizes (50+ units)\n")
cat("   - Complex distance computations\n\n")

cat("2. Overhead considerations:\n")
cat("   - Setting up parallel workers takes time (~1-2 seconds)\n")
cat("   - Data transfer between processes adds overhead\n")
cat("   - For very small problems, sequential may be faster\n\n")

cat("3. Best practices:\n")
cat("   - Set plan once, reuse for multiple operations\n")
cat("   - Leave one core free (workers = availableCores() - 1)\n")
cat("   - Use multisession on Windows, multicore on Unix/Mac\n")
cat("   - Monitor memory usage with large datasets\n\n")

cat("4. Combining optimizations:\n")
cat("   - Use parallel=TRUE for blocked matching\n")
cat("   - Use compute_distances() to cache distances\n")
cat("   - Use greedy_couples() for very large problems\n")
cat("   - Combine all three for maximum performance!\n")

cat("\n=== Demo Complete ===\n")
