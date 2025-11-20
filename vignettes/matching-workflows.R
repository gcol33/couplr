## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)
library(couplr)
library(dplyr)
library(ggplot2)

## ----basic-example------------------------------------------------------------
# Simulate observational data
set.seed(123)
n_left <- 100
n_right <- 150

# Treatment group (tends to be younger, higher income)
left_data <- tibble(
  id = 1:n_left,
  age = rnorm(n_left, mean = 45, sd = 10),
  income = rnorm(n_left, mean = 60000, sd = 15000),
  education = sample(c("HS", "BA", "MA"), n_left, replace = TRUE),
  group = "treatment"
)

# Control group (older, lower income on average)
right_data <- tibble(
  id = 1:n_right,
  age = rnorm(n_right, mean = 52, sd = 12),
  income = rnorm(n_right, mean = 50000, sd = 18000),
  education = sample(c("HS", "BA", "MA"), n_right, replace = TRUE),
  group = "control"
)

# Perform optimal matching
result <- match_couples(
  left = left_data,
  right = right_data,
  vars = c("age", "income"),
  auto_scale = TRUE,
  return_diagnostics = TRUE
)

# View matched pairs
head(result$pairs)

# Summary statistics
result$info

## ----inspect-output-----------------------------------------------------------
# How many matched?
cat("Matched pairs:", result$info$n_matched, "\n")
cat("Unmatched left:", nrow(result$left_unmatched), "\n")
cat("Unmatched right:", nrow(result$right_unmatched), "\n")

# Distribution of match distances
summary(result$pairs$distance)

# Visualize match quality
ggplot(result$pairs, aes(x = distance)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Distribution of Match Distances",
    x = "Euclidean Distance (scaled)",
    y = "Count"
  ) +
  theme_minimal()

## ----preprocessing-demo-------------------------------------------------------
# Create data with scaling challenges
set.seed(456)
challenging_data <- tibble(
  id = 1:50,
  age = rnorm(50, 50, 10),                    # Years (reasonable scale)
  income = rnorm(50, 60000, 20000),           # Dollars (large scale)
  bmi = rnorm(50, 25, 5),                     # Ratio (small scale)
  smoker = sample(0:1, 50, replace = TRUE),   # Binary
  education = factor(
    sample(c("HS", "BA", "MA", "PhD"), 50, replace = TRUE),
    ordered = TRUE,
    levels = c("HS", "BA", "MA", "PhD")
  )
)

left_chal <- challenging_data[1:25, ]
right_chal <- challenging_data[26:50, ]

# Match WITHOUT auto-scaling (income dominates)
result_no_scale <- match_couples(
  left_chal, right_chal,
  vars = c("age", "income", "bmi"),
  auto_scale = FALSE
)

# Match WITH auto-scaling (all variables contribute)
result_scaled <- match_couples(
  left_chal, right_chal,
  vars = c("age", "income", "bmi"),
  auto_scale = TRUE,
  scale = "robust"  # Median/MAD scaling (robust to outliers)
)

# Compare match quality
cat("Without scaling - mean distance:", mean(result_no_scale$pairs$distance), "\n")
cat("With scaling - mean distance:", mean(result_scaled$pairs$distance), "\n")

## ----scaling-comparison-------------------------------------------------------
# Demonstrate scaling methods
demo_var <- c(10, 20, 25, 30, 100)  # Contains outlier (100)

# Function to show scaling
show_scaling <- function(x, method) {
  left_demo <- tibble(id = 1:3, var = x[1:3])
  right_demo <- tibble(id = 1:2, var = x[4:5])

  result <- match_couples(
    left_demo, right_demo,
    vars = "var",
    auto_scale = TRUE,
    scale = method,
    return_diagnostics = TRUE
  )

  # Extract scaled values from cost matrix
  cat(method, "scaling:\n")
  cat("  Original:", x, "\n")
  cat("  Distance matrix diagonal:", diag(result$cost_matrix)[1:2], "\n\n")
}

show_scaling(demo_var, "robust")
show_scaling(demo_var, "standardize")
show_scaling(demo_var, "range")

## ----health-checks, eval=FALSE------------------------------------------------
# # Create data with issues
# problematic_data <- tibble(
#   id = 1:100,
#   age = rnorm(100, 50, 10),
#   constant_var = 5,                           # No variation - will warn
#   mostly_missing = c(rnorm(20, 50, 10), rep(NA, 80)),  # >50% missing
#   extreme_skew = rexp(100, rate = 0.1)       # Very skewed
# )
# 
# # Attempt matching - will show warnings
# result <- match_couples(
#   problematic_data[1:50, ],
#   problematic_data[51:100, ],
#   vars = c("age", "constant_var", "mostly_missing", "extreme_skew"),
#   auto_scale = TRUE
# )
# 
# # Warning messages will indicate:
# # - "constant_var excluded (SD = 0)"
# # - "mostly_missing has 80% missing values"
# # - "extreme_skew has high skewness (3.2)"

## ----optimal-vs-greedy--------------------------------------------------------
# Create moderately large dataset
set.seed(789)
n <- 1000
large_left <- tibble(
  id = 1:n,
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = rnorm(n)
)
large_right <- tibble(
  id = 1:n,
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = rnorm(n)
)

# Optimal matching
time_optimal <- system.time({
  result_optimal <- match_couples(
    large_left, large_right,
    vars = c("x1", "x2", "x3"),
    method = "hungarian"
  )
})

# Greedy matching (row_best strategy)
time_greedy <- system.time({
  result_greedy <- greedy_couples(
    large_left, large_right,
    vars = c("x1", "x2", "x3"),
    strategy = "row_best"
  )
})

# Compare
cat("Optimal matching:\n")
cat("  Time:", round(time_optimal["elapsed"], 3), "seconds\n")
cat("  Mean distance:", round(mean(result_optimal$pairs$distance), 4), "\n\n")

cat("Greedy matching:\n")
cat("  Time:", round(time_greedy["elapsed"], 3), "seconds\n")
cat("  Mean distance:", round(mean(result_greedy$pairs$distance), 4), "\n")
cat("  Speedup:", round(time_optimal["elapsed"] / time_greedy["elapsed"], 1), "x\n")

## ----greedy-strategies--------------------------------------------------------
# Compare greedy strategies on same data
set.seed(101)
test_left <- tibble(id = 1:200, x = rnorm(200))
test_right <- tibble(id = 1:200, x = rnorm(200))

strategies <- c("sorted", "row_best", "pq")
results <- list()

for (strat in strategies) {
  time <- system.time({
    result <- greedy_couples(
      test_left, test_right,
      vars = "x",
      strategy = strat
    )
  })

  results[[strat]] <- list(
    time = time["elapsed"],
    mean_dist = mean(result$pairs$distance),
    total_dist = result$info$total_distance
  )
}

# Display comparison
comparison <- do.call(rbind, lapply(names(results), function(s) {
  data.frame(
    strategy = s,
    time_sec = round(results[[s]]$time, 4),
    mean_distance = round(results[[s]]$mean_dist, 4),
    total_distance = round(results[[s]]$total_dist, 2)
  )
}))

print(comparison)

## ----calipers-----------------------------------------------------------------
# Create data where some units are far apart
set.seed(202)
left_cal <- tibble(
  id = 1:50,
  x = c(rnorm(40, mean = 0, sd = 1), rnorm(10, mean = 5, sd = 0.5))  # Some outliers
)
right_cal <- tibble(
  id = 1:50,
  x = rnorm(50, mean = 0, sd = 1)
)

# Match without caliper - pairs everything
result_no_cal <- match_couples(
  left_cal, right_cal,
  vars = "x",
  auto_scale = FALSE
)

# Match with caliper - excludes poor matches
result_with_cal <- match_couples(
  left_cal, right_cal,
  vars = "x",
  max_distance = 1.5,  # Caliper: max distance = 1.5
  auto_scale = FALSE
)

cat("Without caliper:\n")
cat("  Matched:", result_no_cal$info$n_matched, "\n")
cat("  Mean distance:", round(mean(result_no_cal$pairs$distance), 3), "\n")
cat("  Max distance:", round(max(result_no_cal$pairs$distance), 3), "\n\n")

cat("With caliper (1.5):\n")
cat("  Matched:", result_with_cal$info$n_matched, "\n")
cat("  Mean distance:", round(mean(result_with_cal$pairs$distance), 3), "\n")
cat("  Max distance:", round(max(result_with_cal$pairs$distance), 3), "\n")

# Visualize caliper effect
ggplot(result_no_cal$pairs, aes(x = distance)) +
  geom_histogram(aes(fill = "No caliper"), bins = 30, alpha = 0.5) +
  geom_histogram(
    data = result_with_cal$pairs,
    aes(fill = "With caliper"),
    bins = 30,
    alpha = 0.5
  ) +
  geom_vline(xintercept = 1.5, linetype = "dashed", color = "red") +
  labs(
    title = "Caliper Effect on Match Distances",
    x = "Distance",
    y = "Count",
    fill = "Condition"
  ) +
  theme_minimal()

## ----caliper-sd, eval=FALSE---------------------------------------------------
# # Calculate pooled SD
# combined <- bind_rows(
#   left_data %>% mutate(group = "left"),
#   right_data %>% mutate(group = "right")
# )
# 
# pooled_sd <- sd(combined$age)  # For single variable
# caliper_width <- 0.2 * pooled_sd
# 
# result <- match_couples(
#   left_data, right_data,
#   vars = "age",
#   max_distance = caliper_width
# )

## ----caliper-empirical--------------------------------------------------------
# Fit all matches first
all_matches <- match_couples(left_cal, right_cal, vars = "x")

# Choose caliper at 90th percentile
caliper_90 <- quantile(all_matches$pairs$distance, 0.90)

# Refit with caliper
refined_matches <- match_couples(
  left_cal, right_cal,
  vars = "x",
  max_distance = caliper_90
)

cat("90th percentile caliper:", round(caliper_90, 3), "\n")
cat("Matches retained:",
    round(100 * refined_matches$info$n_matched / all_matches$info$n_matched, 1), "%\n")

