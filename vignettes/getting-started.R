## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4
)
library(lapr)

## ----eval=FALSE---------------------------------------------------------------
# # Install from CRAN
# install.packages("lapr")
# 
# # Or install development version from GitHub
# # install.packages("remotes")
# remotes::install_github("gcol33/lapr")

## ----basic-matrix-------------------------------------------------------------
# Cost matrix: 3 workers × 3 tasks
cost <- matrix(c(
  4, 2, 5,  # Worker 1 costs for tasks 1, 2, 3
  3, 3, 6,  # Worker 2 costs
  7, 5, 4   # Worker 3 costs
), nrow = 3, byrow = TRUE)

# Solve the assignment problem
result <- lap_solve(cost)
print(result)

## ----df-input-----------------------------------------------------------------
library(dplyr)

# Create assignment problem as data frame
assignments <- tibble(
  worker = rep(1:3, each = 3),
  task = rep(1:3, times = 3),
  cost = c(4, 2, 5, 3, 3, 6, 7, 5, 4)
)

# Solve using column names
result <- lap_solve(assignments, worker, task, cost)
print(result)

## ----rectangular--------------------------------------------------------------
# 3 workers, 5 tasks - assign each worker to one task
cost_rect <- matrix(c(
  1, 2, 3, 4, 5,
  6, 5, 4, 3, 2,
  2, 3, 4, 5, 6
), nrow = 3, byrow = TRUE)

result <- lap_solve(cost_rect)
print(result)

## ----forbidden----------------------------------------------------------------
# Worker 1 cannot do task 3 (e.g., lacks required skill)
cost[1, 3] <- NA

# Worker 2 cannot do task 1 (e.g., scheduling conflict)
cost[2, 1] <- Inf

result <- lap_solve(cost)
print(result)

## ----maximize-----------------------------------------------------------------
# Profit matrix - we want to maximize total profit
profit <- matrix(c(
  5, 3, 7,
  4, 6, 2,
  8, 4, 5
), nrow = 3, byrow = TRUE)

result <- lap_solve(profit, maximize = TRUE)
print(result)

cat("\nTotal profit:", get_total_cost(result), "\n")

## ----grouped------------------------------------------------------------------
# Multiple assignment problems in one data frame
simulations <- tibble(
  sim = rep(1:3, each = 9),
  worker = rep(rep(1:3, each = 3), times = 3),
  task = rep(rep(1:3, times = 3), times = 3),
  cost = runif(27, min = 1, max = 10)
)

# Solve all simulations at once
results <- simulations |>
  group_by(sim) |>
  lap_solve(worker, task, cost)

print(results)

## ----batch--------------------------------------------------------------------
# Create list of cost matrices
set.seed(123)
cost_list <- lapply(1:100, function(i) matrix(runif(9, 1, 10), 3, 3))

# Solve all problems at once
batch_results <- lap_solve_batch(cost_list)

# View summary statistics
batch_results |>
  distinct(problem_id, total_cost) |>
  summarise(
    n_problems = n(),
    mean_cost = mean(total_cost),
    min_cost = min(total_cost),
    max_cost = max(total_cost)
  )

## ----parallel, eval=FALSE-----------------------------------------------------
# # Solve with 4 threads
# batch_results <- lap_solve_batch(cost_list, n_threads = 4)
# 
# # Use all available cores
# batch_results <- lap_solve_batch(cost_list, n_threads = NULL)

## ----kbest--------------------------------------------------------------------
cost <- matrix(c(
  1, 2, 3,
  4, 3, 2,
  5, 4, 1
), nrow = 3, byrow = TRUE)

# Find top 5 solutions
kbest <- lap_solve_kbest(cost, k = 5)
print(kbest)

# Get summary of solutions
summary(kbest)

## ----utilities----------------------------------------------------------------
result <- lap_solve(cost)

# Extract total cost
get_total_cost(result)

# Get algorithm used
get_method_used(result)

# Convert to binary assignment matrix
as_assignment_matrix(result)

# Check result type
is_lap_solve_result(result)

## ----methods------------------------------------------------------------------
# Let lapr choose (default)
lap_solve(cost, method = "auto")

# Force specific algorithm:
lap_solve(cost, method = "jv")         # Jonker-Volgenant (general purpose)
lap_solve(cost, method = "hungarian")  # Hungarian algorithm
lap_solve(cost, method = "auction")    # Auction algorithm
lap_solve(cost, method = "sap")        # Sparse assignment (for sparse problems)

## ----auction-variants, eval=FALSE---------------------------------------------
# # Standard fixed-epsilon auction (default)
# lap_solve(cost, method = "auction")
# 
# # Scaled-epsilon auction with epsilon-scaling phases
# # Better for problems with large cost ranges
# lap_solve(cost, method = "auction_scaled")
# 
# # Gauss-Seidel auction with sequential bidding
# # Can be faster for certain problem structures
# lap_solve(cost, method = "auction_gs")

## ----hk01-example-------------------------------------------------------------
# Create binary cost matrix
binary_cost <- matrix(sample(0:1, 9, replace = TRUE), 3, 3)

# Specialized algorithm for binary costs
lap_solve(binary_cost, method = "hk01")

## ----parallel-example, eval=FALSE---------------------------------------------
# # Parallel batch solving (supported)
# lap_solve_batch(cost_list, n_threads = 4)
# 
# # Single problem (currently sequential)
# lap_solve(cost)  # No parallelization yet

## ----real-world---------------------------------------------------------------
set.seed(42)

# Generate employee preferences for shifts
schedule_data <- tibble(
  day = rep(c("Mon", "Tue", "Wed", "Thu", "Fri"), each = 12),
  employee = rep(1:4, times = 15),
  shift = rep(c("morning", "afternoon", "night"), each = 4, times = 5),
  preference_score = rnorm(60, mean = 5, sd = 2)
) |>
  mutate(
    # Some employees can't work certain shifts
    preference_score = case_when(
      employee == 1 & shift == "night" ~ NA_real_,
      employee == 2 & shift == "morning" ~ NA_real_,
      TRUE ~ preference_score
    ),
    # Create shift IDs within each day
    shift_id = as.integer(factor(shift, levels = c("morning", "afternoon", "night")))
  )

# Solve for optimal assignments per day (maximize preferences)
optimal_schedule <- schedule_data |>
  group_by(day) |>
  lap_solve(employee, shift_id, preference_score, maximize = TRUE)

# View first few assignments
head(optimal_schedule, 10)

# Summary statistics by day
optimal_schedule |>
  group_by(day) |>
  summarise(
    shifts_filled = n(),
    avg_preference = mean(cost, na.rm = TRUE),
    .groups = "drop"
  )

