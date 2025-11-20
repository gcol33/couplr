## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(lapr)

## ----hungarian-example--------------------------------------------------------
# Example with Hungarian algorithm
cost <- matrix(c(
  10, 19, 8,
  15, 10, 11,
  9, 12, 14
), nrow = 3, byrow = TRUE)

result <- lap_solve(cost, method = "hungarian")
print(result)

## ----jv-example---------------------------------------------------------------
# Jonker-Volgenant on a larger problem
set.seed(123)
n <- 100
large_cost <- matrix(runif(n * n, 0, 100), n, n)

system.time({
  result <- lap_solve(large_cost, method = "jv")
})

cat("Total cost:", get_total_cost(result), "\n")

## ----auction-standard, eval=FALSE---------------------------------------------
# lap_solve(cost, method = "auction")

## ----auction-scaled, eval=FALSE-----------------------------------------------
# lap_solve(cost, method = "auction_scaled")

## ----auction-scaled-example---------------------------------------------------
# Example with large cost range
set.seed(123)
n <- 50
cost <- matrix(runif(n * n, 1, 1e6), n, n)  # Large range: 1 to 1,000,000

system.time({
  result_scaled <- lap_solve(cost, method = "auction_scaled")
})

cat("Total cost:", get_total_cost(result_scaled), "\n")

## ----auction-gs, eval=FALSE---------------------------------------------------
# lap_solve(cost, method = "auction_gs")

## ----auction-gs-example-------------------------------------------------------
# Example with spatial structure
set.seed(456)
n <- 100
# Create cost matrix with spatial correlation
x_source <- 1:n
y_source <- 1:n
cost <- outer(x_source, y_source, function(i, j) abs(i - j) + runif(n*n, 0, 5))

system.time({
  result_gs <- lap_solve(cost, method = "auction_gs")
})

cat("Total cost:", get_total_cost(result_gs), "\n")

## ----auction-comparison-------------------------------------------------------
# Comparison on a medium-sized problem
set.seed(789)
n <- 200
cost <- matrix(runif(n * n, 0, 100), n, n)

times <- c(
  standard = system.time(lap_solve(cost, method = "auction"))["elapsed"],
  scaled = system.time(lap_solve(cost, method = "auction_scaled"))["elapsed"],
  gauss_seidel = system.time(lap_solve(cost, method = "auction_gs"))["elapsed"]
)

print(times)

## ----sap-example--------------------------------------------------------------
# Sparse assignment problem
set.seed(789)
n <- 200
# Create sparse cost matrix (70% forbidden)
cost <- matrix(Inf, n, n)
n_edges <- floor(0.3 * n^2)
edges <- sample(1:(n^2), n_edges)
cost[edges] <- runif(n_edges, 0, 100)

system.time({
  result <- lap_solve(cost, method = "sap")
})

cat("Assignments found:", nrow(result), "\n")
cat("Total cost:", get_total_cost(result), "\n")

## ----hk01-example-------------------------------------------------------------
# Binary cost problem
set.seed(101)
n <- 300
binary_cost <- matrix(sample(0:1, n^2, replace = TRUE, prob = c(0.3, 0.7)), n, n)

system.time({
  result <- lap_solve(binary_cost, method = "hk01")
})

cat("Total cost:", get_total_cost(result), "\n")

## ----murty-example------------------------------------------------------------
# Find k-best solutions
cost <- matrix(c(
  10, 19, 8, 15,
  10, 18, 7, 17,
  13, 16, 9, 14,
  12, 19, 8, 18
), nrow = 4, byrow = TRUE)

kbest <- lap_solve_kbest(cost, k = 10)

# Show cost progression
summary(kbest) |> 
  print(n = 10)

## ----eval=FALSE---------------------------------------------------------------
# auto_select <- function(cost_matrix) {
#   n <- nrow(cost_matrix)
#   m <- ncol(cost_matrix)
# 
#   # Check for binary costs
#   if (all(cost_matrix %in% c(0, 1, NA, Inf))) {
#     return("hk01")  # Binary costs → HK01
#   }
# 
#   # Check sparsity
#   finite_ratio <- sum(is.finite(cost_matrix)) / (n * m)
#   if (finite_ratio < 0.3 || abs(n - m) > 0.5 * max(n, m)) {
#     return("sap")  # Sparse or very rectangular → SAP
#   }
# 
#   # Large dense problems
#   if (n > 1000) {
#     return("auction")  # Large → Auction
#   }
# 
#   # Default: Jonker-Volgenant
#   return("jv")
# }

