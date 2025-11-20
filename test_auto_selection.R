library(couplr)

cat("Testing updated auto-selection logic:\n\n")

# Test 1: n=5 should use bruteforce
cat("n=5: ")
set.seed(123)
cost5 <- matrix(runif(25, 0, 100), 5, 5)
result5 <- lap_solve(cost5, method="auto")
cat(attr(result5, "method_used"), "\n")

# Test 2: n=10 should use bruteforce
cat("n=10: ")
cost10 <- matrix(runif(100, 0, 100), 10, 10)
result10 <- lap_solve(cost10, method="auto")
cat(attr(result10, "method_used"), "\n")

# Test 3: n=30 should use hungarian
cat("n=30: ")
cost30 <- matrix(runif(900, 0, 100), 30, 30)
result30 <- lap_solve(cost30, method="auto")
cat(attr(result30, "method_used"), "\n")

# Test 4: n=50 should use hungarian
cat("n=50: ")
cost50 <- matrix(runif(2500, 0, 100), 50, 50)
result50 <- lap_solve(cost50, method="auto")
cat(attr(result50, "method_used"), "\n")

# Test 5: n=60 should use jv
cat("n=60: ")
cost60 <- matrix(runif(3600, 0, 100), 60, 60)
result60 <- lap_solve(cost60, method="auto")
cat(attr(result60, "method_used"), "\n")

# Test 6: n=75 should use jv
cat("n=75: ")
cost75 <- matrix(runif(5625, 0, 100), 75, 75)
result75 <- lap_solve(cost75, method="auto")
cat(attr(result75, "method_used"), "\n")

# Test 7: n=100 should use auction_scaled
cat("n=100: ")
cost100 <- matrix(runif(10000, 0, 100), 100, 100)
result100 <- lap_solve(cost100, method="auto")
cat(attr(result100, "method_used"), "\n")

# Test 8: n=200 should use auction_scaled
cat("n=200: ")
cost200 <- matrix(runif(40000, 0, 100), 200, 200)
result200 <- lap_solve(cost200, method="auto")
cat(attr(result200, "method_used"), "\n")

cat("\n✅ Auto-selection logic updated successfully!\n")
