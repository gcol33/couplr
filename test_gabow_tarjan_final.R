library(couplr)

cat("Testing Gabow-Tarjan with incremental updates...\n\n")

# Test 1: Simple matrix
cat("Test 1: Simple matrix\n")
cost <- matrix(c(4,1,3,2,0,5,3,2,2), 3, 3, byrow=TRUE)
result <- lap_solve(cost, method='gabow_tarjan')
cat("  Result: ", nrow(result), " rows matched, total_cost =", attr(result, 'total_cost'), "\n")
stopifnot(nrow(result) == 3)
stopifnot(abs(attr(result, 'total_cost') - 5) < 1e-6)

# Test 2: Random matrix n=100
cat("\nTest 2: Random matrix n=100\n")
set.seed(123)
cost <- matrix(runif(100*100, 0, 100), 100, 100)
result <- lap_solve(cost, method='gabow_tarjan')
cat("  Result: ", nrow(result), " rows matched, total_cost =", attr(result, 'total_cost'), "\n")
stopifnot(nrow(result) == 100)

# Test 3: Compare with Hungarian
cat("\nTest 3: Compare with Hungarian\n")
r1 <- lap_solve(cost, method='hungarian')
r2 <- lap_solve(cost, method='gabow_tarjan')
cost_diff <- abs(attr(r1, 'total_cost') - attr(r2, 'total_cost'))
cat("  Hungarian cost:", attr(r1, 'total_cost'), "\n")
cat("  Gabow-Tarjan cost:", attr(r2, 'total_cost'), "\n")
cat("  Difference:", cost_diff, "\n")
stopifnot(cost_diff < 1e-6)

# Test 4: Large matrix n=500
cat("\nTest 4: Large matrix n=500\n")
set.seed(456)
cost <- matrix(runif(500*500, 0, 100), 500, 500)
start_time <- Sys.time()
result <- lap_solve(cost, method='gabow_tarjan')
elapsed <- as.numeric(Sys.time() - start_time, units="secs")
cat("  Result: ", nrow(result), " rows matched in", round(elapsed, 3), "seconds\n")
stopifnot(nrow(result) == 500)

cat("\n✅ ALL TESTS PASSED!\n")
cat("Gabow-Tarjan with incremental updates is working correctly.\n")
