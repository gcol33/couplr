## Step 1 smoke-test: compile + run JV/Hungarian/duals tests.
## After extracting jv_core, solve_jv and solve_jv_duals should produce
## identical results to the pre-refactor versions.

cat("Compiling attributes...\n")
Rcpp::compileAttributes(".")

cat("Installing package...\n")
devtools::load_all(".", quiet = TRUE)

cat("Running JV tests...\n")
testthat::test_file("tests/testthat/test-assignment-jv.R", reporter = "summary")

cat("Running Hungarian tests (still uses old Munkres code)...\n")
testthat::test_file("tests/testthat/test-assignment-hungarian.R", reporter = "summary")

cat("Running duals tests...\n")
testthat::test_file("tests/testthat/test-assignment-duals.R", reporter = "summary")

cat("\nQuick sanity check on a fixed cost matrix:\n")
set.seed(42)
M <- matrix(runif(20*20), 20, 20)
r_jv <- assignment(M, method = "jv")
r_h  <- assignment(M, method = "hungarian")
r_d  <- assignment_duals(M)
cat(sprintf("  JV total:        %.10f\n", r_jv$total_cost))
cat(sprintf("  Hungarian total: %.10f\n", r_h$total_cost))
cat(sprintf("  Duals total:     %.10f\n", r_d$total_cost))
cat(sprintf("  sum(u) + sum(v) = %.10f (should equal duals total)\n",
            sum(r_d$u) + sum(r_d$v)))
