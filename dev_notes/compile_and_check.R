## Verify couplr compiles cleanly and load_all() works before launching the
## paper benchmarks. Done in a one-shot script to keep stderr/stdout in the
## log rather than scattered across interactive sessions.

cat("Step 1: Rcpp::compileAttributes()\n")
Rcpp::compileAttributes(".")

cat("\nStep 2: devtools::load_all() (this builds C++ if needed)\n")
devtools::load_all(".", quiet = TRUE)

cat("\nStep 3: sanity assignment() call\n")
set.seed(1)
M <- matrix(runif(50 * 50), 50, 50)
r <- assignment(M, method = "jv")
cat(sprintf("  jv total_cost = %.6f, length(row_idx) = %d\n",
            r$total_cost, length(r$row_idx)))
cat("  load_all OK\n")
