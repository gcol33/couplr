## Step 5: CSA matching warm-start across epsilon-scaling phases.
## Expected: 1.3-2x speedup on CSA at n>=500, same correctness.

Rcpp::compileAttributes(".")
devtools::load_all(".", quiet = TRUE)

cat("\n== CSA correctness tests ==\n")
testthat::test_file("tests/testthat/test-solver-csa.R", reporter = "summary")
testthat::test_file("tests/testthat/test-assignment-csa.R", reporter = "summary")

cat("\n== CSA speed comparison ==\n")
set.seed(13)
for (nn in c(100, 200, 500, 1000)) {
  M <- matrix(runif(nn*nn), nn, nn)
  reps <- max(2L, as.integer(20000L / nn))
  t <- system.time(for (i in seq_len(reps)) assignment(M, method = "csa"))[3]
  ms <- (t / reps) * 1000
  cat(sprintf("  n=%-4d  CSA: %7.2f ms  (reps=%d)\n", nn, ms, reps))
}
