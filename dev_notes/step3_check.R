## Step 3: LAPJV warm-start in jv_core.
## solve_jv and solve_jv_duals now opt-in to use_warm_start = true.
## solve_hungarian (also using jv_core) stays opt-out → "classic SAP".
##
## Expected: 1.5-3x speedup at n>=500 on JV/duals, same correctness.

Rcpp::compileAttributes(".")
devtools::load_all(".", quiet = TRUE)

cat("\n== Correctness tests ==\n")
testthat::test_file("tests/testthat/test-assignment-jv.R",        reporter = "summary")
testthat::test_file("tests/testthat/test-assignment-duals.R",     reporter = "summary")
testthat::test_file("tests/testthat/test-assignment-hungarian.R", reporter = "summary")
testthat::test_file("tests/testthat/test-assignment-munkres.R",   reporter = "summary")
testthat::test_file("tests/testthat/test-assignment-auto.R",      reporter = "summary")

cat("\n== Speed comparison (microbenchmark-style) ==\n")
do_bench <- function(M, methods, reps) {
  res <- numeric(length(methods))
  for (k in seq_along(methods)) {
    t <- system.time(for (i in seq_len(reps)) assignment(M, method = methods[k]))[3]
    res[k] <- (t / reps) * 1000
  }
  names(res) <- methods
  res
}

set.seed(11)
for (nn in c(100, 200, 500, 1000)) {
  M <- matrix(runif(nn*nn), nn, nn)
  reps <- max(2L, as.integer(50000L / nn))
  r <- do_bench(M, c("jv", "hungarian", "auto"), reps)
  cat(sprintf("n=%4d (reps=%d):\n", nn, reps))
  for (k in seq_along(r)) cat(sprintf("  %-10s %7.2f ms\n", names(r)[k], r[[k]]))

  ## also confirm totals match
  totals <- sapply(c("jv","hungarian"), function(m) assignment(M, method=m)$total_cost)
  if (abs(totals[1] - totals[2]) > 1e-9) {
    stop(sprintf("MISMATCH at n=%d: JV=%.10f HUN=%.10f", nn, totals[1], totals[2]))
  }
}

cat("\nAll totals match. JV should be cleanly faster than Hungarian at n>=500.\n")
