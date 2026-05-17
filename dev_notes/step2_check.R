## Step 2 verification: split Hungarian (old O(n^4) → method="munkres") +
## new O(n^3) SAP-Hungarian → method="hungarian".

cat("Compiling attributes...\n")
Rcpp::compileAttributes(".")

cat("Installing package...\n")
devtools::load_all(".", quiet = TRUE)

cat("Running JV / Hungarian / Munkres / duals tests...\n")
testthat::test_file("tests/testthat/test-assignment-jv.R",        reporter = "summary")
testthat::test_file("tests/testthat/test-assignment-hungarian.R", reporter = "summary")
testthat::test_file("tests/testthat/test-assignment-munkres.R",   reporter = "summary")
testthat::test_file("tests/testthat/test-assignment-duals.R",     reporter = "summary")

cat("\nQuick sanity + speed comparison (n=100 squares):\n")
set.seed(42)
M <- matrix(runif(100*100), 100, 100)
r_jv  <- assignment(M, method = "jv")
r_h   <- assignment(M, method = "hungarian")
r_m   <- assignment(M, method = "munkres")
cat(sprintf("  JV        total: %.10f\n", r_jv$total_cost))
cat(sprintf("  Hungarian total: %.10f\n", r_h$total_cost))
cat(sprintf("  Munkres   total: %.10f\n", r_m$total_cost))

cat("\nWall-clock comparison at n=200 (was: Munkres=141ms, JV=6.4ms):\n")
set.seed(1)
M <- matrix(runif(200*200), 200, 200)
t_jv <- system.time(for (i in 1:5) assignment(M, method = "jv"))[3] / 5
t_h  <- system.time(for (i in 1:5) assignment(M, method = "hungarian"))[3] / 5
t_m  <- system.time(for (i in 1:3) assignment(M, method = "munkres"))[3] / 3
cat(sprintf("  JV        avg: %7.1f ms\n", t_jv * 1000))
cat(sprintf("  Hungarian avg: %7.1f ms\n", t_h  * 1000))
cat(sprintf("  Munkres   avg: %7.1f ms\n", t_m  * 1000))
