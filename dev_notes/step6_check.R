## Step 6: collapse solve_auction_scaled into a thin wrapper over scaled_params.
## ~200 lines deleted. Behavior should be identical.

Rcpp::compileAttributes(".")
devtools::load_all(".", quiet = TRUE)

cat("\n== Auction-Scaled correctness (all variants) ==\n")
testthat::test_file("tests/testthat/test-assignment-auction.R",         reporter = "summary")
testthat::test_file("tests/testthat/test-assignment-auction-gs.R",      reporter = "summary")
testthat::test_file("tests/testthat/test-auction-hang.R",               reporter = "summary")

cat("\n== Speed (should be unchanged) ==\n")
set.seed(7)
for (nn in c(200, 500, 1000)) {
  M <- matrix(runif(nn*nn), nn, nn)
  reps <- max(2L, as.integer(20000L / nn))
  t  <- system.time(for (i in seq_len(reps)) assignment(M, method = "auction_scaled"))[3]
  ms <- (t / reps) * 1000
  cat(sprintf("  n=%-4d  auction_scaled: %7.2f ms  (reps=%d)\n", nn, ms, reps))
}
