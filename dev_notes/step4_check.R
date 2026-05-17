## Step 4 verification: cached Auction tie-breaker tweak.
## Expected: 10-25% speedup on basic auction and auction-GS at n=200-1000.

cat("Compiling attributes + loading...\n")
Rcpp::compileAttributes(".")
devtools::load_all(".", quiet = TRUE)

cat("\nAuction tests (should pass unchanged):\n")
testthat::test_file("tests/testthat/test-assignment-auction.R",    reporter = "summary")
testthat::test_file("tests/testthat/test-assignment-auction-gs.R", reporter = "summary")
testthat::test_file("tests/testthat/test-auction-hang.R",          reporter = "summary")

cat("\nWall-clock comparison at n=200 (baseline was Auction ~10.8ms, GS ~10.2ms):\n")
set.seed(1)
M <- matrix(runif(200*200), 200, 200)
t_a    <- system.time(for (i in 1:10) assignment(M, method = "auction"))[3] / 10
t_a_gs <- system.time(for (i in 1:10) assignment(M, method = "auction_gs"))[3] / 10
t_a_s  <- system.time(for (i in 1:10) assignment(M, method = "auction_scaled"))[3] / 10
cat(sprintf("  Auction         avg: %7.2f ms\n", t_a    * 1000))
cat(sprintf("  Auction-GS      avg: %7.2f ms\n", t_a_gs * 1000))
cat(sprintf("  Auction-Scaled  avg: %7.2f ms (no tweak in scaled, control)\n", t_a_s  * 1000))

cat("\nAt n=500 (baseline Auction ~83.4ms, GS ~84.1ms):\n")
set.seed(2)
M <- matrix(runif(500*500), 500, 500)
t_a    <- system.time(for (i in 1:5) assignment(M, method = "auction"))[3] / 5
t_a_gs <- system.time(for (i in 1:5) assignment(M, method = "auction_gs"))[3] / 5
cat(sprintf("  Auction    avg: %7.2f ms\n", t_a    * 1000))
cat(sprintf("  Auction-GS avg: %7.2f ms\n", t_a_gs * 1000))
