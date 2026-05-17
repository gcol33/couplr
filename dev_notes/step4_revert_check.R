## Verify step 4 revert: auction times should be back to baseline.

Rcpp::compileAttributes(".")
devtools::load_all(".", quiet = TRUE)

cat("Auction tests after revert (should still pass):\n")
testthat::test_file("tests/testthat/test-assignment-auction.R", reporter = "summary")

set.seed(1)
M <- matrix(runif(200*200), 200, 200)
t_a    <- system.time(for (i in 1:10) assignment(M, method = "auction"))[3] / 10
t_a_gs <- system.time(for (i in 1:10) assignment(M, method = "auction_gs"))[3] / 10
cat(sprintf("\nAuction    n=200 avg: %5.2f ms (baseline ~10.8)\n", t_a    * 1000))
cat(sprintf("Auction-GS n=200 avg: %5.2f ms (baseline ~10.2)\n", t_a_gs * 1000))
