## Try n=2000 where the audit claimed the warm-start kicks in.

Rcpp::compileAttributes(".")
devtools::load_all(".", quiet = TRUE)

set.seed(99)
for (nn in c(1000, 2000)) {
  M <- matrix(runif(nn*nn), nn, nn)
  reps <- if (nn <= 1000) 10 else 4
  t <- system.time(for (i in seq_len(reps)) assignment(M, method = "csa"))[3]
  ms <- (t / reps) * 1000
  cat(sprintf("  n=%-4d  CSA: %7.1f ms  (reps=%d)\n", nn, ms, reps))
}
cat(sprintf("\nBaseline benchmarks (paper):\n"))
cat(sprintf("  n=1000  CSA:   140 ms\n"))
cat(sprintf("  n=2000  CSA:   600 ms\n"))
