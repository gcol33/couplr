## Phase 7 profile probe.
##
## Build the package with -DCOUPLR_GT_PROFILE (set in src/Makevars.win for
## this run), then call lap_solve_gabow_tarjan once at each size. The C++
## prints a one-line breakdown per scale_match call (one per bit-scaling
## phase). The benchmark below aggregates the lines manually.
##
## Run from package root:
##   "C:/Program Files/R/R-4.6.0/bin/Rscript.exe" dev_notes/phase7_profile.R

suppressPackageStartupMessages({
  devtools::load_all(".", quiet = TRUE, recompile = TRUE)
})

set.seed(42L)

probe <- function(n, max_cost = 1e5L) {
  cost <- matrix(sample.int(max_cost, n * n, replace = TRUE), n, n)
  cat(sprintf("\n=== n=%d max_cost=%g ===\n", n, max_cost))
  t0 <- Sys.time()
  res <- couplr:::lap_solve_gabow_tarjan(cost, maximize = FALSE)
  t1 <- Sys.time()
  cat(sprintf("[wall] total %.2f ms  n_matched=%d  cost=%g\n",
              1000 * as.numeric(t1 - t0, units = "secs"),
              res$n_matched, res$total_cost))
  invisible(NULL)
}

for (n in c(64L, 128L, 256L)) probe(n)
