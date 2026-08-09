## Time couplr's memory_mode = "lazy" path on the same synthetic problems the
## scaling table uses, and check that it returns the dense path's assignment.
##
## bench_scaling.R pins memory_mode = "dense" so all three packages are timed on
## comparable work. This script covers the lazy path on its own, at the sizes
## where the dense matrix is the binding constraint.
##
## Reproducible via:  Rscript paper/bench_scaling_lazy.R

repo_root <- if (file.exists("DESCRIPTION")) {
  normalizePath(".", winslash = "/", mustWork = TRUE)
} else if (basename(getwd()) == "paper" && file.exists("../DESCRIPTION")) {
  normalizePath("..", winslash = "/", mustWork = TRUE)
} else {
  stop("Run this script from the package root or the paper directory.")
}

options(pkg.build_extra_flags = FALSE)

suppressPackageStartupMessages({
  library(RhpcBLASctl)
  pkgload::load_all(repo_root, quiet = TRUE)
})

## Single-core wall-clock, matching the rest of the paper's timings.
blas_set_num_threads(1); omp_set_num_threads(1)
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1",
           MKL_NUM_THREADS = "1", VECLIB_MAXIMUM_THREADS = "1")

source(file.path(repo_root, "paper", "bench_common.R"))

paper_dir <- file.path(repo_root, "paper")
out_csv   <- file.path(paper_dir, "scaling-lazy-results.csv")

SIZES <- c(20000L, 50000L)
EQUIV_SIZE <- 5000L   # small enough that the dense path is cheap to run twice

time_match <- function(d, mode) {
  tr <- subset(d, treat == 1); ct <- subset(d, treat == 0)
  t0 <- proc.time()[["elapsed"]]
  m <- match_couples(left = tr, right = ct, vars = covars,
                     distance = "mahalanobis", method = "jv",
                     memory_mode = mode)
  t1 <- proc.time()[["elapsed"]]
  list(elapsed = t1 - t0, total_cost = sum(m$pairs$distance),
       n_pairs = nrow(m$pairs), right_id = m$pairs$right_id)
}

## ---- equivalence: lazy reproduces the dense assignment ----
cat("=== equivalence check at n_total =", EQUIV_SIZE, "===\n"); flush.console()
d_eq <- make_data(EQUIV_SIZE, seed = bench_seed(EQUIV_SIZE))
r_dense <- time_match(d_eq, "dense")
r_lazy  <- time_match(d_eq, "lazy")
cost_gap <- abs(r_dense$total_cost - r_lazy$total_cost)
same_pairing <- identical(r_dense$right_id, r_lazy$right_id)
cat(sprintf("  dense total cost %.6f (%d pairs, %.2fs)\n",
            r_dense$total_cost, r_dense$n_pairs, r_dense$elapsed))
cat(sprintf("  lazy  total cost %.6f (%d pairs, %.2fs)\n",
            r_lazy$total_cost, r_lazy$n_pairs, r_lazy$elapsed))
cat(sprintf("  identical pairing: %s | cost gap: %.3e\n\n",
            same_pairing, cost_gap))
flush.console()

## ---- lazy timings at the large sizes ----
results <- data.frame(
  n_total = integer(0), memory_mode = character(0),
  elapsed_s = numeric(0), total_cost = numeric(0)
)

for (n_total in SIZES) {
  cat("=== n_total =", n_total, "(lazy) ===\n"); flush.console()
  d <- make_data(n_total, seed = bench_seed(n_total))
  r <- time_match(d, "lazy")
  cat(sprintf("  %.2f s, %d pairs\n", r$elapsed, r$n_pairs)); flush.console()
  results <- rbind(results, data.frame(
    n_total = n_total, memory_mode = "lazy",
    elapsed_s = round(r$elapsed, 3), total_cost = r$total_cost
  ))
  write.csv(results, out_csv, row.names = FALSE)
}

cat("\nEquivalence at n_total =", EQUIV_SIZE,
    ": identical pairing =", same_pairing, ", cost gap =", cost_gap, "\n")
print(results)
cat("\nWrote", out_csv, "\n")
