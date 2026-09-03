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
## Over the same instances the timings below use, so the claim that the two
## paths return one assignment rests on every draw that is timed rather than on
## the first of them.
EQUIV_INSTANCES <- 3L
cat("=== equivalence check at n_total =", EQUIV_SIZE, "===\n"); flush.console()
eq <- lapply(seq_len(EQUIV_INSTANCES), function(instance) {
  d_eq <- make_data(EQUIV_SIZE, seed = instance_seed(EQUIV_SIZE, instance))
  r_dense <- time_match(d_eq, "dense")
  r_lazy  <- time_match(d_eq, "lazy")
  same <- identical(r_dense$right_id, r_lazy$right_id)
  gap  <- abs(r_dense$total_cost - r_lazy$total_cost)
  cat(sprintf("  i%d dense %.6f (%d pairs, %.2fs) | lazy %.6f (%d pairs, %.2fs)\n",
              instance, r_dense$total_cost, r_dense$n_pairs, r_dense$elapsed,
              r_lazy$total_cost, r_lazy$n_pairs, r_lazy$elapsed))
  cat(sprintf("  i%d identical pairing: %s | cost gap: %.3e\n",
              instance, same, gap))
  flush.console()
  list(same = same, gap = gap)
})
same_pairing <- all(vapply(eq, function(x) x$same, logical(1)))
cost_gap <- max(vapply(eq, function(x) x$gap, numeric(1)))
cat(sprintf("  %d of %d instances pair identically | worst cost gap: %.3e\n\n",
            sum(vapply(eq, function(x) x$same, logical(1))), EQUIV_INSTANCES,
            cost_gap))
flush.console()

## ---- lazy timings at the large sizes ----------------------------------------
## The instances are the ones `bench_scaling.R` measures the dense path on, so
## the two paths are compared draw by draw and the number the article carries is
## a median over problems rather than a single run. Rows accumulate in
## paper/scaling-lazy-runs.csv and a run already in it is skipped, so a killed
## run resumes where it stopped.
INSTANCES <- 3L
runs_csv  <- file.path(paper_dir, "scaling-lazy-runs.csv")

runs <- if (file.exists(runs_csv)) {
  read.csv(runs_csv, stringsAsFactors = FALSE)
} else {
  data.frame()
}

have_run <- function(n_total, instance) {
  nrow(runs) > 0 && any(runs$n_total == n_total & runs$instance == instance)
}

## The published number is derived from the raw rows rather than accumulated
## beside them, so the two cannot drift apart.
summarise_runs <- function(runs) {
  if (!nrow(runs)) return(data.frame())
  do.call(rbind, lapply(split(runs, runs$n_total), function(g) {
    q <- stats::quantile(g$elapsed_s, c(0.25, 0.5, 0.75), names = FALSE)
    data.frame(n_total = g$n_total[1], memory_mode = "lazy",
               instances = nrow(g), median_s = round(q[2], 3),
               q25_s = round(q[1], 3), q75_s = round(q[3], 3),
               stringsAsFactors = FALSE)
  }))
}

for (n_total in SIZES) {
  cat("=== n_total =", n_total, "(lazy) ===\n"); flush.console()
  for (instance in seq_len(INSTANCES)) {
    if (have_run(n_total, instance)) {
      cat(sprintf("  i%d: already recorded, skipping\n", instance))
      next
    }
    seed <- instance_seed(n_total, instance)
    d <- make_data(n_total, seed = seed)
    r <- time_match(d, "lazy")
    cat(sprintf("  i%d %.2f s, %d pairs\n", instance, r$elapsed, r$n_pairs))
    flush.console()
    new <- data.frame(n_total = n_total, instance = instance, seed = seed,
                      memory_mode = "lazy", elapsed_s = round(r$elapsed, 3),
                      total_cost = r$total_cost, n_pairs = r$n_pairs,
                      stringsAsFactors = FALSE)
    runs <- if (nrow(runs)) rbind(runs, new) else new
    write.csv(runs, runs_csv, row.names = FALSE)
    write.csv(summarise_runs(runs), out_csv, row.names = FALSE)
  }
}

cat("\nEquivalence at n_total =", EQUIV_SIZE,
    ": identical pairing =", same_pairing, ", cost gap =", cost_gap, "\n")
print(summarise_runs(runs))
cat("\nWrote", runs_csv, "and", out_csv, "\n")
