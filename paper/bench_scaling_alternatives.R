## Re-run optmatch and MatchIt at n = 10000 and 20000, using the explicit
## match_on() -> pairmatch() chain for optmatch (the formula-direct path
## crashes at scale; the two-step path completes). MatchIt is given a
## pre-built distance matrix so it does not re-trigger the same path.

suppressPackageStartupMessages({
  library(MatchIt)
  library(optmatch)
  library(RhpcBLASctl)
})
options(optmatch_max_problem_size = Inf)
blas_set_num_threads(1); omp_set_num_threads(1)
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1",
           MKL_NUM_THREADS = "1")

paper_dir <- "C:/GillesC/Documents/dev/couplr/paper"
out_csv   <- file.path(paper_dir, "scaling-results.csv")

make_data <- function(n_total, seed) {
  set.seed(seed)
  n_t <- round(n_total / 3); n_c <- n_total - n_t
  make_group <- function(n, shift) {
    data.frame(
      v1 = rnorm(n,  0.30 * shift, 1),
      v2 = rnorm(n,  0.10 * shift, 1),
      v3 = rnorm(n,  0.40 * shift, 1),
      v4 = rnorm(n,  0.20 * shift, 1),
      v5 = rnorm(n,  0.15 * shift, 1),
      v6 = rnorm(n,  0.05 * shift, 1),
      b1 = rbinom(n, 1, 0.40 + 0.10 * shift),
      b2 = rbinom(n, 1, 0.30 - 0.05 * shift)
    )
  }
  tr <- make_group(n_t, 1); tr$treat <- 1L
  ct <- make_group(n_c, 0); ct$treat <- 0L
  d  <- rbind(tr, ct); d$id <- seq_len(nrow(d)); d
}
covars <- c("v1","v2","v3","v4","v5","v6","b1","b2")
form <- as.formula(paste("treat ~", paste(covars, collapse = " + ")))

results <- read.csv(out_csv, stringsAsFactors = FALSE)

run_one <- function(label, expr) {
  cat("  ", label, ": "); flush.console()
  t0 <- proc.time()[["elapsed"]]
  ok <- tryCatch({ force(expr); TRUE },
                 error = function(e) { cat("ERROR ", conditionMessage(e), "\n"); FALSE })
  t1 <- proc.time()[["elapsed"]]
  if (ok) cat(sprintf("%.2f s\n", t1 - t0))
  list(ok = ok, elapsed = t1 - t0)
}

upsert <- function(results, n_total, pkg, ok, elapsed) {
  status <- if (ok) "ok" else "error"
  med    <- if (ok) round(elapsed, 2) else NA_real_
  reps   <- if (ok) 1L else 0L
  keep   <- !(results$n_total == n_total & results$package == pkg)
  results <- results[keep, , drop = FALSE]
  rbind(results, data.frame(
    n_total = n_total, package = pkg, reps = reps,
    median_s = med, status = status,
    stringsAsFactors = FALSE
  ))
}

for (n_total in c(10000, 20000)) {
  cat(sprintf("\n=== n_total = %d ===\n", n_total)); flush.console()
  d <- make_data(n_total, seed = 20260515 + n_total)

  ## optmatch via the two-step path
  r_opt <- run_one("optmatch (match_on + pairmatch)", {
    dist <- match_on(form, data = d, method = "mahalanobis")
    pairmatch(dist, data = d, controls = 1)
  })
  results <- upsert(results, n_total, "optmatch", r_opt$ok, r_opt$elapsed)
  write.csv(results, out_csv, row.names = FALSE)

  ## MatchIt via standard API (it builds Mahalanobis internally)
  r_mi <- run_one("MatchIt (method='optimal')", {
    matchit(form, data = d, method = "optimal",
            distance = "mahalanobis", ratio = 1)
  })
  results <- upsert(results, n_total, "MatchIt", r_mi$ok, r_mi$elapsed)
  write.csv(results, out_csv, row.names = FALSE)
}
cat("\nDone.\n")
