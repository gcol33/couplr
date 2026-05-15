## Push couplr alone to n = 20k and n = 50k.
## Records into paper/scaling-results.csv (appends).
## optmatch / MatchIt cannot run at these scales (crashes verified).

suppressPackageStartupMessages({
  library(RhpcBLASctl)
  pkgload::load_all("C:/GillesC/Documents/dev/couplr", quiet = TRUE)
})
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
  d <- rbind(tr, ct); d$id <- seq_len(nrow(d)); d
}

covars <- c("v1","v2","v3","v4","v5","v6","b1","b2")

results <- read.csv(out_csv, stringsAsFactors = FALSE)

for (n_total in c(20000, 50000)) {
  if (any(results$n_total == n_total & results$package == "couplr")) {
    cat("n =", n_total, ": already done, skip\n"); next
  }
  cat(sprintf("\n=== n_total = %d ===\n", n_total)); flush.console()
  d  <- make_data(n_total, seed = 20260515 + n_total)
  tr <- subset(d, treat == 1); ct <- subset(d, treat == 0)
  t0 <- proc.time()[["elapsed"]]
  ok <- tryCatch({
    .x <- match_couples(left = tr, right = ct, vars = covars,
                        distance = "mahalanobis")
    TRUE
  }, error = function(e) {
    cat("  ERROR:", conditionMessage(e), "\n"); FALSE
  })
  t1 <- proc.time()[["elapsed"]]
  el <- t1 - t0
  status <- if (ok) "ok" else "error"
  cat(sprintf("  couplr at n=%d : %s s, status = %s\n", n_total,
              if (ok) sprintf("%.2f", el) else "NA", status))
  results <- rbind(results, data.frame(
    n_total = n_total, package = "couplr", reps = if (ok) 1L else 0L,
    median_s = if (ok) round(el, 2) else NA_real_,
    status = status, stringsAsFactors = FALSE
  ))
  write.csv(results, out_csv, row.names = FALSE)
  if (!ok) break
}
cat("\nDone.\n")
