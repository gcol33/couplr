## Time couplr alone on the scaling grid, using the same synthetic data as
## bench_scaling.R (identical seed per size). Covers both the n >= 20000 sizes
## that optmatch / MatchIt refuse to attempt and the smaller sizes, so couplr's
## rows can be re-timed after a release without re-running the alternatives,
## whose timings do not depend on couplr's version.
## Writes to paper/scaling-results.csv, replacing existing couplr rows for the
## sizes it runs and leaving every other package's rows untouched.
##
## Reproducible via:  Rscript paper/bench_scaling_couplr_only.R
##
## Environment overrides:
##   COUPLR_BENCH_SIZES  comma-separated n_total values (default 20000,50000)
##   COUPLR_BENCH_REPS   comma-separated replicate counts, recycled to SIZES

repo_root <- if (file.exists("DESCRIPTION")) {
  normalizePath(".", winslash = "/", mustWork = TRUE)
} else if (basename(getwd()) == "paper" && file.exists("../DESCRIPTION")) {
  normalizePath("..", winslash = "/", mustWork = TRUE)
} else {
  stop("Run this script from the package root or the paper directory.")
}

## pkgbuild's default profile compiles the package at -O0. Timings must come
## from an optimised build, matching how the package is installed in use.
options(pkg.build_extra_flags = FALSE)

suppressPackageStartupMessages({
  library(RhpcBLASctl)
  pkgload::load_all(repo_root, quiet = TRUE)
})
blas_set_num_threads(1); omp_set_num_threads(1)
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1",
           MKL_NUM_THREADS = "1", VECLIB_MAXIMUM_THREADS = "1")

paper_dir <- file.path(repo_root, "paper")
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

parse_env_ints <- function(name, default) {
  raw <- Sys.getenv(name, "")
  if (!nzchar(raw)) return(default)
  as.integer(strsplit(raw, ",", fixed = TRUE)[[1]])
}

SIZES <- parse_env_ints("COUPLR_BENCH_SIZES", c(20000L, 50000L))
REPS  <- rep_len(parse_env_ints("COUPLR_BENCH_REPS", 1L), length(SIZES))

covars <- c("v1","v2","v3","v4","v5","v6","b1","b2")
results <- read.csv(out_csv, stringsAsFactors = FALSE)

time_once <- function(tr, ct) {
  t0 <- proc.time()[["elapsed"]]
  ok <- tryCatch({
    .x <- match_couples(left = tr, right = ct, vars = covars,
                        distance = "mahalanobis")
    TRUE
  }, error = function(e) {
    cat("  ERROR:", conditionMessage(e), "\n"); FALSE
  })
  list(ok = ok, elapsed = proc.time()[["elapsed"]] - t0)
}

for (i in seq_along(SIZES)) {
  n_total <- SIZES[i]; reps <- REPS[i]
  cat(sprintf("\n=== n_total = %d, reps = %d ===\n", n_total, reps))
  flush.console()
  d  <- make_data(n_total, seed = 20260515 + n_total)
  tr <- subset(d, treat == 1); ct <- subset(d, treat == 0)

  times <- numeric(0); failed <- FALSE
  for (r in seq_len(reps)) {
    run <- time_once(tr, ct)
    if (!run$ok) { failed <- TRUE; break }
    times <- c(times, run$elapsed)
    cat(sprintf("  rep %d/%d: %.2f s\n", r, reps, run$elapsed)); flush.console()
  }

  status <- if (failed) "error" else "ok"
  cat(sprintf("  couplr at n=%d : %s s (median of %d), status = %s\n", n_total,
              if (failed) "NA" else sprintf("%.2f", stats::median(times)),
              length(times), status))

  results <- results[!(results$n_total == n_total & results$package == "couplr"), ,
                     drop = FALSE]
  results <- rbind(results, data.frame(
    n_total = n_total, package = "couplr", reps = length(times),
    median_s = if (failed) NA_real_ else round(stats::median(times), 2),
    status = status, stringsAsFactors = FALSE
  ))
  results <- results[order(results$n_total, results$package), , drop = FALSE]
  write.csv(results, out_csv, row.names = FALSE)
  if (failed) break
}
cat("\nDone.\n")
