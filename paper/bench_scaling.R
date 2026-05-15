## Scaling benchmark: couplr vs MatchIt vs optmatch across problem size.
## 1-to-1 optimal Mahalanobis matching on synthetic data, treated:control = 1:2,
## single-core, per-cell timeout. Resume-safe: writes paper/scaling-results.csv
## after each (n, package) cell.
##
## Reproducible via:  Rscript paper/bench_scaling.R
##
## NOTE: From n_total = 10000, `optmatch::pairmatch()` exceeds the default
## `optmatch_max_problem_size` (1e7 entries). Setting the option to Inf
## (done below) lets the call start, but the solve crashes the R process
## without surfacing a recoverable error. The CSV has the refusal recorded
## from a prior run; re-running this script in a fresh session will hit
## the same crash. Use `bench_scaling_couplr_only.R` to push couplr alone
## to n_total in {20000, 50000}.

suppressPackageStartupMessages({
  needed <- c("MatchIt", "optmatch", "R.utils", "RhpcBLASctl")
  for (p in needed) {
    if (!requireNamespace(p, quietly = TRUE))
      install.packages(p, repos = "https://cloud.r-project.org")
  }
  library(MatchIt)
  library(optmatch)
  library(R.utils)
  library(RhpcBLASctl)
  pkgload::load_all("C:/GillesC/Documents/dev/couplr", quiet = TRUE)
})

## Single-core wall-clock: pin BLAS / OpenMP to one thread.
blas_set_num_threads(1)
omp_set_num_threads(1)
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1",
           MKL_NUM_THREADS = "1", VECLIB_MAXIMUM_THREADS = "1")

## optmatch refuses problems with more than 1e7 finite entries by default.
## We raise the cap so the comparison is "what optmatch can solve when asked",
## not "what optmatch refuses to attempt". Flagged in the paper.
options(optmatch_max_problem_size = Inf)

paper_dir <- "C:/GillesC/Documents/dev/couplr/paper"
out_csv   <- file.path(paper_dir, "scaling-results.csv")

## ---- benchmark grid ----
grid <- data.frame(
  n_total = c(500, 2000, 5000, 10000, 20000, 50000),
  reps    = c(  5,    5,    3,     3,     1,     1)
)
TIMEOUT_S <- 300  # per-replicate cap

## ---- synthetic data: 8 covariates, treated:control = 1:2 ----
make_data <- function(n_total, seed) {
  set.seed(seed)
  n_t <- round(n_total / 3)
  n_c <- n_total - n_t
  ## 6 continuous (age-like, educ-like, two-earnings-like, two extras),
  ## 2 binary (race / nodegree-like). Treated shifted slightly to make
  ## the matching problem non-degenerate.
  make_group <- function(n, shift) {
    data.frame(
      v1 = rnorm(n,  0 + 0.30 * shift, 1),
      v2 = rnorm(n,  0 + 0.10 * shift, 1),
      v3 = rnorm(n,  0 + 0.40 * shift, 1),
      v4 = rnorm(n,  0 + 0.20 * shift, 1),
      v5 = rnorm(n,  0 + 0.15 * shift, 1),
      v6 = rnorm(n,  0 + 0.05 * shift, 1),
      b1 = rbinom(n, 1, 0.40 + 0.10 * shift),
      b2 = rbinom(n, 1, 0.30 - 0.05 * shift)
    )
  }
  treated <- make_group(n_t, shift = 1); treated$treat <- 1L
  control <- make_group(n_c, shift = 0); control$treat <- 0L
  d <- rbind(treated, control)
  d$id <- seq_len(nrow(d))
  d
}

covars <- c("v1", "v2", "v3", "v4", "v5", "v6", "b1", "b2")
form   <- as.formula(paste("treat ~", paste(covars, collapse = " + ")))

## ---- per-package callables ----
couplr_call <- function(d) {
  tr <- subset(d, treat == 1); ct <- subset(d, treat == 0)
  match_couples(left = tr, right = ct, vars = covars, distance = "mahalanobis")
}
matchit_call <- function(d) {
  matchit(form, data = d, method = "optimal",
          distance = "mahalanobis", ratio = 1)
}
optmatch_call <- function(d) {
  pairmatch(form, data = d, controls = 1)
}

callables <- list(couplr = couplr_call, optmatch = optmatch_call,
                  MatchIt = matchit_call)

## ---- timed call: returns list(elapsed_s, status) ----
time_one <- function(fn, d, timeout_s) {
  res <- tryCatch(
    withTimeout({
      t0 <- proc.time()[["elapsed"]]
      .x <- fn(d)
      t1 <- proc.time()[["elapsed"]]
      list(elapsed = t1 - t0, status = "ok")
    }, timeout = timeout_s, onTimeout = "error"),
    TimeoutException = function(e) list(elapsed = NA_real_, status = "timeout"),
    error            = function(e) list(elapsed = NA_real_, status = paste0("error: ", conditionMessage(e)))
  )
  res
}

## ---- main loop, writing partial results ----
results <- data.frame(
  n_total = integer(0), package = character(0),
  reps = integer(0), median_s = numeric(0), status = character(0)
)
if (file.exists(out_csv)) {
  prior <- read.csv(out_csv, stringsAsFactors = FALSE)
  if (nrow(prior) > 0) {
    results <- prior
    cat("Resuming from", out_csv, "with", nrow(results), "rows already done.\n")
  }
}

write_partial <- function() {
  write.csv(results, out_csv, row.names = FALSE)
}

for (i in seq_len(nrow(grid))) {
  n_total <- grid$n_total[i]
  reps    <- grid$reps[i]
  cat(sprintf("\n=== n_total = %d, reps = %d ===\n", n_total, reps))
  flush.console()
  d <- make_data(n_total, seed = 20260515 + n_total)

  for (pkg in names(callables)) {
    done <- nrow(results) > 0 &&
      any(results$n_total == n_total & results$package == pkg)
    if (done) {
      cat(sprintf("  %-8s : already recorded, skipping\n", pkg))
      next
    }
    fn <- callables[[pkg]]
    ## warm-up at small n only (avoid burning the timeout twice at large n)
    if (n_total <= 2000) invisible(try(fn(d), silent = TRUE))
    times <- numeric(0); final_status <- "ok"
    for (r in seq_len(reps)) {
      tr <- time_one(fn, d, TIMEOUT_S)
      if (tr$status == "ok") {
        times <- c(times, tr$elapsed)
      } else {
        final_status <- tr$status
        break
      }
    }
    med <- if (length(times) > 0) median(times) else NA_real_
    cat(sprintf("  %-8s : median = %s s, status = %s\n", pkg,
                ifelse(is.na(med), "NA", sprintf("%.3f", med)), final_status))
    flush.console()
    results <- rbind(results, data.frame(
      n_total = n_total, package = pkg, reps = length(times),
      median_s = round(med, 4), status = final_status,
      stringsAsFactors = FALSE
    ))
    write_partial()
  }
}

cat("\nFinal results:\n")
print(results)
write_partial()
cat("\nWrote", out_csv, "\n")
