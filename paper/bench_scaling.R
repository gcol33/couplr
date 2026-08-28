## Scaling benchmark: couplr vs MatchIt vs optmatch across problem size.
## 1-to-1 optimal Mahalanobis matching on synthetic data, treated:control = 1:2,
## single-core, per-run timeout.
##
## Each size is generated as several independent instances and each instance is
## timed several times. The repetitions measure the machine, the instances
## measure the problem, and the two are not the same quantity: a spread taken
## across repetitions of one matrix says how steady the clock is, and says
## nothing about how much the work varies from one draw to the next. Every run
## is written to paper/scaling-runs.csv on its own row, and the summary reports
## the median and the quartiles across instances.
##
## Resume-safe: both files are rewritten after each (n, package, instance).
##
## Reproducible via:  Rscript paper/bench_scaling.R
##                    Rscript paper/bench_scaling.R --quick
##
## NOTE: From n_total = 10000, `optmatch::pairmatch()` exceeds the default
## `optmatch_max_problem_size` (1e7 entries). Setting the option to Inf
## (done below) lets the call start. On macOS / arm64 the solve then runs to
## completion; on Windows / x86_64 it has been observed to terminate the R
## process without surfacing a recoverable error. Where that happens, run
## `bench_scaling_couplr_only.R` to cover couplr at the large sizes and
## `bench_scaling_alternatives.R` for the other two.

repo_root <- if (file.exists("DESCRIPTION")) {
  normalizePath(".", winslash = "/", mustWork = TRUE)
} else if (basename(getwd()) == "paper" && file.exists("../DESCRIPTION")) {
  normalizePath("..", winslash = "/", mustWork = TRUE)
} else {
  stop("Run this script from the package root or the paper directory.")
}

## pkgbuild's default profile compiles the package at -O0, which would time an
## unoptimised couplr against optimised installs of the comparison packages.
options(pkg.build_extra_flags = FALSE)

for (p in c("MatchIt", "optmatch", "RhpcBLASctl")) {
  if (!requireNamespace(p, quietly = TRUE)) {
    stop("This benchmark needs the ", p, " package: install.packages(\"", p,
         "\")", call. = FALSE)
  }
}
suppressPackageStartupMessages({
  library(MatchIt)
  library(optmatch)
  library(RhpcBLASctl)
  pkgload::load_all(repo_root, quiet = TRUE)
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

paper_dir <- file.path(repo_root, "paper")
out_csv   <- file.path(paper_dir, "scaling-results.csv")
runs_csv  <- file.path(paper_dir, "scaling-runs.csv")

argv  <- commandArgs(TRUE)
QUICK <- any(argv == "--quick")

## ---- benchmark grid ----
## Instances first, repetitions inside them. The large sizes carry fewer of
## both: at 20000 a single optmatch solve is minutes, and a second instance
## there costs more than the spread it would report is worth.
grid <- data.frame(
  n_total   = c(500L, 2000L, 5000L, 10000L, 20000L, 50000L),
  instances = c(   5L,   5L,    5L,     3L,     2L,     1L),
  reps      = c(   3L,   3L,    2L,     2L,     1L,     1L)
)
if (QUICK) {
  grid <- data.frame(n_total = c(300L, 600L), instances = 2L, reps = 2L)
}
TIMEOUT_S <- 600  # per run

## ---- synthetic data: 8 covariates, treated:control = 1:2 ----
source(file.path(repo_root, "paper", "bench_common.R"))

## Instance 1 of every size is the problem the earlier single-instance table was
## measured on, so the new rows extend that record rather than replacing it with
## an unrelated draw.
instance_seed <- function(n_total, instance) {
  bench_seed(n_total) + (instance - 1L) * 1000003L
}

## ---- per-package callables ----
## memory_mode is pinned to "dense" so all three packages are timed on the same
## kind of work: MatchIt and optmatch both materialize the distance matrix.
## Under the default "auto", couplr's choice of dense or lazy depends on how
## much RAM happens to be free, which would make the timing depend on machine
## state rather than on problem size. The lazy path is timed separately by
## bench_scaling_lazy.R.
couplr_call <- function(d) {
  tr <- subset(d, treat == 1); ct <- subset(d, treat == 0)
  match_couples(left = tr, right = ct, vars = covars, distance = "mahalanobis",
                memory_mode = "dense")
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
  got <- bounded_call(function() {
    t0 <- proc.time()[["elapsed"]]
    .x <- fn(d)
    proc.time()[["elapsed"]] - t0
  }, timeout_s)
  if (!got$ok) return(list(elapsed = NA_real_, status = got$status))
  list(elapsed = got$value, status = "ok")
}

## ---- resume-safe accumulators ----------------------------------------------
runs <- if (file.exists(runs_csv)) {
  read.csv(runs_csv, stringsAsFactors = FALSE)
} else {
  data.frame()
}

have_run <- function(n_total, pkg, instance) {
  nrow(runs) > 0 &&
    any(runs$n_total == n_total & runs$package == pkg &
          runs$instance == instance)
}

## The published table is the summary, and it is derived from the raw rows
## rather than accumulated alongside them, so the two cannot drift apart.
summarise_runs <- function(runs) {
  if (!nrow(runs)) return(data.frame())
  do.call(rbind, lapply(split(runs, list(runs$n_total, runs$package),
                              drop = TRUE), function(g) {
    ok <- g[g$status == "ok", ]
    ## One number per instance -- the median of its repetitions -- so the
    ## quartiles below are taken over instances and not over repetitions.
    per_instance <- if (nrow(ok)) {
      vapply(split(ok$seconds, ok$instance), median, numeric(1))
    } else {
      numeric(0)
    }
    q <- if (length(per_instance)) {
      stats::quantile(per_instance, c(0.25, 0.5, 0.75), names = FALSE)
    } else {
      rep(NA_real_, 3)
    }
    data.frame(
      n_total = g$n_total[1], package = g$package[1],
      instances = length(per_instance), runs = nrow(ok),
      median_s = round(q[2], 4), q25_s = round(q[1], 4), q75_s = round(q[3], 4),
      min_s = if (length(per_instance)) round(min(per_instance), 4) else NA_real_,
      max_s = if (length(per_instance)) round(max(per_instance), 4) else NA_real_,
      status = if (nrow(ok) == nrow(g)) "ok" else g$status[g$status != "ok"][1],
      stringsAsFactors = FALSE
    )
  }))
}

write_partial <- function() {
  write.csv(runs, runs_csv, row.names = FALSE)
  summ <- summarise_runs(runs)
  summ <- summ[order(summ$n_total, summ$package), ]
  write.csv(summ, out_csv, row.names = FALSE)
  invisible(summ)
}

for (i in seq_len(nrow(grid))) {
  n_total   <- grid$n_total[i]
  instances <- grid$instances[i]
  reps      <- grid$reps[i]
  cat(sprintf("\n=== n_total = %d, %d instance(s) x %d rep(s) ===\n",
              n_total, instances, reps))
  flush.console()

  for (instance in seq_len(instances)) {
    seed <- instance_seed(n_total, instance)
    d <- NULL
    for (pkg in names(callables)) {
      if (have_run(n_total, pkg, instance)) {
        cat(sprintf("  i%d %-8s : already recorded, skipping\n", instance, pkg))
        next
      }
      if (is.null(d)) d <- make_data(n_total, seed = seed)
      fn <- callables[[pkg]]
      ## Warm-up at small n only: at large n it would cost a second full solve.
      if (n_total <= 2000) invisible(try(fn(d), silent = TRUE))

      times <- numeric(0); statuses <- character(0)
      for (r in seq_len(reps)) {
        tr <- time_one(fn, d, TIMEOUT_S)
        times <- c(times, tr$elapsed); statuses <- c(statuses, tr$status)
        if (tr$status != "ok") break   # a failure repeats, and costs the timeout again
      }
      runs <- rbind(runs, data.frame(
        n_total = n_total, package = pkg, instance = instance, seed = seed,
        rep = seq_along(times), seconds = round(times, 4), status = statuses,
        stringsAsFactors = FALSE
      ))
      ok <- statuses == "ok"
      cat(sprintf("  i%d %-8s : %s\n", instance, pkg,
                  if (any(ok)) sprintf("median %.3f s of %d rep(s)",
                                       median(times[ok]), sum(ok))
                  else statuses[1]))
      flush.console()
      write_partial()
    }
  }
}

summ <- write_partial()
cat("\nFinal results:\n")
print(summ, row.names = FALSE)
cat("\nWrote", out_csv, "and", runs_csv, "\n")
