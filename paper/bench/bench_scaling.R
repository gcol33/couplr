## Scaling benchmark: couplr vs MatchIt vs optmatch across problem size.
## 1-to-1 optimal Mahalanobis matching on synthetic data, treated:control = 1:2,
## single-core, per-run timeout.
##
## Each size is generated as several independent instances and each instance is
## timed several times. The repetitions measure the machine, the instances
## measure the problem, and the two are not the same quantity: a spread taken
## across repetitions of one matrix says how steady the clock is, and says
## nothing about how much the work varies from one draw to the next. Every run
## is written to paper/bench/results/scaling-runs.csv on its own row, and the
## summary reports the median and the quartiles across instances.
##
## Resume-safe: both files are rewritten after each (n, package, instance).
##
## Reproducible via:  Rscript paper/bench/bench_scaling.R
##                    Rscript paper/bench/bench_scaling.R --quick
##
## NOTE: From n_total = 10000, `optmatch::pairmatch()` exceeds the default
## `optmatch_max_problem_size` (1e7 entries). Setting the option to Inf
## (done below) lets the call start. On macOS / arm64 the solve then runs to
## completion; on Windows / x86_64 it has been observed to terminate the R
## process without surfacing a recoverable error. Where that happens, run
## `bench_scaling_couplr_only.R` to cover couplr at the large sizes and
## `bench_scaling_alternatives.R` for the other two.

bench_dir <- local({
  f <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(f)) stop("Run this script with Rscript.", call. = FALSE)
  dirname(normalizePath(sub("^--file=", "", f[1]), winslash = "/"))
})
source(file.path(bench_dir, "bench_setup.R"))


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
  load_couplr()
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

out_csv     <- file.path(results_dir, "scaling-results.csv")
runs_csv    <- file.path(results_dir, "scaling-runs.csv")

argv  <- commandArgs(TRUE)
QUICK <- any(argv == "--quick")

## ---- benchmark grid ----
## Instances first, repetitions inside them. Every size carries at least three
## instances, so no reported time is a single draw and the bracket beside it is
## problem-to-problem variation everywhere in the table. The large sizes drop
## repetitions rather than instances: a repetition measures what else the
## machine was doing, which a solve lasting minutes averages over anyway, and at
## 20000 a single optmatch solve is minutes.
grid <- data.frame(
  n_total   = c(500L, 2000L, 5000L, 10000L, 20000L, 50000L),
  instances = c(   5L,   5L,    5L,     3L,     3L,     3L),
  reps      = c(   5L,   5L,    3L,     3L,     1L,     1L)
)
if (QUICK) {
  grid <- data.frame(n_total = c(300L, 600L), instances = 2L, reps = 2L)
}
TIMEOUT_S <- 600  # per run

## ---- synthetic data: 8 covariates, treated:control = 1:2 ----
source(file.path(bench_dir, "bench_common.R"))

## ---- per-package callables ----
## memory_mode is pinned to "dense" so all three packages are timed on the same
## kind of work: MatchIt and optmatch both materialize the distance matrix.
## Under the default "auto", couplr's choice of dense or lazy depends on how
## much RAM happens to be free, which would make the timing depend on machine
## state rather than on problem size. The lazy path is timed separately by
## bench_scaling_lazy.R.
##
## Every callable takes `ctx`: the data (`ctx$d`), and, for the arms that time
## one part of a certified solve, the distance matrix (`ctx$cost`) and an
## assignment with its potentials (`ctx$duals`), both built outside the timing.
##
## `couplr` is the default call, which does not certify a dense matching;
## `couplr_certified` is the same call with `certify = TRUE`, so the difference
## is the cost of the certificate. The three `lap_*` arms split that cost on
## the same matrix: `lap_solve` is the Jonker-Volgenant solve alone,
## `lap_duals` the solve that also returns the potentials, and `lap_verify`
## the check of the matching against those potentials, which calls no solver.
couplr_call <- function(ctx, certify = NULL) {
  d <- ctx$d
  tr <- subset(d, treat == 1); ct <- subset(d, treat == 0)
  match_couples(left = tr, right = ct, vars = covars, distance = "mahalanobis",
                memory_mode = "dense", certify = certify)
}
matchit_call <- function(ctx) {
  matchit(form, data = ctx$d, method = "optimal",
          distance = "mahalanobis", ratio = 1)
}
optmatch_call <- function(ctx) {
  pairmatch(form, data = ctx$d, controls = 1)
}

callables <- list(
  couplr           = couplr_call,
  couplr_certified = function(ctx) couplr_call(ctx, certify = TRUE),
  optmatch         = optmatch_call,
  MatchIt          = matchit_call,
  lap_solve        = function(ctx) assignment(ctx$cost, method = "jv"),
  lap_duals        = function(ctx) assignment_duals(ctx$cost),
  lap_verify       = function(ctx) verify_assignment(ctx$duals, ctx$cost)
)
cost_arms  <- c("lap_solve", "lap_duals", "lap_verify")

## The matrix couplr's dense mode solves, in the treated-by-control orientation.
distance_matrix <- function(d) {
  as.matrix(compute_distances(subset(d, treat == 1), subset(d, treat == 0),
                              vars = covars, distance = "mahalanobis",
                              memory_mode = "dense"))
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
    pkgs <- Filter(function(pkg) !have_run(n_total, pkg, instance), names(callables))
    for (pkg in setdiff(names(callables), pkgs)) {
      cat(sprintf("  i%d %-8s : already recorded, skipping\n", instance, pkg))
    }
    if (!length(pkgs)) next
    ctx <- list(d = make_data(n_total, seed = seed))
    if (any(pkgs %in% cost_arms)) {
      ctx$cost  <- distance_matrix(ctx$d)
      ctx$duals <- assignment_duals(ctx$cost)
    }

    ## Warm-up at small n only: at large n it would cost a second full solve.
    if (n_total <= 2000) {
      for (pkg in pkgs) invisible(try(callables[[pkg]](ctx), silent = TRUE))
    }
    arms <- setNames(lapply(pkgs, function(pkg) {
      fn <- callables[[pkg]]
      function() { fn(ctx); NULL }
    }), pkgs)
    timed <- time_rounds(arms, reps, TIMEOUT_S)$runs
    ctx$cost <- ctx$duals <- NULL

    for (pkg in pkgs) {
      tr <- timed[timed$arm == pkg, ]
      runs <- rbind(runs, data.frame(
        n_total = n_total, package = pkg, instance = instance, seed = seed,
        rep = tr$rep, seconds = round(tr$seconds, 4), status = tr$status,
        stringsAsFactors = FALSE
      ))
      ok <- tr$status == "ok"
      cat(sprintf("  i%d %-8s : %s\n", instance, pkg,
                  if (any(ok)) sprintf("fastest %.3f s of %d rep(s)",
                                       arm_seconds(tr$seconds, tr$status), sum(ok))
                  else tr$status[1]))
    }
    flush.console()
    write_partial()
  }
}

summ <- write_partial()
cat("\nFinal results:\n")
print(summ, row.names = FALSE)
cat("\nWrote", out_csv, "and", runs_csv, "\n")
