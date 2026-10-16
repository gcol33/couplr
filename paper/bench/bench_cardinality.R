## Cardinality matching under mean balance: couplr vs designmatch across
## problem size. The largest number of treated-control pairs whose covariate
## means differ by at most 0.05 pooled standard deviations on each of the eight
## covariates; equal group sizes, single core, per-run timeout.
##
## With twice as many controls as treated units the constraints do not bind:
## controls alone can be chosen to balance the treated, and every treated unit
## is kept. Equal groups make the problem one where balance costs pairs.
##
## designmatch::cardmatch() solves the problem as an integer program with one
## variable per unit, through HiGHS: by default from its linear relaxation with
## rounding (`approximate = 1`), which may leave a constraint slightly
## violated, and exactly with `approximate = 0`. couplr::cardinality_match()
## states the same constraints as mean-difference moments and reports, beside
## its sample, the largest size the constraints admit. It then goes on to the
## least total distance at that size, which designmatch's cardmatch() does not
## ask for, so its time to prove the size (`size_seconds`) is recorded beside
## its total. Both get the same time budget and no other. Each arm's sample is
## checked against the constraints here, by the same function.
##
## Resume-safe: the files are rewritten after each (n, instance).
##
## Reproducible via:  Rscript paper/bench/bench_cardinality.R
##                    Rscript paper/bench/bench_cardinality.R --quick

bench_dir <- local({
  f <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(f)) stop("Run this script with Rscript.", call. = FALSE)
  dirname(normalizePath(sub("^--file=", "", f[1]), winslash = "/"))
})
source(file.path(bench_dir, "bench_setup.R"))

for (p in c("designmatch", "highs", "RhpcBLASctl")) {
  if (!requireNamespace(p, quietly = TRUE)) {
    stop("This benchmark needs the ", p, " package: install.packages(\"", p,
         "\")", call. = FALSE)
  }
}
suppressPackageStartupMessages({
  library(designmatch)
  library(RhpcBLASctl)
  load_couplr()
})

blas_set_num_threads(1)
omp_set_num_threads(1)
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1",
           MKL_NUM_THREADS = "1", VECLIB_MAXIMUM_THREADS = "1")

out_csv    <- file.path(results_dir, "cardinality-results.csv")
runs_csv   <- file.path(results_dir, "cardinality-runs.csv")
values_csv <- file.path(results_dir, "cardinality-values.csv")

argv  <- commandArgs(TRUE)
QUICK <- any(argv == "--quick")

grid <- data.frame(
  n_total   = c(1000L, 2000L, 5000L, 10000L, 20000L),
  instances = 3L,
  reps      = 1L
)
if (QUICK) {
  grid <- data.frame(n_total = c(400L, 800L), instances = 2L, reps = 1L)
}
TIMEOUT_S  <- 600
## The solvers' own budgets, inside the run's: each stops and returns its best
## sample, and couplr its bound, before the run is killed.
SOLVER_S   <- if (QUICK) 20 else 300
STD_TOL    <- 0.05

source(file.path(bench_dir, "bench_common.R"))

make_problem <- function(n_total, seed) {
  d <- make_data(n_total, seed = seed, treated_share = 1 / 2)
  X <- as.matrix(d[covars])
  tr <- d$treat == 1
  pooled_sd <- sqrt((apply(X[tr, ], 2, stats::var) +
                     apply(X[!tr, ], 2, stats::var)) / 2)
  list(d = d, X = X, tol = STD_TOL * pooled_sd)
}

## The sample's size and its largest mean difference as a share of the
## tolerance: at most 1 when every constraint holds.
sample_value <- function(prob, treated_rows, control_rows, extra = list()) {
  diff <- colMeans(prob$X[treated_rows, , drop = FALSE]) -
    colMeans(prob$X[control_rows, , drop = FALSE])
  c(list(n_pairs = length(treated_rows),
         max_ratio = max(abs(diff) / prob$tol)), extra)
}

couplr_arm <- function(prob) {
  d <- prob$d
  moms <- lapply(covars, function(v) list(var = v, stat = "mean_diff",
                                          max = prob$tol[[v]],
                                          min = -prob$tol[[v]]))
  m <- cardinality_match(d[d$treat == 1, ], d[d$treat == 0, ], vars = covars,
                         moments = moms, time_limit = SOLVER_S,
                         node_limit = Inf)
  g <- glance(m)
  sample_value(prob, match(m$pairs$left_id, d$id), match(m$pairs$right_id, d$id),
               list(best_possible = g$best_possible, certified = g$certified,
                    size_certified = g$size_certified,
                    size_seconds = g$size_seconds))
}

designmatch_arm <- function(approximate) function(prob) {
  d <- prob$d
  m <- cardmatch(t_ind = d$treat, mom = list(covs = prob$X, tols = prob$tol),
                 solver = list(name = "highs", approximate = approximate,
                               t_max = SOLVER_S))
  sample_value(prob, m$t_id, m$c_id)
}

callables <- list(
  couplr             = couplr_arm,
  designmatch        = designmatch_arm(1),
  designmatch_exact  = designmatch_arm(0)
)

read_or_empty <- function(f) {
  if (file.exists(f)) read.csv(f, stringsAsFactors = FALSE) else data.frame()
}
runs   <- read_or_empty(runs_csv)
values <- read_or_empty(values_csv)

have_run <- function(n_total, pkg, instance) {
  nrow(runs) > 0 &&
    any(runs$n_total == n_total & runs$package == pkg &
          runs$instance == instance)
}

write_partial <- function() {
  write.csv(runs, runs_csv, row.names = FALSE)
  write.csv(values, values_csv, row.names = FALSE)
  summ <- summarise_runs(runs)
  summ <- summ[order(summ$n_total, summ$package), ]
  write.csv(summ, out_csv, row.names = FALSE)
  invisible(summ)
}

field <- function(v, name, missing) if (is.null(v[[name]])) missing else v[[name]]

for (i in seq_len(nrow(grid))) {
  n_total <- grid$n_total[i]
  cat(sprintf("\n=== n_total = %d, %d instance(s) x %d rep(s) ===\n",
              n_total, grid$instances[i], grid$reps[i]))
  flush.console()

  for (instance in seq_len(grid$instances[i])) {
    seed <- instance_seed(n_total, instance)
    pkgs <- Filter(function(pkg) !have_run(n_total, pkg, instance), names(callables))
    if (!length(pkgs)) next
    prob <- make_problem(n_total, seed)

    arms <- setNames(lapply(pkgs, function(pkg) {
      fn <- callables[[pkg]]
      function() fn(prob)
    }), pkgs)
    timed <- time_rounds(arms, grid$reps[i], TIMEOUT_S)

    for (pkg in pkgs) {
      tr <- timed$runs[timed$runs$arm == pkg, ]
      runs <- rbind(runs, data.frame(
        n_total = n_total, package = pkg, instance = instance, seed = seed,
        rep = tr$rep, seconds = round(tr$seconds, 4), status = tr$status,
        stringsAsFactors = FALSE
      ))
      v <- timed$values[[pkg]]
      values <- rbind(values, data.frame(
        n_total = n_total, package = pkg, instance = instance,
        n_pairs = field(v, "n_pairs", NA_integer_),
        max_ratio = field(v, "max_ratio", NA_real_),
        best_possible = field(v, "best_possible", NA_integer_),
        certified = field(v, "certified", NA),
        size_certified = field(v, "size_certified", NA),
        size_seconds = field(v, "size_seconds", NA_real_),
        stringsAsFactors = FALSE
      ))
      ok <- tr$status == "ok"
      cat(sprintf("  i%d %-18s: %s\n", instance, pkg,
                  if (any(ok)) sprintf("%.3f s, %d pairs, max |diff|/tol %.3f",
                                       arm_seconds(tr$seconds, tr$status),
                                       v$n_pairs, v$max_ratio)
                  else tr$status[1]))
    }
    flush.console()
    write_partial()
  }
}

summ <- write_partial()
cat("\nFinal results:\n")
print(summ, row.names = FALSE)
cat("\nWrote", out_csv, ",", runs_csv, "and", values_csv, "\n")
