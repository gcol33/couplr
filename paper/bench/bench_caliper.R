## Pair matching within a propensity-score caliper: couplr vs optmatch vs
## bigmatch across problem size. Mahalanobis distance on the eight covariates,
## pairs restricted to units whose linear propensity scores differ by at most
## the caliper; treated:control = 1:2, single core, per-run timeout.
##
## This is the problem the large-study workflows of optmatch and bigmatch
## solve: the caliper removes most pairs before the solve, so neither package
## stores the full distance matrix. couplr solves the same problem with a
## caliper on the score column, in column-generation and in dense mode.
##
## The problem of one instance is fixed before any package runs:
##   - the score is the linear predictor of a logistic regression of treatment
##     on the eight covariates;
##   - the caliper is 1.25 times the smallest one under which every treated
##     unit can be matched (bigmatch::optcal()), since optmatch and bigmatch
##     both stop on a caliper that leaves a treated unit without a partner.
## Every arm's matching is then priced by one function, the pooled-covariance
## Mahalanobis distance, so the totals of the arms that solve this problem
## exactly can be compared. bigmatch minimises a rank-based robust Mahalanobis
## distance with costs rounded to integers, a different objective, so its
## total is recorded and not compared.
##
## Resume-safe: the files are rewritten after each (n, instance).
##
## Reproducible via:  Rscript paper/bench/bench_caliper.R
##                    Rscript paper/bench/bench_caliper.R --quick

bench_dir <- local({
  f <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(f)) stop("Run this script with Rscript.", call. = FALSE)
  dirname(normalizePath(sub("^--file=", "", f[1]), winslash = "/"))
})
source(file.path(bench_dir, "bench_setup.R"))

for (p in c("optmatch", "bigmatch", "RhpcBLASctl")) {
  if (!requireNamespace(p, quietly = TRUE)) {
    stop("This benchmark needs the ", p, " package: install.packages(\"", p,
         "\")", call. = FALSE)
  }
}
suppressPackageStartupMessages({
  library(optmatch)
  library(bigmatch)
  library(RhpcBLASctl)
  load_couplr()
})

blas_set_num_threads(1)
omp_set_num_threads(1)
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1",
           MKL_NUM_THREADS = "1", VECLIB_MAXIMUM_THREADS = "1")
options(optmatch_max_problem_size = Inf)

out_csv    <- file.path(results_dir, "caliper-results.csv")
runs_csv   <- file.path(results_dir, "caliper-runs.csv")
values_csv <- file.path(results_dir, "caliper-values.csv")

argv  <- commandArgs(TRUE)
QUICK <- any(argv == "--quick")

grid <- data.frame(
  n_total   = c(2000L, 5000L, 10000L, 20000L, 50000L),
  instances = c(   3L,    3L,     3L,     3L,     3L),
  reps      = c(   3L,    3L,     3L,     1L,     1L)
)
if (QUICK) {
  grid <- data.frame(n_total = c(600L, 1200L), instances = 2L, reps = 2L)
}
TIMEOUT_S <- 600

source(file.path(bench_dir, "bench_common.R"))

## ---- the problem of one instance -------------------------------------------
make_problem <- function(n_total, seed) {
  d <- make_data(n_total, seed = seed)
  rownames(d) <- d$id
  d$lp <- stats::predict(stats::glm(form, data = d, family = stats::binomial))
  smallest <- bigmatch::optcal(z = d$treat, p = d$lp, rank = FALSE)$caliper
  X <- as.matrix(d[covars])
  tr <- d$treat == 1
  pooled <- (stats::cov(X[tr, ]) * (sum(tr) - 1) +
             stats::cov(X[!tr, ]) * (sum(!tr) - 1)) / (nrow(d) - 2)
  list(d = d, caliper = 1.25 * smallest, X = X, precision = solve(pooled))
}

## The total Mahalanobis distance of a matching given as row indices of `d`.
price <- function(prob, treated_rows, control_rows) {
  D <- prob$X[treated_rows, , drop = FALSE] - prob$X[control_rows, , drop = FALSE]
  sum(sqrt(rowSums((D %*% prob$precision) * D)))
}

## Every arm returns the rows of its pairs, priced the same way.
pairs_value <- function(prob, treated_rows, control_rows) {
  list(n_pairs = length(treated_rows),
       total = price(prob, treated_rows, control_rows))
}

couplr_arm <- function(mode) function(prob) {
  d <- prob$d
  m <- match_couples(left = d[d$treat == 1, ], right = d[d$treat == 0, ],
                     vars = covars, distance = "mahalanobis",
                     calipers = list(lp = prob$caliper), memory_mode = mode)
  rows <- match(c(m$pairs$left_id, m$pairs$right_id), d$id)
  k <- nrow(m$pairs)
  pairs_value(prob, rows[seq_len(k)], rows[k + seq_len(k)])
}

optmatch_arm <- function(prob) {
  d <- prob$d
  within <- match_on(setNames(d$lp, rownames(d)), z = d$treat,
                     caliper = prob$caliper)
  pm <- pairmatch(match_on(form, data = d, within = within), data = d,
                  remove.unmatchables = TRUE)
  sets <- split(seq_len(nrow(d)), as.character(pm[rownames(d)]))
  sets <- sets[lengths(sets) == 2L]
  pairs_value(prob,
              vapply(sets, function(s) s[d$treat[s] == 1], integer(1)),
              vapply(sets, function(s) s[d$treat[s] == 0], integer(1)))
}

bigmatch_arm <- function(prob) {
  d <- prob$d
  m <- nfmatch(z = d$treat, p = d$lp, X = prob$X, dat = d,
               caliper = prob$caliper, rank = FALSE)
  if (!is.list(m)) stop("bigmatch returned no match")
  md <- m$data
  sets <- split(match(md$id, d$id), md$mset)
  pairs_value(prob,
              vapply(sets, function(s) s[d$treat[s] == 1], integer(1)),
              vapply(sets, function(s) s[d$treat[s] == 0], integer(1)))
}

callables <- list(
  couplr_implicit = couplr_arm("implicit"),
  couplr_dense    = couplr_arm("dense"),
  optmatch        = optmatch_arm,
  bigmatch        = bigmatch_arm
)

## ---- resume-safe accumulators ----------------------------------------------
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

    if (n_total <= 2000) {
      for (pkg in pkgs) invisible(try(callables[[pkg]](prob), silent = TRUE))
    }
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
        caliper = prob$caliper,
        n_pairs = if (is.null(v)) NA_integer_ else v$n_pairs,
        total = if (is.null(v)) NA_real_ else v$total,
        stringsAsFactors = FALSE
      ))
      ok <- tr$status == "ok"
      cat(sprintf("  i%d %-16s: %s\n", instance, pkg,
                  if (any(ok)) sprintf("fastest %.3f s, %d pairs, total %.6f",
                                       arm_seconds(tr$seconds, tr$status),
                                       v$n_pairs, v$total)
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
