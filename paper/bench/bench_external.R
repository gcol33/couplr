## External validation of couplr's optimal matchings.
##
## 1. Random instances: optimal totals from couplr, clue::solve_LSAP,
##    lpSolve::lp.assign, optmatch::pairmatch and scipy's
##    linear_sum_assignment on the same cost matrices, and each tool's
##    matching checked with verify_assignment() against couplr's duals.
## 2. LaLonde NSW: the MatchIt and optmatch pairings checked with
##    verify_assignment() on the pooled within-group Mahalanobis matrix.
##
## Writes paper/bench/results/external-{runs,results,lalonde}.csv and
## external-ENVIRONMENT.txt. Needs python3 with numpy and scipy on the PATH.
##
## Reproducible via:  Rscript paper/bench/bench_external.R

repo_root <- if (file.exists("DESCRIPTION")) {
  normalizePath(".", winslash = "/", mustWork = TRUE)
} else if (basename(getwd()) == "bench" && file.exists("../../DESCRIPTION")) {
  normalizePath("../..", winslash = "/", mustWork = TRUE)
} else {
  stop("Run this script from the package root or paper/bench.")
}

suppressPackageStartupMessages({
  library(couplr)
  library(clue)
  library(lpSolve)
  library(optmatch)
  library(MatchIt)
})
options(optmatch_max_problem_size = Inf)

out_dir <- file.path(repo_root, "paper", "bench", "results")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

py_solver <- tempfile(fileext = ".py")
writeLines(c(
  "import sys, numpy as np",
  "from scipy.optimize import linear_sum_assignment",
  "n, m = int(sys.argv[2]), int(sys.argv[3])",
  "c = np.fromfile(sys.argv[1], dtype='<f8').reshape((m, n)).T",
  "r, k = linear_sum_assignment(c)",
  "out = np.zeros(n, dtype='<i4'); out[r] = k + 1",
  "out.tofile(sys.argv[4])"
), py_solver)

scipy_match <- function(cost) {
  fin <- tempfile(); fout <- tempfile()
  writeBin(as.vector(cost), fin, endian = "little")
  status <- system2("python3", c(py_solver, fin, nrow(cost), ncol(cost), fout))
  if (status != 0) stop("scipy solve failed")
  readBin(fout, "integer", n = nrow(cost), size = 4, endian = "little")
}

optmatch_match <- function(cost) {
  dimnames(cost) <- list(paste0("t", seq_len(nrow(cost))),
                         paste0("c", seq_len(ncol(cost))))
  f <- pairmatch(cost, controls = 1)
  f <- f[!is.na(f)]
  lab <- names(f)
  tr <- lab[startsWith(lab, "t")]
  ct <- lab[startsWith(lab, "c")]
  ct_of <- setNames(ct[match(as.character(f[tr]), as.character(f[ct]))], tr)
  as.integer(sub("c", "", ct_of[paste0("t", seq_len(nrow(cost)))]))
}

tools <- list(
  clue     = function(cost) as.integer(solve_LSAP(cost)),
  lpSolve  = function(cost) {
    sol <- lp.assign(cost)$solution
    apply(sol, 1, function(r) which(r > 0.5))
  },
  optmatch = optmatch_match,
  scipy    = scipy_match
)

make_cost <- function(kind, n, m) {
  switch(kind,
    integer = matrix(sample.int(10000L, n * m, replace = TRUE), n, m) + 0,
    uniform = matrix(runif(n * m), n, m),
    euclid  = {
      a <- matrix(rnorm(2 * n, mean = 0.5), n, 2)
      b <- matrix(rnorm(2 * m), m, 2)
      sqrt(outer(a[, 1], b[, 1], "-")^2 + outer(a[, 2], b[, 2], "-")^2)
    })
}

shapes <- list(c(100, 100), c(400, 400), c(1000, 1000), c(200, 600))
kinds <- c("integer", "uniform", "euclid")
seeds <- 1:10

rows <- list()
for (kind in kinds) for (sh in shapes) for (s in seeds) {
  n <- sh[1]; m <- sh[2]
  set.seed(1000 * s + n + m)
  cost <- make_cost(kind, n, m)
  ref <- assignment_duals(cost)
  ref_total <- sum(cost[cbind(seq_len(n), ref$match)])
  duals <- list(u = ref$u, v = ref$v)
  ref_cert <- verify_assignment(ref$match, cost, duals = duals)
  rows[[length(rows) + 1]] <- data.frame(
    kind = kind, n = n, m = m, seed = s, tool = "couplr",
    total = ref_total, rel_diff = 0,
    certified = ref_cert$certified_optimal, arithmetic = ref_cert$arithmetic,
    max_subopt = ref_cert$max_suboptimality)
  for (tool in names(tools)) {
    if (tool == "lpSolve" && (n != m || n > 400)) next
    mt <- tools[[tool]](cost)
    total <- sum(cost[cbind(seq_len(n), mt)])
    cert <- verify_assignment(mt, cost, duals = duals)
    rows[[length(rows) + 1]] <- data.frame(
      kind = kind, n = n, m = m, seed = s, tool = tool,
      total = total, rel_diff = (total - ref_total) / ref_total,
      certified = cert$certified_optimal, arithmetic = cert$arithmetic,
      max_subopt = if (is.null(cert$max_suboptimality)) NA_real_ else cert$max_suboptimality)
  }
  cat(kind, n, m, s, "\n")
}
runs <- do.call(rbind, rows)
write.csv(runs, file.path(out_dir, "external-runs.csv"), row.names = FALSE)

summary_tab <- do.call(rbind, lapply(split(runs, list(runs$tool, runs$kind), drop = TRUE), function(d) {
  data.frame(tool = d$tool[1], kind = d$kind[1], instances = nrow(d),
             max_abs_rel_diff = max(abs(d$rel_diff)),
             n_higher = sum(d$rel_diff > 1e-12),
             certified = sum(d$certified),
             exact = sum(d$certified & d$arithmetic == "exact"))
}))
write.csv(summary_tab, file.path(out_dir, "external-results.csv"), row.names = FALSE)
print(summary_tab, row.names = FALSE)

## LaLonde NSW: certify the MatchIt and optmatch pairings
data("lalonde", package = "MatchIt")
lalonde$race_Black    <- as.integer(lalonde$race == "black")
lalonde$race_Hispanic <- as.integer(lalonde$race == "hispan")
covars <- c("age", "educ", "race_Black", "race_Hispanic",
            "married", "nodegree", "re74", "re75")
form <- as.formula(paste("treat ~", paste(covars, collapse = " + ")))
t_rows <- which(lalonde$treat == 1)
c_rows <- which(lalonde$treat == 0)
Xt <- as.matrix(lalonde[t_rows, covars])
Xc <- as.matrix(lalonde[c_rows, covars])
pooled <- ((nrow(Xt) - 1) * cov(Xt) + (nrow(Xc) - 1) * cov(Xc)) /
  (nrow(Xt) + nrow(Xc) - 2)
Sinv <- solve(pooled)
D <- matrix(0, nrow(Xt), nrow(Xc))
for (i in seq_len(nrow(Xt))) {
  dif <- sweep(Xc, 2, Xt[i, ], "-")
  D[i, ] <- sqrt(pmax(rowSums((dif %*% Sinv) * dif), 0))
}

match_from_strata <- function(strata) {
  mt <- integer(length(t_rows))
  keep <- which(!is.na(strata))
  for (rows in split(keep, strata[keep])) {
    tr <- rows[lalonde$treat[rows] == 1]
    ct <- rows[lalonde$treat[rows] == 0]
    mt[match(tr, t_rows)] <- match(ct, c_rows)
  }
  mt
}

ref <- assignment_duals(D)
duals <- list(u = ref$u, v = ref$v)
pairings <- list(
  couplr   = ref$match,
  MatchIt  = match_from_strata(matchit(form, data = lalonde, method = "optimal",
                                       distance = "mahalanobis", ratio = 1)$subclass),
  optmatch = match_from_strata(pairmatch(form, data = lalonde, controls = 1))
)
lal <- do.call(rbind, lapply(names(pairings), function(p) {
  mt <- pairings[[p]]
  cert <- verify_assignment(mt, D, duals = duals)
  data.frame(package = p, n_pairs = sum(mt > 0),
             total = sum(D[cbind(seq_along(mt), mt)]),
             certified = cert$certified_optimal, arithmetic = cert$arithmetic,
             duality_gap = cert$duality_gap,
             max_suboptimality = if (is.null(cert$max_suboptimality)) NA_real_ else cert$max_suboptimality,
             same_as_couplr = sum(mt == ref$match))
}))
write.csv(lal, file.path(out_dir, "external-lalonde.csv"), row.names = FALSE)
print(lal, row.names = FALSE, digits = 10)

env <- c(
  sprintf("date       %s", format(Sys.time(), "%Y-%m-%d %H:%M %Z")),
  sprintf("R          %s", R.version.string),
  sprintf("platform   %s", R.version$platform),
  vapply(c("couplr", "clue", "lpSolve", "optmatch", "MatchIt"),
         function(p) sprintf("%-10s %s", p, packageVersion(p)), ""),
  sprintf("scipy      %s", system2("python3", c("-c", shQuote("import scipy; print(scipy.__version__)")), stdout = TRUE)))
writeLines(env, file.path(out_dir, "external-ENVIRONMENT.txt"))
