## How often verify_assignment() reaches an exact certificate.
##
## The certificate decides its conditions in exact arithmetic and falls back to
## a stated tolerance when they do not hold exactly. They hold exactly when the
## potentials are exactly optimal for the cost matrix as stored: matched arcs
## tight to the last bit, no admissible pair with a negative reduced cost, no
## unmatched column carrying a non-zero potential. That is a property of the
## arithmetic that produced the potentials, so the grid varies the costs, the
## size and shape of the problem, and the solver.
##
## Writes paper/bench/results/certificate-runs.csv (one row per instance) and
## paper/bench/results/certificate-results.csv (one row per cell), and resumes
## from the runs.
##
## Reproducible via:  Rscript paper/bench/bench_certificate.R

repo_root <- if (file.exists("DESCRIPTION")) {
  normalizePath(".", winslash = "/", mustWork = TRUE)
} else if (basename(getwd()) == "bench" && file.exists("../../DESCRIPTION")) {
  normalizePath("../..", winslash = "/", mustWork = TRUE)
} else {
  stop("Run this script from the package root or paper/bench.")
}

options(pkg.build_extra_flags = FALSE)
suppressPackageStartupMessages(pkgload::load_all(repo_root, quiet = TRUE))

results_dir  <- file.path(repo_root, "paper", "bench", "results")
runs_path    <- file.path(results_dir, "certificate-runs.csv")
results_path <- file.path(results_dir, "certificate-results.csv")

REPS <- 10L

make_cost <- function(kind, n, m, seed) {
  set.seed(seed)
  switch(kind,
    integer = matrix(sample.int(10000L, n * m, replace = TRUE), n, m),
    uniform = matrix(runif(n * m), n, m),
    ## Euclidean distances between standard normal clouds in five dimensions.
    ## Every entry is a square root, so every entry carries a full mantissa.
    distance = {
      left  <- matrix(rnorm(n * 5), n, 5)
      right <- matrix(rnorm(m * 5), m, 5)
      sqrt(pmax(outer(rowSums(left^2), rowSums(right^2), "+") -
                  2 * left %*% t(right), 0))
    },
    stop("unknown cost kind: ", kind)
  )
}

## `jv` is certified against the potentials assignment_duals() returns beside
## its own matching. The other solvers return no potentials, so their matching
## is certified against potentials verify_assignment() obtains separately,
## which is what a user of that solver gets.
solve_with <- function(cost, method) {
  if (identical(method, "jv")) assignment_duals(cost)
  else assignment(cost, method = method)
}

one_instance <- function(kind, n, m, method, rep) {
  cost <- make_cost(kind, n, m, seed = 1000L * rep + n)
  solved <- solve_with(cost, method)
  cert <- verify_assignment(solved, cost)
  scale <- stats::median(abs(cost))
  data.frame(
    kind = kind, n = n, m = m, method = method, rep = rep,
    exact_available   = isTRUE(cert$exact_available),
    exact_certificate = isTRUE(cert$exact_certificate),
    certified_optimal = isTRUE(cert$certified_optimal),
    arithmetic        = cert$arithmetic,
    n_exact_untight    = cert$n_exact_untight,
    n_exact_violations = cert$n_exact_violations,
    duality_gap        = cert$duality_gap,
    ## The miss against the scale of the costs: one unit in the last place
    ## reads as about 1e-16 here.
    slack_rel = cert$max_matched_slack / scale,
    minrc_rel = cert$min_reduced_cost / scale,
    stringsAsFactors = FALSE
  )
}

## The size sweep holds the solver at `jv`; the solver sweep holds the size at
## 200 by 200. A negative n marks the rectangular cell, 200 rows against 600
## columns.
solvers <- c("jv", "hungarian", "auction", "auction_scaled", "sap", "sap_dense",
             "csa", "gabow_tarjan", "lapmod", "push_relabel", "network_simplex")
grid <- rbind(
  expand.grid(kind = c("integer", "uniform", "distance"),
              n = c(50L, 200L, 800L, -200L), method = "jv",
              stringsAsFactors = FALSE),
  expand.grid(kind = c("integer", "uniform"), n = 200L,
              method = setdiff(solvers, "jv"), stringsAsFactors = FALSE)
)
grid$m <- ifelse(grid$n < 0, 600L, grid$n)
grid$n <- abs(grid$n)

runs <- if (file.exists(runs_path)) {
  utils::read.csv(runs_path, stringsAsFactors = FALSE)
} else NULL

for (k in seq_len(nrow(grid))) {
  cell <- grid[k, ]
  for (r in seq_len(REPS)) {
    if (!is.null(runs) && any(runs$kind == cell$kind & runs$n == cell$n &
                              runs$m == cell$m & runs$method == cell$method &
                              runs$rep == r)) {
      next
    }
    cat(sprintf("[%d/%d] %s %d x %d %s rep %d\n", k, nrow(grid), cell$kind,
                cell$n, cell$m, cell$method, r))
    flush.console()
    row <- one_instance(cell$kind, cell$n, cell$m, cell$method, r)
    runs <- if (is.null(runs)) row else rbind(runs, row)
    utils::write.csv(runs, runs_path, row.names = FALSE)
  }
}

cells <- split(runs, list(runs$kind, runs$n, runs$m, runs$method), drop = TRUE)
results <- do.call(rbind, lapply(cells, function(d) {
  miss <- d$slack_rel[!d$exact_certificate]
  data.frame(
    kind = d$kind[1], n = d$n[1], m = d$m[1], method = d$method[1],
    instances         = nrow(d),
    exact_available   = sum(d$exact_available),
    exact_certificate = sum(d$exact_certificate),
    certified_optimal = sum(d$certified_optimal),
    slack_rel_min = if (length(miss)) min(miss) else NA_real_,
    slack_rel_max = if (length(miss)) max(miss) else NA_real_,
    stringsAsFactors = FALSE
  )
}))
results <- results[order(results$method != "jv", results$method, results$kind,
                         results$n, results$m), ]
rownames(results) <- NULL
utils::write.csv(results, results_path, row.names = FALSE)

print(results, row.names = FALSE)
cat("\nwrote", runs_path, "and", results_path, "\n")
