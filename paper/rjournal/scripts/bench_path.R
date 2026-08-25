## What a warm-started matching path costs against the solves it replaces.
##
## 20 caliper values, swept as one path by match_path(), against 20 independent
## solves at the same 20 values, on the paper's own benchmark problems
## (paper/bench_common.R: eight covariates, treated:control = 1:2, Mahalanobis).
##
## The independent solve is a one-point path, not a separate entry point. A path
## of one value calls the same restricted-master start over an empty candidate
## set that memory_mode = "implicit" calls, so the two sides of this table are
## the same solver reached the same way, and the only difference between them is
## that the warm side starts from the point before it. It also means both sides
## report their seconds off the same clock, over the same region of the call.
##
## The comparison is the per-point seconds, not the wall clock. A point carries a
## balance reading computed in R after the C++ call returns, which is a KS test
## per variable per point, and an independent solve does not carry one. Wall
## clock would charge the path for that. The per-point seconds the loop reports
## are the solve's own: they start after the knob is moved and end when the
## point's rounds are done, and they exclude the row structure the sweep builds
## once and the twenty independent solves build twenty times. Both exclusions cut
## the same way on both sides, so the wall clock is reported beside the per-point
## seconds rather than instead of it, and the wall clock the cold side is charged
## is twenty one-point paths, which do the same R-level work per point that the
## warm path does.
##
## The caliper grid is the data's rather than a guess. Its tight end is found by
## bisection: the tightest caliper this problem still admits a complete matching
## under. Its wide end is the median distance between a treated unit and a
## control, read off a subsample of the pairs themselves, which admits half of
## them and so constrains nothing. Between the two sits the largest distance the
## unconstrained optimum uses, which is where the answer stops moving and beyond
## which the caliper only adds arcs to search. Every point is feasible, because
## the first one is and widening only adds arcs, and the last point's total is
## checked against the unconstrained solve's.
##
## Reproducible via:  Rscript paper/bench_path.R
##                    Rscript paper/bench_path.R 500,2000
##                    Rscript paper/bench_path.R 50000 20
##
## Results accumulate in paper/path-results.csv and a shape already in it is
## skipped, so a killed run resumes where it stopped. Move the file aside to
## force a full re-run.

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
  if (!requireNamespace("RhpcBLASctl", quietly = TRUE))
    install.packages("RhpcBLASctl", repos = "https://cloud.r-project.org")
  library(RhpcBLASctl)
  pkgload::load_all(repo_root, quiet = TRUE)
})
options(scipen = 0, OutDec = ".", digits = 7, warn = 1, couplr.emoji = FALSE)

## Single-core wall-clock, matching the rest of the paper's timings.
blas_set_num_threads(1); omp_set_num_threads(1)
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1",
           MKL_NUM_THREADS = "1", VECLIB_MAXIMUM_THREADS = "1")

source(file.path(repo_root, "paper", "bench_common.R"))

paper_dir  <- file.path(repo_root, "paper")
out_csv    <- file.path(paper_dir, "path-results.csv")
points_csv <- file.path(paper_dir, "path-points.csv")

argv <- commandArgs(TRUE)
SIZES <- if (length(argv) >= 1L) {
  as.integer(strsplit(argv[1], ",", fixed = TRUE)[[1]])
} else {
  c(500L, 2000L, 5000L, 20000L)
}
N_VALUES <- if (length(argv) >= 2L) as.integer(argv[2]) else 20L

## How closely the bisection has to bracket the feasibility threshold, as a
## fraction of the widest caliper. Ten halvings reach it.
FEAS_TOL <- 1e-3

## The wide end of the sweep: the quantile of the treated-to-control distances a
## caliper there admits, and the subsample those distances are read from. The
## rows of a generated problem are exchangeable, so the first ones are a sample.
TOP_Q <- 0.5
ANCHOR_LEFT <- 400L
ANCHOR_RIGHT <- 2000L

## Repeats per shape. The small shapes solve in milliseconds, where one repeat is
## a reading of the machine rather than of the code; the large ones do not need
## them and would not get them cheaply.
reps_for <- function(n_total) {
  if (n_total <= 2000L) 3L else if (n_total <= 10000L) 2L else 1L
}

fmt_dbl <- function(x) format(x, digits = 17)

rel_gap <- function(a, b) {
  if (is.na(a) && is.na(b)) return(0)
  if (is.na(a) || is.na(b)) return(Inf)
  abs(a - b) / max(1, abs(b))
}

split_sides <- function(d) {
  list(left = d[d$treat == 1L, , drop = FALSE],
       right = d[d$treat == 0L, , drop = FALSE])
}

## Two of the eight covariates are binary, so every KS test the balance reading
## runs on them is tied; and a caliper the data admits no complete matching
## under is what the bisection below is looking for rather than a fault.
quietly <- function(expr) {
  withCallingHandlers(expr, warning = function(w) {
    m <- conditionMessage(w)
    if (grepl("exact p-value|p-value will be approximate", m) ||
        grepl("no complete matching|no feasible full matching", m)) {
      invokeRestart("muffleWarning")
    }
  })
}

lazy_at <- function(sides, v) {
  quietly(match_couples(sides$left, sides$right, vars = covars,
                        distance = "mahalanobis", memory_mode = "lazy",
                        max_distance = v, check_costs = FALSE))
}

## The tightest caliper this problem still has a complete matching under.
## Feasibility is monotone in the caliper, so bisecting between zero and a
## caliper known to be wide enough converges on the threshold, and the upper
## bracket is always a value that has been solved and found feasible.
tightest_feasible <- function(sides, hi, n_left, tol = FEAS_TOL) {
  lo <- 0
  steps <- 0L
  while ((hi - lo) / hi > tol) {
    mid <- 0.5 * (lo + hi)
    if (nrow(lazy_at(sides, mid)$pairs) == n_left) hi <- mid else lo <- mid
    steps <- steps + 1L
  }
  list(value = hi, steps = steps)
}

## The distance scale this problem is on, read off its own pairs.
loose_caliper <- function(sides) {
  i <- seq_len(min(ANCHOR_LEFT, nrow(sides$left)))
  j <- seq_len(min(ANCHOR_RIGHT, nrow(sides$right)))
  dm <- compute_distances(sides$left[i, , drop = FALSE],
                          sides$right[j, , drop = FALSE],
                          vars = covars, distance = "mahalanobis",
                          memory_mode = "dense")$cost_matrix
  as.numeric(stats::quantile(as.numeric(dm), TOP_Q))
}

path_at <- function(sides, values) {
  gc(verbose = FALSE)
  t0 <- proc.time()[["elapsed"]]
  p <- quietly(match_path(sides$left, sides$right, vars = covars,
                          vary = "max_distance", values = values,
                          distance = "mahalanobis"))
  list(wall = proc.time()[["elapsed"]] - t0, path = p)
}

## ---- resume-safe accumulators ----------------------------------------------
shape_rows <- if (file.exists(out_csv)) {
  list(read.csv(out_csv, stringsAsFactors = FALSE))
} else {
  list()
}
point_rows <- if (length(shape_rows) && file.exists(points_csv)) {
  list(read.csv(points_csv, stringsAsFactors = FALSE))
} else {
  list()
}
done <- if (length(shape_rows)) unique(shape_rows[[1]]$n_total) else integer(0)

cat("couplr path timing: a warm path against the solves it replaces\n")
cat("repo: ", repo_root, "\n", sep = "")
cat("shapes: ", paste(SIZES, collapse = ", "), "   values per sweep: ",
    N_VALUES, "\n\n", sep = "")

for (n_total in SIZES) {
  if (n_total %in% done) {
    cat("n_total = ", n_total, " is already in ", basename(out_csv),
        ", skipping\n", sep = "")
    next
  }
  reps <- reps_for(n_total)
  d <- make_data(n_total, seed = bench_seed(n_total))
  sides <- split_sides(d)
  n_left <- nrow(sides$left)
  n_right <- nrow(sides$right)
  shape <- sprintf("%d x %d", n_left, n_right)

  ## The grid, and the reference the top of it is taken from. The unconstrained
  ## optimum is solved on the lazy path because this is a grid probe rather than
  ## a measurement, and the two paths return the same matching.
  gc(verbose = FALSE)
  t0 <- proc.time()[["elapsed"]]
  ref <- match_couples(sides$left, sides$right, vars = covars,
                       distance = "mahalanobis", memory_mode = "lazy",
                       check_costs = FALSE)
  ref_wall <- proc.time()[["elapsed"]] - t0
  dd <- ref$pairs$distance
  ref_total <- sum(dd)
  thr <- tightest_feasible(sides, max(dd), n_left)
  max_dd <- max(dd)
  hi <- max(loose_caliper(sides), max_dd * (1 + 1e-9))
  values <- seq(thr$value, hi, length.out = N_VALUES)
  inert_from <- which(values >= max_dd)[1]

  cat("=== n_total = ", n_total, "  (", shape, "), ", reps, " rep(s) ===\n",
      sep = "")
  cat(sprintf("    unconstrained lazy solve %.3f s, %d pairs, total %s\n",
              ref_wall, nrow(ref$pairs), fmt_dbl(ref_total)))
  cat(sprintf(
    "    caliper grid %.6f .. %.6f, %d bisection steps; inert from %.6f (point %d)\n",
    values[1], values[N_VALUES], thr$steps, max_dd, inert_from))
  flush(stdout())

  warm_s <- matrix(NA_real_, nrow = reps, ncol = N_VALUES)
  cold_s <- matrix(NA_real_, nrow = reps, ncol = N_VALUES)
  warm_wall <- numeric(reps)
  cold_wall <- numeric(reps)

  for (r in seq_len(reps)) {
    warm <- path_at(sides, values)
    warm_s[r, ] <- warm$path$path$seconds
    warm_wall[r] <- warm$wall

    cold_one <- vector("list", N_VALUES)
    w <- 0
    for (k in seq_len(N_VALUES)) {
      one <- path_at(sides, values[k])
      cold_one[[k]] <- one$path
      cold_s[r, k] <- one$path$path$seconds
      w <- w + one$wall
    }
    cold_wall[r] <- w

    if (r == 1L) {
      wp <- warm$path$path
      cp <- do.call(rbind, lapply(cold_one, function(p) p$path))
      point_rows[[length(point_rows) + 1L]] <- data.frame(
        n_total = n_total, shape = shape, point = seq_len(N_VALUES),
        max_distance = values,
        status = wp$status, cold_status = cp$status,
        n_matched = wp$n_matched, cold_n_matched = cp$n_matched,
        warm_s = wp$seconds, cold_s = cp$seconds,
        warm_rounds = wp$n_rounds, cold_rounds = cp$n_rounds,
        warm_evaluated = wp$edges_evaluated,
        cold_evaluated = cp$edges_evaluated,
        pairs_added = wp$pairs_added,
        candidate_edges = wp$candidate_edges,
        total_distance = wp$total_distance,
        cold_total_distance = cp$total_distance,
        stringsAsFactors = FALSE
      )

      ## What the two sides have to agree on. Status and matched count exactly;
      ## the totals to a relative tolerance, because the warm side accumulates
      ## its arc costs across points and the cold side accumulates them from
      ## scratch.
      same_status <- identical(as.character(wp$status), as.character(cp$status))
      same_n <- identical(as.integer(wp$n_matched), as.integer(cp$n_matched))
      gaps <- vapply(seq_len(N_VALUES),
                     function(k) rel_gap(wp$total_distance[k],
                                         cp$total_distance[k]),
                     numeric(1))
      certified <- all(wp$certified[wp$status == "optimal"]) &&
        all(cp$certified[cp$status == "optimal"])
      ## The top value admits the unconstrained optimum, so the last point is
      ## the unconstrained problem solved under a cut that forbids nothing it
      ## uses.
      last_gap <- rel_gap(wp$total_distance[N_VALUES], ref_total)
      n_optimal <- sum(wp$status == "optimal")
    }
  }

  warm_point <- apply(warm_s, 2, min)
  cold_point <- apply(cold_s, 2, min)

  shape_rows[[length(shape_rows) + 1L]] <- data.frame(
    n_total = n_total, shape = shape, points = N_VALUES, reps = reps,
    optimal = n_optimal,
    warm_s = round(sum(warm_point), 3),
    cold_s = round(sum(cold_point), 3),
    speedup = round(sum(cold_point) / sum(warm_point), 2),
    warm_wall = round(min(warm_wall), 3),
    cold_wall = round(min(cold_wall), 3),
    wall_speedup = round(min(cold_wall) / min(warm_wall), 2),
    warm_rounds = sum(point_rows[[length(point_rows)]]$warm_rounds),
    cold_rounds = sum(point_rows[[length(point_rows)]]$cold_rounds),
    warm_evaluated = sum(point_rows[[length(point_rows)]]$warm_evaluated),
    cold_evaluated = sum(point_rows[[length(point_rows)]]$cold_evaluated),
    candidate_edges = point_rows[[length(point_rows)]]$candidate_edges[N_VALUES],
    max_dd = max_dd, inert_from = inert_from,
    ref_lazy_s = round(ref_wall, 3),
    same_status = same_status, same_n = same_n,
    worst_total_gap = signif(max(gaps), 3),
    last_vs_unconstrained = signif(last_gap, 3),
    certified = certified,
    stringsAsFactors = FALSE
  )

  row <- shape_rows[[length(shape_rows)]]
  cat(sprintf("    warm %8.3f s   cold %8.3f s   %5.2fx   (per-point seconds)\n",
              row$warm_s, row$cold_s, row$speedup))
  cat(sprintf("    warm %8.3f s   cold %8.3f s   %5.2fx   (wall clock)\n",
              row$warm_wall, row$cold_wall, row$wall_speedup))
  cat(sprintf("    rounds %d against %d, pairs evaluated %s against %s\n",
              row$warm_rounds, row$cold_rounds,
              format(row$warm_evaluated, big.mark = ",", scientific = FALSE),
              format(row$cold_evaluated, big.mark = ",", scientific = FALSE)))
  cat(sprintf("    %d of %d points optimal, status equal %s, counts equal %s, worst total gap %s\n\n",
              row$optimal, N_VALUES, row$same_status, row$same_n,
              format(row$worst_total_gap, digits = 3)))
  flush(stdout())

  write.csv(do.call(rbind, shape_rows), out_csv, row.names = FALSE)
  write.csv(do.call(rbind, point_rows), points_csv, row.names = FALSE)
}

table <- do.call(rbind, shape_rows)

cat("\n")
print(table[, c("n_total", "shape", "points", "optimal", "warm_s", "cold_s",
                "speedup", "warm_wall", "cold_wall", "wall_speedup")],
      row.names = FALSE)
cat("\n")
print(table[, c("n_total", "warm_rounds", "cold_rounds", "warm_evaluated",
                "cold_evaluated", "candidate_edges", "inert_from", "ref_lazy_s",
                "same_status", "same_n", "worst_total_gap",
                "last_vs_unconstrained", "certified")],
      row.names = FALSE)

bad <- !table$same_status | !table$same_n | !table$certified |
  table$worst_total_gap > 1e-12 | table$last_vs_unconstrained > 1e-12
if (any(bad)) {
  cat("\na shape's warm points and cold solves disagree; that is a defect\n")
  quit(save = "no", status = 1L)
}
cat("\nevery point is the matching an independent solve at that value finds\n")
cat("\nWrote", out_csv, "and", points_csv, "\n")
quit(save = "no", status = 0L)
