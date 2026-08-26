## How the edge-generation loop behaves away from the problem it was shown on.
##
## The implicit results in the article come from one cloud: eight independent
## normal covariates, treated shifted, one draw per size. That is one point of a
## space with several axes, and the quantities the loop is judged on -- how many
## rounds it takes, how much of the graph it ends up holding, how many distances
## it computes, and whether the certificate closes -- can move along any of
## them. This script sweeps those axes one at a time, several seeds deep, and
## records the distribution rather than a point.
##
## Four sweeps, each varying one thing against a fixed base of eight covariates,
## 10,000 units and no caliper:
##
##   dimension  2, 4, 8, 16, 32 covariates
##   cloud      independent normal, clustered, displaced treated, a coarse
##              lattice (near-tied distances), heavy tails, one contested core
##              (where the nearest-neighbour seed is far from sufficient), and a
##              shell of near-equal distances (where the pricing pass has little
##              to prune with)
##   caliper    none, the tightest value the problem is still feasible under,
##              and halfway between that and the unconstrained widest arc; the
##              tight end is a sparse and nearly disconnected admissibility
##              structure rather than a full one
##   size       2,000 to 50,000 units
##
## Every cell is run at several seeds. Where the dense path can afford the
## problem, the implicit answer is checked against it unit by unit, so a cloud
## that changes what the loop costs cannot quietly change what it returns.
##
## Reproducible via:  Rscript paper/bench_implicit_grid.R
##                    Rscript paper/bench_implicit_grid.R --quick
##                    Rscript paper/bench_implicit_grid.R --sweep=cloud
##
## Rows accumulate in paper/implicit-grid-runs.csv and a cell already in it is
## skipped, so a killed run resumes where it stopped.

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

for (p in c("R.utils", "RhpcBLASctl")) {
  if (!requireNamespace(p, quietly = TRUE)) {
    stop("This benchmark needs the ", p, " package: install.packages(\"", p,
         "\")", call. = FALSE)
  }
}
suppressPackageStartupMessages({
  library(R.utils)
  library(RhpcBLASctl)
  pkgload::load_all(repo_root, quiet = TRUE)
})

## Single-core wall-clock, matching the rest of the paper's timings.
blas_set_num_threads(1); omp_set_num_threads(1)
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1",
           MKL_NUM_THREADS = "1", VECLIB_MAXIMUM_THREADS = "1")
options(couplr.emoji = FALSE, warn = 1)

source(file.path(repo_root, "paper", "bench_common.R"))

paper_dir <- file.path(repo_root, "paper")
runs_csv  <- file.path(paper_dir, "implicit-grid-runs.csv")

argv   <- commandArgs(TRUE)
QUICK  <- any(argv == "--quick")
SWEEPS <- sub("^--sweep=", "", argv[grepl("^--sweep=", argv)])

TIMEOUT_S <- 3600
## The dense arm is the equivalence check, not a timing, so it runs only where
## the matrix is affordable.
DENSE_MAX_N <- 10000L
FEAS_TOL <- 1e-3

## ---- the sweeps -------------------------------------------------------------
base_cell <- list(cloud = "gaussian", dim = 8L, n_total = 10000L,
                  caliper = "none")

sweeps <- list(
  dimension = list(vary = "dim",     levels = c(2L, 4L, 8L, 16L, 32L), seeds = 5L),
  cloud     = list(vary = "cloud",   levels = names(point_clouds),     seeds = 5L),
  caliper   = list(vary = "caliper", levels = c("none", "boundary", "mid"),
                   seeds = 5L),
  size      = list(vary = "n_total",
                   levels = c(2000L, 5000L, 10000L, 20000L, 50000L), seeds = 3L)
)
if (QUICK) {
  base_cell$n_total <- 1200L
  sweeps <- list(
    dimension = list(vary = "dim", levels = c(2L, 8L), seeds = 2L),
    cloud     = list(vary = "cloud", levels = c("gaussian", "contested", "shell"),
                     seeds = 2L),
    caliper   = list(vary = "caliper", levels = c("none", "boundary"), seeds = 2L)
  )
}
if (length(SWEEPS)) sweeps <- sweeps[SWEEPS]

cell_seed <- function(cell, seed_index) {
  key <- paste(cell$cloud, cell$dim, cell$n_total, cell$caliper, seed_index,
               sep = "|")
  s <- 5381
  for (ch in utf8ToInt(key)) s <- (s * 33 + ch) %% 2147483647
  as.integer(s)
}

## ---- one solve --------------------------------------------------------------
## Warnings a caliper sweep produces by design: a value the data admits no
## complete matching under is what the bisection is looking for.
quietly <- function(expr) {
  withCallingHandlers(expr, warning = function(w) {
    m <- conditionMessage(w)
    if (grepl("no complete matching|no feasible full matching", m))
      invokeRestart("muffleWarning")
  })
}

run_mode <- function(cl, mode, max_distance, timeout_s = TIMEOUT_S) {
  args <- list(left = cl$left, right = cl$right, vars = cl$vars,
               distance = "mahalanobis", memory_mode = mode,
               check_costs = FALSE)
  if (is.finite(max_distance)) args$max_distance <- max_distance
  if (mode != "implicit") args$method <- "jv"

  gc(reset = TRUE, verbose = FALSE)
  res <- tryCatch(
    withTimeout({
      t0 <- proc.time()[["elapsed"]]
      m <- quietly(do.call(match_couples, args))
      list(m = m, elapsed = proc.time()[["elapsed"]] - t0, status = "ok")
    }, timeout = timeout_s, onTimeout = "error"),
    TimeoutException = function(e) list(m = NULL, elapsed = NA_real_,
                                        status = "timeout"),
    error = function(e) list(m = NULL, elapsed = NA_real_,
                             status = paste0("error: ", conditionMessage(e)))
  )
  ## R's own heap high-water mark over the call. It is not the process peak --
  ## the solver's C++ workspace is invisible to gc() -- and paper/bench_memory.R
  ## measures that separately; this is the part of the footprint R owns.
  heap_mb <- sum(gc(verbose = FALSE)[, "max used"] * c(56, 8)) / 1e6

  if (is.null(res$m)) {
    return(list(status = res$status, elapsed = NA_real_, heap_mb = heap_mb,
                n_pairs = NA_integer_, total_cost = NA_real_,
                certified = NA, duality_gap = NA_real_, n_rounds = NA_integer_,
                seed_width = NA_real_, candidate_edges = NA_real_,
                possible_edges = NA_real_, edges_evaluated = NA_real_,
                max_arc = NA_real_, right_id = NULL))
  }
  m <- res$m; cert <- m$certificate; srch <- m$search
  list(
    status = "ok", elapsed = res$elapsed, heap_mb = heap_mb,
    n_pairs = nrow(m$pairs), total_cost = sum(m$pairs$distance),
    certified   = if (is.null(cert)) NA else isTRUE(cert$certified_optimal),
    duality_gap = if (is.null(cert)) NA_real_ else cert$duality_gap,
    n_rounds        = if (is.null(srch)) NA_integer_ else srch$n_rounds,
    seed_width      = if (is.null(srch)) NA_real_ else srch$seed_width,
    candidate_edges = if (is.null(srch)) NA_real_ else srch$candidate_edges,
    possible_edges  = if (is.null(srch)) NA_real_ else srch$possible_edges,
    edges_evaluated = if (is.null(srch)) NA_real_ else srch$edges_evaluated,
    max_arc = if (nrow(m$pairs)) max(m$pairs$distance) else NA_real_,
    right_id = m$pairs$right_id
  )
}

## The tightest caliper the problem still admits a complete matching under.
## Feasibility is monotone in the caliper, so bisection converges on it and the
## upper bracket is always a value already found feasible.
tightest_feasible <- function(cl, hi, n_left, tol = FEAS_TOL) {
  lo <- 0
  while ((hi - lo) / hi > tol) {
    mid <- 0.5 * (lo + hi)
    got <- run_mode(cl, "lazy", mid)
    feasible <- identical(got$status, "ok") && isTRUE(got$n_pairs == n_left)
    if (feasible) hi <- mid else lo <- mid
  }
  hi
}

## ---- resume-safe accumulator ------------------------------------------------
runs <- if (file.exists(runs_csv)) {
  read.csv(runs_csv, stringsAsFactors = FALSE)
} else {
  data.frame()
}
have_cell <- function(sweep, cell, seed_index) {
  nrow(runs) > 0 &&
    any(runs$sweep == sweep & runs$cloud == cell$cloud & runs$dim == cell$dim &
          runs$n_total == cell$n_total & runs$caliper == cell$caliper &
          runs$seed_index == seed_index)
}

record <- function(sweep, cell, seed_index, seed, caliper_value, mode, r,
                   equal_to_dense = NA, cost_gap_vs_dense = NA_real_) {
  runs <<- rbind(runs, data.frame(
    sweep = sweep, cloud = cell$cloud, dim = cell$dim, n_total = cell$n_total,
    caliper = cell$caliper, caliper_value = caliper_value,
    seed_index = seed_index, seed = seed, memory_mode = mode,
    status = r$status, elapsed_s = round(r$elapsed, 4),
    heap_mb = round(r$heap_mb, 1),
    n_pairs = r$n_pairs, total_cost = r$total_cost,
    certified_optimal = r$certified, duality_gap = r$duality_gap,
    n_rounds = r$n_rounds, seed_width = r$seed_width,
    candidate_edges = r$candidate_edges, possible_edges = r$possible_edges,
    edges_evaluated = r$edges_evaluated,
    equal_to_dense = equal_to_dense, cost_gap_vs_dense = cost_gap_vs_dense,
    stringsAsFactors = FALSE
  ))
  write.csv(runs, runs_csv, row.names = FALSE)
}

cat("couplr edge-generation grid: the loop away from one cloud\n")
cat("repo: ", repo_root, "\n", sep = "")
cat("sweeps: ", paste(names(sweeps), collapse = ", "),
    if (QUICK) "   (quick)" else "", "\n\n", sep = "")

for (sweep_name in names(sweeps)) {
  sw <- sweeps[[sweep_name]]
  for (level in sw$levels) {
    cell <- base_cell
    cell[[sw$vary]] <- if (sw$vary == "caliper") as.character(level) else
      as.integer(level)
    if (sw$vary == "cloud") cell$cloud <- as.character(level)

    for (seed_index in seq_len(sw$seeds)) {
      if (have_cell(sweep_name, cell, seed_index)) {
        cat(sprintf("%s / %s / seed %d: already recorded, skipping\n",
                    sweep_name, as.character(level), seed_index))
        next
      }
      seed <- cell_seed(cell, seed_index)
      cl <- make_cloud(cell$cloud, cell$n_total, cell$dim, seed)
      n_left <- nrow(cl$left)

      cat(sprintf("=== %s: %s = %s   seed %d (%d x %d, %d covariates) ===\n",
                  sweep_name, sw$vary, as.character(level), seed_index,
                  n_left, nrow(cl$right), cell$dim))
      flush.console()

      ## The caliper grid is the problem's own. Its wide end is the widest arc
      ## the unconstrained optimum uses, which is where a caliper stops
      ## constraining anything; its tight end is the tightest value the problem
      ## is still feasible under, found by bisection below that.
      caliper_value <- Inf
      if (cell$caliper != "none") {
        ref <- run_mode(cl, "lazy", Inf)
        if (!identical(ref$status, "ok")) {
          cat("    unconstrained solve failed, skipping cell:", ref$status, "\n")
          record(sweep_name, cell, seed_index, seed, NA_real_, "lazy", ref)
          next
        }
        widest <- ref$max_arc
        thr <- tightest_feasible(cl, widest, n_left)
        ## The tight end is nudged one part in 1e9 above the threshold, as
        ## paper/bench_path.R nudges the top of its grid. A caliper set exactly
        ## at a pair's distance is a floating-point tie: the dense matrix and
        ## the lazy path each compute that distance in their own arithmetic and
        ## can land an ulp either side of the cut, so a cell sitting on it would
        ## measure the tie rather than the loop.
        caliper_value <- if (cell$caliper == "boundary") thr * (1 + 1e-9) else
          0.5 * (thr + widest)
        cat(sprintf("    caliper %s = %.6f (feasible from %.6f, inert from %.6f)\n",
                    cell$caliper, caliper_value, thr, widest))
        flush.console()
      }

      imp <- run_mode(cl, "implicit", caliper_value)
      lazy <- run_mode(cl, "lazy", caliper_value)

      dense <- if (cell$n_total <= DENSE_MAX_N) {
        run_mode(cl, "dense", caliper_value)
      } else {
        NULL
      }

      equal <- NA; gap <- NA_real_
      if (!is.null(dense) && identical(dense$status, "ok") &&
          identical(imp$status, "ok")) {
        equal <- identical(dense$right_id, imp$right_id)
        gap <- abs(dense$total_cost - imp$total_cost)
      }

      record(sweep_name, cell, seed_index, seed, caliper_value, "implicit", imp,
             equal, gap)
      record(sweep_name, cell, seed_index, seed, caliper_value, "lazy", lazy)
      if (!is.null(dense))
        record(sweep_name, cell, seed_index, seed, caliper_value, "dense", dense)

      if (identical(imp$status, "ok")) {
        cat(sprintf("    implicit %8.3f s  %d rounds  graph %.4f%% of %s pairs  distances %.2fx  gap %.3g\n",
                    imp$elapsed, imp$n_rounds,
                    100 * imp$candidate_edges / imp$possible_edges,
                    format(imp$possible_edges, big.mark = ","),
                    imp$edges_evaluated / imp$possible_edges, imp$duality_gap))
      } else {
        cat("    implicit", imp$status, "\n")
      }
      if (identical(lazy$status, "ok")) {
        cat(sprintf("    lazy     %8.3f s\n", lazy$elapsed))
      } else {
        cat("    lazy    ", lazy$status, "\n")
      }
      if (!is.null(dense)) {
        cat(sprintf("    dense    %8.3f s   identical pairing %s, cost gap %.3g\n",
                    dense$elapsed, equal, gap))
      }
      cat("\n"); flush.console()
    }
  }
}

## ---- what the sweeps say ----------------------------------------------------
if (!nrow(runs)) {
  cat("no runs recorded\n")
  quit(save = "no", status = 0L)
}

imp <- runs[runs$memory_mode == "implicit" & runs$status == "ok", ]
imp$graph_pct <- 100 * imp$candidate_edges / imp$possible_edges
imp$distances_x <- imp$edges_evaluated / imp$possible_edges

lazy <- runs[runs$memory_mode == "lazy" & runs$status == "ok", ]
key <- function(d) paste(d$sweep, d$cloud, d$dim, d$n_total, d$caliper,
                         d$seed_index)
imp$lazy_s <- lazy$elapsed_s[match(key(imp), key(lazy))]

summ <- do.call(rbind, lapply(split(imp, paste(imp$sweep, imp$cloud, imp$dim,
                                               imp$n_total, imp$caliper)),
                              function(g) {
  q <- function(x) stats::quantile(x, c(0.25, 0.5, 0.75), names = FALSE,
                                   na.rm = TRUE)
  s <- q(g$elapsed_s)
  data.frame(
    sweep = g$sweep[1], cloud = g$cloud[1], dim = g$dim[1],
    n_total = g$n_total[1], caliper = g$caliper[1], seeds = nrow(g),
    rounds_min = min(g$n_rounds), rounds_med = median(g$n_rounds),
    rounds_max = max(g$n_rounds),
    graph_pct_med = round(median(g$graph_pct), 4),
    graph_pct_max = round(max(g$graph_pct), 4),
    distances_x_med = round(median(g$distances_x), 3),
    distances_x_max = round(max(g$distances_x), 3),
    implicit_s_med = round(s[2], 3), implicit_s_q25 = round(s[1], 3),
    implicit_s_q75 = round(s[3], 3),
    lazy_s_med = round(median(g$lazy_s, na.rm = TRUE), 3),
    speedup_med = round(median(g$lazy_s / g$elapsed_s, na.rm = TRUE), 2),
    heap_mb_med = round(median(g$heap_mb), 1),
    worst_gap = max(g$duality_gap, na.rm = TRUE),
    all_certified = all(g$certified_optimal %in% TRUE),
    equal_to_dense = if (all(is.na(g$equal_to_dense))) NA
      else all(g$equal_to_dense[!is.na(g$equal_to_dense)]),
    stringsAsFactors = FALSE
  )
}))
rownames(summ) <- NULL
summ <- summ[order(summ$sweep, summ$n_total, summ$dim, summ$cloud,
                   summ$caliper), ]
write.csv(summ, file.path(paper_dir, "implicit-grid-results.csv"),
          row.names = FALSE)

cat("\n--- the loop, by cell ---\n")
print(summ[, c("sweep", "cloud", "dim", "n_total", "caliper", "seeds",
               "rounds_med", "rounds_max", "graph_pct_med", "graph_pct_max",
               "distances_x_med", "implicit_s_med", "lazy_s_med",
               "speedup_med")], row.names = FALSE)

cat("\n--- the certificate, by cell ---\n")
print(summ[, c("sweep", "cloud", "dim", "n_total", "caliper", "worst_gap",
               "all_certified", "equal_to_dense")], row.names = FALSE)

failed <- runs[runs$status != "ok" & runs$memory_mode == "implicit", ]
if (nrow(failed)) {
  cat("\n--- cells the loop did not finish ---\n")
  print(failed[, c("sweep", "cloud", "dim", "n_total", "caliper", "seed_index",
                   "status")], row.names = FALSE)
}

bad <- summ[!summ$all_certified |
              (!is.na(summ$equal_to_dense) & !summ$equal_to_dense), ]
if (nrow(bad)) {
  cat("\na cell either did not certify or disagreed with the dense solve\n")
  print(bad, row.names = FALSE)
  quit(save = "no", status = 1L)
}

cat("\nWrote", runs_csv, "and implicit-grid-results.csv\n")
quit(save = "no", status = 0L)
