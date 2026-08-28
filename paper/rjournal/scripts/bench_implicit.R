## Time couplr's memory_mode = "implicit" edge-generation loop against the lazy
## and dense paths on the same synthetic problems the scaling table uses, check
## that it returns the dense path's assignment wherever dense can run, and
## record what the loop paid to get there.
##
## The implicit mode solves the complete problem without holding it: each row
## starts with its nearest admissible partners, the omitted pairs are priced
## against the duals the restricted master returns, a pair enters on a negative
## reduced cost, and the loop stops when none prices in. The duals then certify
## the sparse solution optimal for the complete problem. Two things have to be
## measured for that to be a result rather than an assertion: the assignment has
## to equal the dense one wherever both can run, and the pair counts have to say
## how little of the complete problem was ever touched.
##
## The dense and lazy arms are pinned to method = "jv", so those two differ only
## in memory mode. The implicit arm cannot be pinned: its restricted master is
## the flow model, and naming a solver under memory_mode = "implicit" is an
## error rather than a setting, so that arm carries its own solver by
## construction. That is a difference between the arms, and it is the mode's
## definition rather than a choice this script makes. A fourth arm solves the
## same materialized matrix with a flow solver, so the table can be read both
## ways: down one solver across two representations, and across two solvers on
## one representation. Without it the implicit row is a representation and a
## solver at once and neither can be attributed.
##
## Reproducible via:  Rscript paper/bench_implicit.R
##                    Rscript paper/bench_implicit.R --quick

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

## Single-core wall-clock, matching the rest of the paper's timings.
blas_set_num_threads(1); omp_set_num_threads(1)
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1",
           MKL_NUM_THREADS = "1", VECLIB_MAXIMUM_THREADS = "1")

source(file.path(repo_root, "paper", "bench_common.R"))

paper_dir <- file.path(repo_root, "paper")
out_csv   <- file.path(paper_dir, "implicit-results.csv")
equiv_csv <- file.path(paper_dir, "implicit-equivalence.csv")

## Every size the scaling table reports. The dense arm is run only where the
## matrix is affordable to materialize; that is a property of the machine, so
## the cell records why it stopped instead of being left out of the grid.
SIZES       <- c(500L, 2000L, 5000L, 10000L, 20000L, 50000L)
DENSE_SIZES <- c(500L, 2000L, 5000L, 10000L)
TIMEOUT_S   <- 3600   # per cell; a cell that exceeds it is recorded, not dropped

## A reduced grid, small enough that every arm runs to completion in seconds
## and every size carries the dense arm. It exercises the arms themselves; the
## article reads the full grid above.
if (any(commandArgs(TRUE) == "--quick")) {
  SIZES       <- c(300L, 600L)
  DENSE_SIZES <- SIZES
  TIMEOUT_S   <- 60
}

## ---- one timed cell ---------------------------------------------------------
## Returns the timing, the answer, and, for the implicit arm, the certificate
## and the pair counts the loop kept for itself.
## The arms, as (representation, solver) pairs. Reading the table sideways is
## what separates the two: `dense` and `lazy` are one solver over two
## representations, `dense` and `dense_csflow` are one representation under two
## solvers, and `implicit` is the mode as a user meets it, which is a
## representation and a solver at once because its restricted master is the
## flow model and naming a solver under it is an error rather than a setting.
mode_spec <- list(
  dense        = list(memory_mode = "dense",    method = "jv"),
  lazy         = list(memory_mode = "lazy",     method = "jv"),
  implicit     = list(memory_mode = "implicit", method = NULL),
  dense_csflow = list(memory_mode = "dense",    method = "csflow")
)

time_match <- function(d, mode) {
  spec <- mode_spec[[mode]]
  tr <- subset(d, treat == 1); ct <- subset(d, treat == 0)
  args <- list(left = tr, right = ct, vars = covars,
               distance = "mahalanobis", memory_mode = spec$memory_mode)
  if (!is.null(spec$method)) args$method <- spec$method

  ## The solve and everything read off it happen in one bounded call, so the
  ## matching itself never crosses back: only the summary below does.
  res <- bounded_call(function() {
    t0 <- proc.time()[["elapsed"]]
    m  <- do.call(match_couples, args)
    elapsed <- proc.time()[["elapsed"]] - t0
    cert <- m$certificate
    srch <- m$search
    list(
      elapsed     = elapsed,
      status      = "ok",
      total_cost  = sum(m$pairs$distance),
      n_pairs     = nrow(m$pairs),
      right_id    = m$pairs$right_id,
      certified   = if (is.null(cert)) NA else isTRUE(cert$certified_optimal),
      duality_gap = if (is.null(cert)) NA_real_ else cert$duality_gap,
      candidate_edges = if (is.null(srch)) NA_real_ else srch$candidate_edges,
      possible_edges  = if (is.null(srch)) NA_real_ else srch$possible_edges,
      edges_evaluated = if (is.null(srch)) NA_real_ else srch$edges_evaluated,
      n_rounds        = if (is.null(srch)) NA_integer_ else srch$n_rounds
    )
  }, TIMEOUT_S)

  if (!res$ok) {
    return(list(elapsed = NA_real_, status = res$status, total_cost = NA_real_,
                n_pairs = NA_integer_, right_id = NULL,
                certified = NA, duality_gap = NA_real_,
                candidate_edges = NA_real_, possible_edges = NA_real_,
                edges_evaluated = NA_real_, n_rounds = NA_integer_))
  }
  res$value
}

## ---- resume-safe accumulators ----------------------------------------------
results <- if (file.exists(out_csv)) {
  read.csv(out_csv, stringsAsFactors = FALSE)
} else {
  data.frame(n_total = integer(0), memory_mode = character(0),
             elapsed_s = numeric(0), status = character(0),
             total_cost = numeric(0), n_pairs = integer(0),
             certified_optimal = logical(0), duality_gap = numeric(0),
             candidate_edges = numeric(0), possible_edges = numeric(0),
             edges_evaluated = numeric(0), n_rounds = integer(0),
             stringsAsFactors = FALSE)
}

have_cell <- function(n, mode) {
  nrow(results) > 0 && any(results$n_total == n & results$memory_mode == mode)
}

record <- function(n, mode, r) {
  results <<- rbind(results, data.frame(
    n_total = n, memory_mode = mode,
    elapsed_s = if (is.na(r$elapsed)) NA_real_ else round(r$elapsed, 3),
    status = r$status, total_cost = r$total_cost, n_pairs = r$n_pairs,
    certified_optimal = r$certified, duality_gap = r$duality_gap,
    candidate_edges = r$candidate_edges, possible_edges = r$possible_edges,
    edges_evaluated = r$edges_evaluated, n_rounds = r$n_rounds,
    stringsAsFactors = FALSE
  ))
  write.csv(results, out_csv, row.names = FALSE)
}

## ---- the grid ---------------------------------------------------------------
## Each size is generated once and all of its arms are run against that one
## data frame, so an arm never differs from another by which sample it saw.
equiv <- data.frame(n_total = integer(0), cost_gap = numeric(0),
                    identical_pairing = logical(0),
                    certified_optimal = logical(0),
                    stringsAsFactors = FALSE)

for (n_total in SIZES) {
  cat("=== n_total =", n_total, "===\n"); flush.console()
  d <- make_data(n_total, seed = bench_seed(n_total))

  modes <- if (n_total %in% DENSE_SIZES) {
    c("dense", "lazy", "implicit", "dense_csflow")
  } else {
    c("lazy", "implicit")
  }

  held <- list()
  for (mode in modes) {
    if (have_cell(n_total, mode)) {
      cat(sprintf("  %-8s already in %s, skipped\n", mode, basename(out_csv)))
      next
    }
    r <- time_match(d, mode)
    held[[mode]] <- r
    if (identical(r$status, "ok")) {
      cat(sprintf("  %-8s %8.2f s  %d pairs  cost %.6f\n",
                  mode, r$elapsed, r$n_pairs, r$total_cost))
      ## Two different quantities, so they are printed as two different kinds
      ## of number. candidate_edges is the graph that was built, against the
      ## complete one that was not, and is a share. edges_evaluated counts
      ## distance evaluations over all rounds, so a pair priced in three rounds
      ## counts three times and the total can exceed the complete pair count;
      ## it is reported as a multiple of that count, not as a share of it.
      if (mode == "implicit" && !is.na(r$possible_edges) &&
          r$possible_edges > 0) {
        cat(sprintf("           graph built: %s of %s pairs (%.4f%%), %d rounds\n",
                    format(r$candidate_edges, big.mark = ","),
                    format(r$possible_edges, big.mark = ","),
                    100 * r$candidate_edges / r$possible_edges,
                    r$n_rounds))
        cat(sprintf("           distances computed: %s (%.2fx the complete pair count)\n",
                    format(r$edges_evaluated, big.mark = ","),
                    r$edges_evaluated / r$possible_edges))
      }
    } else {
      cat(sprintf("  %-8s %s\n", mode, r$status))
    }
    flush.console()
    record(n_total, mode, r)
  }

  ## The gate section C is judged by: identical to the dense solve on every
  ## problem small enough to run both.
  if (!is.null(held$dense) && !is.null(held$implicit) &&
      identical(held$dense$status, "ok") &&
      identical(held$implicit$status, "ok")) {
    gap  <- abs(held$dense$total_cost - held$implicit$total_cost)
    same <- identical(held$dense$right_id, held$implicit$right_id)
    cat(sprintf("  equivalence: identical pairing %s, cost gap %.3e, certified %s\n",
                same, gap, held$implicit$certified))
    equiv <- rbind(equiv, data.frame(
      n_total = n_total, cost_gap = gap, identical_pairing = same,
      certified_optimal = held$implicit$certified, stringsAsFactors = FALSE
    ))
    write.csv(equiv, equiv_csv, row.names = FALSE)
  }
  cat("\n"); flush.console()
}

## ---- summary ----------------------------------------------------------------
cat("\n--- timings ---\n")
print(results[, c("n_total", "memory_mode", "elapsed_s", "status",
                  "total_cost", "n_pairs")])

imp <- results[results$memory_mode == "implicit" & results$status == "ok", ]
if (nrow(imp) > 0) {
  ## `graph_pct` is the sparsity claim: the arcs the loop ended up holding
  ## against the arcs the complete problem has. `distances_x` is the price paid
  ## for it, as a multiple of the complete pair count, and it is above 1 here:
  ## at eight covariates the loop computes more distances than one full sweep
  ## would and still wins, because what it never builds is the graph.
  cat("\n--- what the loop built, and what it paid ---\n")
  imp$graph_pct   <- round(100 * imp$candidate_edges / imp$possible_edges, 4)
  imp$distances_x <- round(imp$edges_evaluated / imp$possible_edges, 2)
  print(imp[, c("n_total", "possible_edges", "candidate_edges", "graph_pct",
                "edges_evaluated", "distances_x", "n_rounds",
                "certified_optimal")])
}

if (nrow(equiv) > 0) {
  cat("\n--- equivalence against the dense solve ---\n")
  print(equiv)
  if (!all(equiv$identical_pairing)) {
    cat("\nNOTE: a size disagreed with the dense pairing. Equal total cost with a\n",
        "different pairing is a tie between equally optimal assignments; a\n",
        "different total cost is not, and is a failure of the mode.\n", sep = "")
  }
}

cat("\nWrote", out_csv, "and", equiv_csv, "\n")
