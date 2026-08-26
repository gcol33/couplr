## What each matching path actually costs in memory.
##
## The article argues that memory is the limiting resource and then reports the
## share of pairs a mode holds, which is a proxy. This measures the thing
## itself: peak resident set size of an R process that does one matching and
## nothing else, for every mode and every comparator, against a baseline process
## that loads the same packages and matches nothing.
##
## Peak RSS is a property of a process, not of an expression, so each cell runs
## in a fresh R process launched by this script and measured from outside it by
## the platform's `time` utility. That is what makes the number include
## everything the review asks for: the R objects, the C++ workspace, the copies
## made converting between them, whatever the garbage collector had not
## reclaimed at the peak, and the restricted master's own structures. It is also
## why the cells cannot share a session: one process's peak would carry the
## previous cell's high-water mark.
##
## The baseline cell is what makes the rest readable. An empty R process with
## the packages loaded is 100-200 MB before any matching happens, so the
## interesting quantity is the increment over that, and it is reported beside
## the total.
##
## Reproducible via:  Rscript paper/bench_memory.R
##                    Rscript paper/bench_memory.R --quick
##
## Rows accumulate in paper/memory-results.csv and a cell already in it is
## skipped, so a killed run resumes where it stopped.

repo_root <- if (file.exists("DESCRIPTION")) {
  normalizePath(".", winslash = "/", mustWork = TRUE)
} else if (basename(getwd()) == "paper" && file.exists("../DESCRIPTION")) {
  normalizePath("..", winslash = "/", mustWork = TRUE)
} else {
  stop("Run this script from the package root or the paper directory.")
}

argv <- commandArgs(TRUE)
arg_value <- function(name, default = NA_character_) {
  hit <- argv[grepl(paste0("^--", name, "="), argv)]
  if (!length(hit)) return(default)
  sub(paste0("^--", name, "="), "", hit[1])
}
IS_WORKER <- any(argv == "--worker")
QUICK     <- any(argv == "--quick")

`%||%` <- function(a, b) if (is.null(a)) b else a

options(pkg.build_extra_flags = FALSE)

## ---- what a cell is ---------------------------------------------------------
## Each arm is one call. `baseline` matches nothing: it is the floor every other
## arm in the same column is read against. `dense_matrix` allocates the cost
## matrix and stops, which separates what the representation costs from what the
## solver costs on top of it.
arm_needs <- list(
  baseline      = character(0),
  baseline_alt  = c("MatchIt", "optmatch"),
  dense_matrix  = character(0),
  dense         = character(0),
  lazy          = character(0),
  implicit      = character(0),
  optmatch      = "optmatch",
  MatchIt       = c("MatchIt", "optmatch")
)

## An arm is read against the floor of a process that loaded the same packages
## and matched nothing, so the increment is the matching and not the library.
arm_baseline <- c(dense_matrix = "baseline", dense = "baseline",
                  lazy = "baseline", implicit = "baseline",
                  optmatch = "baseline_alt", MatchIt = "baseline_alt")

run_arm <- function(arm, n_total) {
  source(file.path(repo_root, "paper", "bench_common.R"), local = TRUE)
  if (arm %in% c("baseline", "baseline_alt")) {
    return(list(n_pairs = 0L, total_cost = 0))
  }
  d <- make_data(n_total, seed = bench_seed(n_total))
  tr <- subset(d, treat == 1); ct <- subset(d, treat == 0)

  if (arm == "dense_matrix") {
    cm <- compute_distances(tr, ct, vars = covars, distance = "mahalanobis",
                            memory_mode = "dense")$cost_matrix
    return(list(n_pairs = 0L, total_cost = sum(dim(cm))))
  }
  if (arm %in% c("dense", "lazy", "implicit")) {
    args <- list(left = tr, right = ct, vars = covars,
                 distance = "mahalanobis", memory_mode = arm)
    if (arm != "implicit") args$method <- "jv"
    m <- do.call(match_couples, args)
    return(list(n_pairs = nrow(m$pairs), total_cost = sum(m$pairs$distance)))
  }
  if (arm == "optmatch") {
    options(optmatch_max_problem_size = Inf)
    m <- optmatch::pairmatch(form, data = d, controls = 1)
    return(list(n_pairs = sum(!is.na(m)) / 2, total_cost = NA_real_))
  }
  if (arm == "MatchIt") {
    options(optmatch_max_problem_size = Inf)
    m <- MatchIt::matchit(form, data = d, method = "optimal",
                          distance = "mahalanobis", ratio = 1)
    return(list(n_pairs = sum(m$weights > 0) / 2, total_cost = NA_real_))
  }
  stop("unknown arm: ", arm)
}

## ---- the worker -------------------------------------------------------------
## One process, one cell. Everything the arm needs is loaded before the timer
## starts, so the baseline cell of the same column carries the same loads.
if (IS_WORKER) {
  arm <- arg_value("arm")
  n_total <- as.integer(arg_value("n"))
  out <- arg_value("out")

  suppressPackageStartupMessages({
    for (p in arm_needs[[arm]]) library(p, character.only = TRUE)
    pkgload::load_all(repo_root, quiet = TRUE)
    if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
      RhpcBLASctl::blas_set_num_threads(1); RhpcBLASctl::omp_set_num_threads(1)
    }
  })
  Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1",
             MKL_NUM_THREADS = "1", VECLIB_MAXIMUM_THREADS = "1")

  gc(reset = TRUE, full = TRUE, verbose = FALSE)
  t0 <- proc.time()[["elapsed"]]
  res <- tryCatch(run_arm(arm, n_total),
                  error = function(e) list(n_pairs = NA_integer_,
                                           total_cost = NA_real_,
                                           error = conditionMessage(e)))
  elapsed <- proc.time()[["elapsed"]] - t0
  heap_mb <- sum(gc(verbose = FALSE)[, "max used"] * c(56, 8)) / 1e6

  write.csv(data.frame(
    arm = arm, n_total = n_total, elapsed_s = round(elapsed, 3),
    heap_peak_mb = round(heap_mb, 1),
    n_pairs = res$n_pairs %||% NA_integer_,
    status = if (is.null(res$error)) "ok" else paste0("error: ", res$error),
    stringsAsFactors = FALSE
  ), out, row.names = FALSE)
  quit(save = "no", status = 0L)
}

## ---- the parent -------------------------------------------------------------
paper_dir <- file.path(repo_root, "paper")
out_csv   <- file.path(paper_dir, "memory-results.csv")

## The parent loads the package for one reason: the footprint estimate the
## memory dispatcher warns from is read out of it rather than restated here.
suppressPackageStartupMessages(pkgload::load_all(repo_root, quiet = TRUE))

sizes <- if (QUICK) c(1000L, 2000L) else c(2000L, 5000L, 10000L, 20000L)
arms  <- if (QUICK) c("baseline", "dense_matrix", "dense", "implicit") else
  names(arm_needs)
TIMEOUT_S <- if (QUICK) 300 else 1800

## The platform's `time` utility is what sees the peak: R cannot report its own
## process high-water mark portably, and `gc()` sees only the R heap.
peak_reader <- switch(Sys.info()[["sysname"]],
  Darwin = list(
    cmd = "/usr/bin/time", pre = "-l",
    parse = function(lines) {
      hit <- grep("maximum resident set size", lines, value = TRUE)
      if (!length(hit)) return(NA_real_)
      as.numeric(sub("^[^0-9]*([0-9]+).*$", "\\1", hit[1])) / 1e6  # bytes -> MB
    }),
  Linux = list(
    cmd = "/usr/bin/time", pre = "-v",
    parse = function(lines) {
      hit <- grep("Maximum resident set size", lines, value = TRUE)
      if (!length(hit)) return(NA_real_)
      as.numeric(sub("^.*:\\s*([0-9]+).*$", "\\1", hit[1])) / 1e3  # kB -> MB
    }),
  NULL
)
if (is.null(peak_reader) || !file.exists(peak_reader$cmd)) {
  message("No external peak-RSS reader on this platform; the process peak ",
          "column will be NA and only the R heap peak is measured. Run this ",
          "script on the machine the paper's timings come from.")
  peak_reader <- NULL
}

rscript <- file.path(R.home("bin"), "Rscript")
script  <- file.path(paper_dir, "bench_memory.R")

## The summary block below rewrites this file with three derived columns. A
## resumed run reads its own measurements back and re-derives them, so they are
## dropped here rather than being carried into a row that does not have them.
DERIVED <- c("baseline_arm", "baseline_mb", "over_baseline_mb")
results <- if (file.exists(out_csv)) {
  prior <- read.csv(out_csv, stringsAsFactors = FALSE)
  prior[, setdiff(names(prior), DERIVED), drop = FALSE]
} else {
  data.frame()
}
have_cell <- function(arm, n) {
  nrow(results) > 0 && any(results$arm == arm & results$n_total == n)
}

cat("couplr memory benchmark: peak resident set size, one process per cell\n")
cat("repo: ", repo_root, "\n", sep = "")
cat("sizes: ", paste(sizes, collapse = ", "), "\n\n", sep = "")

for (n_total in sizes) {
  for (arm in arms) {
    if (have_cell(arm, n_total)) {
      cat(sprintf("  %-13s n=%6d already recorded, skipping\n", arm, n_total))
      next
    }
    tmp <- tempfile(fileext = ".csv")
    ## Every path here can carry a space -- the repository, the temporary file,
    ## R's own bin directory -- and system2() does not quote for us.
    worker_args <- c(shQuote(script), "--worker", paste0("--arm=", arm),
                     paste0("--n=", n_total), shQuote(paste0("--out=", tmp)))
    cmd  <- if (is.null(peak_reader)) rscript else peak_reader$cmd
    args <- if (is.null(peak_reader)) worker_args else
      c(peak_reader$pre, shQuote(rscript), worker_args)

    t0 <- proc.time()[["elapsed"]]
    lines <- suppressWarnings(system2(cmd, args, stdout = TRUE, stderr = TRUE,
                                      timeout = TIMEOUT_S))
    wall <- proc.time()[["elapsed"]] - t0
    peak_mb <- if (is.null(peak_reader)) NA_real_ else peak_reader$parse(lines)

    row <- if (file.exists(tmp)) {
      read.csv(tmp, stringsAsFactors = FALSE)
    } else {
      data.frame(arm = arm, n_total = n_total, elapsed_s = NA_real_,
                 heap_peak_mb = NA_real_, n_pairs = NA_integer_,
                 status = "worker produced no result", stringsAsFactors = FALSE)
    }
    unlink(tmp)
    row$peak_rss_mb <- round(peak_mb, 1)
    row$wall_s <- round(wall, 2)
    ## What the package would predict for the dense matrix at this shape: the
    ## bytes the matrix itself needs, and the four-fold estimate the memory
    ## dispatcher warns from.
    n_left <- round(n_total / 3); n_right <- n_total - n_left
    row$matrix_mb <- round(n_left * n_right * 8 / 1e6, 1)
    row$estimate_mb <- round(estimate_dense_matrix_mb(n_left, n_right), 1)

    results <- if (nrow(results)) rbind(results, row) else row
    write.csv(results, out_csv, row.names = FALSE)

    cat(sprintf("  %-13s n=%6d  peak %8.1f MB   heap %7.1f MB   %6.1f s   %s\n",
                arm, n_total, row$peak_rss_mb, row$heap_peak_mb, row$elapsed_s,
                row$status))
    flush.console()
  }
  cat("\n")
}

## ---- what the cells say -----------------------------------------------------
if (!nrow(results)) {
  cat("no cells recorded\n")
  quit(save = "no", status = 0L)
}

floors <- results[results$arm %in% c("baseline", "baseline_alt"),
                  c("n_total", "arm", "peak_rss_mb")]
tab <- results
tab$baseline_arm <- unname(arm_baseline[tab$arm])
tab$baseline_mb <- floors$peak_rss_mb[match(paste(tab$n_total, tab$baseline_arm),
                                            paste(floors$n_total, floors$arm))]
tab$over_baseline_mb <- round(tab$peak_rss_mb - tab$baseline_mb, 1)
tab <- tab[order(tab$n_total, tab$arm), ]
write.csv(tab, out_csv, row.names = FALSE)

cat("\n--- peak resident set size ---\n")
print(tab[, c("n_total", "arm", "peak_rss_mb", "over_baseline_mb",
              "heap_peak_mb", "matrix_mb", "estimate_mb", "elapsed_s",
              "status")], row.names = FALSE)

dm <- tab[tab$arm == "dense_matrix" & !is.na(tab$over_baseline_mb), ]
if (nrow(dm)) {
  cat("\n--- the dispatcher's four-fold estimate against the measurement ---\n")
  dm$measured_over_matrix <- round(dm$over_baseline_mb / dm$matrix_mb, 2)
  print(dm[, c("n_total", "matrix_mb", "over_baseline_mb",
               "measured_over_matrix", "estimate_mb")], row.names = FALSE)
}

cat("\nWrote", out_csv, "\n")
quit(save = "no", status = 0L)
