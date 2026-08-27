## What the automatic solver rules are worth away from the regime they were
## drawn on.
##
## The published solver figure varies one thing: problem size, on square dense
## integer costs drawn uniformly. The rules `method = "auto"` reads are stated
## over properties that figure holds fixed -- the aspect ratio, how much of the
## matrix is finite, whether the finite part is one component or several, the
## cost type, the cost distribution, and whether the costs are a metric. This
## script crosses those properties and asks, in each cell, whether the solver
## the rules pick is the one that wins.
##
## Design. Every cell is a (regime, forbidden pattern, shape) triple from
## `paper/bench_common.R`. Each cell is generated as several independent
## instances, each instance is timed several times, and every run is written
## out; the medians and quartiles this script reports are taken across
## instances, so the spread is problem-to-problem variation rather than clock
## noise. Every solver in the panel sees the same instance.
##
## Every cell also records what `explain_dispatch()` decides and why, so the
## comparison is between a rule that fired and the timings of the panel it was
## choosing from, and not between a rule and a solver named here.
##
## Reproducible via:  Rscript paper/bench_regimes.R
##                    Rscript paper/bench_regimes.R --quick
##                    Rscript paper/bench_regimes.R --tier=base
##
## Rows accumulate in paper/regime-runs.csv and a run already in it is skipped,
## so a killed run resumes where it stopped. Move the file aside to force a full
## re-run.

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

## A 500-row instance solves in single-digit milliseconds, which `proc.time()`
## cannot resolve. microbenchmark reads the platform's high-resolution counter
## and returns each repetition separately, which is what the raw rows hold.
HAVE_MB <- requireNamespace("microbenchmark", quietly = TRUE)
if (!HAVE_MB) {
  warning("microbenchmark is not installed; timings fall back to proc.time(), ",
          "whose resolution is coarse against a millisecond solve.",
          call. = FALSE)
}

## Single-core wall-clock, matching the rest of the paper's timings.
blas_set_num_threads(1); omp_set_num_threads(1)
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1",
           MKL_NUM_THREADS = "1", VECLIB_MAXIMUM_THREADS = "1")

source(file.path(repo_root, "paper", "bench_common.R"))

paper_dir <- file.path(repo_root, "paper")
runs_csv  <- file.path(paper_dir, "regime-runs.csv")
cells_csv <- file.path(paper_dir, "regime-cells.csv")

argv  <- commandArgs(TRUE)
QUICK <- any(argv == "--quick")
TIER_ARG <- sub("^--tier=", "", argv[grepl("^--tier=", argv)])

## ---- the panel --------------------------------------------------------------
## `hk01` solves a cardinality problem and is optimal only where there is no
## cost scale to exploit, so it is run where that holds and left out elsewhere
## rather than recorded as a solver that returns the wrong number.
panel <- list(
  list(method = "auto",            applies = function(p) TRUE),
  list(method = "jv",              applies = function(p) TRUE),
  list(method = "sap",             applies = function(p) TRUE),
  list(method = "sap_dense",       applies = function(p) TRUE),
  list(method = "lapmod",          applies = function(p) TRUE),
  list(method = "ramshaw_tarjan",  applies = function(p) TRUE),
  list(method = "hungarian",       applies = function(p) TRUE),
  list(method = "auction_scaled",  applies = function(p) TRUE),
  list(method = "csa",             applies = function(p) TRUE),
  list(method = "csflow",          applies = function(p) TRUE),
  list(method = "network_simplex", applies = function(p) TRUE),
  list(method = "hk01",
       applies = function(p) p$cost_type %in% c("binary", "constant"))
)
fast_panel <- Filter(function(x) x$method %in%
                       c("auto", "jv", "sap", "lapmod", "ramshaw_tarjan"),
                     panel)

## ---- the grid ---------------------------------------------------------------
## The base tier crosses everything against the whole panel at a size every
## solver in it can reach. The large tier repeats the crossing at a size only
## the scalable solvers reach, which is where a rule that holds at 500 rows and
## fails at 1500 would show.
tiers <- list(
  base = list(
    shapes    = list(c(500L, 500L), c(500L, 1500L), c(500L, 5000L)),
    regimes   = names(cost_regimes),
    patterns  = names(forbidden_patterns),
    panel     = panel,
    instances = 3L, reps = 3L, timeout_s = 120
  ),
  large = list(
    shapes    = list(c(1500L, 1500L), c(1500L, 4500L), c(1000L, 10000L)),
    regimes   = c("int_uniform", "dbl_uniform", "binary", "heavy_tailed",
                  "metric_clustered"),
    patterns  = c("none", "random_25"),
    panel     = fast_panel,
    instances = 2L, reps = 2L, timeout_s = 600
  )
)

if (QUICK) {
  tiers <- list(base = list(
    shapes    = list(c(60L, 60L), c(60L, 240L)),
    regimes   = c("int_uniform", "binary", "metric_clustered"),
    patterns  = c("none", "random_25", "block_4"),
    panel     = panel,
    instances = 2L, reps = 2L, timeout_s = 60
  ))
}
if (length(TIER_ARG)) tiers <- tiers[TIER_ARG]

## The instance seed is a function of everything that defines the instance, so
## the same cell regenerates the same problems in any session and two cells
## never share a draw.
instance_seed <- function(tier, regime, pattern, n_rows, n_cols, instance) {
  key <- paste(tier, regime, pattern, n_rows, n_cols, instance, sep = "|")
  ## A 32-bit digest of the key, so the seed is stable across platforms without
  ## depending on a hashing package.
  chars <- utf8ToInt(key)
  s <- 5381
  for (ch in chars) s <- (s * 33 + ch) %% 2147483647
  as.integer(s)
}

## ---- one instance, one solver, `reps` timed runs -----------------------------
## The first call is untimed: it establishes that the solver accepts the problem,
## records what it returns, and pays whatever allocation the first call pays.
## The repetitions are then timed on a clock with sub-millisecond resolution,
## because a 500-row instance solves in single-digit milliseconds and
## `proc.time()` cannot resolve that. Each repetition is written out on its own
## row.
measure <- function(cost, method, reps, timeout_s) {
  probe <- tryCatch(
    withTimeout({
      t0 <- proc.time()[["elapsed"]]
      res <- assignment(cost, method = method)
      list(seconds = proc.time()[["elapsed"]] - t0, status = "ok",
           total_cost = res$total_cost, n_matched = sum(res$match > 0L))
    }, timeout = timeout_s, onTimeout = "error"),
    TimeoutException = function(e)
      list(seconds = NA_real_, status = "timeout",
           total_cost = NA_real_, n_matched = NA_integer_),
    error = function(e)
      list(seconds = NA_real_, status = paste0("error: ", conditionMessage(e)),
           total_cost = NA_real_, n_matched = NA_integer_)
  )
  if (!identical(probe$status, "ok")) {
    return(list(seconds = NA_real_, status = probe$status,
                total_cost = probe$total_cost, n_matched = probe$n_matched))
  }
  secs <- if (HAVE_MB) {
    mb <- microbenchmark::microbenchmark(assignment(cost, method = method),
                                         times = reps, unit = "s")
    as.numeric(mb$time) / 1e9
  } else {
    vapply(seq_len(reps), function(i) {
      t0 <- proc.time()[["elapsed"]]
      invisible(assignment(cost, method = method))
      proc.time()[["elapsed"]] - t0
    }, numeric(1))
  }
  list(seconds = secs, status = rep("ok", length(secs)),
       total_cost = probe$total_cost, n_matched = probe$n_matched)
}

## ---- resume-safe accumulators ----------------------------------------------
runs <- if (file.exists(runs_csv)) {
  read.csv(runs_csv, stringsAsFactors = FALSE)
} else {
  data.frame()
}
cells <- if (file.exists(cells_csv)) {
  read.csv(cells_csv, stringsAsFactors = FALSE)
} else {
  data.frame()
}

have_run <- function(tier, regime, pattern, n_rows, n_cols, instance, method) {
  nrow(runs) > 0 &&
    any(runs$tier == tier & runs$regime == regime & runs$pattern == pattern &
          runs$n_rows == n_rows & runs$n_cols == n_cols &
          runs$instance == instance & runs$method == method)
}

cat("couplr solver grid: what the automatic rules are worth off the diagonal\n")
cat("repo: ", repo_root, "\n", sep = "")
cat("tiers: ", paste(names(tiers), collapse = ", "),
    if (QUICK) "   (quick)" else "", "\n\n", sep = "")

for (tier_name in names(tiers)) {
  tier <- tiers[[tier_name]]
  for (shape in tier$shapes) {
    n_rows <- shape[1]; n_cols <- shape[2]
    for (regime in tier$regimes) {
      for (pattern in tier$patterns) {
        cat(sprintf("=== %s  %d x %d  %s / %s ===\n", tier_name, n_rows, n_cols,
                    regime, pattern))
        flush.console()

        for (instance in seq_len(tier$instances)) {
          seed <- instance_seed(tier_name, regime, pattern, n_rows, n_cols,
                                instance)
          todo <- Filter(function(x) !have_run(tier_name, regime, pattern,
                                               n_rows, n_cols, instance,
                                               x$method), tier$panel)
          prob <- NULL
          if (length(todo)) {
            prob <- make_cost_problem(regime, n_rows, n_cols, pattern, seed)
            todo <- Filter(function(x) x$applies(prob), todo)
          }
          if (!length(todo)) {
            cat(sprintf("  instance %d: already recorded, skipping\n", instance))
            next
          }

          ## The dispatch decision is a property of the instance, so it is read
          ## once per instance and off the same matrix the panel is timed on.
          dec <- explain_dispatch(prob$cost)
          if (instance == 1L) {
            keep <- nrow(cells) == 0 ||
              !any(cells$tier == tier_name & cells$regime == regime &
                     cells$pattern == pattern & cells$n_rows == n_rows &
                     cells$n_cols == n_cols)
            if (keep) {
              cells <- rbind(cells, data.frame(
                tier = tier_name, regime = regime, pattern = pattern,
                n_rows = n_rows, n_cols = n_cols,
                cost_type = prob$cost_type, distribution = prob$distribution,
                metric = prob$metric, components = prob$components,
                finite_share = round(prob$finite_share, 4),
                auto_method = dec$method, auto_rule = dec$rule,
                auto_condition = dec$condition,
                stringsAsFactors = FALSE
              ))
              write.csv(cells, cells_csv, row.names = FALSE)
            }
          }

          for (entry in todo) {
            method <- entry$method
            got <- measure(prob$cost, method, tier$reps, tier$timeout_s)
            secs <- got$seconds
            ok <- got$status == "ok"
            new <- data.frame(
              tier = tier_name, regime = regime, pattern = pattern,
              n_rows = n_rows, n_cols = n_cols, instance = instance,
              seed = seed, method = method,
              rep = seq_along(secs),
              seconds = signif(secs, 7),
              status = got$status,
              total_cost = got$total_cost,
              n_matched = got$n_matched,
              auto_method = dec$method, auto_rule = dec$rule,
              cost_type = prob$cost_type, distribution = prob$distribution,
              metric = prob$metric, components = prob$components,
              finite_share = round(prob$finite_share, 4),
              stringsAsFactors = FALSE
            )
            runs <- if (nrow(runs)) rbind(runs, new) else new
            write.csv(runs, runs_csv, row.names = FALSE)

            cat(sprintf("  i%d %-15s %s\n", instance, method,
                        if (any(ok)) sprintf("%9.4f s", median(secs[ok]))
                        else got$status[1]))
            flush.console()
          }
        }
        cat("\n"); flush.console()
      }
    }
  }
}

## ---- what the grid says -----------------------------------------------------
if (!nrow(runs)) {
  cat("no runs recorded\n")
  quit(save = "no", status = 0L)
}

ok <- runs[runs$status == "ok", ]
cell_key <- function(df) paste(df$tier, df$regime, df$pattern, df$n_rows,
                               df$n_cols, sep = "|")

## An instance's time for a method is the median of its repetitions; a method's
## time in a cell is the median across instances, with the quartiles reported
## beside it. That is the order the review asks for: repetitions inside
## instances, spread across instances.
per_instance <- aggregate(seconds ~ tier + regime + pattern + n_rows + n_cols +
                            instance + method + auto_method + auto_rule,
                          data = ok, FUN = median)

summ <- do.call(rbind, lapply(split(per_instance, list(cell_key(per_instance),
                                                       per_instance$method),
                                    drop = TRUE), function(g) {
  q <- stats::quantile(g$seconds, c(0.25, 0.5, 0.75), names = FALSE)
  data.frame(tier = g$tier[1], regime = g$regime[1], pattern = g$pattern[1],
             n_rows = g$n_rows[1], n_cols = g$n_cols[1], method = g$method[1],
             auto_method = g$auto_method[1], auto_rule = g$auto_rule[1],
             instances = nrow(g), median_s = q[2], q25_s = q[1], q75_s = q[3],
             stringsAsFactors = FALSE)
}))
rownames(summ) <- NULL
write.csv(summ, file.path(paper_dir, "regime-results.csv"), row.names = FALSE)

## The comparison the rules are on trial for, and it is two comparisons rather
## than one. `ratio` is what a caller pays for `"auto"`: it carries the probing
## pass, which on a small problem can be a large share of a short solve.
## `picked_ratio` is what the rule's *choice* costs, the solver it names timed as
## a named solver against the cell's fastest, with no probe in either number.
## Reporting only the first would charge the rules for the probe; reporting only
## the second would hide what the probe costs.
verdict <- do.call(rbind, lapply(split(summ, cell_key(summ)), function(g) {
  named <- g[g$method != "auto", ]
  auto  <- g[g$method == "auto", ]
  if (!nrow(named) || !nrow(auto)) return(NULL)
  best <- named[which.min(named$median_s), ]
  picked <- named[named$method == auto$auto_method[1], ]
  picked_s <- if (nrow(picked)) picked$median_s[1] else NA_real_
  data.frame(tier = g$tier[1], regime = g$regime[1], pattern = g$pattern[1],
             shape = sprintf("%d x %d", g$n_rows[1], g$n_cols[1]),
             auto_rule = auto$auto_rule[1], auto_picks = auto$auto_method[1],
             auto_s = round(auto$median_s, 4),
             picked_s = round(picked_s, 4),
             best_method = best$method, best_s = round(best$median_s, 4),
             ratio = round(auto$median_s / best$median_s, 2),
             picked_ratio = round(picked_s / best$median_s, 2),
             stringsAsFactors = FALSE)
}))
rownames(verdict) <- NULL
verdict <- verdict[order(-verdict$ratio), ]
write.csv(verdict, file.path(paper_dir, "regime-verdict.csv"), row.names = FALSE)

cat("\n--- where the dispatched solver is furthest off the cell's best ---\n")
print(utils::head(verdict, 20), row.names = FALSE)

cat("\n--- how often each rule fires, and what it costs ---\n")
by_rule <- do.call(rbind, lapply(split(verdict, verdict$auto_rule), function(g)
  data.frame(rule = g$auto_rule[1], solver = g$auto_picks[1], cells = nrow(g),
             picked_the_best = sum(g$auto_picks == g$best_method),
             median_picked_ratio = round(median(g$picked_ratio, na.rm = TRUE), 2),
             worst_picked_ratio = round(max(g$picked_ratio, na.rm = TRUE), 2),
             median_auto_ratio = round(median(g$ratio), 2),
             worst_auto_ratio = round(max(g$ratio), 2),
             stringsAsFactors = FALSE)))
print(by_rule, row.names = FALSE)

## Every solver in a cell solves the same instance, so their totals have to
## agree. A disagreement is a defect in a solver, not a property of the regime,
## and it is reported as one.
cat("\n--- do the solvers agree on the optimum ---\n")
agree <- do.call(rbind, lapply(split(ok, paste(cell_key(ok), ok$instance)),
                               function(g) {
  g <- g[!is.na(g$total_cost), ]
  if (nrow(g) < 2) return(NULL)
  ref <- median(g$total_cost)
  data.frame(cell = paste(g$tier[1], g$regime[1], g$pattern[1],
                          sprintf("%dx%d", g$n_rows[1], g$n_cols[1])),
             instance = g$instance[1],
             worst_rel_gap = max(abs(g$total_cost - ref) / pmax(1, abs(ref))),
             worst_method = g$method[which.max(abs(g$total_cost - ref))],
             stringsAsFactors = FALSE)
}))
if (is.null(agree)) {
  cat("no cell has two solvers in it\n")
} else {
  bad <- agree[agree$worst_rel_gap > 1e-9, ]
  if (nrow(bad)) {
    cat("solvers disagree on", nrow(bad), "of", nrow(agree), "instances:\n")
    print(utils::head(bad[order(-bad$worst_rel_gap), ], 20), row.names = FALSE)
  } else {
    cat("every solver returns the same optimum on all", nrow(agree),
        "instances\n")
  }
}

cat("\nWrote", runs_csv, ", regime-results.csv, regime-verdict.csv and",
    cells_csv, "\n")
quit(save = "no", status = 0L)
