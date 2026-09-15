## The `few_costs` dispatch rule, scored on a grid it was not chosen on.
##
## The regime grid in `paper/bench_regimes.R` showed `"auto"` far off the best
## solver where the finite costs take few distinct values, and a rule read off
## those cells and scored on those cells would be scored on the measurements that
## chose it. So the rule is fixed first, as `few_costs` in `R/lap_dispatch.R`
## sending such a matrix to `auction_scaled`, and a grid generated independently
## of the regime grid decides whether it stays. Each instance draws k distinct
## integer levels at random spacing from 1..100000, in uneven shares, where the
## regime grid uses the levels 1..5; the shapes and admissibility patterns are
## ones the regime grid does not use, and every seed is a digest of a key
## beginning "dispatch-validation", which no regime-grid key does.
##
## Two versions of the rule have been stated, each with its own grid.
##
##   1. At most 32 distinct finite values, and at most half as many as the
##      longer side has units. Grid 1: k in 3, 6, 12, 24 and 32 where the rule
##      fires and 33, 48, 64 and 500 where it does not; shapes 400 x 400,
##      400 x 1200, 800 x 800 and 300 x 3000; complete, 40% finite, 10% finite
##      and three disconnected blocks. Not kept: over the 82 cells it fired on,
##      `auction_scaled` took a median 0.38 of the time of `jv`, and 52 times it
##      at worst, on a 300 x 3000 matrix with a tenth of its entries finite. It
##      was quicker in all 42 square cells, slower in 10 of the 20 cells with
##      three columns per row and in all 20 with ten, where the auction solves
##      over dummy rows.
##   2. The same, on a problem with at most 1.25 columns per row. Grid 2: k in
##      4, 9, 16 and 28, which fire on the shapes 600 x 600, 1000 x 1000,
##      600 x 660 and 800 x 1000, and 40 and 120, which do not; 600 x 900 and
##      500 x 1000, past the column bound, where no k fires; complete, 75%
##      finite, 15% finite and five disconnected blocks. No shape, pattern, level
##      count or seed key is shared with grid 1 or with the regime grid. Not
##      kept: over the 64 cells it fired on, `auction_scaled` took a median 0.13
##      of the time of `jv` and was quicker in 61, but 2.62 times it at worst,
##      on an 800 x 1000 matrix with 15% of its entries finite, and 1.95 times it
##      on a 600 x 660 one in five blocks, both at 28 levels and both solved by
##      `jv` in about 5 to 12 ms.
##
## Neither version is in the dispatch table, and this script is not a suite
## stage: a version runs only at the commit holding its rule, 85eefde for grid 1
## and 853f91f for grid 2.
## Decision, stated with each version before its grid ran. Over the cells where `"auto"` fires the
## rule, take each cell's median time of `auction_scaled` over its median time
## of `jv`, the solver the default would have named, both timed as named solvers
## so the probe `"auto"` pays either way is in neither. The rule is adopted when
##
##   1. the median of that ratio over those cells is below 1,
##   2. no cell's ratio exceeds 1.5,
##   3. every cell where the rule does not fire dispatches to `jv`, and
##   4. every solve in the grid certifies optimal.
##
## Otherwise it is removed. The verdict, the numbers behind each condition, the
## commit and the rule's condition are written to the version's verdict file,
## `paper/dispatch-validation-verdict.csv` for grid 1 and
## `paper/dispatch-validation-2-verdict.csv` for grid 2.
##
## Each grid is a version below, and a version is fixed once it has run: its
## level counts, shapes, patterns, seed key and the rule condition it was stated
## for. A rule revised after a grid is scored on a new version with its own seed
## key, never on the grid that prompted the revision. The script refuses to run a
## version against a rule whose condition is not the one that version names.
##
## Reproducible via:  Rscript paper/bench_dispatch_validation.R --grid=2
##                    Rscript paper/bench_dispatch_validation.R --grid=2 --quick

repo_root <- if (file.exists("DESCRIPTION")) {
  normalizePath(".", winslash = "/", mustWork = TRUE)
} else if (basename(getwd()) == "paper" && file.exists("../DESCRIPTION")) {
  normalizePath("..", winslash = "/", mustWork = TRUE)
} else {
  stop("Run this script from the package root or the paper directory.")
}

options(pkg.build_extra_flags = FALSE)

if (!requireNamespace("RhpcBLASctl", quietly = TRUE)) {
  stop("This benchmark needs the RhpcBLASctl package: ",
       "install.packages(\"RhpcBLASctl\")", call. = FALSE)
}
suppressPackageStartupMessages({
  library(RhpcBLASctl)
  pkgload::load_all(repo_root, quiet = TRUE)
})
blas_set_num_threads(1); omp_set_num_threads(1)
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1",
           MKL_NUM_THREADS = "1", VECLIB_MAXIMUM_THREADS = "1")

source(file.path(repo_root, "paper", "bench_common.R"))

argv  <- commandArgs(TRUE)
QUICK <- any(argv == "--quick")
GRID  <- sub("^--grid=", "", argv[grepl("^--grid=", argv)])

## ---- the grids ---------------------------------------------------------------
patterns_v1 <- list(
  complete  = function(cost, n, m) cost,
  finite_40 = function(cost, n, m) .forbid_random(cost, n, m, 0.40),
  finite_10 = function(cost, n, m) .forbid_random(cost, n, m, 0.10),
  blocks_3  = function(cost, n, m) .forbid_blocks(cost, n, m, 3L)
)
grids <- list(
  `1` = list(
    key = "dispatch-validation",
    files = "dispatch-validation",
    rule = "few_costs", method = "auction_scaled",
    condition = paste("at most 32 distinct finite values, and at most half as",
                      "many as the longer side has units"),
    level_counts = c(3L, 6L, 12L, 24L, 32L, 33L, 48L, 64L, 500L),
    shapes = list(c(400L, 400L), c(400L, 1200L), c(800L, 800L), c(300L, 3000L)),
    patterns = patterns_v1
  )
)
grids[["2"]] <- list(
  key = "dispatch-validation-2",
  files = "dispatch-validation-2",
  rule = "few_costs", method = "auction_scaled",
  condition = paste("at most 32 distinct finite values, at most half as many",
                    "as the longer side has units, and at most 1.25 columns",
                    "per row"),
  level_counts = c(4L, 9L, 16L, 28L, 40L, 120L),
  shapes = list(c(600L, 600L), c(1000L, 1000L), c(600L, 660L), c(800L, 1000L),
                c(600L, 900L), c(500L, 1000L)),
  patterns = list(
    complete  = function(cost, n, m) cost,
    finite_75 = function(cost, n, m) .forbid_random(cost, n, m, 0.75),
    finite_15 = function(cost, n, m) .forbid_random(cost, n, m, 0.15),
    blocks_5  = function(cost, n, m) .forbid_blocks(cost, n, m, 5L)
  )
)
## With no --grid the latest version runs, which is the one stated for the rule
## the dispatch table holds.
if (!length(GRID)) GRID <- names(grids)[length(grids)]
grid <- grids[[GRID]]
if (is.null(grid)) stop("No validation grid version '", GRID, "'.", call. = FALSE)

paper_dir   <- file.path(repo_root, "paper")
runs_csv    <- file.path(paper_dir, paste0(grid$files, "-runs.csv"))
results_csv <- file.path(paper_dir, paste0(grid$files, "-results.csv"))
verdict_csv <- file.path(paper_dir, paste0(grid$files, "-verdict.csv"))

## ---- the rule on trial -------------------------------------------------------
RULE_ID <- grid$rule
rule <- Filter(function(r) identical(r$id, RULE_ID), .dispatch_rules)
if (length(rule) != 1L) {
  stop("The dispatch table has no rule '", RULE_ID, "' to validate.", call. = FALSE)
}
rule <- rule[[1L]]
if (!identical(rule$method, grid$method) || !identical(rule$condition, grid$condition)) {
  stop("Grid ", GRID, " was stated for '", grid$condition, "' -> ", grid$method,
       ", and the dispatch table holds '", rule$condition, "' -> ", rule$method,
       ". A grid scores the rule it was stated for.", call. = FALSE)
}

commit <- tryCatch(
  system2("git", c("-C", shQuote(repo_root), "rev-parse", "--short", "HEAD"),
          stdout = TRUE),
  error = function(e) NA_character_)
dirty <- tryCatch(
  length(system2("git", c("-C", shQuote(repo_root), "status", "--porcelain"),
                 stdout = TRUE)),
  error = function(e) NA_integer_)

## ---- the grid ----------------------------------------------------------------
level_counts <- grid$level_counts
shapes <- grid$shapes
patterns <- grid$patterns
instances <- 3L; reps <- 3L; timeout_s <- 300
methods <- c("auto", "jv", "auction_scaled", "csa", "network_simplex")

if (QUICK) {
  level_counts <- c(3L, 33L)
  shapes <- list(c(60L, 60L), c(60L, 180L))
  patterns <- patterns[1:2]
  instances <- 1L; reps <- 1L; timeout_s <- 60
}

## k distinct integer levels at random spacing, in uneven shares.
tied_levels <- function(n, m, k) {
  values <- sort(sample.int(100000L, k))
  shares <- stats::rgamma(k, shape = 1)
  matrix(values[sample.int(k, n * m, replace = TRUE, prob = shares)], n, m)
}

RUN_SCHEMA <- c("levels", "pattern", "n_rows", "n_cols", "instance", "seed",
                "n_distinct", "auto_rule", "auto_method", "method", "rep",
                "seconds", "status", "total_cost", "n_matched", names(CERT_NA))
runs <- if (file.exists(runs_csv)) {
  got <- read.csv(runs_csv, stringsAsFactors = FALSE)
  if (length(setdiff(RUN_SCHEMA, names(got)))) {
    stop(runs_csv, " was written by an earlier version of this script. Move it ",
         "aside so the whole grid is measured once.", call. = FALSE)
  }
  got
} else {
  data.frame()
}
have_run <- function(k, pattern, n_rows, n_cols, instance) {
  nrow(runs) > 0 &&
    any(runs$levels == k & runs$pattern == pattern & runs$n_rows == n_rows &
          runs$n_cols == n_cols & runs$instance == instance)
}

cat(sprintf("%s dispatch rule, validation grid %s\n", RULE_ID, GRID))
cat(sprintf("commit %s, %s modified paths; rule: %s -> %s\n\n", commit, dirty,
            rule$condition, rule$method))

for (shape in shapes) {
  n_rows <- shape[1]; n_cols <- shape[2]
  for (k in level_counts) {
    for (pattern in names(patterns)) {
      cat(sprintf("=== %d x %d  %d levels  %s ===\n", n_rows, n_cols, k, pattern))
      flush.console()
      for (instance in seq_len(instances)) {
        if (have_run(k, pattern, n_rows, n_cols, instance)) {
          cat(sprintf("  instance %d: already recorded, skipping\n", instance))
          next
        }
        seed <- key_seed(grid$key, k, pattern, n_rows, n_cols, instance)
        set.seed(seed)
        cost <- patterns[[pattern]](tied_levels(n_rows, n_cols, k), n_rows, n_cols)
        dec <- explain_dispatch(cost)

        ref <- bounded_call(function() {
          dd <- assignment_duals(cost)
          list(u = as.numeric(dd$u), v = as.numeric(dd$v))
        }, timeout_s * 3)
        duals <- if (ref$ok) ref$value else NULL

        panel_runs <- solve_panel(cost, methods, reps, timeout_s, duals)
        for (method in methods) {
          got <- panel_runs[[method]]
          new <- data.frame(
            levels = k, pattern = pattern, n_rows = n_rows, n_cols = n_cols,
            instance = instance, seed = seed, n_distinct = dec$probe$n_distinct,
            auto_rule = dec$rule, auto_method = dec$method, method = method,
            rep = seq_along(got$seconds), seconds = signif(got$seconds, 7),
            status = got$status, total_cost = got$total_cost,
            n_matched = got$n_matched, objective = got$objective,
            duality_gap = got$duality_gap,
            max_suboptimality = got$max_suboptimality,
            certified_optimal = got$certified_optimal,
            primal_feasible = got$primal_feasible,
            all_rows_matched = got$all_rows_matched,
            structurally_valid = got$structurally_valid,
            stringsAsFactors = FALSE)
          runs <- if (nrow(runs)) rbind(runs, new) else new
          write.csv(runs, runs_csv, row.names = FALSE)
          cat(sprintf("  i%d %-15s %s%s\n", instance, method,
                      if (any(got$status == "ok"))
                        sprintf("%9.4f s", arm_seconds(got$seconds, got$status))
                      else got$status[1],
                      if (isFALSE(got$certified_optimal)) "  NOT OPTIMAL" else ""))
          flush.console()
        }
      }
      cat("\n")
    }
  }
}

## ---- the verdict -------------------------------------------------------------
ok <- runs[runs$status == "ok", ]
cell <- function(df) paste(df$levels, df$pattern, df$n_rows, df$n_cols, sep = "|")
per_instance <- aggregate(seconds ~ levels + pattern + n_rows + n_cols + instance +
                            auto_rule + auto_method + method,
                          data = ok, FUN = min)
results <- aggregate(seconds ~ levels + pattern + n_rows + n_cols + auto_rule +
                       auto_method + method,
                     data = per_instance, FUN = stats::median)
names(results)[names(results) == "seconds"] <- "median_s"
results$commit <- commit
write.csv(results, results_csv, row.names = FALSE)

by_cell <- split(results, cell(results))
cells <- do.call(rbind, lapply(by_cell, function(g) {
  jv <- g$median_s[g$method == "jv"]
  picked <- g$median_s[g$method == "auction_scaled"]
  named <- g[g$method != "auto", ]
  data.frame(levels = g$levels[1], pattern = g$pattern[1],
             shape = sprintf("%d x %d", g$n_rows[1], g$n_cols[1]),
             auto_rule = g$auto_rule[1], auto_method = g$auto_method[1],
             jv_s = jv, auction_scaled_s = picked,
             ratio = picked / jv,
             best_method = named$method[which.min(named$median_s)],
             stringsAsFactors = FALSE)
}))
rownames(cells) <- NULL

fire <- cells[cells$auto_rule == RULE_ID, ]
rest <- cells[cells$auto_rule != RULE_ID, ]
solve_key <- paste(cell(ok), ok$instance, ok$method)
one <- ok[!duplicated(solve_key), ]

median_ratio <- if (nrow(fire)) stats::median(fire$ratio) else NA_real_
worst_ratio <- if (nrow(fire)) max(fire$ratio) else NA_real_
controls_jv <- nrow(rest) == 0 || all(rest$auto_method == "jv")
all_certified <- nrow(one) > 0 && all(one$certified_optimal %in% TRUE)
complete_grid <- nrow(one) == length(level_counts) * length(shapes) *
  length(patterns) * instances * length(methods)

adopted <- isTRUE(median_ratio < 1) && isTRUE(worst_ratio <= 1.5) &&
  controls_jv && all_certified && complete_grid

verdict <- data.frame(
  grid = GRID, rule = RULE_ID, condition = rule$condition, method = rule$method,
  commit = commit, modified_paths = dirty, quick = QUICK,
  fire_cells = nrow(fire), control_cells = nrow(rest),
  median_ratio = round(median_ratio, 3), worst_ratio = round(worst_ratio, 3),
  controls_dispatch_jv = controls_jv, all_certified = all_certified,
  complete_grid = complete_grid, adopted = adopted,
  stringsAsFactors = FALSE)
write.csv(verdict, verdict_csv, row.names = FALSE)

cat("\n--- cells where the rule fires ---\n")
print(fire[order(-fire$ratio), ], row.names = FALSE)
cat("\n--- verdict ---\n")
print(verdict, row.names = FALSE)
quit(save = "no", status = 0L)
