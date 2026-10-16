## Run every benchmark the article reports, in order, on one machine.
##
##   Rscript run_benchmarks.R                    # the default stages
##   Rscript run_benchmarks.R memory implicit    # selected stages
##   Rscript run_benchmarks.R --fresh memory     # re-measure from scratch
##
## Works from any directory and in an unpacked replication archive: every path
## is relative to this file, results go to results/ beside it (or to
## COUPLR_BENCH_RESULTS), and logs to logs/ beside it. Nothing is written
## outside this directory.
##
## The stages are sequential by design: they are wall-clock measurements pinned
## to a single core, and two running at once would time each other. Each stage
## resumes from its own CSVs, so a stopped run continues where it ended;
## --fresh moves the selected stages' outputs to logs/archive-<time>/ first.
##
## A stage that fails prints the end of its log here and the run continues
## with the next stage; the script exits with status 1 if any stage failed.

bench_dir <- local({
  f <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(f)) stop("Run this script with Rscript.", call. = FALSE)
  dirname(normalizePath(sub("^--file=", "", f[1]), winslash = "/"))
})
source(file.path(bench_dir, "bench_setup.R"))

source(file.path(bench_dir, "bench_registry.R"))
stages <- benchmark_stages()
default_stages <- names(benchmark_stages(default_only = TRUE))

args <- commandArgs(trailingOnly = TRUE)
fresh <- "--fresh" %in% args
which <- setdiff(args, "--fresh")
if (!length(which)) which <- default_stages
unknown <- setdiff(which, names(stages))
if (length(unknown)) {
  stop("Unknown stage(s): ", paste(unknown, collapse = ", "), ". Stages: ",
       paste(names(stages), collapse = ", "), call. = FALSE)
}

scripts <- vapply(stages[which], `[[`, "", "script")
missing <- scripts[!file.exists(file.path(bench_dir, scripts))]
if (length(missing)) {
  stop("Missing benchmark scripts: ", paste(missing, collapse = ", "),
       ". Re-extract the complete replication archive.", call. = FALSE)
}

logs <- file.path(bench_dir, "logs")
dir.create(logs, showWarnings = FALSE)
stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")

couplr_version <- if (is.na(repo_root)) {
  load_couplr()
  as.character(utils::packageVersion("couplr"))
} else {
  paste(read.dcf(file.path(repo_root, "DESCRIPTION"), "Version")[1, 1],
        "(source tree)")
}
env <- c(
  paste("run       ", stamp),
  paste("platform  ", R.version$platform),
  paste("R         ", R.version.string),
  sprintf("%-10s %s", "couplr", couplr_version),
  vapply(c("MatchIt", "optmatch", "clue", "lpSolve", "bigmatch",
           "designmatch", "highs"), function(p) {
    v <- if (requireNamespace(p, quietly = TRUE)) {
      as.character(utils::packageVersion(p))
    } else {
      "not installed"
    }
    sprintf("%-10s %s", p, v)
  }, character(1))
)
writeLines(env, file.path(logs, "ENVIRONMENT.txt"))
cat(env, sep = "\n")

if (fresh) {
  archive <- tempfile(paste0("archive-", stamp, "-"), tmpdir = logs)
  if (!dir.create(archive)) {
    stop("Cannot create benchmark archive: ", archive, call. = FALSE)
  }
  for (name in which) {
    old <- file.path(results_dir, stages[[name]]$outputs)
    old <- old[file.exists(old)]
    moved <- file.rename(old, file.path(archive, basename(old)))
    if (!all(moved)) {
      stop("Cannot archive previous benchmark outputs: ",
           paste(old[!moved], collapse = ", "), call. = FALSE)
    }
  }
  cat("previous outputs moved to", archive, "\n")
}

rscript <- file.path(R.home("bin"), "Rscript")
Sys.setenv(COUPLR_BENCH_RESULTS = results_dir)
failed <- character()
for (name in which) {
  log <- file.path(logs, sprintf("%s-%s.log", name, stamp))
  cat(sprintf("\n=== %s (log: %s)\n", name, log))
  status <- system2(rscript, shQuote(file.path(bench_dir, stages[[name]]$script)),
                    stdout = log, stderr = log)
  cat(sprintf("=== %s exited with status %d\n", name, status))
  if (!identical(status, 0L)) {
    failed <- c(failed, name)
    cat("--- last lines of the log ---\n")
    cat(utils::tail(readLines(log, warn = FALSE), 25), sep = "\n")
  }
}

if (length(failed)) {
  cat("\nStages failed:", paste(failed, collapse = ", "), "\n")
  quit(status = 1)
}
cat("\nAll stages completed; results are in", results_dir, "\n")
