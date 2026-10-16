args <- grep("^--file=", commandArgs(FALSE), value = TRUE)
bench_dir <- dirname(normalizePath(sub("^--file=", "", args[1]),
                                  winslash = "/"))
source(file.path(bench_dir, "bench_registry.R"))

fixture_root <- tempfile("benchmark-runner-", tmpdir = bench_dir)
dir.create(fixture_root)
fixture <- file.path(fixture_root, "bench")
dir.create(fixture)
dir.create(file.path(fixture, "results"))
files <- benchmark_bundle_files(bench_dir)
stopifnot(all(file.copy(file.path(bench_dir, setdiff(files, "results")),
                       fixture)))
stages <- benchmark_stages()
for (stage in stages) {
  writeLines(c(
    'f <- grep("^--file=", commandArgs(FALSE), value = TRUE)',
    'bench_dir <- dirname(normalizePath(sub("^--file=", "", f[1])))',
    'source(file.path(bench_dir, "bench_setup.R"))',
    sprintf('writeLines("measured", file.path(results_dir, "%s"))',
            stage$outputs[1])), file.path(fixture, stage$script))
}

runner <- file.path(fixture, "run_benchmarks.R")
code <- parse(runner)
for (i in seq_along(code)) {
  x <- code[[i]]
  if (is.call(x) && identical(x[[1]], as.name("<-")) &&
      identical(x[[2]], as.name("stamp"))) code[[i]][[3]] <- "fixed-clock"
}
writeLines(unlist(lapply(code, deparse)), runner)
rscript <- file.path(R.home("bin"), "Rscript")
run <- function(args = character()) {
  output <- suppressWarnings(system2(rscript, c("--vanilla", shQuote(runner), args),
                                     stdout = TRUE, stderr = TRUE))
  list(status = if (is.null(attr(output, "status"))) 0L else
         attr(output, "status"), output = output)
}
old_dir <- setwd(dirname(fixture))
default <- run()
if (default$status != 0L) stop(paste(default$output, collapse = "\n"))
stopifnot(default$status == 0L)
produced <- list.files(file.path(fixture, "results"))
expected <- vapply(benchmark_stages(default_only = TRUE),
                   function(stage) stage$outputs[1], "")
stopifnot(setequal(produced, expected))

named <- run("certificate")
stopifnot(named$status == 0L,
          file.exists(file.path(fixture, "results", "certificate-runs.csv")))
writeLines("recorded", file.path(fixture, "results", "memory-results.csv"))
fresh <- run(c("--fresh", "memory"))
archives <- list.files(file.path(fixture, "logs"), "^archive-", full.names = TRUE)
stopifnot(fresh$status == 0L, length(archives) == 1L,
          identical(readLines(file.path(archives, "memory-results.csv")),
                    "recorded"),
          identical(readLines(file.path(fixture, "results", "memory-results.csv")),
                    "measured"))

writeLines("recorded-again", file.path(fixture, "results", "memory-results.csv"))
fresh_again <- run(c("--fresh", "memory"))
archives <- list.files(file.path(fixture, "logs"), "^archive-", full.names = TRUE)
stopifnot(fresh_again$status == 0L, length(archives) == 2L,
          setequal(vapply(archives, function(path)
            readLines(file.path(path, "memory-results.csv")), ""),
            c("recorded", "recorded-again")))

checkpoint <- file.path(fixture, "results", "path-checkpoint.rds")
saved <- list(shapes = data.frame(n_total = 500L), points = data.frame(point = 1L))
saveRDS(saved, checkpoint)
fresh_path <- run(c("--fresh", "path"))
archived_checkpoint <- list.files(file.path(fixture, "logs"), "path-checkpoint.rds",
                                  recursive = TRUE, full.names = TRUE)
stopifnot(fresh_path$status == 0L, !file.exists(checkpoint),
          length(archived_checkpoint) == 1L,
          identical(readRDS(archived_checkpoint), saved))

writeLines('stop("stage failure detail")', file.path(fixture, "bench_memory.R"))
failed <- run(c("memory", "path"))
stopifnot(failed$status == 1L,
          any(grepl("stage failure detail", failed$output, fixed = TRUE)),
          any(grepl("path exited with status 0", failed$output, fixed = TRUE)))

unlink(file.path(fixture, "bench_caliper.R"))
before <- list.files(fixture, recursive = TRUE)
missing <- run(c("--fresh", "caliper"))
stopifnot(missing$status == 1L,
          any(grepl("Missing benchmark scripts: bench_caliper.R",
                    missing$output, fixed = TRUE)),
          identical(before, list.files(fixture, recursive = TRUE)))
error <- tryCatch(benchmark_bundle_files(fixture), error = identity)
stopifnot(inherits(error, "error"),
          grepl("bench_caliper.R", conditionMessage(error), fixed = TRUE))
unknown <- run("unknown-stage")
stopifnot(unknown$status == 1L,
          any(grepl("Unknown stage(s): unknown-stage", unknown$output,
                    fixed = TRUE)))
writeLines("uninstalled snapshot",
           file.path(dirname(fixture), "couplr_test-runner.tar.gz"))
wrong_snapshot <- run("path")
unlink(file.path(dirname(fixture), "couplr_test-runner.tar.gz"))
stopifnot(wrong_snapshot$status == 1L,
          any(grepl("Run replication.R first", wrong_snapshot$output,
                    fixed = TRUE)))
setwd(old_dir)
resolved_fixture <- normalizePath(fixture_root, winslash = "/", mustWork = TRUE)
resolved_parent <- normalizePath(bench_dir, winslash = "/", mustWork = TRUE)
stopifnot(startsWith(tolower(resolved_fixture), paste0(tolower(resolved_parent), "/")))
unlink(resolved_fixture, recursive = TRUE)
cat("Benchmark runner: selection, archives, failures and bundle closure passed.\n")
