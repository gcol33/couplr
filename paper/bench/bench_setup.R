## Paths and the couplr build every benchmark script uses.
##
## A script sets `bench_dir` to its own directory and sources this file.
## Results go to results/ beside the scripts, or to the directory named by the
## environment variable COUPLR_BENCH_RESULTS. When the scripts sit inside the
## couplr source tree (two levels up holds couplr's DESCRIPTION), they time
## that tree, compiled with optimisation; anywhere else, for instance in an
## unpacked replication archive, they time the installed package.

results_dir <- normalizePath(
  Sys.getenv("COUPLR_BENCH_RESULTS", file.path(bench_dir, "results")),
  winslash = "/", mustWork = FALSE)
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

repo_root <- local({
  root <- normalizePath(file.path(bench_dir, "..", ".."), winslash = "/",
                        mustWork = FALSE)
  desc <- file.path(root, "DESCRIPTION")
  if (file.exists(desc) && any(readLines(desc, warn = FALSE) == "Package: couplr")) {
    root
  } else {
    NA_character_
  }
})

bundle_library <- file.path(dirname(bench_dir), "library")
if (is.na(repo_root) && dir.exists(bundle_library)) {
  .libPaths(c(bundle_library, .libPaths()))
}

## pkgbuild's default profile compiles the package at -O0, which would time an
## unoptimised couplr against optimised installs of the comparison packages.
options(pkg.build_extra_flags = FALSE)

load_couplr <- function() {
  if (is.na(repo_root)) {
    tarballs <- Sys.glob(file.path(dirname(bench_dir), "couplr_*.tar.gz"))
    if (length(tarballs)) {
      stamp <- file.path(bundle_library, "couplr-source.md5")
      installed <- file.path(bundle_library, "couplr", "DESCRIPTION")
      if (length(tarballs) != 1L || !file.exists(installed) ||
          !file.exists(stamp) ||
          !identical(readLines(stamp, warn = FALSE),
                     unname(tools::md5sum(tarballs)))) {
        stop("Run replication.R first to install the bundled couplr snapshot ",
             "in library/ before running these benchmarks.", call. = FALSE)
      }
    }
    if (!requireNamespace("couplr", quietly = TRUE)) {
      stop("This benchmark needs the couplr package: install the tarball ",
           "shipped with the replication materials.", call. = FALSE)
    }
    suppressPackageStartupMessages(library(couplr))
  } else {
    pkgload::load_all(repo_root, quiet = TRUE)
  }
  invisible(NULL)
}
