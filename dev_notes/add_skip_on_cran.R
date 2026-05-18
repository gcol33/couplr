# Add `skip_on_cran()` as the first line inside every `test_that(...)` block in
# coverage-padding test files. Skips files that already have skip_on_cran.

files <- c(
  "test-assignment-push_relabel.R",
  "test-batch-coverage-final.R",
  "test-batch-kbest-extended.R",
  "test-blocks-coverage.R",
  "test-constraints-coverage.R",
  "test-coverage-90-final.R",
  "test-coverage-95.R",
  "test-coverage-boost.R",
  "test-cpp-solvers-coverage.R",
  "test-cycle-cancel-coverage.R",
  "test-cycle-cancel-coverage-2.R",
  "test-distance-cache-coverage.R",
  "test-kbest-coverage-final.R",
  "test-lap-solve-batch-coverage-2.R",
  "test-lap-solve-batch-extended.R",
  "test-lap-solve-coverage.R",
  "test-lap-solve-extended-coverage.R",
  "test-lap-solve-final-coverage.R",
  "test-matching-core-coverage-2.R",
  "test-matching-core-coverage-3.R",
  "test-matching-core-coverage-4.R",
  "test-matching-core-extended.R",
  "test-matching-diagnostics-extended.R",
  "test-matching-distance-coverage.R",
  "test-matching-join-coverage.R",
  "test-matching-parallel-coverage-2.R",
  "test-matching-preprocessing-coverage.R",
  "test-matching-preprocessing-coverage-2.R",
  "test-matching-utils-coverage.R",
  "test-matching-utils-extended.R",
  "test-messages-coverage.R",
  "test-messages-extended.R",
  "test-morph-coverage-boost.R",
  "test-morph-coverage-final.R",
  "test-morph-final-push.R",
  "test-morph-pixel-cpp-coverage.R",
  "test-morph-tiling-coverage.R",
  "test-morph-tiling-extended.R",
  "test-morph-utils-coverage.R",
  "test-morph-utils-coverage-2.R",
  "test-morph-utils-coverage-3.R",
  "test-morph-utils-coverage-4.R",
  "test-morph-utils-extended.R",
  "test-morph-utils-final.R",
  "test-network-simplex-coverage.R",
  "test-preprocessing-coverage.R",
  "test-rcpp-interface-coverage.R",
  "test-utils-extended-2.R"
)

base_dir <- "tests/testthat"
patched <- 0L
skipped <- 0L

for (f in files) {
  path <- file.path(base_dir, f)
  if (!file.exists(path)) {
    cat("MISSING:", f, "\n")
    next
  }
  lines <- readLines(path, warn = FALSE)
  if (any(grepl("skip_on_cran", lines, fixed = TRUE))) {
    cat("SKIP (already has skip_on_cran):", f, "\n")
    skipped <- skipped + 1L
    next
  }
  # Match "test_that(\"...\",\\s*{" possibly with trailing comments. We rewrite
  # by injecting "  skip_on_cran()" on the next line, preserving indent.
  out <- character(0)
  added <- 0L
  for (line in lines) {
    out <- c(out, line)
    # The "{" appears at the end of a test_that opening line. We catch both
    # "test_that(... {" and continuation "{".
    if (grepl("test_that\\(", line) && grepl("\\{\\s*$", line)) {
      # Determine indentation of the next line by inspecting the test_that line
      indent <- sub("\\S.*$", "", line)
      out <- c(out, paste0(indent, "  skip_on_cran()"))
      added <- added + 1L
    }
  }
  if (added > 0L) {
    writeLines(out, path)
    cat("PATCHED:", f, "  (", added, "test_that blocks)\n")
    patched <- patched + 1L
  } else {
    cat("NOT MATCHED:", f, "  (no test_that(..., {) found - inspect manually)\n")
  }
}

cat("\n---\n")
cat("Patched:", patched, "\n")
cat("Already had skip_on_cran:", skipped, "\n")
cat("Total processed:", length(files), "\n")
