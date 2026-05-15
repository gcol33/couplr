## Run all Gabow-Tarjan tests + assignment-duals + lap-solve-batch coverage.
suppressPackageStartupMessages({
  library(devtools)
  library(testthat)
})

devtools::load_all(".", quiet = TRUE)

filt <- function(pat) {
  files <- list.files("tests/testthat", pattern = pat,
                       recursive = TRUE, full.names = TRUE)
  files
}

target_patterns <- c(
  "test-gabow_tarjan_complexity\\.R$",
  "test-gabow_tarjan_solver\\.R$",
  "test-assignment-ramshaw_tarjan\\.R$",
  "test-assignment-duals\\.R$",
  "test_gabow_tarjan_moduleA\\.R$",
  "test_gabow_tarjan_moduleB\\.R$",
  "test_gabow_tarjan_moduleC\\.R$",
  "test_gabow_tarjan_moduleD\\.R$",
  "test_gabow_tarjan_moduleE\\.R$",
  "test_gabow_tarjan_moduleF\\.R$",
  "test_gabow_tarjan_moduleG\\.R$",
  "test_gabow_tarjan_moduleH\\.R$"
)

files <- unique(unlist(lapply(target_patterns, filt)))
cat("Running", length(files), "test files:\n")
cat(paste0("  ", files, "\n"), sep = "")

reporter <- testthat::SummaryReporter$new()

for (f in files) {
  cat("\n=== ", basename(f), " ===\n", sep = "")
  res <- tryCatch(
    testthat::test_file(f, reporter = reporter),
    error = function(e) {
      cat("FATAL: ", conditionMessage(e), "\n", sep = "")
      NULL
    }
  )
}

cat("\nDone.\n")
