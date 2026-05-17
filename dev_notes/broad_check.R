## Run a broad sweep of tests to confirm steps 1+2 didn't break any indirect
## consumer of JV / Hungarian / duals.

devtools::load_all(".", quiet = TRUE)

files <- c(
  "test-assign.R",
  "test-assignment.R",
  "test-assignment-auto.R",
  "test-assignment-duals.R",
  "test-assignment-hungarian.R",
  "test-assignment-jv.R",
  "test-assignment-munkres.R",
  "test-matching.R",
  "test-cpp-interface.R",
  "test-kbest-murty.R",
  "test-kbest-lawler.R"
)

for (f in files) {
  path <- file.path("tests/testthat", f)
  if (file.exists(path)) {
    cat(sprintf("\n== %s ==\n", f))
    testthat::test_file(path, reporter = "summary")
  } else {
    cat(sprintf("(skip missing %s)\n", f))
  }
}
