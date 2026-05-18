# Run a focused subset of tests covering files that could be affected by the
# trace work and the bug-fixes in prepare_cost_matrix.cpp / lap_solve.R.
suppressPackageStartupMessages(library(testthat))
suppressPackageStartupMessages(devtools::load_all(quiet = TRUE))

filters <- c(
  "trace-parity",
  "assignment-auction",
  "assignment-auction-gs",
  "assignment-bottleneck",
  "assignment-csa",
  "assignment-csflow",
  "assignment-hk01",
  "assignment-hungarian",
  "assignment-jv",
  "assignment-network-simplex",
  "assignment-orlin",
  "assignment-push_relabel",
  "assignment-ramshaw_tarjan",
  "assignment-ssap-bucket",
  "assignment-ssp",
  "cycle-cancel",
  "cycle-cancel-coverage",
  "cycle-cancel-coverage-2",
  "prepare-cost-matrix"
)

for (f in filters) {
  cat(sprintf("\n=== %s ===\n", f))
  tryCatch({
    devtools::test(filter = f, reporter = "summary")
  }, error = function(e) {
    cat("ERROR:", conditionMessage(e), "\n")
  })
}
