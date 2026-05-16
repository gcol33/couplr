## Release-build GT testthat run after Phase 5.
suppressPackageStartupMessages({
  devtools::load_all(".", quiet = TRUE, recompile = TRUE)
})
cat("\n== GT test files ==\n")
res_gt <- testthat::test_dir(
  "tests/testthat",
  filter = "gabow_tarjan",
  reporter = testthat::SummaryReporter$new(),
  stop_on_failure = FALSE
)
cat("\n== Module-level GT tests ==\n")
res_mod <- testthat::test_dir(
  "tests/testthat/gabow-tarjan",
  reporter = testthat::SummaryReporter$new(),
  stop_on_failure = FALSE
)

df_gt <- as.data.frame(res_gt)
df_mod <- as.data.frame(res_mod)
total_fail <- sum(df_gt$failed) + sum(df_mod$failed)
total_pass <- sum(df_gt$nb) + sum(df_mod$nb)
cat(sprintf("\nGT tests: %d expectations, %d failures\n",
            total_pass, total_fail))
