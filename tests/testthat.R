# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(couplr)

# Diagnostic: stream test-by-test progress to testthat.Rout (alongside the
# normal CheckReporter R CMD check expects), so a hanging test on macOS CI
# can be located from the partial log when the step times out.
test_check("couplr", reporter = testthat::MultiReporter$new(
  reporters = list(
    testthat::CheckReporter$new(),
    testthat::LocationReporter$new()
  )
))
