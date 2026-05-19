# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(couplr)

# Diagnostic side-channel: when COUPLR_TEST_PROGRESS_LOG is set, write each
# test start to that file with cat(file=...,append=TRUE). R closes the
# connection per call, so output survives a SIGKILL'd R CMD check process.
# The rescue step in R-CMD-check.yaml dumps this file when macOS times out.
progress_log <- Sys.getenv("COUPLR_TEST_PROGRESS_LOG", unset = "")
reporters <- list(testthat::CheckReporter$new())

if (nzchar(progress_log)) {
  ProgressLogReporter <- R6::R6Class(
    "ProgressLogReporter",
    inherit = testthat::Reporter,
    public = list(
      start_file = function(filename) {
        cat(sprintf("[%s] FILE %s\n",
                    format(Sys.time(), "%H:%M:%S"), filename),
            file = progress_log, append = TRUE)
      },
      start_test = function(context, test) {
        cat(sprintf("  [%s] %s :: %s\n",
                    format(Sys.time(), "%H:%M:%S"), context, test),
            file = progress_log, append = TRUE)
      },
      end_reporter = function() {
        cat(sprintf("[%s] DONE\n",
                    format(Sys.time(), "%H:%M:%S")),
            file = progress_log, append = TRUE)
      }
    )
  )
  reporters <- c(reporters, list(ProgressLogReporter$new()))
}

reporter <- if (length(reporters) > 1) {
  testthat::MultiReporter$new(reporters = reporters)
} else {
  reporters[[1]]
}

test_check("couplr", reporter = reporter)
