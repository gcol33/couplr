# Standalone driver for the parity validators (not part of the testthat suite).
# Run from package root via:
#   "C:/Program Files/R/R-4.6.0/bin/Rscript.exe" dev_notes/run_parity.R
suppressPackageStartupMessages(devtools::load_all(quiet = TRUE))

set.seed(2026)
cases <- list(
  list(name = "3x3 small integer",      cost = matrix(sample.int(20, 9),  nrow = 3)),
  list(name = "4x4 integer with ties",  cost = matrix(sample.int(5, 16, replace = TRUE), nrow = 4)),
  list(name = "6x6 random integer",     cost = matrix(sample.int(100, 36), nrow = 6)),
  list(name = "5x5 forbidden edges",
       cost = local({
         m <- matrix(sample.int(50, 25), nrow = 5)
         m[1, 2] <- NA_real_; m[3, 4] <- Inf; m[5, 1] <- NA_real_
         m
       })),
  list(name = "4x4 doubles",            cost = matrix(runif(16) * 10, nrow = 4))
)

set.seed(2027)
bin_cases <- list(
  list(name = "4x4 binary balanced",
       cost = matrix(sample(c(0, 1), 16, replace = TRUE, prob = c(0.6, 0.4)), nrow = 4)),
  list(name = "5x5 binary dense",
       cost = matrix(sample(c(0, 1), 25, replace = TRUE, prob = c(0.7, 0.3)), nrow = 5))
)

methods <- animated_methods()
cat("Methods:", paste(methods, collapse = ", "), "\n\n")

# Match the testthat skip-list for the run_parity dev driver.
.known_bugs <- list()
is_known_bug <- function(method, name, mx) {
  for (b in .known_bugs) {
    if (b$method == method && b$case_name == name && b$maximize == mx) return(TRUE)
  }
  FALSE
}

fails <- 0L
passes <- 0L
skipped <- 0L
for (method in methods) {
  use_cases <- if (method == "hk01") bin_cases else if (method == "bruteforce") cases[1:3] else cases
  for (case in use_cases) {
    for (mx in c(FALSE, TRUE)) {
      if (is_known_bug(method, case$name, mx)) {
        skipped <- skipped + 1L
        cat("SKIP ", sprintf("%s | %s | max=%s", method, case$name, mx),
            "(known C++ oracle bug)\n")
        next
      }
      label <- sprintf("%s | %s | max=%s", method, case$name, mx)
      tryCatch({
        trace_fn <- get_trace_fn(method)
        tr <- trace_fn(case$cost, maximize = mx)
        validate_trace_parity(tr, case$cost, maximize = mx, method = method)
        passes <- passes + 1L
        cat("PASS ", label, "\n")
      }, error = function(e) {
        fails <<- fails + 1L
        cat("FAIL ", label, "\n      ", conditionMessage(e), "\n")
      })
    }
  }
}

cat(sprintf("\n%d pass / %d fail / %d skipped / %d total\n",
            passes, fails, skipped, passes + fails + skipped))
