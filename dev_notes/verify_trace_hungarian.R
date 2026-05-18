# Correctness + frame-count check for trace_hungarian.
# Compares the trace's final matching against the production C++ Hungarian
# across a battery of random matrices and a few edge cases.

suppressMessages(devtools::load_all(quiet = TRUE))

set.seed(0xC0FFEE)

mat_cost <- function(rows, cols, lo = 0, hi = 100, integers = FALSE) {
  v <- runif(rows * cols, lo, hi)
  if (integers) v <- floor(v)
  matrix(v, rows, cols)
}

check_one <- function(label, cost, maximize = FALSE) {
  ref <- assignment(cost, maximize = maximize, method = "hungarian")

  w <- lap_animate(cost, method = "hungarian", maximize = maximize)
  trace_final <- w$x$frames[[length(w$x$frames)]]
  trace_matching <- trace_final$matching
  trace_total <- w$x$meta$total_cost

  match_eq  <- identical(as.integer(ref$match), as.integer(trace_matching))
  cost_eq   <- isTRUE(all.equal(ref$total_cost, trace_total))
  n_frames  <- length(w$x$frames)

  status <- if (match_eq && cost_eq) "OK" else "FAIL"
  cat(sprintf(
    "  %-30s n=%3d m=%3d  frames=%3d  ref_cost=%9.4g  trace_cost=%9.4g  %s\n",
    label, nrow(cost), ncol(cost), n_frames, ref$total_cost, trace_total, status
  ))

  if (!match_eq) {
    cat("    ref match  :", paste(ref$match, collapse = ", "), "\n")
    cat("    trace match:", paste(trace_matching, collapse = ", "), "\n")
  }
  invisible(match_eq && cost_eq)
}

cat("=== trace_hungarian vs assignment(method = 'hungarian') ===\n\n")

ok <- TRUE

# Tiny worked example from the smoke test
c0 <- matrix(c(4, 2, 5,
               3, 3, 6,
               7, 5, 4), nrow = 3, byrow = TRUE)
ok <- check_one("tiny 3x3 worked",  c0) && ok

# Random squares of increasing size
for (n in c(4, 6, 8, 12, 20, 40)) {
  ok <- check_one(sprintf("random %dx%d", n, n), mat_cost(n, n)) && ok
}

# Rectangular: trace_hungarian should error with a clear message
rect_err <- tryCatch(
  lap_animate(mat_cost(3, 7), method = "hungarian"),
  error = function(e) conditionMessage(e)
)
got_err <- is.character(rect_err) && grepl("square", rect_err)
cat(sprintf("  %-30s  %s  (msg: %s...)\n",
            "rectangular 3x7 expect-error",
            if (got_err) "OK" else "FAIL",
            substr(rect_err, 1, 60)))
ok <- got_err && ok

# Integer-valued (tests for floating-point drift in dual updates)
ok <- check_one("integer 8x8",  mat_cost(8, 8, integers = TRUE)) && ok
ok <- check_one("integer 15x15", mat_cost(15, 15, integers = TRUE)) && ok

# Ties (many equal costs) — exercises tie-breaking in which.min
c_ties <- matrix(c(1, 1, 1,
                   1, 1, 1,
                   1, 2, 3), 3, 3, byrow = TRUE)
ok <- check_one("ties 3x3", c_ties) && ok

# Forbidden entries
cf <- mat_cost(6, 6)
cf[sample.int(36, 8)] <- NA
ok <- check_one("with NA forbidden 6x6", cf) && ok

# Maximize
ok <- check_one("maximize 5x5", mat_cost(5, 5), maximize = TRUE) && ok

# A case crafted to force a non-trivial dual update (multiple rows fight
# over the same tight column)
c_fight <- matrix(c( 1,  5,  5,  5,
                     1,  5,  5,  5,
                     5,  1,  5,  5,
                     5,  5,  1,  1), 4, 4, byrow = TRUE)
ok <- check_one("contention 4x4", c_fight) && ok

cat("\n", if (ok) "ALL OK" else "FAILURES PRESENT", "\n", sep = "")

# Frame breakdown for one mid-sized problem
cat("\n=== Frame breakdown for random 8x8 ===\n")
set.seed(1)
c8 <- mat_cost(8, 8)
w8 <- lap_animate(c8, method = "hungarian")
phase_counts <- table(vapply(w8$x$frames, `[[`, character(1), "phase"))
print(phase_counts)
cat("Total frames:", length(w8$x$frames), "\n")

# Save a standalone HTML on the worked example
out_html <- file.path("dev_notes", "trace_hungarian.html")
htmlwidgets::saveWidget(
  lap_animate(c0, method = "hungarian"),
  file = normalizePath(out_html, mustWork = FALSE),
  selfcontained = TRUE
)
cat("\nWrote", out_html, "\n")

invisible(NULL)
