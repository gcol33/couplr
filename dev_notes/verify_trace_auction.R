# Correctness + frame-count check for trace_auction.
# Compares trace's final cost to the Hungarian production solver (treated as
# the optimal-cost oracle) across a battery of square inputs.

suppressMessages(devtools::load_all(quiet = TRUE))

set.seed(0xBADCAFE)

mat_cost <- function(n, lo = 0, hi = 100, integers = FALSE) {
  v <- runif(n * n, lo, hi)
  if (integers) v <- floor(v)
  matrix(v, n, n)
}

check_one <- function(label, cost, maximize = FALSE, tol = 1e-6) {
  ref <- assignment(cost, maximize = maximize, method = "hungarian")
  w <- lap_animate(cost, method = "auction", maximize = maximize)

  trace_final <- w$x$frames[[length(w$x$frames)]]
  trace_matching <- trace_final$matching
  trace_total <- w$x$meta$total_cost
  n_frames <- length(w$x$frames)

  cost_eq <- isTRUE(all.equal(ref$total_cost, trace_total, tolerance = tol))
  status <- if (cost_eq) "OK" else "FAIL"

  cat(sprintf(
    "  %-30s n=%3d  frames=%5d  ref_cost=%9.4g  trace_cost=%9.4g  diff=%9.2e  %s\n",
    label, nrow(cost), n_frames, ref$total_cost, trace_total,
    trace_total - ref$total_cost, status
  ))

  if (!cost_eq) {
    cat("    ref match  :", paste(ref$match, collapse = ", "), "\n")
    cat("    trace match:", paste(trace_matching, collapse = ", "), "\n")
  }
  invisible(cost_eq)
}

cat("=== trace_auction vs assignment(method = 'hungarian') as oracle ===\n\n")

ok <- TRUE

# Tiny worked example
c0 <- matrix(c(4, 2, 5,
               3, 3, 6,
               7, 5, 4), nrow = 3, byrow = TRUE)
ok <- check_one("tiny 3x3 worked", c0) && ok

# Random squares
for (n in c(4, 6, 8, 12)) {
  ok <- check_one(sprintf("random %dx%d", n, n), mat_cost(n)) && ok
}

# Integer-valued (tests the eps = 1/(n+1) branch + exact-optimality guarantee)
ok <- check_one("integer 5x5",  mat_cost(5,  integers = TRUE)) && ok
ok <- check_one("integer 8x8",  mat_cost(8,  integers = TRUE)) && ok
ok <- check_one("integer 12x12", mat_cost(12, integers = TRUE)) && ok

# Contention: many tied costs forcing repeated bidding wars
c_fight <- matrix(c( 1,  5,  5,  5,
                     1,  5,  5,  5,
                     5,  1,  5,  5,
                     5,  5,  1,  1), 4, 4, byrow = TRUE)
ok <- check_one("contention 4x4", c_fight) && ok

# Maximize
ok <- check_one("maximize 5x5", mat_cost(5), maximize = TRUE) && ok

# Rectangular: should error
rect_err <- tryCatch(
  lap_animate(mat_cost(3)[, 1:2], method = "auction"),
  error = function(e) conditionMessage(e)
)
got_err <- is.character(rect_err) && grepl("square", rect_err)
cat(sprintf("  %-30s  %s\n",
            "rectangular expect-error",
            if (got_err) "OK" else "FAIL"))
ok <- got_err && ok

cat("\n", if (ok) "ALL OK" else "FAILURES PRESENT", "\n", sep = "")

# Frame breakdown for one mid-sized run
cat("\n=== Frame breakdown for random 6x6 ===\n")
set.seed(1)
c6 <- mat_cost(6)
w6 <- lap_animate(c6, method = "auction")
phase_counts <- table(vapply(w6$x$frames, `[[`, character(1), "phase"))
print(phase_counts)
cat("Total frames:", length(w6$x$frames), "\n")

# Save a standalone HTML
out_html <- file.path("dev_notes", "trace_auction.html")
htmlwidgets::saveWidget(
  lap_animate(c0, method = "auction"),
  file = normalizePath(out_html, mustWork = FALSE),
  selfcontained = TRUE
)
cat("\nWrote", out_html, "\n")

invisible(NULL)
