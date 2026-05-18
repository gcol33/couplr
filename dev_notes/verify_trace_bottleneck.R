suppressMessages(devtools::load_all(quiet = TRUE))

check_one <- function(label, cost, maximize = FALSE) {
  ref <- bottleneck_assignment(cost, maximize = maximize)
  w <- lap_animate(cost, method = "bottleneck", maximize = maximize)
  ok <- isTRUE(all.equal(ref$bottleneck, w$x$meta$total_cost))
  cat(sprintf("%-30s n=%2d frames=%3d ref=%.4g trace=%.4g %s\n",
              label, nrow(cost), length(w$x$frames),
              ref$bottleneck, w$x$meta$total_cost,
              if (ok) "OK" else "FAIL"))
  ok
}

set.seed(0xCAFE)
ok <- TRUE

c0 <- matrix(c(4,2,5,3,3,6,7,5,4), 3, 3, byrow=TRUE)
ok <- check_one("3x3 worked", c0) && ok

for (n in c(4, 6, 8, 10)) {
  ok <- check_one(sprintf("random %dx%d", n, n),
                  matrix(runif(n*n, 0, 100), n, n)) && ok
}

ok <- check_one("integer 5x5", matrix(sample(1:50, 25, replace=TRUE), 5, 5)) && ok

ok <- check_one("all-equal 4x4", matrix(5, 4, 4)) && ok

c_fight <- matrix(c( 1,  9,  9,  9,
                     1,  9,  9,  9,
                     9,  1,  9,  9,
                     9,  9,  1,  9), 4, 4, byrow=TRUE)
ok <- check_one("contention 4x4", c_fight) && ok

ok <- check_one("maximize 5x5", matrix(runif(25,0,100), 5, 5), maximize=TRUE) && ok

cf <- matrix(runif(25, 0, 100), 5, 5); cf[sample.int(25, 3)] <- NA
ok <- check_one("with NA forbidden 5x5", cf) && ok

cat("\n", if (ok) "ALL OK" else "FAILURES PRESENT", "\n", sep="")

cat("\n--- 4x4 frame breakdown ---\n")
set.seed(3)
w4 <- lap_animate(matrix(runif(16, 0, 100), 4, 4), method="bottleneck")
print(table(vapply(w4$x$frames, `[[`, character(1), "phase")))

htmlwidgets::saveWidget(
  lap_animate(c0, method="bottleneck"),
  file=normalizePath(file.path("dev_notes", "trace_bottleneck_3x3.html"), mustWork=FALSE),
  selfcontained=TRUE)

htmlwidgets::saveWidget(
  lap_animate(c_fight, method="bottleneck"),
  file=normalizePath(file.path("dev_notes", "trace_bottleneck_contention.html"), mustWork=FALSE),
  selfcontained=TRUE)

cat("Wrote demos.\n")
