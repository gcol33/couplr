suppressMessages(devtools::load_all(quiet = TRUE))
set.seed(1)
for (n in 3:5) {
  c <- matrix(runif(n*n, 0, 100), n, n)
  ref <- assignment(c, method="hungarian")
  w <- lap_animate(c, method="bruteforce")
  cat(sprintf("n=%d perms=%d frames=%d ref=%.4g trace=%.4g %s\n",
              n, factorial(n), length(w$x$frames),
              ref$total_cost, w$x$meta$total_cost,
              if (isTRUE(all.equal(ref$total_cost, w$x$meta$total_cost))) "OK" else "FAIL"))
}
cat("\n--- maximize 3x3 ---\n")
c0 <- matrix(c(4,2,5,3,3,6,7,5,4),3,3,byrow=TRUE)
ref <- assignment(c0, maximize=TRUE, method="hungarian")
w <- lap_animate(c0, method="bruteforce", maximize=TRUE)
cat(sprintf("ref=%.4g trace=%.4g %s\n", ref$total_cost, w$x$meta$total_cost,
            if (isTRUE(all.equal(ref$total_cost, w$x$meta$total_cost))) "OK" else "FAIL"))

cat("\n--- with forbidden 4x4 ---\n")
cf <- matrix(runif(16, 0, 100), 4, 4)
cf[1, 1] <- NA; cf[2, 2] <- NA
ref <- assignment(cf, method="hungarian")
w <- lap_animate(cf, method="bruteforce")
cat(sprintf("ref=%.4g trace=%.4g %s\n", ref$total_cost, w$x$meta$total_cost,
            if (isTRUE(all.equal(ref$total_cost, w$x$meta$total_cost))) "OK" else "FAIL"))

cat("\n--- frame sampling for n=6 ---\n")
set.seed(2)
c6 <- matrix(runif(36, 0, 100), 6, 6)
w6 <- lap_animate(c6, method="bruteforce")
cat(sprintf("n=6: 720 perms, %d frames emitted\n", length(w6$x$frames)))
print(table(vapply(w6$x$frames, `[[`, character(1), "phase")))
ref6 <- assignment(c6, method="hungarian")
cat(sprintf("ref=%.4g trace=%.4g %s\n", ref6$total_cost, w6$x$meta$total_cost,
            if (isTRUE(all.equal(ref6$total_cost, w6$x$meta$total_cost))) "OK" else "FAIL"))

htmlwidgets::saveWidget(
  lap_animate(c0, method="bruteforce"),
  file = normalizePath(file.path("dev_notes", "trace_bruteforce_3x3.html"), mustWork = FALSE),
  selfcontained = TRUE
)
htmlwidgets::saveWidget(
  lap_animate(c6, method="bruteforce"),
  file = normalizePath(file.path("dev_notes", "trace_bruteforce_6x6.html"), mustWork = FALSE),
  selfcontained = TRUE
)
cat("\nWrote demos.\n")
