suppressMessages(devtools::load_all(quiet = TRUE))
cat("Total animated methods:", length(animated_methods()), "\n")
cat(paste(animated_methods(), collapse=", "), "\n\n")
cost <- matrix(c(4,2,5,3,3,6,7,5,4), 3, 3, byrow=TRUE)
for (m in animated_methods()) {
  w <- lap_animate(cost, method=m)
  cat(sprintf("%-15s frames=%3d total_cost=%g\n",
              m, length(w$x$frames), w$x$meta$total_cost))
}
