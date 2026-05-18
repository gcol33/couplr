suppressMessages(devtools::load_all(quiet = TRUE))
set.seed(7)
c8 <- matrix(sample(1:50, 64, replace = TRUE), 8, 8)
w <- lap_animate(c8, method = "jv")
cat("Frames:", length(w$x$frames), "Cost:", w$x$meta$total_cost, "\n")
htmlwidgets::saveWidget(
  w,
  file = normalizePath(file.path("dev_notes", "trace_jv_8x8.html"), mustWork = FALSE),
  selfcontained = TRUE
)
cat("Wrote dev_notes/trace_jv_8x8.html\n")
