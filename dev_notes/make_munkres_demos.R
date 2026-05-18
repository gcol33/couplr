suppressMessages(devtools::load_all(quiet = TRUE))

# Contention case: cleanest demo of the augmentation step
c_fight <- matrix(c( 1,  9,  9,  9,
                     1,  9,  9,  9,
                     9,  1,  9,  9,
                     9,  9,  1,  9), 4, 4, byrow = TRUE)
w_fight <- lap_animate(c_fight, method = "munkres")
phase_counts <- table(vapply(w_fight$x$frames, `[[`, character(1), "phase"))
cat("Contention 4x4 frames:", length(w_fight$x$frames), "\n")
print(phase_counts)
htmlwidgets::saveWidget(
  w_fight,
  file = normalizePath(file.path("dev_notes", "trace_munkres_contention.html"),
                       mustWork = FALSE),
  selfcontained = TRUE
)

# Mid-size 6x6 random integer: shows full algorithm flow
set.seed(11)
c6 <- matrix(sample(1:50, 36, replace = TRUE), 6, 6)
w6 <- lap_animate(c6, method = "munkres")
phase_counts6 <- table(vapply(w6$x$frames, `[[`, character(1), "phase"))
cat("6x6 integer frames:", length(w6$x$frames), "\n")
print(phase_counts6)
htmlwidgets::saveWidget(
  w6,
  file = normalizePath(file.path("dev_notes", "trace_munkres_6x6.html"),
                       mustWork = FALSE),
  selfcontained = TRUE
)

cat("Wrote trace_munkres_contention.html and trace_munkres_6x6.html\n")
