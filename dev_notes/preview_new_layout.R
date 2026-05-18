# Render three lap_animate widgets at increasing n to verify the new
# "ladder + draw-in" layout scales from teaching size to stress size.
# Also re-renders the algorithms vignette so the in-vignette widgets
# pick up the JS/CSS rewrite.

suppressMessages(devtools::load_all(quiet = TRUE))

stopifnot(dir.exists("dev_notes"))

# 3x3: classic teaching example, jv shows column-reduction pre-stages
cost_3 <- matrix(c(4, 2, 5,
                   3, 3, 6,
                   7, 5, 4), nrow = 3, byrow = TRUE)
w3 <- lap_animate(cost_3, method = "jv")
htmlwidgets::saveWidget(w3,
  file = normalizePath(file.path("dev_notes", "preview_jv_3x3.html"),
                       mustWork = FALSE),
  selfcontained = TRUE
)
cat("wrote dev_notes/preview_jv_3x3.html  (frames=", length(w3$x$frames), ")\n",
    sep = "")

# 20x20: mid-scale, labels still visible
set.seed(20)
cost_20 <- matrix(sample(1:100, 400, replace = TRUE), 20, 20)
w20 <- lap_animate(cost_20, method = "jv")
htmlwidgets::saveWidget(w20,
  file = normalizePath(file.path("dev_notes", "preview_jv_20x20.html"),
                       mustWork = FALSE),
  selfcontained = TRUE
)
cat("wrote dev_notes/preview_jv_20x20.html  (frames=", length(w20$x$frames), ")\n",
    sep = "")

# 50x50: stress test, no labels, just dots + edges drawing in
set.seed(50)
cost_50 <- matrix(sample(1:1000, 2500, replace = TRUE), 50, 50)
w50 <- lap_animate(cost_50, method = "jv")
htmlwidgets::saveWidget(w50,
  file = normalizePath(file.path("dev_notes", "preview_jv_50x50.html"),
                       mustWork = FALSE),
  selfcontained = TRUE
)
cat("wrote dev_notes/preview_jv_50x50.html  (frames=", length(w50$x$frames), ")\n",
    sep = "")

# Re-render the vignette so the JS/CSS rewrite shows in the embedded widgets.
rmarkdown::render(
  "vignettes/algorithms.Rmd",
  output_format = "html_document",
  output_file = "../dev_notes/algorithms_vignette_preview.html",
  quiet = TRUE
)
cat("re-rendered dev_notes/algorithms_vignette_preview.html\n")
