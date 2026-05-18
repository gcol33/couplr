suppressMessages(devtools::load_all(quiet = TRUE))
tmp_rmd <- tempfile(fileext = ".Rmd")
writeLines(c(
  "---",
  "output: rmarkdown::html_vignette",
  "---",
  "",
  "Test widget:",
  "",
  "```{r}",
  "library(couplr)",
  "cost <- matrix(c(4,2,5,3,3,6,7,5,4),3,3,byrow=TRUE)",
  "lap_animate(cost, method = \"hungarian\")",
  "```"
), tmp_rmd)
knit_out <- tempfile(fileext = ".html")
rmarkdown::render(tmp_rmd, output_file = knit_out, quiet = TRUE)
html <- readLines(knit_out, warn = FALSE)
cat("Output size:", file.info(knit_out)$size, "bytes\n")
cat("htmlwidget hits:", sum(grepl("htmlwidget", html)), "\n")
cat("couplr-lap-animate hits:", sum(grepl("couplr-lap-animate", html)), "\n")
cat("script hits:", sum(grepl("<script", html)), "\n")
