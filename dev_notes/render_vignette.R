devtools::load_all(quiet = TRUE)
library(rmarkdown)
res <- tryCatch(
  render("vignettes/matching-workflows.Rmd",
         output_format = "html_document",
         quiet = FALSE),
  error = function(e) conditionMessage(e)
)
cat("\nresult:", res, "\n")
