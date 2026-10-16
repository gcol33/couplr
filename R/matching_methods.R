# ==============================================================================
# plot() for matching_result
# ==============================================================================
# Histogram, density or ECDF of the pair distances. print() and summary() are
# shared by the couplr_result family in R/result_methods.R.
# ==============================================================================

#' @param type For `plot()` on a `"matching_result"`: `"histogram"` (default),
#'   `"density"` or `"ecdf"` of the pair distances.
#' @rdname couplr_result
#' @export
plot.matching_result <- function(x, type = c("histogram", "density", "ecdf"), ...) {
  type <- match.arg(type)
  distances <- x$pairs$distance

  if (length(distances) == 0) {
    message("No matched pairs to plot")
    return(invisible(x))
  }

  main_title <- paste0("Matching Distances (n=", length(distances), ")")


  switch(type,
    histogram = {
      graphics::hist(distances,
                     main = main_title,
                     xlab = "Distance",
                     col = "steelblue",
                     border = "white",
                     ...)
    },
    density = {
      d <- stats::density(distances)
      graphics::plot(d,
                     main = main_title,
                     xlab = "Distance",
                     ...)
      graphics::polygon(d, col = "steelblue", border = "steelblue")
    },
    ecdf = {
      graphics::plot(stats::ecdf(distances),
                     main = main_title,
                     xlab = "Distance",
                     ylab = "Cumulative Proportion",
                     ...)
    }
  )

  invisible(x)
}
