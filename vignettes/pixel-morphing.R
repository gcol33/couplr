## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo       = FALSE,
  message    = FALSE,
  warning    = FALSE,
  fig.align  = "center",
  out.width  = "100%"
)

library(lapr)
library(knitr)

# Helper function to locate example assets
ext_demo <- function(...) {
  system.file("extdata", ..., package = "lapr")
}

## ----all-inputs, results='asis', echo=FALSE-----------------------------------
real_A   <- ext_demo("work",  "ImageA_80.png")
real_B   <- ext_demo("work",  "ImageB_80.png")
circle_A <- ext_demo("icons", "circleA_80.png")
circle_B <- ext_demo("icons", "circleB_80.png")

files <- c(real_A, real_B, circle_A, circle_B)

cat('<div class="pixel-row">\n')
for (f in files) {
  cat(sprintf('<img src="%s" alt="" />\n', f))
}
cat('</div>\n')

## ----exact-vis, results='asis', echo=FALSE------------------------------------
gif_image_exact  <- ext_demo("morphs", "image_exact.gif")
gif_circle_exact <- ext_demo("icons",  "circle_exact.gif")

files <- c(gif_image_exact, gif_circle_exact)

cat('<div class="pixel-row">\n')
for (f in files) {
  cat(sprintf('<img src="%s" alt="" />\n', f))
}
cat('</div>\n')

## ----color-walk-vis, results='asis', echo=FALSE-------------------------------
gif_image_cw  <- ext_demo("morphs", "image_color_walk.gif")
gif_circle_cw <- ext_demo("icons",  "circle_color_walk.gif")

files <- c(gif_image_cw, gif_circle_cw)

cat('<div class="pixel-row">\n')
for (f in files) {
  cat(sprintf('<img src="%s" alt="" />\n', f))
}
cat('</div>\n')

## ----recursive-vis, results='asis', echo=FALSE--------------------------------
gif_image_rec  <- ext_demo("morphs", "image_recursive.gif")
gif_circle_rec <- ext_demo("icons",  "circle_recursive.gif")

files <- c(gif_image_rec, gif_circle_rec)

cat('<div class="pixel-row">\n')
for (f in files) {
  cat(sprintf('<img src="%s" alt="" />\n', f))
}
cat('</div>\n')

