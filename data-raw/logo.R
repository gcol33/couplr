out <- "output/icon-concepts"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
palette <- c(paper = "#F5F6F8", border = "#2B3A4A", blue = "#2E6F9E", red = "#B5342B")
hex <- function(r) {
  a <- pi / 2 + (0:5) * pi / 3
  paste(sprintf("%.2f,%.2f", 260 + r * cos(a), 300 - r * sin(a)), collapse = " ")
}
left <- c(130, 180, 235, 290, 350)
right <- c(325, 145, 270, 205, 365)
permutations <- function(x) {
  if (length(x) == 1L) return(matrix(x, nrow = 1L))
  do.call(rbind, lapply(seq_along(x), function(i) cbind(x[i], permutations(x[-i]))))
}
orders <- permutations(seq_along(right))
cost <- apply(orders, 1, function(p) sum((left - right[p])^2))
assignment <- orders[which.min(cost), ]
line <- function(x1, y1, x2, y2, ...) sprintf(
  '<line x1="%g" y1="%g" x2="%g" y2="%g" %s/>', x1, y1, x2, y2, paste(..., collapse = " "))
texture <- unlist(lapply(seq(-600, 900, 100), function(offset) {
  vapply(seq_along(left), function(i) line(-40, left[i] + offset, 560,
    right[assignment[i]] + offset, 'stroke="#2B3A4A" stroke-width="2" opacity="0.08"'), "")
}))
background <- paste(texture, collapse = "\n")
motion <- paste0('<g class="moving">', background,
  '<animateTransform attributeName="transform" type="translate" from="0 0" to="0 100" dur="32s" repeatCount="indefinite"/></g>',
  '<g class="still">', background, '</g>')
paired <- paste(vapply(seq_along(left), function(i) paste0(
  line(140, left[i], 380, right[assignment[i]], 'stroke="#2B3A4A" stroke-width="7" stroke-linecap="round"'),
  sprintf('<circle cx="140" cy="%g" r="15" fill="%s"/><circle cx="380" cy="%g" r="15" fill="%s"/>',
    left[i], palette[['red']], right[assignment[i]], palette[['blue']])), ""), collapse = "\n")
split_colour <- paste0('<path d="M260 0H0V600H260Z" fill="#B5342B"/>',
  '<path d="M260 0H520V600H260Z" fill="#2E6F9E"/>',
  motion)
upper <- list(
  rest = 'M84 254C111 223 137 187 172 157C194 137 211 131 231 141C243 146 252 153 263 153C277 153 287 141 302 137C329 128 351 146 376 168C401 190 421 218 438 243C456 274 376 261 333 242C302 222 283 244 263 244C240 246 221 228 193 240C159 253 78 282 84 254Z',
  pucker = 'M110 254C137 230 147 182 181 148C201 128 219 127 237 137C248 142 255 148 263 148C274 148 282 135 299 132C325 126 344 145 363 167C385 195 393 230 413 250C427 267 365 268 326 256C298 249 281 256 263 247C246 258 227 251 201 256C169 267 102 274 110 254Z')
lower <- list(
  rest = 'M97 277C113 266 126 278 145 271C166 264 181 262 201 269C224 276 246 280 260 287C272 294 284 280 299 277C321 267 338 258 358 267C383 262 411 278 431 280C440 286 393 340 366 357C334 379 307 391 278 385C247 389 211 389 181 378C145 366 110 325 97 277Z',
  pucker = 'M111 269C130 265 150 276 168 272C188 268 201 268 220 271C240 275 253 280 263 286C273 279 284 275 299 272C319 267 335 268 352 272C372 275 394 268 412 265C421 277 386 332 360 353C332 376 307 389 278 386C250 390 219 388 192 376C157 361 126 316 111 269Z')
crease_rest <- list(
  matrix(c(210,156,216,169,223,184,226,210,218,201,217,182,210,156), ncol=2, byrow=TRUE),
  matrix(c(178,179,184,191,196,206,204,219,191,214,185,197,178,179), ncol=2, byrow=TRUE),
  matrix(c(302,154,293,171,289,183,288,196,287,179,293,163,302,154), ncol=2, byrow=TRUE),
  matrix(c(337,172,334,190,328,207,317,216,325,199,331,188,337,172), ncol=2, byrow=TRUE),
  matrix(c(195,300,180,316,174,333,176,351,169,335,178,312,195,300), ncol=2, byrow=TRUE),
  matrix(c(229,309,221,326,215,347,214,366,210,348,217,323,229,309), ncol=2, byrow=TRUE),
  matrix(c(293,310,306,325,310,346,308,366,304,345,299,326,293,310), ncol=2, byrow=TRUE),
  matrix(c(329,296,344,309,350,326,349,343,342,322,337,308,329,296), ncol=2, byrow=TRUE))
crease_path <- function(p) paste0('M', paste(p[1, ], collapse=' '), 'C',
  paste(t(p[2:4, ]), collapse=' '), 'C', paste(t(p[5:7, ]), collapse=' '), 'Z')
crease_states <- lapply(seq_along(crease_rest), function(i) {
  p <- crease_rest[[i]]
  pucker <- p
  pucker[,1] <- 263 + (p[,1] - 263) * 0.84
  anchor <- if (i <= 4) 246 else 280
  pucker[,2] <- anchor + (p[,2] - anchor) * 0.9 - 3
  lapply(list(rest=p, pucker=pucker), crease_path)
})
animated_path <- function(states, fill, animate=TRUE, phase='rest') {
  sequence <- states[c('rest','rest','pucker','pucker','rest','rest')]
  paste0('<path fill="', fill, '" d="', states[[phase]], '">',
    if (animate) paste0('<animate attributeName="d" dur="9s" repeatCount="indefinite" ',
      'keyTimes="0;0.62;0.69;0.73;0.82;1" calcMode="spline" keySplines="',
      paste(rep('0.4 0 0.6 1',5),collapse=';'), '" values="', paste(sequence,collapse=';'), '"/>') else '', '</path>')
}
make_kiss <- function(animate=TRUE, phase='rest') paste0(
  animated_path(upper, '#FF4944', animate, phase), animated_path(lower, '#FF4944', animate, phase),
  paste(vapply(crease_states, animated_path, '', fill='#F5F6F8', animate=animate, phase=phase), collapse=''))
kiss_flash <- paste0('<defs><clipPath id="kiss-outline">',
  '<path d="M110 254C137 230 147 182 181 148C201 128 219 127 237 137',
  'C248 142 255 148 263 148C274 148 282 135 299 132',
  'C325 126 344 145 363 167C385 195 393 230 413 250',
  'L412 265C421 277 386 332 360 353C332 376 307 389 278 386',
  'C250 390 219 388 192 376C157 361 126 316 111 269Z"/>',
  '</clipPath><radialGradient id="kiss-flash" cx="50%" cy="50%" r="50%">',
  '<stop offset="0" stop-color="#F84150" stop-opacity="0.95"/>',
  '<stop offset="0.7" stop-color="#FF626A" stop-opacity="0.7"/>',
  '<stop offset="1" stop-color="#FF4944" stop-opacity="0"/>',
  '</radialGradient></defs>',
  '<ellipse cx="263" cy="263" rx="184" ry="72" fill="url(#kiss-flash)"/>')
kiss_motion <- paste0('<g class="moving">', make_kiss(),
  '<g opacity="0" clip-path="url(#kiss-outline)">', kiss_flash,
  '<animate attributeName="opacity" values="0;0;0.7;0;0" ',
  'keyTimes="0;0.68;0.71;0.75;1" dur="9s" repeatCount="indefinite"/>',
  '</g></g><g class="still">', make_kiss(FALSE), '</g>')
make_logo <- function(picture, coloured = FALSE) {
  name <- paste0('<text x="260" y="472" text-anchor="middle" font-family="Georgia, &apos;Times New Roman&apos;, serif" font-style="italic" font-weight="bold" font-size="78" ')
  paste0('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 520 600" width="520" height="600" role="img" aria-label="couplr logo">',
    '<title>couplr logo</title><style>.still{display:none}@media(prefers-reduced-motion:reduce){.moving{display:none}.still{display:inline}}</style>',
    '<defs><clipPath id="inner"><polygon points="', hex(288), '"/></clipPath></defs>',
    '<polygon points="', hex(288), '" fill="#F5F6F8"/>',
    '<g clip-path="url(#inner)">', if (!coloured) motion else "", picture,
    name, 'fill="none" stroke="', if (coloured) '#2B3A4A' else '#F5F6F8',
    '" stroke-width="10" stroke-linejoin="round" aria-hidden="true">couplr</text>',
    name, 'fill="', if (coloured) '#F5F6F8' else '#2E6F9E', '">couplr</text></g>',
    '<polygon points="', hex(291), '" fill="none" stroke="#2B3A4A" stroke-width="18" stroke-linejoin="round"/></svg>')
}
concepts <- list('01-matched-pairs' = make_logo(paired), '02-split' = make_logo(split_colour, TRUE),
                 '03-kiss' = make_logo(kiss_motion))
write_svg <- function(svg, path) {
  con <- file(path, 'wb')
  on.exit(close(con))
  writeBin(charToRaw(paste0(svg, '\n')), con)
}
for (name in names(concepts)) write_svg(concepts[[name]], file.path(out, paste0(name, '.svg')))
dir.create('man/figures', recursive=TRUE, showWarnings=FALSE)
write_svg(concepts[['03-kiss']], 'man/figures/logo.svg')
for (phase in c('rest', 'pucker')) {
  write_svg(make_logo(make_kiss(FALSE, phase)), file.path(out, paste0('kiss-', phase, '-frame.svg')))
}
write_svg(make_logo(paste0(make_kiss(FALSE, 'pucker'),
  '<g opacity="0.7" clip-path="url(#kiss-outline)">',
  kiss_flash, '</g>')), file.path(out, 'kiss-flash-frame.svg'))
writeLines(paste0('<!doctype html><html lang="en"><head><meta charset="utf-8">',
  '<meta name="viewport" content="width=device-width,initial-scale=1"><title>couplr kiss</title>',
  '<style>body{margin:0;min-height:100vh;display:grid;place-items:center;background:#fff}',
  'svg{display:block;width:min(80vw,440px);height:auto;max-height:90vh}</style>',
  '</head><body>', concepts[['03-kiss']], '</body></html>'), file.path(out, 'kiss-animated.html'))
cards <- paste(vapply(seq_along(concepts), function(i) sprintf(
  '<div><object type="image/svg+xml" data="%s.svg" width="312" height="360"></object><br><img src="%s.svg" height="139" alt="couplr concept %d"></div>',
  names(concepts)[i], names(concepts)[i], i), ""), collapse = "")
writeLines(paste0('<!doctype html><html><head><meta charset="utf-8"><title>couplr icons</title>',
  '<style>body{margin:24px;background:white}main{display:flex;gap:24px}div{text-align:center}object{display:block}</style></head><body><main>',
  cards, '</main></body></html>'), file.path(out, 'preview.html'))
