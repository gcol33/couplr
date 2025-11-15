# R/pixel_morph.R
# Pixel-level image morphing with optional square or recursive tiling
#
# Modes:
# - "exact"      : pixel-level LAP on normalized color (+ optional small spatial beta)
#                  Global LAP for patch_size = 1, block-level LAP for patch_size > 1
# - "color_walk" : color-driven assignment (quantize, then match by color + spatial)
#                  Scales much better; warnings only for very large images
# - "recursive"  : multi-scale 2×2 recursive tiling, falling back to square-tiling
#                  at the smallest scale, no color blending (transport only)


# =============================================================================
# Public API
# =============================================================================

#' Pixel-level image morphing (animation)
#'
#' Creates an animated morph by computing optimal pixel assignment from image A 
#' to image B, then rendering intermediate frames showing the transport.
#'
#' @param imgA Source image (file path or magick image object)
#' @param imgB Target image (file path or magick image object)
#' @param n_frames Integer number of animation frames (default: 16)
#' @param fps Frames per second for playback (default: 10)
#' @param format Output format: "gif", "webp", or "mp4"
#' @param outfile Optional output file path
#' @param show Logical, display animation in viewer (default: interactive())
#' @param mode Assignment algorithm: "color_walk" (default), "exact", or "recursive"
#' @param lap_method LAP solver method (default: "jv")
#' @param maximize Logical, maximize instead of minimize cost (default: FALSE)
#' @param quantize_bits Color quantization for "color_walk" mode (default: 5)
#' @param downscale_steps Number of 2x reductions before computing assignment (default: 0)
#' @param alpha Weight for color distance in cost function (default: 1)
#' @param beta Weight for spatial distance in cost function (default: 0)
#' @param patch_size Tile size for tiled modes (default: 1)
#' @param upscale Post-rendering upscaling factor (default: 1)
#'
#' @return Invisibly returns a list with animation object and metadata:
#'   \item{animation}{magick animation object}
#'   \item{width}{Image width in pixels}
#'   \item{height}{Image height in pixels}  
#'   \item{assignment}{Integer vector of 1-based assignment indices (R convention)}
#'   \item{n_pixels}{Total number of pixels}
#'   \item{mode}{Mode used for matching}
#'   \item{upscale}{Upscaling factor applied}
#'
#' @details
#' ## Assignment vs Rendering Semantics
#' 
#' **CRITICAL:** This function has two separate phases with different semantics:
#' 
#' **Phase 1 - Assignment Computation:**
#' 
#' The assignment is computed by minimizing:
#' \preformatted{
#'   cost(i,j) = alpha * color_distance(A[i], B[j]) + 
#'               beta * spatial_distance(pos_i, pos_j)
#' }
#' 
#' This means B's COLORS influence which pixels from A map to which positions.
#' 
#' **Phase 2 - Rendering (Transport-Only):**
#' 
#' The renderer uses ONLY A's colors:
#' - Intermediate frames: A's pixels move along paths with motion blur
#' - Final frame: A's pixels at their assigned positions (sharp, no blur)
#' - B's colors NEVER appear in the output
#' 
#' **Result:** You get A's colors rearranged to match B's geometry/layout.
#' 
#' ## What This Means
#' 
#' - B influences WHERE pixels go (via similarity in cost function)
#' - B does NOT determine WHAT COLORS appear in output
#' - Final image has A's palette arranged to mimic B's structure
#' 
#' ## Parameter Guidance
#' 
#' **For pure spatial rearrangement (ignore B's colors in assignment):**
#' \preformatted{
#'   pixel_morph_animate(A, B, alpha = 0, beta = 1)
#' }
#' 
#' **For color-similarity matching (default):**
#' \preformatted{
#'   pixel_morph_animate(A, B, alpha = 1, beta = 0)
#' }
#' 
#' **For hybrid (color + spatial):**
#' \preformatted{
#'   pixel_morph_animate(A, B, alpha = 1, beta = 0.2)
#' }
#' 
#' ## Permutation Guarantees
#' 
#' Assignment is guaranteed to be a bijection (permutation) ONLY when:
#' - \code{downscale_steps = 0} (no resolution changes)
#' - \code{mode = "exact"} with \code{patch_size = 1}
#' 
#' With downscaling or tiled modes, assignment may have:
#' - **Overlaps:** Multiple source pixels map to same destination (last write wins)
#' - **Holes:** Some destinations never filled (remain transparent)
#' 
#' A warning is issued if overlaps/holes are detected in the final frame.
#'
#' @examples
#' \dontrun{
#' # Basic animation with default settings
#' pixel_morph_animate("imageA.png", "imageB.png", outfile = "morph.gif")
#'
#' # Pure spatial rearrangement (ignore B's colors in assignment)
#' pixel_morph_animate("imageA.png", "imageB.png",
#'                     alpha = 0, beta = 1, outfile = "spatial.gif")
#'
#' # Large image with downscaling
#' pixel_morph_animate("largeA.png", "largeB.png",
#'                     mode = "color_walk", downscale_steps = 2,
#'                     outfile = "large_morph.gif")
#' }
#'
#' @export
pixel_morph_animate <- function(imgA,
                                imgB,
                                n_frames = 16L,
                                fps = 10L,
                                format = c("gif", "webp", "mp4"),
                                outfile = NULL,
                                show = interactive(),
                                mode = c("color_walk", "exact", "recursive"),
                                lap_method = "jv",
                                maximize = FALSE,
                                quantize_bits = 5L,
                                downscale_steps = 0L,
                                alpha = 1,
                                beta  = 0,
                                patch_size = 1L,
                                upscale = 1) {

  format  <- match.arg(format)
  mode    <- match.arg(mode)
  
  # Robust input validation
  if (!is.numeric(upscale) || length(upscale) != 1 || is.na(upscale)) {
    stop("upscale must be a single numeric value", call. = FALSE)
  }
  upscale <- as.numeric(upscale)
  if (upscale <= 0) {
    warning("upscale must be positive, setting to 1", call. = FALSE)
    upscale <- 1
  }
  
  if (!is.numeric(n_frames) || length(n_frames) != 1 || is.na(n_frames)) {
    stop("n_frames must be a single numeric value", call. = FALSE)
  }
  n_frames <- as.integer(n_frames)
  if (n_frames < 2L) {
    warning("n_frames must be at least 2, setting to 2", call. = FALSE)
    n_frames <- 2L
  }
  
  if (!is.numeric(alpha) || length(alpha) != 1 || is.na(alpha) || alpha < 0) {
    stop("alpha must be a single non-negative numeric value", call. = FALSE)
  }
  if (!is.numeric(beta) || length(beta) != 1 || is.na(beta) || beta < 0) {
    stop("beta must be a single non-negative numeric value", call. = FALSE)
  }
  if (alpha == 0 && beta == 0) {
    stop("alpha and beta cannot both be zero", call. = FALSE)
  }
  
  if (!is.numeric(patch_size) || length(patch_size) != 1 || is.na(patch_size)) {
    stop("patch_size must be a single numeric value", call. = FALSE)
  }
  patch_size <- as.integer(patch_size)
  if (patch_size < 1L) {
    stop("patch_size must be at least 1", call. = FALSE)
  }
  
  if (!is.numeric(downscale_steps) || length(downscale_steps) != 1 || is.na(downscale_steps)) {
    stop("downscale_steps must be a single numeric value", call. = FALSE)
  }
  downscale_steps <- as.integer(downscale_steps)
  if (downscale_steps < 0L) {
    stop("downscale_steps must be non-negative", call. = FALSE)
  }

  if (!.has_namespace("magick")) stop("Package 'magick' is required.")

  # Read images and align sizes
  A <- if (is.character(imgA)) magick::image_read(imgA) else imgA
  B <- if (is.character(imgB)) magick::image_read(imgB) else imgB

  infoA <- magick::image_info(A)
  infoB <- magick::image_info(B)

  if (infoA$width != infoB$width || infoA$height != infoB$height) {
    B <- magick::image_resize(
      B,
      geometry = sprintf("%dx%d!", infoA$width, infoA$height)
    )
    infoB <- magick::image_info(B)
  }

  H <- as.integer(infoA$height)
  W <- as.integer(infoA$width)
  N <- H * W

  # Planar RGB buffers (0–255, column-major)
  arrA    <- .to_array_rgb(A)
  arrB    <- .to_array_rgb(B)
  planarA <- .to_planar_rgb(arrA)
  planarB <- .to_planar_rgb(arrB)

  # Optional downscaling for assignment computation
  ds   <- .downscale_both(planarA, planarB, H, W, steps = downscale_steps)
  Hs   <- ds$Hs
  Ws   <- ds$Ws
  A_s  <- ds$A_s
  B_s  <- ds$B_s
  Ns   <- Hs * Ws

  # Size checks based on mode (after downscaling)
  patch_size <- as.integer(patch_size)
  if (patch_size < 1L) patch_size <- 1L

  if (mode %in% c("exact", "recursive")) {

    if (patch_size <= 1L && mode == "exact") {
      MAX_EXACT <- 4096L
      if (Ns > MAX_EXACT) {
        warning(sprintf(
          paste0(
            "Image is large for 'exact' global LAP: %d x %d = %d pixels (recommended max %d).\n",
            "Computation may be slow — consider using patch_size > 1, mode='recursive',",
            " or mode='color_walk'."
          ),
          Hs, Ws, Ns, MAX_EXACT
        ), call. = FALSE)
      }

    } else {
      tile_n <- patch_size * patch_size
      MAX_TILE_EXACT <- 400L

      if (tile_n > MAX_TILE_EXACT) {
        warning(sprintf(
          paste0(
            "Tile is large for LAP: patch_size=%d -> %d pixels per tile (recommended max %d).\n",
            "Computation may be slow — reduce patch_size or use mode='color_walk'."
          ),
          patch_size, tile_n, MAX_TILE_EXACT
        ), call. = FALSE)
      }
    }

  } else if (mode == "color_walk") {

    WARN_THRESHOLD <- 250000L
    if (Ns > WARN_THRESHOLD) {
      warning(sprintf(
        paste0(
          "Large image: %d x %d = %d pixels. This may take a while.\n",
          "Consider increasing downscale_steps or reducing image size."
        ),
        Hs, Ws, Ns
      ), call. = FALSE)
    }
  }

  # Compute pixel assignment at (Hs, Ws)
  if (mode == "exact") {

    if (patch_size > 1L) {
      assign_s <- .square_tiling_solver(
        A_planar      = A_s,
        B_planar      = B_s,
        H             = Hs,
        W             = Ws,
        max_tile_size = patch_size,
        alpha         = alpha,
        beta          = beta,
        method        = lap_method,
        maximize      = maximize
      )
    } else {
      assign_s <- .exact_cost_and_solve(
        A_s, B_s, Hs, Ws, alpha, beta, lap_method, maximize
      )
    }

  } else if (mode == "recursive") {

    assign_s <- .recursive_tiling_solver(
      A_planar   = A_s,
      B_planar   = B_s,
      H          = Hs,
      W          = Ws,
      patch_size = patch_size,
      alpha      = alpha,
      beta       = beta,
      method     = lap_method,
      maximize   = maximize
    )

  } else {
    assign_s <- .solve_color_walk_pipeline(
      A_s,
      B_s,
      Hs,
      Ws,
      quantize_bits,
      lap_method,
      maximize
    )
  }

  # Upscale assignment back to original resolution and convert to 0-based
  assign_in <- as.integer(assign_s) - 1L

  if (downscale_steps > 0L && (Hs != H || Ws != W)) {
    assign_0based <- .upscale_assignment(
      assign_in,
      H  = H,
      W  = W,
      Hs = Hs,
      Ws = Ws
    )
  } else {
    assign_0based <- assign_in
  }

  # Render morph frames via C++ (assignment is 0-based here)
  # NOTE: morph_pixel_level_cpp() returns a sharp, transport-only final frame
  #       (no motion blur), while intermediate frames use bilinear splatting.
  frames <- morph_pixel_level_cpp(
    planarA,
    planarB,
    assign_0based,
    H,
    W,
    n_frames
  )

  magick_list <- lapply(frames, function(fr) {
    Hf  <- fr$H
    Wf  <- fr$W
    vec <- fr$data
    Cch <- length(vec) / (Hf * Wf)

    if (!Cch %in% c(3, 4)) {
      stop("Unexpected channel count from morph_pixel_level_cpp: ", Cch)
    }

    arr <- array(vec / 255, dim = c(Hf, Wf, Cch))
    magick::image_read(arr)
  })

  magick_frames <- magick::image_join(magick_list)

  # Optional upscaling of rendered frames (display only, LAP already done)
  if (upscale != 1) {
    if (abs(upscale - round(upscale)) < 1e-9) {
      target_width  <- as.integer(round(W * upscale))
      target_height <- as.integer(round(H * upscale))
      magick_frames <- magick::image_scale(
        magick_frames,
        geometry = sprintf("%dx%d!", target_width, target_height)
      )
    } else {
      pct <- upscale * 100
      magick_frames <- magick::image_scale(
        magick_frames,
        geometry = sprintf("%.2f%%", pct)
      )
    }
  }

  # Per-frame delays so last frame lingers a bit longer
  base_delay <- 100 / fps

  if (n_frames <= 1L) {
    delays <- base_delay
  } else {
    delays <- rep(base_delay, n_frames)
    linger_delay <- base_delay * (n_frames - 1L) / 3
    delays[n_frames] <- linger_delay
  }

  animation <- magick::image_animate(
    magick_frames,
    delay   = delays,
    dispose = "background",
    loop    = 0
  )

  # Save to file if requested
  if (!is.null(outfile)) {
    if (format == "mp4") {
      if (!.has_namespace("av")) {
        stop("Package 'av' is required for mp4 output.")
      }
      temp_dir   <- tempdir()
      temp_files <- file.path(
        temp_dir,
        sprintf("frame_%04d.png", seq_len(n_frames))
      )

      for (i in seq_len(n_frames)) {
        frame_i <- magick_frames[i]
        magick::image_write(frame_i, temp_files[i], format = "png")
      }

      av::av_encode_video(
        temp_files,
        output    = outfile,
        framerate = fps
      )
      unlink(temp_files)

    } else if (format == "webp") {
      magick::image_write(animation, path = outfile, format = "webp")

    } else if (format == "gif") {
      magick::image_write(animation, path = outfile, format = "gif")

    } else {
      stop("Unknown format: ", format)
    }

    message("Saved animation to: ", outfile)
  }

  # Show in viewer if requested
  if (show) {
    print(animation)
  }

  invisible(list(
    animation  = animation,
    width      = W,
    height     = H,
    assignment = assign_0based + 1L,  # Return 1-based for R convention
    n_pixels   = N,
    mode       = mode,
    upscale    = upscale
  ))
}

#' Pixel-level image morphing (final frame only)
#'
#' Computes optimal pixel assignment from A to B and returns the final 
#' transported frame (without intermediate animation frames).
#'
#' @param imgA Source image (file path or magick image object)
#' @param imgB Target image (file path or magick image object)
#' @param n_frames Internal parameter for rendering (default: 16)
#' @param mode Assignment algorithm: "color_walk" (default), "exact", or "recursive"
#' @param lap_method LAP solver method (default: "jv")
#' @param maximize Logical, maximize instead of minimize cost (default: FALSE)
#' @param quantize_bits Color quantization for "color_walk" mode (default: 5)
#' @param downscale_steps Number of 2x reductions before computing assignment (default: 0)
#' @param alpha Weight for color distance in cost function (default: 1)
#' @param beta Weight for spatial distance in cost function (default: 0)
#' @param patch_size Tile size for tiled modes (default: 1)
#' @param upscale Post-rendering upscaling factor (default: 1)
#' @param show Logical, display result in viewer (default: interactive())
#'
#' @return magick image object of the final transported frame
#'
#' @details
#' ## Transport-Only Semantics
#' 
#' This function returns a SHARP, pixel-perfect transport of A's pixels
#' to positions determined by the assignment to B.
#' 
#' **Key Points:**
#' - Assignment computed using: \code{cost = alpha * color_dist + beta * spatial_dist}
#' - B's COLORS influence assignment but DO NOT appear in output
#' - Result has A's colors arranged to match B's layout
#' - No motion blur (unlike intermediate frames in animation)
#' 
#' See \code{\link{pixel_morph_animate}} for detailed explanation of 
#' assignment vs rendering semantics.
#' 
#' ## Permutation Warnings
#' 
#' Assignment is guaranteed to be a bijection (permutation) ONLY when:
#' - \code{downscale_steps = 0} (no resolution changes)
#' - \code{mode = "exact"} with \code{patch_size = 1}
#' 
#' With downscaling or tiled modes, assignment may have:
#' - **Overlaps:** Multiple source pixels map to same destination (last write wins)
#' - **Holes:** Some destinations never filled (remain transparent)
#' 
#' If assignment is not a bijection (due to downscaling or tiling),
#' a warning will be issued. The result may contain:
#' - Overlapped pixels (multiple sources → one destination)
#' - Transparent holes (some destinations unfilled)
#' 
#' For guaranteed pixel-perfect results, use:
#' \preformatted{
#'   pixel_morph(A, B, mode = "exact", downscale_steps = 0)
#' }
#'
#' @examples
#' \dontrun{
#' # Basic morph
#' result <- pixel_morph("imageA.png", "imageB.png")
#'
#' # Pure spatial rearrangement
#' result <- pixel_morph("imageA.png", "imageB.png",
#'                       alpha = 0, beta = 1)
#'
#' # Exact mode for small images (guaranteed permutation)
#' result <- pixel_morph("small_A.png", "small_B.png",
#'                       mode = "exact", downscale_steps = 0)
#' }
#'
#' @seealso \code{\link{pixel_morph_animate}} for animated version
#'
#' @export
pixel_morph <- function(imgA,
                        imgB,
                        n_frames = 16L,
                        mode = c("color_walk", "exact", "recursive"),
                        lap_method = "jv",
                        maximize = FALSE,
                        quantize_bits = 5L,
                        downscale_steps = 0L,
                        alpha = 1,
                        beta  = 0,
                        patch_size = 1L,
                        upscale = 1,
                        show = interactive()) {

  mode <- match.arg(mode)
  
  # Robust input validation
  if (!is.numeric(upscale) || length(upscale) != 1 || is.na(upscale)) {
    stop("upscale must be a single numeric value", call. = FALSE)
  }
  upscale <- as.numeric(upscale)
  if (upscale <= 0) {
    warning("upscale must be positive, setting to 1", call. = FALSE)
    upscale <- 1
  }
  
  if (!is.numeric(n_frames) || length(n_frames) != 1 || is.na(n_frames)) {
    stop("n_frames must be a single numeric value", call. = FALSE)
  }
  n_frames <- as.integer(n_frames)
  if (n_frames < 2L) {
    warning("n_frames must be at least 2, setting to 2", call. = FALSE)
    n_frames <- 2L
  }
  
  if (!is.numeric(alpha) || length(alpha) != 1 || is.na(alpha) || alpha < 0) {
    stop("alpha must be a single non-negative numeric value", call. = FALSE)
  }
  if (!is.numeric(beta) || length(beta) != 1 || is.na(beta) || beta < 0) {
    stop("beta must be a single non-negative numeric value", call. = FALSE)
  }
  if (alpha == 0 && beta == 0) {
    stop("alpha and beta cannot both be zero", call. = FALSE)
  }
  
  if (!is.numeric(patch_size) || length(patch_size) != 1 || is.na(patch_size)) {
    stop("patch_size must be a single numeric value", call. = FALSE)
  }
  patch_size <- as.integer(patch_size)
  if (patch_size < 1L) {
    stop("patch_size must be at least 1", call. = FALSE)
  }
  
  if (!is.numeric(downscale_steps) || length(downscale_steps) != 1 || is.na(downscale_steps)) {
    stop("downscale_steps must be a single numeric value", call. = FALSE)
  }
  downscale_steps <- as.integer(downscale_steps)
  if (downscale_steps < 0L) {
    stop("downscale_steps must be non-negative", call. = FALSE)
  }

  if (!.has_namespace("magick")) stop("Package 'magick' is required.")

  # Read images and align sizes
  A <- if (is.character(imgA)) magick::image_read(imgA) else imgA
  B <- if (is.character(imgB)) magick::image_read(imgB) else imgB

  infoA <- magick::image_info(A)
  infoB <- magick::image_info(B)

  if (infoA$width != infoB$width || infoA$height != infoB$height) {
    B <- magick::image_resize(
      B,
      geometry = sprintf("%dx%d!", infoA$width, infoA$height)
    )
    infoB <- magick::image_info(B)
  }

  H <- as.integer(infoA$height)
  W <- as.integer(infoA$width)

  # Planar RGB buffers
  arrA    <- .to_array_rgb(A)
  arrB    <- .to_array_rgb(B)
  planarA <- .to_planar_rgb(arrA)
  planarB <- .to_planar_rgb(arrB)

  # Optional downscaling for assignment computation
  ds   <- .downscale_both(planarA, planarB, H, W, steps = downscale_steps)
  Hs   <- ds$Hs
  Ws   <- ds$Ws
  A_s  <- ds$A_s
  B_s  <- ds$B_s
  Ns   <- Hs * Ws

  # Size checks based on mode (after downscaling)
  patch_size <- as.integer(patch_size)
  if (patch_size < 1L) patch_size <- 1L

  if (mode %in% c("exact", "recursive")) {

    if (patch_size <= 1L && mode == "exact") {
      MAX_EXACT <- 4096L
      if (Ns > MAX_EXACT) {
        warning(sprintf(
          paste0(
            "Image is large for 'exact' global LAP: %d x %d = %d pixels (recommended max %d).\n",
            "Computation may be slow — consider using patch_size > 1, mode='recursive',",
            " or mode='color_walk'."
          ),
          Hs, Ws, Ns, MAX_EXACT
        ), call. = FALSE)
      }

    } else {
      tile_n <- patch_size * patch_size
      MAX_TILE_EXACT <- 400L

      if (tile_n > MAX_TILE_EXACT) {
        warning(sprintf(
          paste0(
            "Tile is large for LAP: patch_size=%d -> %d pixels per tile (recommended max %d).\n",
            "Computation may be slow — reduce patch_size or use mode='color_walk'."
          ),
          patch_size, tile_n, MAX_TILE_EXACT
        ), call. = FALSE)
      }
    }

  } else if (mode == "color_walk") {

    WARN_THRESHOLD <- 250000L
    if (Ns > WARN_THRESHOLD) {
      warning(sprintf(
        paste0(
          "Large image: %d x %d = %d pixels. This may take a while.\n",
          "Consider increasing downscale_steps or reducing image size."
        ),
        Hs, Ws, Ns
      ), call. = FALSE)
    }
  }

  # Compute pixel assignment at (Hs, Ws)
  if (mode == "exact") {

    if (patch_size > 1L) {
      assign_s <- .square_tiling_solver(
        A_planar      = A_s,
        B_planar      = B_s,
        H             = Hs,
        W             = Ws,
        max_tile_size = patch_size,
        alpha         = alpha,
        beta          = beta,
        method        = lap_method,
        maximize      = maximize
      )
    } else {
      assign_s <- .exact_cost_and_solve(
        A_s, B_s, Hs, Ws, alpha, beta, lap_method, maximize
      )
    }

  } else if (mode == "recursive") {

    assign_s <- .recursive_tiling_solver(
      A_planar   = A_s,
      B_planar   = B_s,
      H          = Hs,
      W          = Ws,
      patch_size = patch_size,
      alpha      = alpha,
      beta       = beta,
      method     = lap_method,
      maximize   = maximize
    )

  } else {
    # color_walk mode - use positional arguments matching function signature
    assign_s <- .solve_color_walk_pipeline(
      A_s,             # Ap
      B_s,             # Bp
      Hs,              # H
      Ws,              # W
      quantize_bits,   # quantize_bits
      lap_method,      # method
      maximize         # maximize
    )
  }

  # 1-based → 0-based
  assign_in <- as.integer(assign_s) - 1L

  # Upscale assignment if we computed it at a smaller resolution
  if (downscale_steps > 0L && (Hs != H || Ws != W)) {
    assign_0based <- .upscale_assignment(
      assign_in,
      H  = H,
      W  = W,
      Hs = Hs,
      Ws = Ws
    )
  } else {
    assign_0based <- assign_in
  }

  # Render morph frames and take final frame only
  # NOTE: morph_pixel_level_cpp() returns a sharp, non-splatted final frame.
  #       We take only that frame here (transport-only morph).
  frames <- morph_pixel_level_cpp(
    planarA,
    planarB,
    assign_0based,
    H,
    W,
    n_frames
  )
  
  # Safety check: ensure we got the expected number of frames
  stopifnot(length(frames) == n_frames)

  last_fr <- frames[[length(frames)]]
  Hf      <- last_fr$H
  Wf      <- last_fr$W
  vec     <- last_fr$data
  Cch     <- length(vec) / (Hf * Wf)

  if (!Cch %in% c(3, 4)) {
    stop("Unexpected channel count from morph_pixel_level_cpp: ", Cch)
  }

  arr <- array(vec / 255, dim = c(Hf, Wf, Cch))
  final_img <- magick::image_read(arr)

  # Optional upscaling of final frame
  if (upscale != 1) {
    if (abs(upscale - round(upscale)) < 1e-9) {
      target_width  <- as.integer(round(W * upscale))
      target_height <- as.integer(round(H * upscale))
      final_img <- magick::image_scale(
        final_img,
        geometry = sprintf("%dx%d!", target_width, target_height)
      )
    } else {
      pct <- upscale * 100
      final_img <- magick::image_scale(
        final_img,
        geometry = sprintf("%.2f%%", pct)
      )
    }
  }

  if (show) {
    print(final_img)
  }

  invisible(final_img)
}


# =============================================================================
# Core solvers
# =============================================================================

#' Exact pixel-level LAP on full N x N cost (global)
#' @noRd
.exact_cost_and_solve <- function(Ap, Bp, H, W, alpha, beta, method, maximize) {
  C   <- compute_pixel_cost_cpp(Ap, Bp, H, W, alpha, beta)
  asg <- .lap_assign(C, method = method, maximize = maximize)
  as.integer(asg) + 1L
}

#' Square tiling solver: move tiles as rigid blocks
#' @noRd
.square_tiling_solver <- function(A_planar, B_planar, H, W,
                                  max_tile_size = 3L,
                                  alpha = 1, beta = 0,
                                  method = "jv", maximize = FALSE) {

  N <- H * W
  max_tile_size <- as.integer(max_tile_size)
  if (max_tile_size < 1L) max_tile_size <- 1L

  idx_cm <- function(x, y, H) x * H + y + 1L

  # Build tiles
  tiles   <- .generate_square_tiles(W, H, P = max_tile_size)
  n_tiles <- length(tiles)

  # Compute tile sizes, centers + mean colors
  sizes    <- integer(n_tiles)
  centers  <- matrix(0, n_tiles, 2)
  colors_A <- matrix(0, n_tiles, 3)
  colors_B <- matrix(0, n_tiles, 3)

  for (k in seq_len(n_tiles)) {
    tile <- tiles[[k]]
    x0   <- tile$x0
    y0   <- tile$y0
    sz   <- tile$size
    sizes[k] <- sz

    centers[k, 1] <- x0 + (sz - 1) / 2
    centers[k, 2] <- y0 + (sz - 1) / 2

    idxs  <- integer(sz * sz)
    c_idx <- 0L
    for (dy in 0:(sz - 1L)) {
      for (dx in 0:(sz - 1L)) {
        x <- x0 + dx
        y <- y0 + dy
        c_idx <- c_idx + 1L
        idxs[c_idx] <- idx_cm(x, y, H)
      }
    }

    colors_A[k, ] <- c(
      mean(A_planar[idxs])          / 255,
      mean(A_planar[idxs + N])      / 255,
      mean(A_planar[idxs + 2 * N])  / 255
    )

    colors_B[k, ] <- c(
      mean(B_planar[idxs])          / 255,
      mean(B_planar[idxs + N])      / 255,
      mean(B_planar[idxs + 2 * N])  / 255
    )
  }

  # Assignment vector (1-based)
  assignment <- rep(NA_integer_, N)

  # Spatial distances normalized by image diagonal
  diag_norm <- sqrt(H^2 + W^2)

  # Solve LAP inside each tile-size group
  for (sz in sort(unique(sizes))) {

    group <- which(sizes == sz)
    ng    <- length(group)
    if (ng == 0L) next

    # Build separate color + spatial distance matrices
    dc_mat <- matrix(0, ng, ng)
    ds_mat <- matrix(0, ng, ng)

    for (i in seq_len(ng)) {
      ti <- group[i]
      for (j in seq_len(ng)) {
        tj <- group[j]

        dc <- sqrt(sum((colors_A[ti, ] - colors_B[tj, ])^2))
        ds <- sqrt(sum((centers[ti, ] - centers[tj, ])^2)) / diag_norm

        dc_mat[i, j] <- dc
        ds_mat[i, j] <- ds
      }
    }

    # Normalize so alpha / beta are comparable
    mean_dc <- mean(dc_mat)
    mean_ds <- mean(ds_mat)

    if (mean_dc <= 0) mean_dc <- 1
    if (mean_ds <= 0) mean_ds <- 1

    dc_norm <- dc_mat / mean_dc
    ds_norm <- ds_mat / mean_ds

    C <- alpha * dc_norm + beta * ds_norm

    # LAP
    perm0 <- .lap_assign(C, method = method, maximize = maximize)
    perm  <- as.integer(perm0) + 1L

    # Map tiles as rigid blocks
    for (i in seq_len(ng)) {
      src_id <- group[i]
      dst_id <- group[perm[i]]
      src    <- tiles[[src_id]]
      dst    <- tiles[[dst_id]]

      stopifnot(src$size == dst$size)
      sz_tile <- src$size

      for (dy in 0:(sz_tile - 1L)) {
        for (dx in 0:(sz_tile - 1L)) {

          xs <- src$x0 + dx
          ys <- src$y0 + dy
          xd <- dst$x0 + dx
          yd <- dst$y0 + dy

          ia <- idx_cm(xs, ys, H)
          ib <- idx_cm(xd, yd, H)

          assignment[ia] <- ib
        }
      }
    }
  }

  # Unassigned pixels (should not happen, but keep safe)
  unassigned <- which(is.na(assignment))
  if (length(unassigned)) {
    assignment[unassigned] <- unassigned
  }

  assignment
}

#' Recursive tiling solver: multi-scale 2×2 splitting + square tiling at leaves
#' @noRd
.recursive_tiling_solver <- function(A_planar, B_planar, H, W,
                                     patch_size = 3L,
                                     alpha = 1, beta = 0,
                                     method = "jv", maximize = FALSE) {

  N <- H * W
  patch_size <- as.integer(patch_size)
  if (patch_size < 1L) patch_size <- 1L

  idx_cm <- function(x, y, H) x * H + y + 1L

  assignment <- rep(NA_integer_, N)
  N_global   <- N

  # Helper: solve a sub-block via square-tiling, then map back to global indices
  solve_small_block <- function(src_x0, src_y0, dst_x0, dst_y0, w, h) {
    if (w <= 0 || h <= 0) return()

    N_local <- w * h
    eff_tile <- min(patch_size, w, h)

    A_sub <- numeric(3L * N_local)
    B_sub <- numeric(3L * N_local)

    for (xl in 0:(w - 1L)) {
      for (yl in 0:(h - 1L)) {
        xg_src <- src_x0 + xl
        yg_src <- src_y0 + yl
        xg_dst <- dst_x0 + xl
        yg_dst <- dst_y0 + yl

        ia <- idx_cm(xg_src, yg_src, H)
        ib <- idx_cm(xg_dst, yg_dst, H)

        il <- xl * h + yl + 1L

        A_sub[il]                 <- A_planar[ia]
        A_sub[il + N_local]       <- A_planar[ia + N_global]
        A_sub[il + 2L * N_local]  <- A_planar[ia + 2L * N_global]

        B_sub[il]                 <- B_planar[ib]
        B_sub[il + N_local]       <- B_planar[ib + N_global]
        B_sub[il + 2L * N_local]  <- B_planar[ib + 2L * N_global]
      }
    }

    local_asg <- .square_tiling_solver(
      A_planar      = A_sub,
      B_planar      = B_sub,
      H             = h,
      W             = w,
      max_tile_size = eff_tile,
      alpha         = alpha,
      beta          = beta,
      method        = method,
      maximize      = maximize
    )

    for (xl in 0:(w - 1L)) {
      for (yl in 0:(h - 1L)) {
        il <- xl * h + yl + 1L
        j  <- local_asg[il]

        xl2 <- (j - 1L) %/% h
        yl2 <- (j - 1L) %%  h

        xg_src <- src_x0 + xl
        yg_src <- src_y0 + yl
        xg_dst <- dst_x0 + xl2
        yg_dst <- dst_y0 + yl2

        ia <- idx_cm(xg_src, yg_src, H)
        ib <- idx_cm(xg_dst, yg_dst, H)

        assignment[ia] <<- ib
      }
    }
  }

  # Main recursive solver for a block
  solve_block <- function(src_x0, src_y0, dst_x0, dst_y0, w, h) {
    if (w <= patch_size || h <= patch_size) {
      solve_small_block(src_x0, src_y0, dst_x0, dst_y0, w, h)
      return(invisible(NULL))
    }

    main_w <- w - (w %% 2L)
    main_h <- h - (h %% 2L)

    if (main_w < 2L || main_h < 2L) {
      solve_small_block(src_x0, src_y0, dst_x0, dst_y0, w, h)
      return(invisible(NULL))
    }

    tile_w <- main_w %/% 2L
    tile_h <- main_h %/% 2L

    tilesA <- list(
      list(x0 = src_x0,             y0 = src_y0,             w = tile_w, h = tile_h),
      list(x0 = src_x0 + tile_w,    y0 = src_y0,             w = tile_w, h = tile_h),
      list(x0 = src_x0,             y0 = src_y0 + tile_h,    w = tile_w, h = tile_h),
      list(x0 = src_x0 + tile_w,    y0 = src_y0 + tile_h,    w = tile_w, h = tile_h)
    )

    tilesB <- list(
      list(x0 = dst_x0,             y0 = dst_y0,             w = tile_w, h = tile_h),
      list(x0 = dst_x0 + tile_w,    y0 = dst_y0,             w = tile_w, h = tile_h),
      list(x0 = dst_x0,             y0 = dst_y0 + tile_h,    w = tile_w, h = tile_h),
      list(x0 = dst_x0 + tile_w,    y0 = dst_y0 + tile_h,    w = tile_w, h = tile_h)
    )

    tile_stats <- function(tiles, is_A) {
      cols <- matrix(0, 4L, 3L)
      ctrs <- matrix(0, 4L, 2L)

      for (k in 1:4) {
        tx <- tiles[[k]]$x0
        ty <- tiles[[k]]$y0
        tw <- tiles[[k]]$w
        th <- tiles[[k]]$h

        ctrs[k, 1] <- tx + (tw - 1) / 2
        ctrs[k, 2] <- ty + (th - 1) / 2

        idxs  <- integer(tw * th)
        c_idx <- 0L
        for (dx in 0:(tw - 1L)) {
          for (dy in 0:(th - 1L)) {
            x <- tx + dx
            y <- ty + dy
            c_idx <- c_idx + 1L
            idxs[c_idx] <- idx_cm(x, y, H)
          }
        }

        if (is_A) {
          cols[k, ] <- c(
            mean(A_planar[idxs])                   / 255,
            mean(A_planar[idxs + N_global])        / 255,
            mean(A_planar[idxs + 2L * N_global])   / 255
          )
        } else {
          cols[k, ] <- c(
            mean(B_planar[idxs])                   / 255,
            mean(B_planar[idxs + N_global])        / 255,
            mean(B_planar[idxs + 2L * N_global])   / 255
          )
        }
      }

      list(cols = cols, ctrs = ctrs)
    }

    A_stats <- tile_stats(tilesA, is_A = TRUE)
    B_stats <- tile_stats(tilesB, is_A = FALSE)

    colors_A <- A_stats$cols
    colors_B <- B_stats$cols
    centersA <- A_stats$ctrs
    centersB <- B_stats$ctrs

    diag_norm <- sqrt(H^2 + W^2)

    dc_mat <- matrix(0, 4L, 4L)
    ds_mat <- matrix(0, 4L, 4L)

    for (i in 1:4) {
      for (j in 1:4) {
        dc <- sqrt(sum((colors_A[i, ] - colors_B[j, ])^2))
        ds <- sqrt(sum((centersA[i, ] - centersB[j, ])^2)) / diag_norm
        dc_mat[i, j] <- dc
        ds_mat[i, j] <- ds
      }
    }

    mean_dc <- mean(dc_mat)
    mean_ds <- mean(ds_mat)
    if (mean_dc <= 0) mean_dc <- 1
    if (mean_ds <= 0) mean_ds <- 1

    dc_norm <- dc_mat / mean_dc
    ds_norm <- ds_mat / mean_ds

    C <- alpha * dc_norm + beta * ds_norm

    perm0 <- .lap_assign(C, method = method, maximize = maximize)
    perm  <- as.integer(perm0) + 1L

    for (i in 1:4) {
      src_tile <- tilesA[[i]]
      dst_tile <- tilesB[[perm[i]]]
      solve_block(
        src_x0 = src_tile$x0,
        src_y0 = src_tile$y0,
        dst_x0 = dst_tile$x0,
        dst_y0 = dst_tile$y0,
        w      = src_tile$w,
        h      = src_tile$h
      )
    }

    rest_w <- w - main_w
    rest_h <- h - main_h

    if (rest_w > 0L) {
      solve_block(
        src_x0 = src_x0 + main_w,
        src_y0 = src_y0,
        dst_x0 = dst_x0 + main_w,
        dst_y0 = dst_y0,
        w      = rest_w,
        h      = main_h
      )
    }

    if (rest_h > 0L) {
      solve_block(
        src_x0 = src_x0,
        src_y0 = src_y0 + main_h,
        dst_x0 = dst_x0,
        dst_y0 = dst_y0 + main_h,
        w      = main_w,
        h      = rest_h
      )
    }

    if (rest_w > 0L && rest_h > 0L) {
      solve_block(
        src_x0 = src_x0 + main_w,
        src_y0 = src_y0 + main_h,
        dst_x0 = dst_x0 + main_w,
        dst_y0 = dst_y0 + main_h,
        w      = rest_w,
        h      = rest_h
      )
    }

    invisible(NULL)
  }

  solve_block(0L, 0L, 0L, 0L, W, H)

  na_idx <- which(is.na(assignment))
  if (length(na_idx)) {
    assignment[na_idx] <- na_idx
  }

  assignment
}

#' Reduced-palette color-walk → expand to per-pixel via spatial mini-LAP
#' @noRd
.solve_color_walk_pipeline <- function(Ap, Bp, H, W, quantize_bits, method, maximize) {
  pal     <- color_palette_info_cpp(Ap, Bp, H, W, quantize_bits)
  groupsA <- pal$groupsA
  groupsB <- pal$groupsB
  Dcol    <- pal$color_dist

  pal_asg <- .lap_assign(Dcol, method = method, maximize = FALSE) + 1L

  N     <- H * W
  out0  <- rep.int(NA_integer_, N)
  freeB <- rep(TRUE, N)

  order_pairs <- order(Dcol[cbind(seq_along(pal_asg), pal_asg)], decreasing = FALSE)
  for (ia in order_pairs) {
    ib   <- pal_asg[ia]
    idxA <- groupsA[[ia]]
    idxB <- groupsB[[ib]]

    if (length(idxA) == 0L || length(idxB) == 0L) next
    idxB <- idxB[freeB[idxB]]
    if (!length(idxB)) next

    Csp   <- spatial_cost_matrix_cpp(idxA, idxB, H, W)
    match <- .lap_assign(Csp, method = method, maximize = FALSE)

    take  <- seq_len(min(length(idxA), length(idxB)))
    a_sel <- idxA[take]
    b_sel <- idxB[match[take] + 1L]
    out0[a_sel] <- b_sel - 1L
    freeB[b_sel] <- FALSE
  }

  remainA <- which(is.na(out0))
  if (length(remainA)) {
    idxB  <- which(freeB)
    Csp   <- spatial_cost_matrix_cpp(remainA, idxB, H, W)
    match <- .lap_assign(Csp, method = method, maximize = FALSE)
    out0[remainA] <- idxB[match + 1L] - 1L
    freeB[idxB[match + 1L]] <- FALSE
  }

  as.integer(out0 + 1L)
}


# =============================================================================
# Tile generation utilities
# =============================================================================

#' Generate deterministic square tiles covering W x H
#' @noRd
.generate_square_tiles <- function(W, H, P = 3L) {
  tiles   <- list()
  covered <- matrix(FALSE, nrow = H, ncol = W)

  core_w <- (W %/% P) * P
  core_h <- (H %/% P) * P

  for (x0 in seq(0, core_w - P, by = P)) {
    for (y0 in seq(0, core_h - P, by = P)) {
      tiles[[length(tiles) + 1L]] <- list(x0 = x0, y0 = y0, size = P)
      for (dx in 0:(P - 1L)) {
        for (dy in 0:(P - 1L)) {
          covered[y0 + dy + 1L, x0 + dx + 1L] <- TRUE
        }
      }
    }
  }

  if (core_w < W) {
    remaining_width <- W - core_w
    x0 <- core_w

    for (y0 in seq(0, core_h - 1L, by = 1L)) {
      if (covered[y0 + 1L, x0 + 1L]) next

      max_size <- min(remaining_width, core_h - y0)
      for (size in min(P, max_size):1L) {
        if (y0 + size <= core_h && x0 + size <= W) {
          all_free <- TRUE
          for (dx in 0:(size - 1L)) {
            for (dy in 0:(size - 1L)) {
              if (covered[y0 + dy + 1L, x0 + dx + 1L]) {
                all_free <- FALSE
                break
              }
            }
            if (!all_free) break
          }

          if (all_free) {
            tiles[[length(tiles) + 1L]] <- list(x0 = x0, y0 = y0, size = size)
            for (dx in 0:(size - 1L)) {
              for (dy in 0:(size - 1L)) {
                covered[y0 + dy + 1L, x0 + dx + 1L] <- TRUE
              }
            }
            break
          }
        }
      }
    }
  }

  if (core_h < H) {
    remaining_height <- H - core_h
    y0 <- core_h

    for (x0 in seq(0, W - 1L, by = 1L)) {
      if (covered[y0 + 1L, x0 + 1L]) next

      max_size <- min(remaining_height, W - x0)
      for (size in min(P, max_size):1L) {
        if (x0 + size <= W && y0 + size <= H) {
          all_free <- TRUE
          for (dx in 0:(size - 1L)) {
            for (dy in 0:(size - 1L)) {
              if (covered[y0 + dy + 1L, x0 + dx + 1L]) {
                all_free <- FALSE
                break
              }
            }
            if (!all_free) break
          }

          if (all_free) {
            tiles[[length(tiles) + 1L]] <- list(x0 = x0, y0 = y0, size = size)
            for (dx in 0:(size - 1L)) {
              for (dy in 0:(size - 1L)) {
                covered[y0 + dy + 1L, x0 + dx + 1L] <- TRUE
              }
            }
            break
          }
        }
      }
    }
  }

  for (y in 0:(H - 1L)) {
    for (x in 0:(W - 1L)) {
      if (!covered[y + 1L, x + 1L]) {
        tiles[[length(tiles) + 1L]] <- list(x0 = x, y0 = y, size = 1L)
      }
    }
  }

  tiles
}
