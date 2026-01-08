# Pixel-level image morphing (animation)

Creates an animated morph by computing optimal pixel assignment from
image A to image B, then rendering intermediate frames showing the
transport.

## Usage

``` r
pixel_morph_animate(
  imgA,
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
  beta = 0,
  patch_size = 1L,
  upscale = 1
)
```

## Arguments

- imgA:

  Source image (file path or magick image object)

- imgB:

  Target image (file path or magick image object)

- n_frames:

  Integer number of animation frames (default: 16)

- fps:

  Frames per second for playback (default: 10)

- format:

  Output format: "gif", "webp", or "mp4"

- outfile:

  Optional output file path

- show:

  Logical, display animation in viewer (default: interactive())

- mode:

  Assignment algorithm: "color_walk" (default), "exact", or "recursive"

- lap_method:

  LAP solver method (default: "jv")

- maximize:

  Logical, maximize instead of minimize cost (default: FALSE)

- quantize_bits:

  Color quantization for "color_walk" mode (default: 5)

- downscale_steps:

  Number of 2x reductions before computing assignment (default: 0)

- alpha:

  Weight for color distance in cost function (default: 1)

- beta:

  Weight for spatial distance in cost function (default: 0)

- patch_size:

  Tile size for tiled modes (default: 1)

- upscale:

  Post-rendering upscaling factor (default: 1)

## Value

Invisibly returns a list with animation object and metadata:

- animation:

  magick animation object

- width:

  Image width in pixels

- height:

  Image height in pixels

- assignment:

  Integer vector of 1-based assignment indices (R convention)

- n_pixels:

  Total number of pixels

- mode:

  Mode used for matching

- upscale:

  Upscaling factor applied

## Details

### Assignment vs Rendering Semantics

**CRITICAL:** This function has two separate phases with different
semantics:

**Phase 1 - Assignment Computation:**

The assignment is computed by minimizing:


      cost(i,j) = alpha * color_distance(A[i], B[j]) +
                  beta * spatial_distance(pos_i, pos_j)

This means B's COLORS influence which pixels from A map to which
positions.

**Phase 2 - Rendering (Transport-Only):**

The renderer uses ONLY A's colors:

- Intermediate frames: A's pixels move along paths with motion blur

- Final frame: A's pixels at their assigned positions (sharp, no blur)

- B's colors NEVER appear in the output

**Result:** You get A's colors rearranged to match B's geometry/layout.

### What This Means

- B influences WHERE pixels go (via similarity in cost function)

- B does NOT determine WHAT COLORS appear in output

- Final image has A's palette arranged to mimic B's structure

### Parameter Guidance

**For pure spatial rearrangement (ignore B's colors in assignment):**


      pixel_morph_animate(A, B, alpha = 0, beta = 1)

**For color-similarity matching (default):**


      pixel_morph_animate(A, B, alpha = 1, beta = 0)

**For hybrid (color + spatial):**


      pixel_morph_animate(A, B, alpha = 1, beta = 0.2)

### Permutation Guarantees

Assignment is guaranteed to be a bijection (permutation) ONLY when:

- `downscale_steps = 0` (no resolution changes)

- `mode = "exact"` with `patch_size = 1`

With downscaling or tiled modes, assignment may have:

- **Overlaps:** Multiple source pixels map to same destination (last
  write wins)

- **Holes:** Some destinations never filled (remain transparent)

A warning is issued if overlaps/holes are detected in the final frame.

## Examples

``` r
# \donttest{
# Use bundled example images
imgA <- system.file("extdata/icons/circleA_40.png", package = "couplr")
imgB <- system.file("extdata/icons/circleB_40.png", package = "couplr")

# Basic animation (writes to temp file)
outfile <- tempfile(fileext = ".gif")
pixel_morph_animate(imgA, imgB, outfile = outfile, n_frames = 4, show = FALSE)
#> Saved animation to: C:\Users\GILLES~1\AppData\Local\Temp\Rtmpi2e8Cg\file30fcc6ed011e4.gif

# Pure spatial rearrangement (ignore B's colors in assignment)
pixel_morph_animate(imgA, imgB, alpha = 0, beta = 1,
                    outfile = tempfile(fileext = ".gif"), show = FALSE)
#> Saved animation to: C:\Users\GILLES~1\AppData\Local\Temp\Rtmpi2e8Cg\file30fcc4d1c1d4f.gif
# }
```
