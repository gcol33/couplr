# Pixel-level image morphing (final frame only)

Computes optimal pixel assignment from A to B and returns the final
transported frame (without intermediate animation frames).

## Usage

``` r
pixel_morph(
  imgA,
  imgB,
  n_frames = 16L,
  mode = c("color_walk", "exact", "recursive"),
  lap_method = "jv",
  maximize = FALSE,
  quantize_bits = 5L,
  downscale_steps = 0L,
  alpha = 1,
  beta = 0,
  patch_size = 1L,
  upscale = 1,
  show = interactive()
)
```

## Arguments

- imgA:

  Source image (file path or magick image object)

- imgB:

  Target image (file path or magick image object)

- n_frames:

  Internal parameter for rendering (default: 16)

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

- show:

  Logical, display result in viewer (default: interactive())

## Value

magick image object of the final transported frame

## Details

### Transport-Only Semantics

This function returns a SHARP, pixel-perfect transport of A's pixels to
positions determined by the assignment to B.

**Key Points:**

- Assignment computed using:
  `cost = alpha * color_dist + beta * spatial_dist`

- B's COLORS influence assignment but DO NOT appear in output

- Result has A's colors arranged to match B's layout

- No motion blur (unlike intermediate frames in animation)

See
[`pixel_morph_animate`](https://gcol33.github.io/couplr/reference/pixel_morph_animate.md)
for detailed explanation of assignment vs rendering semantics.

### Permutation Warnings

Assignment is guaranteed to be a bijection (permutation) ONLY when:

- `downscale_steps = 0` (no resolution changes)

- `mode = "exact"` with `patch_size = 1`

With downscaling or tiled modes, assignment may have:

- **Overlaps:** Multiple source pixels map to same destination (last
  write wins)

- **Holes:** Some destinations never filled (remain transparent)

If assignment is not a bijection (due to downscaling or tiling), a
warning will be issued. The result may contain:

- Overlapped pixels (multiple sources -\> one destination)

- Transparent holes (some destinations unfilled)

For guaranteed pixel-perfect results, use:


      pixel_morph(A, B, mode = "exact", downscale_steps = 0)

## See also

[`pixel_morph_animate`](https://gcol33.github.io/couplr/reference/pixel_morph_animate.md)
for animated version

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic morph
result <- pixel_morph("imageA.png", "imageB.png")

# Pure spatial rearrangement
result <- pixel_morph("imageA.png", "imageB.png",
                      alpha = 0, beta = 1)

# Exact mode for small images (guaranteed permutation)
result <- pixel_morph("small_A.png", "small_B.png",
                      mode = "exact", downscale_steps = 0)
} # }
```
