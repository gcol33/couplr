# R/utils_morph.R
# Helpers for pixel-level morphing (exact / color_walk / auto)

# -------------------------------------------------------------------
# Basic helpers
# -------------------------------------------------------------------

#' @keywords internal
.has_namespace <- function(pkg) requireNamespace(pkg, quietly = TRUE)

#' GIF delay (centiseconds) from FPS
#' @keywords internal
.gif_delay_from_fps <- function(fps) {
  fps <- suppressWarnings(as.integer(round(fps)))
  if (!is.finite(fps) || fps < 1L) fps <- 10L
  as.integer(round(100 / fps))  # ImageMagick expects centiseconds/frame
}

# -------------------------------------------------------------------
# Image <-> array conversions
# -------------------------------------------------------------------

#' Convert magick image to H x W x 3 integer array in 0–255
#' @keywords internal
.to_array_rgb <- function(img) {
  a <- magick::image_data(img, channels = "rgb")
  d <- dim(a)
  if (length(d) != 3) stop("magick::image_data did not return a 3D array")

  # Convert to integer 0..255
  if (is.character(a)) {
    a <- array(as.integer(strtoi(a, base = 16L)), dim = d)
  } else if (is.raw(a)) {
    storage.mode(a) <- "integer"
  } else if (is.numeric(a)) {
    mx <- suppressWarnings(max(a, na.rm = TRUE))
    if (is.finite(mx) && mx <= 1.00001) a <- a * 255
    a <- round(a); storage.mode(a) <- "integer"
  } else {
    storage.mode(a) <- "integer"
  }

  # magick returns [channel, width, height] = [3, W, H]
  if (d[1] == 3 && d[3] != 3) {
    W <- d[2]; H <- d[3]
    out <- array(0L, dim = c(H, W, 3L))
    for (ch in 1:3) out[,,ch] <- t(a[ch,,])
  } else if (d[3] == 3) {
    out <- a
  } else if (d[2] == 3) {
    W <- d[1]; H <- d[3]
    out <- array(0L, dim = c(H, W, 3L))
    for (ch in 1:3) out[,,ch] <- t(a[,ch,])
  } else {
    stop("Unexpected RGB array dims: ", paste(d, collapse = " x "))
  }
  storage.mode(out) <- "integer"
  out
}

#' Convert H x W x 3 array to planar format (RRR...GGG...BBB...)
#' @keywords internal
.to_planar_rgb <- function(rgb_arr) {
  H <- dim(rgb_arr)[1]; W <- dim(rgb_arr)[2]
  planar <- numeric(H * W * 3L)
  for (ch in 1:3) {
    offset <- (ch - 1L) * H * W
    planar[(offset + 1L):(offset + H * W)] <- as.vector(rgb_arr[,,ch])
  }
  planar
}

#' Convert planar RGB vector (RRR...GGG...BBB...) back to H x W x 3 array
#' @keywords internal
.from_planar_rgb <- function(planar, H, W) {
  H <- as.integer(H)
  W <- as.integer(W)
  N <- H * W

  if (length(planar) != 3L * N) {
    stop(
      sprintf(
        ".from_planar_rgb: expected length %d (3 * %d * %d), got %d",
        3L * N, H, W, length(planar)
      ),
      call. = FALSE
    )
  }

  arr <- array(0, dim = c(H, W, 3L))

  for (ch in 1:3) {
    offset <- (ch - 1L) * N
    block  <- planar[(offset + 1L):(offset + N)]
    # as.vector() in .to_planar_rgb used column-major;
    # so we reconstruct with byrow = FALSE
    arr[,, ch] <- matrix(block, nrow = H, ncol = W, byrow = FALSE)
  }

  arr
}


#' Convert planar format back to H x W x 3 array
#' @keywords internal
.from_planar_rgb <- function(planar, H, W) {
  HW <- as.integer(H) * as.integer(W)
  if (!is.numeric(planar) || length(planar) != HW * 3L) {
    stop("planar data has wrong length; expected ", HW * 3L)
  }
  arr <- array(0, dim = c(as.integer(H), as.integer(W), 3L))
  arr[,,1] <- matrix(planar[1:HW], nrow = H, ncol = W, byrow = FALSE)
  arr[,,2] <- matrix(planar[(HW + 1):(2 * HW)], nrow = H, ncol = W, byrow = FALSE)
  arr[,,3] <- matrix(planar[(2 * HW + 1):(3 * HW)], nrow = H, ncol = W, byrow = FALSE)
  arr
}

#' Clamp RGB values to 0–255
#' @keywords internal
.clamp_rgb <- function(x) {
  d <- dim(x); x <- suppressWarnings(as.integer(round(x)))
  x[x < 0L] <- 0L; x[x > 255L] <- 255L
  if (!is.null(d)) dim(x) <- d
  x
}

# -------------------------------------------------------------------
# C++ glue (robust wrappers: prefer new _symbols; fallback to *cpp names)
# -------------------------------------------------------------------

.call_or <- function(sym, fallback, ...) {
  if (exists(sym, mode = "function")) {
    get(sym)(...)
  } else if (exists(fallback, mode = "function")) {
    get(fallback)(...)
  } else {
    stop("Neither ", sym, " nor ", fallback, " is available")
  }
}

#' @keywords internal
.cpp_palette_info <- function(Ap, Bp, H, W, bits) {
  .call_or("_color_palette_info", "color_palette_info_cpp",
           Ap, Bp, as.integer(H), as.integer(W), as.integer(bits))
}

#' @keywords internal
.cpp_spatial_cost <- function(idxA, idxB, H, W) {
  .call_or("_spatial_cost_matrix", "spatial_cost_matrix_cpp",
           as.integer(idxA), as.integer(idxB), as.integer(H), as.integer(W))
}

#' @keywords internal
.cpp_compute_pixel_cost <- function(Ap, Bp, H, W, alpha, beta) {
  .call_or("_compute_pixel_cost", "compute_pixel_cost_cpp",
           Ap, Bp, as.integer(H), as.integer(W), as.numeric(alpha), as.numeric(beta))
}

#' @keywords internal
.cpp_downscale <- function(planar, H, W, Hn, Wn) {
  .call_or("_downscale_image", "downscale_image_cpp",
           planar, as.integer(H), as.integer(W), as.integer(Hn), as.integer(Wn))
}

#' @keywords internal
.cpp_upscale_assignment <- function(asg_scaled, H, W, Hs, Ws) {
  .call_or("_upscale_assignment", "upscale_assignment_cpp",
           as.integer(asg_scaled), as.integer(H), as.integer(W), as.integer(Hs), as.integer(Ws))
}

#' @keywords internal
.cpp_render_morph <- function(Ap, Bp, asg0, H, W, nF) {
  .call_or("_morph_pixel_level_impl", "morph_pixel_level_cpp",
           Ap, Bp, as.integer(asg0), as.integer(H), as.integer(W), as.integer(nF))
}

#' @keywords internal
.cpp_overlap <- function(Ap, Bp, H, W, bits) {
  .call_or("_analyze_color_overlap", "analyze_color_overlap_cpp",
           Ap, Bp, as.integer(H), as.integer(W), as.integer(bits))
}

#' @keywords internal
.cpp_extract_patches <- function(P, H, W, patch) {
  .call_or("_extract_patches", "extract_patches_cpp",
           P, as.integer(H), as.integer(W), as.integer(patch))
}

# -------------------------------------------------------------------
# Downscale helpers for orchestration
# -------------------------------------------------------------------

#' Compute downscaled images, return both originals and downscaled with dims
#' @keywords internal
.downscale_both <- function(A_planar, B_planar, H, W, steps) {
  if (is.null(steps) || steps <= 0L) {
    return(list(Hs = as.integer(H), Ws = as.integer(W),
                A_s = A_planar, B_s = B_planar,
                H  = as.integer(H), W  = as.integer(W)))
  }
  Hs <- as.integer(max(8L, floor(H / (2^steps))))
  Ws <- as.integer(max(8L, floor(W / (2^steps))))
  A_s <- .cpp_downscale(A_planar, H, W, Hs, Ws)
  B_s <- .cpp_downscale(B_planar, H, W, Hs, Ws)
  list(Hs = Hs, Ws = Ws, A_s = A_s, B_s = B_s, H = as.integer(H), W = as.integer(W))
}

#' Upscale a downscaled assignment back to original resolution
#' @keywords internal
.upscale_assignment <- function(assign_s, H, W, Hs, Ws) {
  .cpp_upscale_assignment(assign_s, H, W, Hs, Ws)
}

# -------------------------------------------------------------------
# LAP glue — normalize outputs
# -------------------------------------------------------------------

#' Solve LAP and return a consistent **0-based** column index per row
#' @keywords internal
.lap_assign <- function(C, method = "jv", maximize = FALSE) {
  if (!exists("lap_solve", mode = "function") && !exists("lap_solve_batch", mode = "function"))
    stop("No lap_solve / lap_solve_batch available")

  res <- if (exists("lap_solve", mode = "function")) {
    lap_solve(C, method = method, maximize = maximize)
  } else {
    lap_solve_batch(list(C), method = method, maximize = maximize)[[1L]]
  }

  n <- nrow(C)
  # Direct integer vector?
  if (is.integer(res) && length(res) == n) return(res - 1L)     # assume 1-based -> 0-based
  if (is.numeric(res) && length(res) == n) return(as.integer(round(res)) - 1L)

  # tibble/data.frame with source/target
  if (is.data.frame(res) && all(c("source","target") %in% names(res))) {
    perm <- integer(n); perm[res$source] <- res$target
    perm[perm == 0L] <- seq_len(n)[perm == 0L]
    return(as.integer(perm - 1L))
  }

  # common list shapes
  if (is.list(res)) {
    for (nm in c("assignment","perm","match")) {
      if (!is.null(res[[nm]]) && length(res[[nm]]) == n) {
        v <- res[[nm]]
        v <- if (is.integer(v)) v else as.integer(round(v))
        return(v - 1L)
      }
    }
  }
  stop("LAP solver returned an unsupported structure")
}

# -------------------------------------------------------------------
# Patch helpers
# -------------------------------------------------------------------

#' Compute cost matrix between two sets of patches (color+spatial)
#' @keywords internal
.patch_cost_matrix <- function(patches_a, patches_b, alpha = 1, beta = 0.1, H = NULL, W = NULL) {
  Ca <- as.matrix(patches_a$colors) / 255
  Cb <- as.matrix(patches_b$colors) / 255
  Sa <- as.matrix(patches_a$centers)
  Sb <- as.matrix(patches_b$centers)
  Cc <- as.matrix(stats::dist(rbind(Ca, Cb)))[seq_len(nrow(Ca)), nrow(Ca) + seq_len(nrow(Cb))]
  if (!is.null(H) && !is.null(W)) {
    diag_norm <- sqrt(H^2 + W^2)
  } else {
    # approximate from patch centers
    diag_norm <- max(dist(rbind(Sa, Sb)))
    if (!is.finite(diag_norm) || diag_norm <= 0) diag_norm <- 1
  }
  Cs <- as.matrix(stats::dist(rbind(Sa, Sb)))[seq_len(nrow(Sa)), nrow(Sa) + seq_len(nrow(Sb))] / diag_norm
  alpha * Cc + beta * Cs
}

#' Expand patch assignment to pixel assignment (simple spatial truncation)
#' @keywords internal
.expand_patch_assignment <- function(patch_assign, patches_a, patches_b, N) {
  out <- rep(-1L, N)
  for (i in seq_along(patch_assign)) {
    j <- patch_assign[[i]]
    if (!is.finite(j) || j < 1L) next
    ia <- patches_a$indices[[i]]
    ib <- patches_b$indices[[j]]
    take <- min(length(ia), length(ib))
    if (take > 0L) out[ia[seq_len(take)]] <- ib[seq_len(take)]
  }
  out
}

# -------------------------------------------------------------------
# Exact / Patch solves (R orchestrates LAP; C++ only builds costs)
# -------------------------------------------------------------------

#' Exact pixel-level: returns **1-based** assignment (A->B)
#' @keywords internal
.exact_cost_and_solve <- function(A_planar, B_planar, H, W, alpha = 1, beta = 0,
                                  method = "jv", maximize = FALSE) {
  C <- .cpp_compute_pixel_cost(A_planar, B_planar, H, W, alpha, beta)
  .lap_assign(C, method = method, maximize = maximize) + 1L
}

# NOTE: .patch_cost_and_solve() has been removed and replaced with
# .square_tiling_solver() in pixel_morph.R which is much more efficient

# -------------------------------------------------------------------
# Palette pipelines
# -------------------------------------------------------------------

# Build spatial assignments for palette pairs (returns index pairs)
.build_spatial_assignments_for_pairs <- function(info, pairs, H, W, method = "jv", maximize = FALSE) {
  if (nrow(pairs) == 0L) return(list(i_idx = integer(), j_idx = integer()))
  groupsA <- info$groupsA; groupsB <- info$groupsB
  i_all <- integer(0); j_all <- integer(0)
  for (r in seq_len(nrow(pairs))) {
    ia <- pairs$ia[[r]]; ib <- pairs$ib[[r]]; k <- pairs$k[[r]]
    if (k <= 0L) next
    idxA <- as.integer(groupsA[[ia]]); idxB <- as.integer(groupsB[[ib]])
    if (!length(idxA) || !length(idxB)) next
    idxA <- idxA[seq_len(min(k, length(idxA)))]
    idxB <- idxB[seq_len(min(k, length(idxB)))]
    Csp  <- .cpp_spatial_cost(idxA, idxB, H, W)
    perm <- .lap_assign(Csp, method = method, maximize = maximize)
    i_all <- c(i_all, idxA); j_all <- c(j_all, idxB[perm + 1L])
  }
  list(i_idx = i_all, j_idx = j_all)
}

# Assemble assignment from (i,j) pairs (1-based target); unfilled remain -1
.assemble_assignment <- function(N, i_idx, j_idx) {
  assign <- rep.int(-1L, N)
  if (length(i_idx) && length(j_idx)) {
    take <- min(length(i_idx), length(j_idx))
    if (take > 0L) assign[i_idx[seq_len(take)]] <- as.integer(j_idx[seq_len(take)])
  }
  assign
}

# Identity fill helper (kept, though color_walk won’t use it)
.fill_unassigned_identity <- function(assign) {
  N <- length(assign)
  z <- which(assign < 0L)
  if (length(z)) assign[z] <- z
  assign
}

# Exact-identity palette pairs (quantized equality)
.palette_pairs_identity <- function(info) {
  A <- info$colorsA_rgb; B <- info$colorsB_rgb
  cA <- as.integer(info$countsA); cB <- as.integer(info$countsB)
  keyA <- paste(A[,1], A[,2], A[,3], sep = "_")
  keyB <- paste(B[,1], B[,2], B[,3], sep = "_")
  mapB <- seq_along(keyB); names(mapB) <- keyB
  ia_vec <- integer(0); ib_vec <- integer(0); k_vec <- integer(0)
  for (ia in seq_along(keyA)) {
    key <- keyA[[ia]]
    if (key %in% names(mapB)) {
      ib <- mapB[[key]]
      ia_vec <- c(ia_vec, ia)
      ib_vec <- c(ib_vec, ib)
      k_vec  <- c(k_vec, min(cA[[ia]], cB[[ib]]))
    }
  }
  if (!length(ia_vec)) return(data.frame(ia = integer(), ib = integer(), k = integer()))
  data.frame(ia = ia_vec, ib = ib_vec, k = k_vec)
}

# Palette LAP on color distance
.palette_pairs_lap <- function(info, method = "jv", maximize = FALSE) {
  cA <- as.integer(info$countsA); cB <- as.integer(info$countsB)
  M  <- info$color_dist
  if (nrow(M) == 0L || ncol(M) == 0L) return(data.frame(ia = integer(), ib = integer(), k = integer()))
  perm <- .lap_assign(M, method = method, maximize = maximize) + 1L
  ia <- seq_len(nrow(M)); ib <- as.integer(perm); k <- pmin(cA[ia], cB[ib])
  data.frame(ia = ia, ib = ib, k = as.integer(k))
}

# Color walk: process A colors in fixed order, match to nearest free B pixels by pure color distance
.solve_color_walk_pipeline <- function(A_planar, B_planar, H, W, quantize_bits = 5,
                                       method = "jv", maximize = FALSE) {
  info <- .cpp_palette_info(A_planar, B_planar, H, W, quantize_bits)
  N <- H * W
  
  # Extract full-resolution RGB for all pixels (0-255 range)
  A_rgb <- matrix(A_planar, nrow = N, ncol = 3)  # N x 3
  B_rgb <- matrix(B_planar, nrow = N, ncol = 3)  # N x 3
  
  # Sort A color groups by frequency (descending) for deterministic processing
  groupsA <- info$groupsA
  countsA <- info$countsA
  color_order <- order(countsA, decreasing = TRUE)
  
  # Initialize assignment and free set
  assignment <- rep(NA_integer_, N)  # will store 1-based B indices
  freeB <- rep(TRUE, N)
  
  # Process each A color group in order
  for (ia in color_order) {
    idxA <- as.integer(groupsA[[ia]])  # 1-based indices
    if (!length(idxA)) next
    
    # Get currently free B pixels
    idxB_free <- which(freeB)  # 1-based indices
    if (!length(idxB_free)) break  # no more free B pixels
    
    nA <- length(idxA)
    nB <- length(idxB_free)
    
    # Warn if this color group is very large (memory could be an issue)
    # Use as.numeric to avoid integer overflow in multiplication
    matrix_size <- as.numeric(nA) * as.numeric(nB)
    
    if (matrix_size > 1e8) {  # ~100M entries = ~800MB
      warning(sprintf(
        "Large color group: %d A pixels × %d B pixels. Using spatial fallback to avoid memory issues.",
        nA, nB
      ), call. = FALSE)
      # Use spatial matching as fallback for huge groups
      Csp <- .cpp_spatial_cost(idxA, idxB_free, H, W)
      match <- .lap_assign(Csp, method = method, maximize = FALSE)
    } else {
      # Normal color-based matching
      C_color <- matrix(0, nrow = nA, ncol = nB)
      
      for (i in seq_len(nA)) {
        a_idx <- idxA[i]
        for (j in seq_len(nB)) {
          b_idx <- idxB_free[j]
          dr <- (A_rgb[a_idx, 1] - B_rgb[b_idx, 1]) / 255.0
          dg <- (A_rgb[a_idx, 2] - B_rgb[b_idx, 2]) / 255.0
          db <- (A_rgb[a_idx, 3] - B_rgb[b_idx, 3]) / 255.0
          C_color[i, j] <- sqrt(dr*dr + dg*dg + db*db)
        }
      }
      
      match <- .lap_assign(C_color, method = method, maximize = FALSE)
    }
    
    # Apply assignments
    take <- min(nA, nB)
    if (take > 0) {
      for (i in seq_len(take)) {
        a_idx <- idxA[i]
        b_idx <- idxB_free[match[i] + 1L]
        assignment[a_idx] <- b_idx
        freeB[b_idx] <- FALSE
      }
    }
  }
  
  # Handle any remaining unassigned A pixels (shouldn't happen with equal sizes, but be safe)
  remainA <- which(is.na(assignment))
  if (length(remainA) && any(freeB)) {
    idxB_free <- which(freeB)
    nA <- length(remainA)
    nB <- length(idxB_free)
    C_color <- matrix(0, nrow = nA, ncol = nB)
    
    for (i in seq_len(nA)) {
      a_idx <- remainA[i]
      for (j in seq_len(nB)) {
        b_idx <- idxB_free[j]
        dr <- (A_rgb[a_idx, 1] - B_rgb[b_idx, 1]) / 255.0
        dg <- (A_rgb[a_idx, 2] - B_rgb[b_idx, 2]) / 255.0
        db <- (A_rgb[a_idx, 3] - B_rgb[b_idx, 3]) / 255.0
        C_color[i, j] <- sqrt(dr*dr + dg*dg + db*db)
      }
    }
    
    match <- .lap_assign(C_color, method = method, maximize = FALSE)
    for (i in seq_along(remainA)) {
      assignment[remainA[i]] <- idxB_free[match[i] + 1L]
    }
  }
  
  # Final fallback: any still unassigned -> identity
  still_na <- which(is.na(assignment))
  if (length(still_na)) assignment[still_na] <- still_na
  
  as.integer(assignment)  # return 1-based indices
}

# Identity palette pipeline (kept for completeness; not used in auto)
.solve_color_match_pipeline <- function(A_planar, B_planar, H, W, quantize_bits = 5,
                                        method = "jv", maximize = FALSE,
                                        fill_identity_for_unmatched = TRUE) {
  info  <- .cpp_palette_info(A_planar, B_planar, H, W, quantize_bits)
  pairs <- .palette_pairs_identity(info)
  pj    <- .build_spatial_assignments_for_pairs(info, pairs, H, W, method = method, maximize = maximize)
  assign <- .assemble_assignment(N = H * W, pj$i_idx, pj$j_idx)
  if (fill_identity_for_unmatched) assign <- .fill_unassigned_identity(assign)
  assign
}

# -------------------------------------------------------------------
# Hierarchical patch matching functions REMOVED
# -------------------------------------------------------------------
# The following inefficient functions have been removed:
# - .extract_patches_at_size() - replaced by square tiling
# - .extract_hierarchical_patches() - replaced by .generate_square_tiles()
# - .match_patches_at_size() - replaced by .solve_tile_lap()
# - .expand_patch_assignment_spatial() - replaced by local LAP per tile
# - .solve_hierarchical_patch_pipeline() - replaced by .square_tiling_solver()
#
# These functions used global LAPs with O(n³) complexity and have been
# replaced with efficient local LAP solving in pixel_morph.R
# -------------------------------------------------------------------

# small infix helper
`%||%` <- function(a, b) if (!is.null(a)) a else b
