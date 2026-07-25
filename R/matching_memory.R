# ==============================================================================
# Matching Memory - RAM-aware guard for dense cost-matrix construction
# ==============================================================================
# By default couplr materializes a dense n_left x n_right cost matrix before
# solving. This file estimates that matrix's memory footprint against free
# system RAM and resolves the requested `memory_mode` ("auto"/"dense"/"lazy")
# to a concrete decision, warning loudly instead of silently crashing/thrashing
# on oversized problems. A lazy path exists for method = "jv"/"auction" with a
# built-in distance metric (see R/matching_lazy.R); callers whose downstream
# solve path is not lazy-aware pass solver_supports_lazy = FALSE, so "auto"
# only ever switches to "lazy" where a lazy solve actually exists.

# Below this cell count, never even probe free RAM: ~1e7 cells is ~80MB dense,
# well within any machine's headroom, and probing on every ordinary-sized
# problem would add needless overhead (a full memory-mode audit costs nothing
# for the overwhelming majority of existing, small problems).
COUPLR_MEMORY_PROBE_THRESHOLD_CELLS <- 1e7

#' Estimate free system RAM in megabytes
#'
#' Cross-platform, base-R-only (shells out; no new package dependency).
#' Never errors: returns `NA_real_` if detection fails or the platform is
#' unrecognized, so callers must treat `NA` as "unknown" and fall back to a
#' fixed threshold rather than skipping the guard entirely.
#'
#' @return Numeric scalar (MB of free RAM), or `NA_real_` if undetermined.
#' @keywords internal
get_free_ram_mb <- function() {
  sysname <- Sys.info()[["sysname"]]
  tryCatch({
    if (identical(sysname, "Windows")) {
      out <- suppressWarnings(system2("powershell",
        c("-NoProfile", "-Command",
          "(Get-CimInstance Win32_OperatingSystem).FreePhysicalMemory"),
        stdout = TRUE, stderr = FALSE))
      kb <- suppressWarnings(as.numeric(trimws(out[1])))
      if (is.na(kb)) {
        out <- suppressWarnings(system2("wmic",
          c("OS", "get", "FreePhysicalMemory", "/Value"),
          stdout = TRUE, stderr = FALSE))
        hit <- out[grepl("=", out)]
        kb <- suppressWarnings(as.numeric(sub("FreePhysicalMemory=", "", hit)))
      }
      if (length(kb) == 0 || is.na(kb[1])) return(NA_real_)
      kb[1] / 1024
    } else if (identical(sysname, "Linux")) {
      lines <- readLines("/proc/meminfo", warn = FALSE)
      hit <- grep("^MemAvailable:", lines, value = TRUE)
      if (length(hit) == 0) return(NA_real_)
      kb <- suppressWarnings(as.numeric(regmatches(hit, regexpr("[0-9]+", hit))))
      if (length(kb) == 0 || is.na(kb[1])) return(NA_real_)
      kb[1] / 1024
    } else if (identical(sysname, "Darwin")) {
      vm <- suppressWarnings(system2("vm_stat", stdout = TRUE, stderr = FALSE))
      free_line <- grep("^Pages free:", vm, value = TRUE)
      if (length(free_line) == 0) return(NA_real_)
      free_pages <- suppressWarnings(as.numeric(
        regmatches(free_line, regexpr("[0-9]+", free_line))))
      if (length(free_pages) == 0 || is.na(free_pages[1])) return(NA_real_)
      (free_pages[1] * 4096) / 1024^2
    } else {
      NA_real_
    }
  }, error = function(e) NA_real_, warning = function(w) NA_real_)
}

#' Estimate dense cost-matrix memory footprint in megabytes
#'
#' The true peak footprint is not 8 bytes/cell: `matrix(0, n, m)` at the R
#' level (8B) is followed by `rcpp_to_cost_matrix()`'s copy into a
#' `lap::CostMatrix` (8B data + 4B mask), then `prepare_for_solve()`'s
#' unconditional copy (another 12B), and a possible `t()` transpose copy (8B).
#' Several of these are transient but can coexist during GC lag. A conservative
#' multiplier avoids systematically under-warning; `n`/`m` are coerced to
#' `double` before multiplying so the estimate itself can't overflow the way
#' `lap::CostMatrix`'s old `int` flat-index arithmetic did.
#'
#' @keywords internal
estimate_dense_matrix_mb <- function(n, m, overhead_factor = 4) {
  (as.numeric(n) * as.numeric(m) * 8 * overhead_factor) / 1e6
}

#' Resolve a requested memory_mode to a concrete decision
#'
#' @param n,m Problem dimensions (left/right unit counts).
#' @param memory_mode One of "auto" (probe RAM and decide), "dense" (always,
#'   skip probing entirely), or "lazy" (always, error if unsupported here).
#' @param solver_supports_lazy Whether a lazy path actually exists for the
#'   caller's chosen solver/distance combination (`TRUE` only for `method =
#'   "jv"`/`"auction"` with a built-in distance metric, on a caller whose
#'   solve path consumes a `lazy_cost_spec`; see R/matching_lazy.R).
#' @param ram_fraction Fraction of free RAM the dense matrix may consume
#'   before "auto" switches away from dense.
#' @param fallback_threshold_mb Fixed threshold used when free RAM can't be
#'   determined (mirrors the warn+fallback precedent in
#'   `R/morph_utils.R`'s `matrix_size > 1e8` cell guard).
#'
#' @return "dense" or "lazy".
#' @keywords internal
resolve_memory_mode <- function(n, m,
                                memory_mode = c("auto", "dense", "lazy"),
                                solver_supports_lazy = FALSE,
                                ram_fraction = 0.5,
                                fallback_threshold_mb = 4000) {
  memory_mode <- match.arg(memory_mode)

  if (identical(memory_mode, "dense")) {
    return("dense")
  }

  if (identical(memory_mode, "lazy")) {
    if (!solver_supports_lazy) {
      stop("memory_mode = \"lazy\" is not supported for this method/path yet.",
           call. = FALSE)
    }
    return("lazy")
  }

  # memory_mode == "auto"
  n_cells <- as.numeric(n) * as.numeric(m)
  if (n_cells < COUPLR_MEMORY_PROBE_THRESHOLD_CELLS) {
    return("dense")
  }

  needed_mb <- estimate_dense_matrix_mb(n, m)
  free_mb <- get_free_ram_mb()

  if (is.na(free_mb)) {
    if (needed_mb > fallback_threshold_mb) {
      if (solver_supports_lazy) {
        warning(sprintf(
          "Could not determine free system RAM; the dense cost matrix would need ~%.0f MB. Switching to memory_mode = \"lazy\".",
          needed_mb), call. = FALSE)
        return("lazy")
      }
      warning(sprintf(
        "Could not determine free system RAM; the dense cost matrix would need ~%.0f MB. Proceeding densely -- consider blocking (block_id), method = \"greedy\", or reducing the problem size.",
        needed_mb), call. = FALSE)
    }
    return("dense")
  }

  if (needed_mb > ram_fraction * free_mb) {
    if (solver_supports_lazy) {
      warning(sprintf(
        "Dense cost matrix would need ~%.1f GB against ~%.1f GB free RAM. Switching to memory_mode = \"lazy\".",
        needed_mb / 1e3, free_mb / 1e3), call. = FALSE)
      return("lazy")
    }
    warning(sprintf(
      "Dense cost matrix would need ~%.1f GB against ~%.1f GB free RAM, and this path does not support memory_mode = \"lazy\" yet. Proceeding densely -- consider blocking (block_id), method = \"greedy\", reducing the problem size, or running on a machine with more RAM.",
      needed_mb / 1e3, free_mb / 1e3), call. = FALSE)
  }

  "dense"
}
