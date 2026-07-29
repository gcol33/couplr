# Resolve a requested memory_mode to a concrete decision

Resolve a requested memory_mode to a concrete decision

## Usage

``` r
resolve_memory_mode(
  n,
  m,
  memory_mode = c("auto", "dense", "lazy"),
  solver_supports_lazy = FALSE,
  ram_fraction = 0.5,
  fallback_threshold_mb = 4000
)
```

## Arguments

- n, m:

  Problem dimensions (left/right unit counts).

- memory_mode:

  One of "auto" (probe RAM and decide), "dense" (always, skip probing
  entirely), or "lazy" (always, error if unsupported here).

- solver_supports_lazy:

  Whether a lazy path actually exists for the caller's chosen
  solver/distance combination (`TRUE` only for
  `method = "jv"`/`"auction"` with a built-in distance metric, on a
  caller whose solve path consumes a `lazy_cost_spec`; see
  R/matching_lazy.R).

- ram_fraction:

  Fraction of available RAM the dense matrix may consume before "auto"
  switches away from dense.

- fallback_threshold_mb:

  Fixed threshold used when available RAM can't be determined (mirrors
  the warn+fallback precedent in `R/morph_utils.R`'s `matrix_size > 1e8`
  cell guard).

## Value

"dense" or "lazy".
