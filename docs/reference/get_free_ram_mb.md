# Estimate free system RAM in megabytes

Cross-platform, base-R-only (shells out; no new package dependency).
Never errors: returns `NA_real_` if detection fails or the platform is
unrecognized, so callers must treat `NA` as "unknown" and fall back to a
fixed threshold rather than skipping the guard entirely.

## Usage

``` r
get_free_ram_mb()
```

## Value

Numeric scalar (MB of free RAM), or `NA_real_` if undetermined.
