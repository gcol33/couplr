# Estimate available system RAM in megabytes

Cross-platform, base-R-only (shells out; no new package dependency).
Never errors: returns `NA_real_` if detection fails or the platform is
unrecognized, so callers must treat `NA` as "unknown" and fall back to a
fixed threshold rather than skipping the guard entirely.

## Usage

``` r
get_free_ram_mb()
```

## Value

Numeric scalar (MB of available RAM), or `NA_real_` if undetermined.

## Details

"Available" means memory an allocation can obtain without swapping,
which on every platform is more than the untouched free list: Linux
reports it directly as `MemAvailable`, and on macOS it is the free,
inactive and speculative pages together, since the kernel keeps almost
nothing on the free list and reclaims the rest on demand.
