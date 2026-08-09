# Page size behind a block of `vm_stat` output

`vm_stat` counts in pages and states its own page size in the header
line ("page size of 16384 bytes"). Apple Silicon pages are 16K and Intel
pages are 4K, so the size is read rather than assumed;
`sysctl hw.pagesize` is the fallback when the header cannot be parsed.

## Usage

``` r
vm_stat_page_size(vm)
```

## Arguments

- vm:

  Character vector of `vm_stat` output lines.

## Value

Page size in bytes, or `NA_real_` if undetermined.
