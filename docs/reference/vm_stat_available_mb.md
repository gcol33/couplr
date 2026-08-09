# Available megabytes implied by a block of `vm_stat` output

Inactive and speculative pages are reclaimed on demand, so they count as
available to an allocation in the same sense as Linux's `MemAvailable`;
macOS keeps almost nothing on the free list, so the free count alone
understates what a large matrix can actually obtain.

## Usage

``` r
vm_stat_available_mb(vm, page_size)
```

## Arguments

- vm:

  Character vector of `vm_stat` output lines.

- page_size:

  Page size in bytes, as returned by
  [`vm_stat_page_size()`](https://gillescolling.com/couplr/reference/vm_stat_page_size.md).

## Value

Numeric scalar (MB available), or `NA_real_` if unparseable.

## Details

Split from
[`get_free_ram_mb()`](https://gillescolling.com/couplr/reference/get_free_ram_mb.md)
so the page-size and page-class handling can be checked without a macOS
host.
