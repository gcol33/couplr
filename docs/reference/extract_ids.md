# Extract and standardize IDs from data frames

The id a matching carries is the key every downstream verb joins on, so
it is resolved once, here, and the same resolution answers
[`match_couples()`](https://gillescolling.com/couplr/reference/match_couples.md)
and
[`join_matched()`](https://gillescolling.com/couplr/reference/join_matched.md).
`id_col` names the column to read; with no name given a column called
`id` is used, then meaningful row names, then ids synthesized from
`prefix`.

## Usage

``` r
extract_ids(df, prefix = "id", id_col = NULL, warn_synthetic = FALSE)
```

## Arguments

- df:

  Data frame to read ids from.

- prefix:

  Prefix for synthesized ids, also the side name used in messages
  ("left" / "right").

- id_col:

  Name of the id column, or NULL to resolve one.

- warn_synthetic:

  If TRUE, warn when ids are synthesized because no id column was named
  or found.

## Value

Character vector of IDs.

## Details

Ids read from the data must be unique: a repeated value makes every
id-keyed join downstream many-to-many, which expands rows and attaches
one unit's covariates to another unit's pair. Synthesized ids are unique
by construction and are not checked.
