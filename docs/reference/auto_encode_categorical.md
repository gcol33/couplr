# Automatically encode categorical variables

Converts categorical variables to numeric representations suitable for
matching. Currently supports binary variables (0/1) and ordered factors.

## Usage

``` r
auto_encode_categorical(left, right, var)
```

## Arguments

- left:

  Data frame of left units

- right:

  Data frame of right units

- var:

  Variable name to encode

## Value

List with encoded left and right columns, plus encoding metadata
