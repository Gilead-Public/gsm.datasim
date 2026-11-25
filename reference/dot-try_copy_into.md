# Copy into a directory, treating errors and interrupts as failure

Copy into a directory, treating errors and interrupts as failure

## Usage

``` r
.try_copy_into(from, to)
```

## Arguments

- from:

  Paths to copy.

- to:

  Destination directory.

## Value

`TRUE` if every copy succeeded, otherwise `FALSE`.
