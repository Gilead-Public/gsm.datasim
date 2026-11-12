# Hash the packages that built a bundle

See the Snapshot section of
[`build_testdata_bundle()`](https://gilead-public.github.io/gsm.datasim/dev/reference/build_testdata_bundle.md)
for the exact definition, which consumers reproduce from their installed
packages.

## Usage

``` r
.inputs_hash(packages)
```

## Arguments

- packages:

  Named list; each element has `version` and `remote_sha` (`NULL` or
  `NA` when unknown).

## Value

A 64-character hex SHA-256.
