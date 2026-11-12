# Record pipeline package versions, rejecting local installs

Record pipeline package versions, rejecting local installs

## Usage

``` r
.check_package_versions(allow_local)
```

## Arguments

- allow_local:

  Allow packages without a `RemoteSha`.

## Value

Named list from
[`.installed_package_versions()`](https://gilead-public.github.io/gsm.datasim/dev/reference/dot-installed_package_versions.md).
