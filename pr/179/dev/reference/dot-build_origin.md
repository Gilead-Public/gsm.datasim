# Identify the build origin

Identify the build origin

## Usage

``` r
.build_origin()
```

## Value

`"<GITHUB_REPOSITORY>@<GITHUB_SHA>"` when run in GitHub Actions,
otherwise `NULL`.
