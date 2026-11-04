# Generate a release tag

Generate a release tag

## Usage

``` r
.testdata_key(channel, time = Sys.time())
```

## Arguments

- channel:

  Release channel.

- time:

  Build time.

## Value

`"testdata-<channel>-<YYYYMMDD>T<HHMMSS>Z"` (UTC).
