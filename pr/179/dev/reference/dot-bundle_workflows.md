# Assemble the bundle workflows

Loads mapping, metric (including inactive gsm.kri metrics), and
reporting workflows, with step names namespace-qualified.

## Usage

``` r
.bundle_workflows(config)
```

## Arguments

- config:

  A validated config from
  [`read_testdata_config()`](https://gilead-public.github.io/gsm.datasim/dev/reference/read_testdata_config.md).

## Value

List with `mappings`, `spec`, `metrics`, and `reporting`.
