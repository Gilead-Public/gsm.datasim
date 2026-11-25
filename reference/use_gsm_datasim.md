# Use gsm.datasim as gsm.core's local test data builder

Add
`options(gsm.testdata_builder = "gsm.datasim::build_testdata_bundle")`
to your user or project `.Rprofile`. When installed gsm packages don't
match a published test data build and at least one is local (e.g. loaded
with `devtools::load_all()`), gsm.core calls this builder to build test
data from your local code, caches it, and reuses it in later sessions.
If the `.Rprofile` already sets `gsm.testdata_builder` (to any value),
it is left unchanged.

## Usage

``` r
use_gsm_datasim(scope = c("user", "project"))
```

## Arguments

- scope:

  Which `.Rprofile` to edit: `"user"` (`~/.Rprofile`, the default) or
  `"project"` (`.Rprofile` in the working directory).

## Value

The path to the `.Rprofile`, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
use_gsm_datasim()
use_gsm_datasim("project")
} # }
```
