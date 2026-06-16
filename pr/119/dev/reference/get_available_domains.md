# Get Available Domain Names

**\[experimental\]**

## Usage

``` r
get_available_domains(study)
```

## Arguments

- study:

  Longitudinal study data structure

## Value

Character vector of domain names

## Details

Return list of available domain names across all snapshots.

## Examples

``` r
if (FALSE) { # \dontrun{
study <- create_longitudinal_study("STUDY-001", participants = 50, sites = 5, snapshots = 2)
get_available_domains(study)
} # }
```
