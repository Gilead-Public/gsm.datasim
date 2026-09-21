# Ensure core mappings are included

Ensure core mappings are included

## Usage

``` r
ensure_core_mappings(domains)
```

## Arguments

- domains:

  Vector of domain names

## Value

Vector with required core mappings added

## Examples

``` r
ensure_core_mappings(c("AE", "LB", "VISIT"))
#> [1] "Raw_STUDY"  "Raw_SITE"   "Raw_SUBJ"   "Raw_ENROLL" "Raw_AE"    
#> [6] "Raw_LB"     "Raw_VISIT" 
```
