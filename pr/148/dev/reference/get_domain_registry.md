# Domain Registry (draft)

**\[experimental\]**

## Usage

``` r
get_domain_registry()
```

## Value

Named list of domain registry entries.

## Details

Returns a named list that defines per-domain generation behavior for the
registry-based migration path.

## Examples

``` r
registry <- get_domain_registry()
names(registry)              # all supported domain keys
#>  [1] "Raw_SITE"            "Raw_SUBJ"            "Raw_ENROLL"         
#>  [4] "Raw_IE"              "Raw_VISIT"           "Raw_STUDCOMP"       
#>  [7] "Raw_DATACHG"         "Raw_DATAENT"         "Raw_QUERY"          
#> [10] "Raw_AE"              "Raw_LB"              "Raw_PD"             
#> [13] "Raw_SDRGCOMP"        "Raw_Consents"        "Raw_Death"          
#> [16] "Raw_AntiCancer"      "Raw_Randomization"   "Raw_OverallResponse"
#> [19] "Raw_PK"              "Raw_VS"              "Raw_Baseline"       
names(registry[["Raw_AE"]]) # structure of a single entry
#> [1] "dataset"         "required_inputs" "count_fn"        "generate_fn"    
```
