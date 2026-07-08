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
#>  [4] "Raw_IE"              "Raw_EXCLUSION"       "Raw_VISIT"          
#>  [7] "Raw_STUDCOMP"        "Raw_DATACHG"         "Raw_DATAENT"        
#> [10] "Raw_QUERY"           "Raw_AE"              "Raw_LB"             
#> [13] "Raw_PD"              "Raw_SDRGCOMP"        "Raw_Consents"       
#> [16] "Raw_Death"           "Raw_AntiCancer"      "Raw_Randomization"  
#> [19] "Raw_OverallResponse" "Raw_PK"              "Raw_Baseline"       
names(registry[["Raw_AE"]]) # structure of a single entry
#> [1] "dataset"         "required_inputs" "count_fn"        "generate_fn"    
```
