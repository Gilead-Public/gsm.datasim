# Get Available Domain Names

**\[experimental\]**

## Usage

``` r
get_available_domains(study = NULL)
```

## Arguments

- study:

  Longitudinal study data structure, or `NULL` to look up all registry
  domain names.

## Value

Character vector of domain names

## Details

Return list of available domain names.

When `study` is `NULL` (the default), returns all domain names defined
in the domain registry (a standalone registry lookup). When a study is
supplied, returns the domain names present across that study's
snapshots.

## Examples

``` r
# All registry domain names (no study needed)
get_available_domains()
#>  [1] "Raw_SITE"            "Raw_SUBJ"            "Raw_ENROLL"         
#>  [4] "Raw_IE"              "Raw_EXCLUSION"       "Raw_VISIT"          
#>  [7] "Raw_STUDCOMP"        "Raw_DATACHG"         "Raw_DATAENT"        
#> [10] "Raw_QUERY"           "Raw_AE"              "Raw_LB"             
#> [13] "Raw_PD"              "Raw_SDRGCOMP"        "Raw_Consents"       
#> [16] "Raw_Death"           "Raw_AntiCancer"      "Raw_Randomization"  
#> [19] "Raw_OverallResponse" "Raw_PK"              "Raw_Baseline"       

if (FALSE) { # \dontrun{
study <- create_longitudinal_study("STUDY-001", participants = 50, sites = 5, snapshots = 2)
get_available_domains(study)
} # }
```
