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
#>  [4] "Raw_IE"              "Raw_VISIT"           "Raw_STUDCOMP"       
#>  [7] "Raw_DATACHG"         "Raw_DATAENT"         "Raw_QUERY"          
#> [10] "Raw_AE"              "Raw_LB"              "Raw_PD"             
#> [13] "Raw_SDRGCOMP"        "Raw_Consents"        "Raw_Death"          
#> [16] "Raw_AntiCancer"      "Raw_Randomization"   "Raw_OverallResponse"
#> [19] "Raw_PK"              "Raw_VS"              "Raw_Baseline"       

if (FALSE) { # \dontrun{
study <- create_longitudinal_study("STUDY-001", participants = 50, sites = 5, snapshots = 2)
get_available_domains(study)
} # }
```
