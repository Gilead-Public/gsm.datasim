# Set Outlier Configuration

Updates outlier generation intensity in a study config.

## Usage

``` r
set_outlier_config(config, intensity = 1)
```

## Arguments

- config:

  Study configuration list

- intensity:

  Global outlier intensity multiplier. `1` keeps current behavior,
  values `>1` increase outlier prevalence.

## Value

Updated study configuration
