# *Multi Site Anomaly Cross-Sectional*

*Multi Site Anomaly Cross-Sectional*

## Usage

``` r
ca_ms_anom_cs(process_output, output, large_n = FALSE, large_n_sites = NULL)
```

## Arguments

- process_output:

  output from compute_attrition_diff

- output:

  the column that should be used as the y-axis:

                options are:
                `num_pts` (raw patient count),
                `prop_retained_start` (proportion patients retained from starting step),
                `prop_retained_prior` (proportion patients retained from prior step),
                `prop_diff_prior` (proportion difference between each step and the prior step)

- large_n:

  a boolean indicating whether the large N visualization, intended for a
  high volume of sites, should be used; defaults to FALSE

- large_n_sites:

  a vector of site names that can optionally generate a filtered
  visualization

## Value

a dot plot indicating anomalous sites based on the user selected output
value

        anomalies are indicated by STARS, the color of each dot represents the raw output value,
        and the size of each dot represents the mean output value per attrition step
