# *Single Site Exploratory Cross-Sectional*

*Single Site Exploratory Cross-Sectional*

## Usage

``` r
ca_ss_exp_cs(process_output, log_scale = FALSE, output = "num_pts")
```

## Arguments

- process_output:

  output from compute_attrition_diff

- log_scale:

  logical to determine whether a log transform should be applied to the
  y axis

- output:

  the column that should be used as the y-axis:

                options are:
                `num_pts` (raw patient count),
                `prop_retained_start` (proportion patients retained from starting step),
                `prop_retained_prior` (proportion patients retained from prior step),
                `prop_diff_prior` (proportion difference between each step and the prior step)

## Value

a line graph with the output value for each step and an accompanying
table with the full descriptions of each step
