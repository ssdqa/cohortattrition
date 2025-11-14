# Compute difference between attrition steps

Compute difference between attrition steps

## Usage

``` r
compute_attrition_diff(attrition_tbl, start_step_num = 0, site_col = "site")
```

## Arguments

- attrition_tbl:

  CSV file or dataframe with attrition information - should include the
  following columns:

  - `num_pts`

  - `step_number`

  - `attrition_step`

  - `site`

- start_step_num:

  integer indicating the number of the "start" step against which other
  steps should be compared; defaults to 0

- site_col:

  the column in the attrition table with the name(s) of the site(s)

## Value

the attrition information plus columns that describe the patient drop &
percent difference between each step and between each step and step 0
