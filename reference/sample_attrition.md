# Sample Attrition File

A sample version of the file structure expected for the attrition_table
parameter in the `ca_process` function. The user should recreate this
file structure and include their study-specific attrition information.

## Usage

``` r
sample_attrition
```

## Format

### `sample_attrition`

A data frame with 4 columns

- site:

  The name of the institution with which the attrition counts are
  associated

- step_number:

  An integer indicating the attrition step number

- attrition_step:

  A string describing the attrition step

- num_pts:

  The patient count
