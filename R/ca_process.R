#' Cohort Attrition
#'
#' This is a plausibility module that will assess attrition counts and how counts
#' change from each step to the next to help identify the most impactful and any
#' potentially anomalous attrition criteria. The user will provide a precomputed
#' attrition. A sample `attrition_tbl` with the expected formatting can be found
#' using `cohortattrition::`.
#'
#' @param attrition_tbl *tabular input* || **required**
#'
#'  A table or CSV file with attrition information for each site included in the cohort.
#'  This table should minimally contain:
#'  - `site` | *character* | the name of the institution
#'  - `step_number` | *integer* | a numeric identifier for the attrition step
#'  - `attrition_step` | *character* | a description of the attrition step
#'  - `num_pts` | *integer* | the patient count for the attrition step
#'
#' @param multi_or_single_site *string* || defaults to `single`
#'
#'   A string, either `single` or `multi`, indicating whether a single-site or
#'   multi-site analysis should be executed
#'
#' @param anomaly_or_exploratory *string* || defaults to `exploratory`
#'
#'   A string, either `anomaly` or `exploratory`, indicating what type of results
#'   should be produced.
#'
#'   Exploratory analyses give a high level summary of the data to examine the
#'   fact representation within the cohort. Anomaly detection analyses are
#'   specialized to identify outliers within the cohort.
#'
#' @param start_step_num *integer* || defaults to `0`
#'
#'   The `step_number` from the `attrition_tbl` that should be considered the "start" for
#'   the analysis. This will drive comparisons of later steps to the start.
#'
#' @param var_col *string* || defaults to `num_pts`
#'
#'   The name of the column that should be used to conduct the analysis for
#'   the Multi-Site, Anomaly Detection, Cross-Sectional check. The options are:
#'   - `num_pts`: raw patient count
#'   - `prop_retained_start`: proportion patients retained from the starting step, as indicated by `start_step_num`
#'   - `prop_retained_prior`: proportion patients retained from prior step
#'   - `prop_diff_prior`: proportion difference between each step and the prior step
#'
#' @param p_value *numeric* || defaults to `0.9`
#'
#'   The p value to be used as a threshold in the Multi-Site,
#'   Anomaly Detection, Cross-Sectional analysis
#'
#' @return This function will return a dataframe with all the original attrition
#'         information, plus columns examining the difference between each step and others.
#'         If the Multi-Site, Anomaly Detection, Cross-Sectional check is run, this output
#'         will also include some descriptive statistics about the chosen `var_col` and
#'         an indication of which sites are outliers at each attrition step.
#'
#' @import squba.gen
#' @import argos
#' @importFrom stringr str_wrap
#'
#' @example inst/example-ca_process_output.R
#'
#' @export
#'
ca_process <- function(attrition_tbl,
                       multi_or_single_site = 'single',
                       anomaly_or_exploratory = 'exploratory',
                       start_step_num = 0,
                       var_col = 'num_pts',
                       p_value = 0.9){

  ## Check proper arguments
  cli::cli_div(theme = list(span.code = list(color = 'blue')))

  if(!multi_or_single_site %in% c('single', 'multi')){cli::cli_abort('Invalid argument for {.code multi_or_single_site}: please enter either {.code multi} or {.code single}')}
  if(!anomaly_or_exploratory %in% c('anomaly', 'exploratory')){cli::cli_abort('Invalid argument for {.code anomaly_or_exploratory}: please enter either {.code anomaly} or {.code exploratory}')}

  ## parameter summary output
  output_type <- suppressWarnings(param_summ(check_string = 'ca',
                                             as.list(environment())))

  ## Add site check
  site_filter <- check_site_type(cohort = attrition_tbl,
                                 multi_or_single_site = multi_or_single_site)
  attrition_tbl <- site_filter$cohort
  site_col <- site_filter$grouped_list

  if(multi_or_single_site == 'single' & site_col == 'site_summ'){
    cli::cli_abort('Multiple sites detected. For single-site analysis, please only provide the attrition for one site.')
  }

  ## Attrition computations
  attrition_process <- compute_attrition_diff(attrition_tbl = attrition_tbl,
                                              start_step_num = start_step_num,
                                              site_col = site_col)

  if(multi_or_single_site == 'multi' && anomaly_or_exploratory == 'anomaly'){

    att_int <- compute_dist_anomalies(df_tbl = attrition_process,
                                      grp_vars = c('step_number', 'attrition_step'),
                                      var_col = var_col,
                                      denom_cols = c('step_number', 'attrition_step'))

    att_final <- detect_outliers(df_tbl = att_int,
                                 p_input = p_value,
                                 column_analysis = var_col,
                                 column_variable = c('step_number', 'attrition_step'))

  }else{att_final <- attrition_process}

  print(cli::boxx(c('You can optionally use this dataframe in the accompanying',
                    '`ca_output` function. Here are the parameters you will need:', '', output_type$vector, '',
                    'See ?ca_output for more details.'), padding = c(0,1,0,1),
                  header = cli::col_cyan('Output Function Details')))

  return(att_final %>% replace_site_col() %>% mutate(output_function = paste0(output_type$string)))

}
