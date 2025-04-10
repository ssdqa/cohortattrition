
#' Build mock study attrition
sample_attrition <- dplyr::tibble('site' = c('Site A', 'Site A', 'Site A', 'Site A'),
                                  'step_number' = c(1,2,3,4),
                                  'attrition_step' = c('step 1', 'step 2', 'step 3', 'step 4'),
                                  'num_pts' = c(100, 90, 70, 50))

#' Execute `ca_process` function
#' This example will use the single site, exploratory, cross sectional
#' configuration
ca_process_example <- ca_process(attrition_tbl = sample_attrition,
                                 multi_or_single_site = 'single',
                                 anomaly_or_exploratory = 'exploratory',
                                 start_step_num = 1)

ca_process_example

#' Execute `ca_output` function
ca_output_example <- ca_output(process_output = ca_process_example,
                               output_function = 'ca_ss_exp_cs',
                               log_scale = FALSE,
                               var_col = 'prop_retained_start')

ca_output_example

#' Easily convert the graph into an interactive ggiraph or plotly object with
#' `make_interactive_squba()`

make_interactive_squba(ca_output_example[[1]])
