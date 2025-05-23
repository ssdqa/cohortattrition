
test_that('ss exp nt', {

  opt <- tibble('site' = c('a', 'a', 'a', 'a',
                           'b', 'b', 'b', 'b'),
                'step_number' = c(1,2,3,4,1,2,3,4),
                'attrition_step' = c('step 1', 'step 2', 'step 3', 'step 4',
                                     'step 1', 'step 2', 'step 3', 'step 4'),
                'num_pts' = c(100, 90, 70, 50, 150, 100, 60, 30),
                'prop_retained_start' = c(NA, 0.9, 0.7, 0.5, NA, 0.8, 0.7, 0.4),
                'output_function' = c('ca_ss_exp_cs', 'ca_ss_exp_cs', 'ca_ss_exp_cs', 'ca_ss_exp_cs',
                                      'ca_ss_exp_cs', 'ca_ss_exp_cs', 'ca_ss_exp_cs', 'ca_ss_exp_cs'))

  expect_no_error(ca_output(process_output = opt %>% filter(site == 'a'),
                            var_col = 'prop_retained_start'))

})

test_that('ms_exp_nt', {

  opt <- tibble('site' = c('a', 'a', 'a', 'a',
                           'b', 'b', 'b', 'b'),
                'step_number' = c(1,2,3,4,1,2,3,4),
                'attrition_step' = c('step 1', 'step 2', 'step 3', 'step 4',
                                     'step 1', 'step 2', 'step 3', 'step 4'),
                'num_pts' = c(100, 90, 70, 50, 150, 100, 60, 30),
                'prop_retained_start' = c(NA, 0.9, 0.7, 0.5, NA, 0.8, 0.7, 0.4),
                'output_function' = c('ca_ms_exp_cs', 'ca_ms_exp_cs', 'ca_ms_exp_cs', 'ca_ms_exp_cs',
                                      'ca_ms_exp_cs', 'ca_ms_exp_cs', 'ca_ms_exp_cs', 'ca_ms_exp_cs'))

  expect_no_error(ca_output(process_output = opt,
                            var_col = 'prop_retained_start'))


})

test_that('ms_anom_nt', {

  opt <- tibble('site' = c('a', 'a', 'a', 'a',
                           'b', 'b', 'b', 'b'),
                'step_number' = c(1,2,3,4,1,2,3,4),
                'attrition_step' = c('step 1', 'step 2', 'step 3', 'step 4',
                                     'step 1', 'step 2', 'step 3', 'step 4'),
                'num_pts' = c(100, 90, 70, 50, 150, 100, 60, 30),
                'prop_retained_start' = c(NA, 0.9, 0.7, 0.5, NA, 0.8, 0.7, 0.4),
                'mean_val' = c(0.85, 0.85, 0.85, 0.85, 0.85, 0.85, 0.85, 0.85),
                'median_val' = c(0.82, 0.82, 0.82, 0.82, 0.82, 0.82, 0.82, 0.82),
                'sd_val' = c(0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05),
                'mad_val' = c(0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02),
                'cov_val' = c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01),
                'max_val' = c(0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95),
                'min_val' = c(0.79, 0.79, 0.79, 0.79, 0.79, 0.79, 0.79, 0.79),
                'range_val' = c(0.16, 0.16, 0.16, 0.16, 0.16, 0.16, 0.16, 0.16),
                'total_ct' = c(2,2,2,2,2,2,2,2),
                'analysis_eligible' = c('yes','yes','yes', 'yes','yes','yes', 'yes','yes'),
                'lower_tail' = c(0.8134, 0.8134, 0.8134, 0.8134, 0.8134, 0.8134, 0.8134, 0.8134),
                'upper_tail' = c(0.932, 0.932, 0.932, 0.932, 0.932, 0.932, 0.932, 0.932),
                'anomaly_yn' = c('no outlier', 'outlier', 'outlier', 'no outlier', 'outlier', 'outlier',
                                 'no outlier', 'outlier'),
                'output_function' = c('ca_ms_anom_cs', 'ca_ms_anom_cs', 'ca_ms_anom_cs', 'ca_ms_anom_cs',
                                      'ca_ms_anom_cs', 'ca_ms_anom_cs', 'ca_ms_anom_cs', 'ca_ms_anom_cs'))

  expect_no_error(ca_output(process_output = opt,
                            var_col = 'prop_retained_start'))

  expect_no_error(ca_output(process_output = opt %>% mutate(anomaly_yn = 'no outlier in group'),
                            var_col = 'prop_retained_start'))

})
