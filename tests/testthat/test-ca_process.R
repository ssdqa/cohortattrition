
## Testing error functionality
test_that('only single & multi are allowed inputs', {

  cht <- data.frame('person_id' = c(1000, 1001),
                    'site' = c('a', 'b'),
                    'start_date' = c('2007-01-01','2008-01-01'),
                    'end_date' = c('2011-01-01','2009-01-01'))

  expect_error(ca_process(attrition_tbl = cht,
                          multi_or_single_site = 'test',
                          anomaly_or_exploratory = 'exploratory'))
})


test_that('only anomaly & exploratory are allowed inputs', {

  cht <- data.frame('person_id' = c(1000, 1001),
                    'site' = c('a', 'b'),
                    'start_date' = c('2007-01-01','2008-01-01'),
                    'end_date' = c('2011-01-01','2009-01-01'))

  expect_error(ca_process(attrition_tbl = cht,
                          multi_or_single_site = 'single',
                          anomaly_or_exploratory = 'test'))
})

test_that('ca exp nt', {

  samp_att <- tibble('site' = c('a', 'a', 'a', 'a'),
                     'step_number' = c(1,2,3,4),
                     'attrition_step' = c('step 1', 'step 2', 'step 3', 'step 4'),
                     'num_pts' = c(100, 90, 70, 50))

  expect_no_error(ca_process(attrition_tbl = samp_att,
                             multi_or_single_site = 'single',
                             anomaly_or_exploratory = 'exploratory',
                             start_step_num = 1))
})

test_that('ca ms anom nt', {

  samp_att <- tibble('site' = c('a', 'a', 'a', 'a',
                                'b', 'b', 'b', 'b'),
                     'step_number' = c(1,2,3,4,1,2,3,4),
                     'attrition_step' = c('step 1', 'step 2', 'step 3', 'step 4',
                                          'step 1', 'step 2', 'step 3', 'step 4'),
                     'num_pts' = c(100, 90, 70, 50, 150, 100, 60, 30))

  expect_no_error(ca_process(attrition_tbl = samp_att,
                             multi_or_single_site = 'multi',
                             anomaly_or_exploratory = 'anomaly',
                             start_step_num = 1))
})
