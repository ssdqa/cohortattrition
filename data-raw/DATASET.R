## code to prepare `sample_attrition` dataset goes here

sample_attrition <- tibble(site = c('site 1', 'site 1', 'site 1', 'site 1', 'site 1', 'site 2',
                                    'site 2', 'site 2', 'site 2', 'site 2'),
                           step_number = c(0, 1, 2, 3, 4, 0, 1, 2, 3, 4),
                           attrition_step = c('All Patients', 'Patients with at least 2 visits since 2015',
                                              'Patients with a T2DM diagnosis', 'Patients with an Hba1c lab',
                                              'Patients with an Hba1c result > 6.5%', 'All Patients',
                                              'Patients with at least 2 visits since 2015',
                                              'Patients with a T2DM diagnosis', 'Patients with an Hba1c lab',
                                              'Patients with an Hba1c result > 6.5%'),
                           num_pts = c(10900, 7800, 2200, 2000, 1900, 17000, 10500, 5500, 5000, 4800))


usethis::use_data(sample_attrition, overwrite = TRUE)
