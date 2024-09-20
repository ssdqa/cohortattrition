#' Compute difference between attrition steps
#'
#' @param attrition_tbl CSV file with attrition information - should include the following columns:
#'
#'                      num_pts | step_number | attrition_step | site
#' @param start_step_num integer indicating the number of the "start" step against which other steps should
#'                       be compared
#' @param site_col the column in the attrition table with the name(s) of the site(s)
#'
#' @return the attrition information plus columns that describe the patient drop & percent difference between
#'         each step and between each step and step 0
#'
#' @import dplyr
#'

compute_attrition_diff <- function(attrition_tbl,
                                   start_step_num = 0,
                                   site_col = 'site'){

  #if(! site_col %in% colnames(attrition_tbl)){attrition_tbl$site <- 'attrition_site'}

  pct_prior <- attrition_tbl %>%
    filter(step_number >= start_step_num) %>%
    group_by(!!sym(site_col)) %>%
    arrange(step_number) %>%
    mutate(
      prop_retained_prior = num_pts / lag(num_pts),
      ct_diff_prior = lag(num_pts) - num_pts,
      prop_diff_prior = (lag(num_pts) - num_pts) / lag(num_pts)
    ) %>%
    ungroup()

  step0_cts <- attrition_tbl %>%
    filter(step_number == start_step_num) %>%
    rename('step0_pts' = num_pts) %>%
    select(qry_site, step0_pts)

  pct_step0 <- attrition_tbl %>%
    left_join(step0_cts) %>%
    mutate(prop_retained_start = num_pts / step0_pts) %>%
    select(-step0_pts)

  final_attrition <- attrition_tbl %>%
    filter(step_number >= start_step_num) %>%
    left_join(pct_prior) %>%
    left_join(pct_step0) %>%
    arrange(step_number)

  return(final_attrition)
}


#' #' Combine attritions from multiple sites
#' #'
#' #' This function reads in CSV files with a naming structure of site_file_suffix.csv and
#' #' combines them into a master file with data from all provided sites
#' #'
#' #' @param site_list list of all sites for which there are attrition files
#' #' @param file_directory the directory holding all of the attrition files
#' #' @param file_suffix the suffix of the attrition files
#' #'
#' #' @return a combined dataframe with attrition information from all of the sites
#' #'         provided in site_list
#' #'
#' #' @importFrom readr read_csv
#' #'
#' #'
#' combine_attritions <- function(site_list,
#'                                file_directory = paste0(base_dir, '/results/'),
#'                                file_suffix = NULL){
#'
#'   attrition_list <- list()
#'
#'   for(i in 1:length(site_list)){
#'
#'     file_name <- paste0(site_list[i], file_suffix, '.csv')
#'
#'     file <- read_csv(file = paste0(file_directory, file_name))
#'
#'
#'     attrition_list[[i]] <- file
#'
#'   }
#'
#'   combined_attrition <- reduce(.x = attrition_list,
#'                                .f = dplyr::union)
#'
#'   return(combined_attrition)
#'
#' }
