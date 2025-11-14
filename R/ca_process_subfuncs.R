#' Compute difference between attrition steps
#'
#' @param attrition_tbl CSV file or dataframe with attrition information -
#'                      should include the following columns:
#' - `num_pts`
#' - `step_number`
#' - `attrition_step`
#' - `site`
#' @param start_step_num integer indicating the number of the "start" step against which other steps should
#'                       be compared; defaults to 0
#' @param site_col the column in the attrition table with the name(s) of the site(s)
#'
#' @return the attrition information plus columns that describe the patient drop & percent difference between
#'         each step and between each step and step 0
#'
#' @import dplyr
#' @importFrom tidyr replace_na
#'
#' @keywords internal
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
    select(!!sym(site_col), step0_pts)

  pct_step0 <- attrition_tbl %>%
    left_join(step0_cts) %>%
    mutate(prop_retained_start = num_pts / step0_pts) %>%
    select(-step0_pts)

  final_attrition <- attrition_tbl %>%
    filter(step_number >= start_step_num) %>%
    left_join(pct_prior) %>%
    left_join(pct_step0) %>%
    arrange(step_number)

  step_join <- final_attrition %>%
    filter(step_number != start_step_num) %>%
    distinct(step_number, attrition_step)
  site_join <- final_attrition %>%
    distinct(!!sym(site_col))
  fill_join <- site_join %>% cross_join(step_join)

  edit_attrition <- final_attrition %>%
    filter(step_number != start_step_num) %>%
    full_join(fill_join) %>%
    mutate(across(where(is.numeric), .fns = ~replace_na(.,0)))

  final_attrition <- final_attrition %>%
    filter(step_number == start_step_num) %>%
    union(edit_attrition)

  return(final_attrition)
}
