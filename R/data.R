
#' Sample Attrition File
#'
#' A sample version of the file structure expected for the attrition_table
#' parameter in the `ca_process` function. The user should recreate this file
#' structure and include their study-specific attrition information.
#'
#' @format ## `sample_attrition`
#' A data frame with 10 rows and 4 columns
#' \describe{
#'   \item{site}{The name of the institution with which the attrition counts are associated}
#'   \item{step_number}{An integer indicating the attrition step number}
#'   \item{attrition_step}{A string describing the attrition step}
#'   \item{num_pts}{The patient count}
#' }
#'
"sample_attrition"
