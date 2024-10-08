% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ca_process_subfuncs.R
\name{compute_attrition_diff}
\alias{compute_attrition_diff}
\title{Compute difference between attrition steps}
\usage{
compute_attrition_diff(attrition_tbl, start_step_num = 0, site_col = "site")
}
\arguments{
\item{attrition_tbl}{CSV file with attrition information - should include the following columns:

\if{html}{\out{<div class="sourceCode">}}\preformatted{                 num_pts | step_number | attrition_step | site
}\if{html}{\out{</div>}}}

\item{start_step_num}{integer indicating the number of the "start" step against which other steps should
be compared}

\item{site_col}{the column in the attrition table with the name(s) of the site(s)}
}
\value{
the attrition information plus columns that describe the patient drop & percent difference between
each step and between each step and step 0
}
\description{
Compute difference between attrition steps
}
