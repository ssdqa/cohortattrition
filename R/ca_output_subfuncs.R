
#' @import cli
#' @import ggiraph
#' @import gt
#' @import ggplot2
#' @importFrom graphics text
#' @importFrom stats median
#'
NULL


#' *Single Site Exploratory Cross-Sectional*
#'
#' @param process_output output from compute_attrition_diff
#' @param log_scale logical to determine whether a log transform should be applied
#'                  to the y axis
#' @param output the column that should be used as the y-axis:
#'
#'               options are:
#'               `num_pts` (raw patient count),
#'               `prop_retained_start` (proportion patients retained from starting step),
#'               `prop_retained_prior` (proportion patients retained from prior step),
#'               `prop_diff_prior` (proportion difference between each step and the prior step)
#'
#' @return a line graph with the output value for each step and an accompanying
#'         table with the full descriptions of each step
#'
#'
ca_ss_exp_cs <- function(process_output,
                         log_scale = FALSE,
                         output = 'num_pts'){

  cli::cli_div(theme = list(span.code = list(color = 'blue')))

  if(output == 'num_pts'){
    title = 'Patient Count'
  }else if(output == 'prop_retained_start'){
    title = 'Proportion Retained from Start'
  }else if(output == 'prop_retained_prior'){
    title = 'Proportion Retained from Prior Step'
  }else if(output == 'prop_diff_prior'){
    title = 'Proportion Difference from Prior Step'
  }else{cli::cli_abort("Please select a valid output option: {.code num_pts}, {.code prop_retained_start}, {.code prop_retained_prior},
                       or {.code prop_diff_prior}")}

  min_step <- process_output %>% filter(step_number == min(step_number)) %>% distinct(step_number) %>% pull()
  max_step <- process_output %>% filter(step_number == max(step_number)) %>% distinct(step_number) %>% pull()

  grph <- ggplot(process_output %>% mutate(text = paste0('Step: ', attrition_step,
                                                         '\nPatient Count: ', formatC(num_pts, format = 'd', big.mark = ','),
                                                         '\n',output, ': ', round(!!sym(output), 4))),
                 aes(y = !!sym(output), x = step_number)) +
    geom_line(color = 'gray') +
    geom_point_interactive(aes(color = as.character(step_number), tooltip = text), show.legend = FALSE) +
    #scale_y_continuous(transform = 'log2') +
    scale_x_continuous(breaks = seq(min_step, max_step, 1)) +
    labs(y = title,
         x = 'Step') +
    theme_minimal() +
    scale_color_squba()

  if(log_scale){grph <- grph + scale_y_continuous(transform = 'log') + ggtitle(paste0(title, ' per Attrition Step (Log)'))}
  if(!log_scale){grph <- grph + ggtitle(paste0(title, ' per Attrition Step'))}

  grph[['metadata']] <- tibble('pkg_backend' = 'ggiraph',
                               'tooltip' = TRUE)

  tbl <- process_output %>%
    distinct(step_number, attrition_step) %>%
    gt() %>%
    cols_label('step_number' = 'Step Number',
               'attrition_step' = 'Description') %>%
    opt_stylize(style = 2) %>%
    #opt_interactive() %>%
    tab_header('Attrition Step Reference')

  output <- list(grph,
                 tbl)


  return(output)

}



#' *Multi Site Exploratory Cross-Sectional*
#'
#' @param process_output output from compute_attrition_diff
#' @param log_scale logical to determine whether a log transform should be applied
#'                  to the y axis
#' @param output the column that should be used as the y-axis:
#'
#'               options are:
#'               `num_pts` (raw patient count),
#'               `prop_retained_start` (proportion patients retained from starting step),
#'               `prop_retained_prior` (proportion patients retained from prior step),
#'               `prop_diff_prior` (proportion difference between each step and the prior step)
#'
#' @return a line graph with the output value for each step and an accompanying
#'         table with the full descriptions of each step
#'
#' @importFrom rlang :=
#'
ca_ms_exp_cs <- function(process_output,
                         log_scale = FALSE,
                         output = 'num_pts'){

  cli::cli_div(theme = list(span.code = list(color = 'blue')))

  if(output == 'num_pts'){
    title = 'Patient Count'
  }else if(output == 'prop_retained_start'){
    title = 'Proportion Retained from Start'
  }else if(output == 'prop_retained_prior'){
    title = 'Proportion Retained from Prior Step'
  }else if(output == 'prop_diff_prior'){
    title = 'Proportion Difference from Prior Step'
  }else{cli::cli_abort("Please select a valid output option: {.code num_pts}, {.code prop_retained_start}, {.code prop_retained_prior},
                       or {.code prop_diff_prior}")}

  min_step <- process_output %>% filter(step_number == min(step_number)) %>% pull(step_number) %>% unique()
  max_step <- process_output %>% filter(step_number == max(step_number)) %>% pull(step_number) %>% unique()


  allsite_med <- process_output %>%
    group_by(step_number, attrition_step) %>%
    summarise(allsite_median = median(!!sym(output)),
              site = 'All Site Median',
              text = paste0('Site: ', site,
                            '\nMedian Value: ', allsite_median)) %>%
    rename(!!output := allsite_median)

  grph <- ggplot(process_output %>% mutate(text = paste0('Site: ', site,
                                                         '\nStep: ', attrition_step,
                                                         '\nPatient Count: ', formatC(num_pts, format = 'd', big.mark = ','),
                                                         '\n',output,': ', round(!!sym(output), 4))),
                 aes(y = !!sym(output), x = step_number, color = site, group = site)) +
    geom_line() +
    geom_line(data = allsite_med, linewidth = 1.1) +
    geom_point_interactive(aes(tooltip = text), show.legend = FALSE) +
    scale_x_continuous(breaks = seq(min_step, max_step, 1)) +
    labs(y = title,
         x = 'Step',
         color = 'Site') +
    theme_minimal() +
    scale_color_squba()

  if(log_scale){grph <- grph + scale_y_continuous(transform = 'log') + ggtitle(paste0(title, ' per Attrition Step (Log)'))}
  if(!log_scale){grph <- grph + ggtitle(paste0(title, ' per Attrition Step'))}

  grph[['metadata']] <- tibble('pkg_backend' = 'ggiraph',
                               'tooltip' = TRUE)


  tbl <- process_output %>%
    distinct(step_number, attrition_step) %>%
    gt() %>%
    cols_label('step_number' = 'Step Number',
               'attrition_step' = 'Description') %>%
    opt_stylize(style = 2) %>%
    #opt_interactive() %>%
    tab_header('Attrition Step Reference')

  output <- list(grph,
                 tbl)


  return(output)

}

#' *Multi Site Anomaly Cross-Sectional*
#'
#' @param process_output output from compute_attrition_diff
#' @param output the column that should be used as the y-axis:
#'
#'               options are:
#'               `num_pts` (raw patient count),
#'               `prop_retained_start` (proportion patients retained from starting step),
#'               `prop_retained_prior` (proportion patients retained from prior step),
#'               `prop_diff_prior` (proportion difference between each step and the prior step)
#'
#' @return a dot plot indicating anomalous sites based on the user selected output value
#'
#'         anomalies are indicated by STARS, the color of each dot represents the raw output value,
#'         and the size of each dot represents the mean output value per attrition step
#'
ca_ms_anom_cs <-function(process_output,
                         output){

  cli::cli_div(theme = list(span.code = list(color = 'blue')))

  if(output == 'num_pts'){
    title = 'Patient Count'
  }else if(output == 'prop_retained_start'){
    title = 'Proportion Retained from Start'
  }else if(output == 'prop_retained_prior'){
    title = 'Proportion Retained from Prior Step'
  }else if(output == 'prop_diff_prior'){
    title = 'Proportion Difference from Prior Step'
  }else{cli::cli_abort("Please select a valid output option: {.code num_pts}, {.code prop_retained_start}, {.code prop_retained_prior},
                       or {.code prop_diff_prior}")}

  min_step <- process_output %>% filter(step_number == min(step_number)) %>% pull(step_number) %>% unique()
  max_step <- process_output %>% filter(step_number == max(step_number)) %>% pull(step_number) %>% unique()

  check_n <- process_output %>%
    filter(anomaly_yn != 'no outlier in group')

  dat_to_plot <- process_output %>%
    mutate(text=paste("Step: ",attrition_step,
                      "\nSite: ",site,
                      "\n",output,": ",round(!!sym(output),4),
                      "\nMean: ",round(mean_val,2),
                      '\nSD: ', round(sd_val,2),
                      "\nMedian: ",round(median_val,2),
                      "\nMAD: ", round(mad_val,2))) %>%
    mutate(anomaly_yn = ifelse(anomaly_yn == 'no outlier in group', 'not outlier', anomaly_yn))

  if(nrow(check_n) > 0){

    plt<-ggplot(dat_to_plot,
                aes(x=site, y=step_number, text=text, color=!!sym(output)))+
      geom_point_interactive(aes(size=mean_val,shape=anomaly_yn, tooltip = text))+
      geom_point_interactive(data = dat_to_plot %>% filter(anomaly_yn == 'not outlier'),
                             aes(size=mean_val,shape=anomaly_yn, tooltip = text), shape = 1, color = 'black')+
      scale_color_squba(palette = 'diverging', discrete = FALSE) +
      scale_shape_manual(values=c(19,8))+
      #scale_y_continuous(breaks = seq(min_step, max_step, 1)) +
      scale_y_reverse(breaks = seq(min_step, max_step, 1)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle=60, hjust = 1, vjust = 1)) +
      labs(y = "Attrition Step",
           size="",
           title=paste0('Anomalous ', title, ' per Attrition Step'),
           subtitle = 'Dot size is the mean value per step') +
      guides(color = guide_colorbar(title = 'Proportion'),
             shape = guide_legend(title = 'Anomaly'),
             size = 'none')

    plt[['metadata']] <- tibble('pkg_backend' = 'ggiraph',
                                 'tooltip' = TRUE)

    return(plt)

  }else{
    plt <- ggplot(dat_to_plot, aes(x = site, y = step_number, fill = !!sym(output),
                                   tooltip = text)) +
      geom_tile_interactive() +
      theme_minimal() +
      scale_fill_squba(discrete = FALSE, palette = 'diverging') +
      labs(y = 'Attrition Step',
           x = 'Site',
           fill = title)

    # Test Site Score using SD Computation
    test_site_score <- process_output %>%
      mutate(dist_mean = (!!sym(output) - mean_val)^2) %>%
      group_by(site) %>%
      summarise(n_grp = n(),
                dist_mean_sum = sum(dist_mean),
                overall_sd = sqrt(dist_mean_sum / n_grp)) %>%
      mutate(tooltip = paste0('Site: ', site,
                              '\nStandard Deviation: ', round(overall_sd, 3)))

    ylim_max <- test_site_score %>% filter(overall_sd == max(overall_sd)) %>% pull(overall_sd) + 1
    ylim_min <- test_site_score %>% filter(overall_sd == min(overall_sd)) %>% pull(overall_sd) - 1

    g2 <- ggplot(test_site_score, aes(y = overall_sd, x = site, color = site,
                                      tooltip = tooltip)) +
      geom_point_interactive(show.legend = FALSE) +
      theme_minimal() +
      scale_color_squba() +
      geom_hline(yintercept = 0, linetype = 'solid') +
      labs(title = 'Average Standard Deviation per Site',
           y = 'Average Standard Deviation',
           x = 'Site')

    plt[["metadata"]] <- tibble('pkg_backend' = 'ggiraph',
                                'tooltip' = TRUE)
    g2[["metadata"]] <- tibble('pkg_backend' = 'ggiraph',
                               'tooltip' = TRUE)

    opt <- list(plt,
                g2)

    return(opt)
  }

}
