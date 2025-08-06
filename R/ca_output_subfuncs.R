
#' @import cli
#' @import ggiraph
#' @import gt
#' @import ggplot2
#' @importFrom graphics text
#' @importFrom stats median
#' @importFrom stats quantile
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
#' @param large_n a boolean indicating whether the large N visualization, intended for a high
#'                volume of sites, should be used; defaults to FALSE
#' @param large_n_sites a vector of site names that can optionally be compared against summary statistics
#' @return a line graph with the output value for each step and an accompanying
#'         table with the full descriptions of each step
#'
#' @importFrom rlang :=
#'
ca_ms_exp_cs <- function(process_output,
                         log_scale = FALSE,
                         output = 'num_pts',
                         large_n = FALSE,
                         large_n_sites = NULL){

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

  if(!large_n){

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
  }else{

    allsite_q1 <- process_output %>%
      group_by(step_number, attrition_step) %>%
      summarise(allsite_q1 = quantile(!!sym(output), 0.25),
                site = 'All Site Q1',
                text = paste0('Site: ', site,
                              '\nQ1 Value: ', allsite_q1)) %>%
      rename(!!output := allsite_q1)

    allsite_q3 <- process_output %>%
      group_by(step_number, attrition_step) %>%
      summarise(allsite_q3 = quantile(!!sym(output), 0.75),
                site = 'All Site Q3',
                text = paste0('Site: ', site,
                              '\nQ3 Value: ', allsite_q3)) %>%
      rename(!!output := allsite_q3)

    plt_input <- process_output %>%
      mutate(text = paste0('Site: ', site,
                           '\nStep: ', attrition_step,
                           '\nPatient Count: ', formatC(num_pts, format = 'd', big.mark = ','),
                           '\n',output,': ', round(!!sym(output), 4))) %>%
      select(site, step_number, attrition_step, !!sym(output), text) %>%
      filter(site %in% large_n_sites)

    summary_stats <- allsite_med %>%
      union(allsite_q1) %>%
      union(allsite_q3)

    if(!is.null(large_n_sites)){
      alp <- 0.75
      ltp <- 'dashed'
    }else{
      alp <- 1
      ltp <- 'solid'
    }

    grph <- ggplot(summary_stats,
                   aes(y = !!sym(output), x = step_number, color = site, group = site)) +
      geom_line(linewidth = 1.2, alpha = alp, linetype = ltp) +
      geom_line(data = plt_input) +
      geom_point_interactive(data = plt_input,
                             aes(tooltip = text), show.legend = FALSE) +
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
  }


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
#' @param large_n a boolean indicating whether the large N visualization, intended for a high
#'                volume of sites, should be used; defaults to FALSE
#' @param large_n_sites a vector of site names that can optionally generate a filtered visualization
#' @return a dot plot indicating anomalous sites based on the user selected output value
#'
#'         anomalies are indicated by STARS, the color of each dot represents the raw output value,
#'         and the size of each dot represents the mean output value per attrition step
#'
ca_ms_anom_cs <-function(process_output,
                         output,
                         large_n = FALSE,
                         large_n_sites = NULL){

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

  if(!large_n){
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
  }else{
    suppressWarnings(
      far_site <- process_output %>%
        # filter(anomaly_yn != 'no outlier in group') %>%
        mutate(zscr = (!!sym(output) - mean_val) / sd_val,
               zscr = ifelse(is.nan(zscr), NA, zscr),
               zscr = abs(zscr)) %>%
        group_by(step_number, attrition_step) %>%
        filter(zscr == max(zscr, na.rm = TRUE)) %>%
        summarise(farthest_site = site,
                  nvar = n())
    )

    if(any(far_site$nvar > 1)){
      far_site <- far_site %>%
        summarise_all(toString) %>% select(-nvar)
    }else{
      far_site <- far_site %>% select(-nvar)
    }

    suppressWarnings(
      close_site <- process_output %>%
        # filter(anomaly_yn != 'no outlier in group') %>%
        mutate(zscr = (!!sym(output) - mean_val) / sd_val,
               zscr = ifelse(is.nan(zscr), NA, zscr),
               zscr = abs(zscr)) %>%
        group_by(step_number, attrition_step) %>%
        filter(zscr == min(zscr, na.rm = TRUE)) %>%
        summarise(closest_site = site,
                  nvar = n())
    )

    if(any(close_site$nvar > 1)){
      close_site <- close_site %>%
        summarise_all(toString) %>% select(-nvar)
    }else{
      close_site <- close_site %>% select(-nvar)
    }

    nsite_anom <- process_output %>%
      group_by(step_number, attrition_step, anomaly_yn) %>%
      summarise(site_w_anom = n_distinct(site)) %>%
      filter(anomaly_yn == 'outlier') %>%
      ungroup() %>%
      select(-anomaly_yn)

    # clps_vals <- process_output %>%
    #   group_by(step_number, attrition_step,) %>%
    #   summarise(valcol = list(!!sym(output)))

    if(output == 'num_pts'){ndec = 0}else{ndec = 3}

    tbl <- process_output %>%
      group_by(step_number, attrition_step) %>%
      mutate(iqr_val = stats::IQR(!!sym(output))) %>%
      ungroup() %>%
      distinct(step_number, attrition_step, mean_val, sd_val, median_val, iqr_val) %>%
      left_join(nsite_anom) %>%
      left_join(far_site) %>%
      left_join(close_site) %>%
      # left_join(clps_vals) %>%
      gt::gt() %>%
      tab_header('Large N Anomaly Detection Summary Table') %>%
      # gtExtras::gt_plt_dist(column = valcol,
      #                       type = 'boxplot', same_limit = FALSE) %>%
      cols_label(step_number = 'Step Number',
                 attrition_step = 'Step Description',
                 mean_val = 'Mean',
                 sd_val = 'Standard Deviation',
                 median_val = 'Median',
                 iqr_val = 'IQR',
                 site_w_anom = 'No. Sites w/ Anomaly',
                 farthest_site = 'Site(s) Farthest from Mean',
                 closest_site = 'Site(s) Closest to Mean') %>%
      sub_missing(missing_text = 0,
                  columns = site_w_anom) %>%
      sub_missing(missing_text = '--',
                  columns = c(farthest_site, closest_site)) %>%
      fmt_number(columns = c(mean_val, median_val, sd_val, iqr_val),
                 decimals = ndec) %>%
      opt_stylize(style = 2)

    if(!is.null(large_n_sites)){
      plt<-ggplot(dat_to_plot %>% filter(site %in% large_n_sites),
                  aes(x=site, y=step_number, text=text, color=!!sym(output)))+
        geom_point_interactive(aes(size=mean_val,shape=anomaly_yn, tooltip = text))+
        geom_point_interactive(data = dat_to_plot %>% filter(anomaly_yn == 'not outlier',
                                                             site %in% large_n_sites),
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

      opt <- list(plt,
                  tbl)

      return(opt)
    }else{
      return(tbl)
    }
  }

}
