# This script analyzes a set of inflammatory parameters in the COVID-19 severity groups: moderate vs severe/critical
# Healthy and non-COVID-19 cases are ignored for now

  insert_head()
  
# data container -----
  
  inflammation <- list()
  
# script globals -----
  
  insert_msg('Globals setup')
  
  ## inflammatory variables
  
  inflammation$variable <- c('wbc', 
                             'crp',
                             'pct', 
                             'il6', 
                             'neopterin', 
                             'ferritin',
                             'iron', 
                             'tf_sat')
  
  ## normal ranges of the variables
  
  inflammation$lower_limit <- list('wbc' = 4.5, 
                                   'crp' = 0,
                                   'pct' = 0, 
                                   'il6' = 0, 
                                   'neopterin' = 0, 
                                   'ferritin' = 12,
                                   'iron' = 10.74, 
                                   'tf_sat' = 15)
  
  inflammation$upper_limit <- list('wbc' = 11, 
                                   'crp' = 3,
                                   'pct' = 0.05, 
                                   'il6' = 7, 
                                   'neopterin' = 10, 
                                   'ferritin' = 300,
                                   'iron' = 30, 
                                   'tf_sat' = 50)

# analysis table: admission, CoV  and healthy cases only ------
  
  insert_msg('Analysis table')
  
  inflammation$analysis_tbl <- mono_data$data_tbl %>% 
    filter(covid_status  == 'COVID-19') %>% 
    select(id, 
           sev_paper, 
           all_of(inflammation$variable))
  
# variable distribution, normality test (Shapiro) -----
  
  insert_msg('Normality check')
  
  inflammation$normality <- explore(data = inflammation$analysis_tbl, 
                                    variables = inflammation$variable, 
                                    what = 'normality')
  
# Descriptive statistic ------
  
  insert_msg('Descriptive statistics in the severity groups')
  
  inflammation$desc_stats <- explore(data = inflammation$analysis_tbl, 
                                     split_factor = 'sev_paper', 
                                     variables = inflammation$variable, 
                                     what = 'table')
  
# Comparing the values between the groups -----
  # the variables are in substantial part non-normally distributed, hence Wilcoxon test
  
  insert_msg('Comparing the variables between the severity strata')
  
  inflammation$test_results <- compare_variables(inflammation$analysis_tbl, 
                                                 variables = inflammation$variable, 
                                                 split_factor = 'sev_paper', 
                                                 what = 'eff_size', 
                                                 types = 'wilcoxon_r', 
                                                 ci = FALSE)
  
# Plotting -----
  
  insert_msg('Plotting')
  
  inflammation$single_plots <- list(variable = inflammation$test_results$variable, 
                                    plot_subtitle = inflammation$test_results$significance, 
                                    plot_title = translate_var(inflammation$test_results$variable, 
                                                               type = 'plot_title'), 
                                    y_lab = translate_var(inflammation$test_results$variable, 
                                                          type = 'plot_lab')) %>% 
    pmap(plot_variable, 
         inflammation$analysis_tbl, 
         split_factor = 'sev_paper', 
         type = 'box', 
         cust_theme = proj_globals$common_theme, 
         point_alpha = 0.8) %>% 
    set_names(inflammation$test_results$variable) %>% 
    map(~.x + 
          scale_fill_manual(values = proj_globals$sev_paper_colors, 
                            labels = proj_globals$sev_paper_labels) + 
          scale_x_discrete(labels = proj_globals$sev_paper_labels))

# Adding normal parameter range to the plots ------
  
  insert_msg('Adding the normal ranges to the plots')
  
  inflammation$single_plots <- list(plot = inflammation$single_plots, 
                                    lower_limit = inflammation$lower_limit, 
                                    upper_limit = inflammation$upper_limit) %>% 
    pmap(add_norm_range)

# END -----
  
  insert_tail()