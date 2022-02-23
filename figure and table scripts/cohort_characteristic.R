# This script analyzes the features in the moderate and severe CoV and makes a raw cohort characteristic table

  insert_head()
  
# data container ----
  
  cohort <- list()

# globals and analysis table -----
  
  insert_msg('Globals and analysis table')
  
  ## variables of interest
  
  cohort$variable <- c('age', 
                       'sex', 
                       'bmi',  
                       'hospital_stay',
                       'oxygen', 
                       'icu', 
                       'mortality')

  ## healthy control data
  
  cohort$healthy_data <- read_excel('./clinics/healthy_controls.xlsx') %>% 
    mutate(oxygen = 'no', 
           icu = 'no', 
           hospital_stay = 0, 
           mortality = 'no', 
           sev_paper = 'healthy')
  
  ## analysis table
  
  cohort$analysis_tbl <- mono_data$data_tbl %>% 
    filter(covid_status == 'COVID-19') %>% 
    select(id, 
           sev_paper, 
           all_of(cohort$variable)) %>% 
    rbind(cohort$healthy_data, .) %>% 
    mutate(icu = factor(icu, c('no', 'yes')), 
           oxygen = factor(oxygen, c('no', 'yes')), 
           mortality = factor(mortality, c('no', 'yes')), 
           sex = factor(sex, c('female', 'male')))
  
  ## n numbers
  
  cohort$n_numbers <- cohort$analysis_tbl %>% 
    count(sev_paper)
  
# Distribution and EOV of the numeric variables -----
  
  insert_msg('Normality and EOV of the numeric variables')
  
  cohort$normality <- explore(cohort$analysis_tbl, 
                              variables = c('age', 'bmi', 'hospital_stay'), 
                              what = 'normality', 
                              pub_styled = TRUE)
  
  cohort$eov <- compare_variables(cohort$analysis_tbl, 
                                  variables = c('age', 'bmi', 'hospital_stay'), 
                                  split_factor = 'sev_paper', 
                                  what = 'variance', pub_styled = TRUE)
  
# Descriptive statistic -----
  
  insert_msg('Descriptive statistics')
  
  cohort$desc_stats <- explore(cohort$analysis_tbl, 
                               variables = cohort$variable, 
                               split_factor = 'sev_paper', 
                               what = 'table', 
                               pub_styled = TRUE) %>% 
    reduce(left_join, by = 'variable') %>%
    set_names(c('variable', 'healthy', 'moderate', 'severe')) %>% 
    mutate(variable = translate_var(variable, type = 'plot_lab')) %>% 
    rbind(tibble(variable = 'N participants', 
                 healthy = cohort$n_numbers$n[1], 
                 moderate = cohort$n_numbers$n[2], 
                 severe = cohort$n_numbers$n[3]), .) %>% 
    map_dfc(stri_replace, regex = '\\nComplete.*$', replacement = '') %>% 
    map_dfc(stri_replace, regex = 'no:.*\\nyes:\\s{1}', replacement = '') %>% 
    map_dfc(stri_replace_all, fixed = '% (', replacement = '% (n = ')
  
# Healthy, moderate and severe COVID: testing with Kruskal-Wallis and Chi-squared test -----
  
  insert_msg('Kruskal-Wallis test')
  
  cohort$kruskal_results <- compare_variables(cohort$analysis_tbl, 
                                              variables = c('age', 'sex', 'bmi'), 
                                              split_factor = 'sev_paper', 
                                              what = 'test', 
                                              types = c('kruskal_test', 'chisq_test', 'kruskal_test'), 
                                              ci = FALSE, 
                                              pub_styled = TRUE, 
                                              adj_method = 'BH') %>% 
    mutate(variable = translate_var(variable, type = 'plot_lab'))
  
# Comparison of the moderate and severe COVID-19 patients with Wilcoxon test ---
  
  insert_msg('Wilcoxon test')
  
  cohort$wilcoxon_results <- compare_variables(filter(cohort$analysis_tbl, 
                                                      sev_paper %in% c('moderate', 'severe')), 
                                               variables = c('age', 'sex', 'bmi', 'hospital_stay'), 
                                               split_factor = 'sev_paper', 
                                               what = 'eff_size', 
                                               types = c('wilcoxon_r', 'cramer_v', 'wilcoxon_r', 'wilcoxon_r'), 
                                               ci = FALSE, 
                                               pub_styled = TRUE, 
                                               adj_method = 'BH') %>% 
    mutate(variable = translate_var(variable, type = 'plot_lab'))
  
# Stitching the table together ------
  
  insert_msg('Complete table')
  
  cohort$result_tbl <- list(cohort$desc_stats, 
                            cohort$kruskal_results[c('variable', 'significance')] %>% 
                              set_names(c('variable', 'test_all')), 
                            cohort$wilcoxon_results[c('variable', 'significance')] %>% 
                              set_names(c('variable', 'test_covid'))) %>% 
    reduce(left_join, by = 'variable') %>% 
    map_dfc(stri_replace, fixed = 'Chi-squared test', replacement = '\u03C7\u00B2') %>% 
    map_dfc(stri_replace, fixed = 'Kruskal-Wallis test', replacement = 'Kruskal') %>% 
    map_dfc(stri_replace, fixed = ' test', replacement = '')
  
# END ----
  
  insert_tail()