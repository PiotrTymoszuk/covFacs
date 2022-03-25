# Compares demographic, inflammatory and iron paramaters in the clusters ----

  insert_head()

# container list -----

  clust_chara <- list()
  
# globals -----
  
  clust_chara$infl_variable <- c('il6', 'crp', 'neopterin', 
                                 'ferritin', 'iron', 'tf_sat')
  
  clust_chara$variable <- c('age', 'sex', 'bmi', 'hospital_stay', 'oxygen', 'icu',
                            clust_chara$infl_variable)
  
  clust_chara$types <- c('kruskal_test', 'chisq_test', 'kruskal_test', 'kruskal_test', 'chisq_test', 'chisq_test', 
                         rep('kruskal_test', 6))
  
  clust_chara$post_hoc_types <- c('wilcoxon_r', 'cramer_v', 'wilcoxon_r', 'wilcoxon_r', 'cramer_v', 'cramer_v', 
                                  rep('wilcoxon_r', 6))
  
  ## analysis table
  
  clust_chara$analysis_tbl <- extract(cyto_clust$part_object, 'assignment') %>% 
    mutate(id = observation) %>% 
    left_join(mono_data$data_tbl[c('id', clust_chara$variable, 'sev_paper')], by = 'id')
  
  ## n numbers
  
  clust_chara$n_numbers <- clust_chara$analysis_tbl %>% 
    filter(sev_paper %in% c('moderate', 'severe')) %>% 
    count(clust_id)
  
  
  ## participant cluster colors
  
  clust_chara$clust_colors <- c('#1' = 'darkolivegreen3', 
                                '#2' = 'cornflowerblue', 
                                '#3' = 'coral3', 
                                '#4' = 'gray60')
  
# Normality and EOV of the numeric features ----
  
  insert_msg('Normality and EOV of the numeric variables')
  
  clust_chara$normality <- explore(filter(clust_chara$analysis_tbl, 
                                          sev_paper %in% c('moderate', 'severe')), 
                                   variables = c('age', 'bmi', 'hospital_stay', 
                                                 'il6', 'crp', 'neopterin', 
                                                 'ferritin', 'iron', 'tf_sat'), 
                                   what = 'normality', 
                                   pub_styled = TRUE)
  
  clust_chara$eov <- compare_variables(filter(clust_chara$analysis_tbl, 
                                              sev_paper %in% c('moderate', 'severe')), 
                                       variables = c('age', 'bmi', 'hospital_stay', 
                                                     'il6', 'crp', 'neopterin', 
                                                     'ferritin', 'iron', 'tf_sat'), 
                                       split_factor = 'clust_id', 
                                       what = 'variance', 
                                       pub_styled = TRUE)
  
# Descriptive stats -----
  
  insert_msg('Descriptive stats')
  
  clust_chara$desc_stats <- explore(filter(clust_chara$analysis_tbl, 
                                           sev_paper %in% c('moderate', 'severe')) %>% 
                                      mutate(sev_paper = droplevels(sev_paper)), 
                                    variables = clust_chara$variable, 
                                    split_factor = 'clust_id', 
                                    what = 'table', 
                                    pub_styled = TRUE) %>% 
    reduce(left_join, by = 'variable') %>% 
    set_names(c('variable', 'clust_1', 'clust_2', 'clust_3', 'clust_4')) %>% 
    rbind(tibble(variable = 'N participants', 
                 clust_1 = clust_chara$n_numbers$n[1], 
                 clust_2 = clust_chara$n_numbers$n[2], 
                 clust_3 = clust_chara$n_numbers$n[3], 
                 clust_4 = clust_chara$n_numbers$n[4]), .) %>% 
    map_dfc(stri_replace, regex = '\\nComplete.*$', replacement = '') %>% 
    map_dfc(stri_replace, regex = 'no:.*\\nyes:\\s{1}', replacement = '') %>% 
    map_dfc(stri_replace, regex = '^no: 100%.*', replacement = '0% (0)') %>% 
    map_dfc(stri_replace_all, fixed = '% (', replacement = '% (n = ')
  
# Kruskal-Wallis and Chi-squared tests ------
  
  insert_msg('Kruskal-Wallis and Chi-squared test')
  
  clust_chara$main_results <- compare_variables(filter(clust_chara$analysis_tbl, 
                                                       sev_paper %in% c('moderate', 'severe')), 
                                                variables = clust_chara$variable, 
                                                what = 'test', 
                                                split_factor = 'clust_id', 
                                                types = clust_chara$types, 
                                                ci = FALSE, 
                                                adj_method = 'BH')

# Post-hoc testing: Cluster 1 vs rest ----
  
  insert_msg('Post-hoc test')
  
  clust_chara$post_hoc_results <- list(clust_2 = c('#1', '#2'), 
                                       clust_3 = c('#1', '#3'), 
                                       clust_4 = c('#1', '#4')) %>% 
    map(~filter(clust_chara$analysis_tbl, 
                clust_id %in% .x, 
                sev_paper %in% c('moderate', 'severe'))) %>% 
    map(~compare_variables(.x, 
                           variables = clust_chara$variable, 
                           split_factor = 'clust_id', 
                           what = 'eff_size', 
                           types = clust_chara$post_hoc_types, 
                           ci = FALSE, 
                           adj_method = 'BH'))

# Common cluster characteristic table ------
  
  insert_msg('Cluster characteristic table')
  
  clust_chara$result_tbl <- clust_chara$post_hoc_results %>% 
    map(~.x[c('variable', 'significance')]) %>% 
    reduce(left_join, by = 'variable') %>% 
    set_names(c('variable', 'signif_2', 'signif_3', 'signif_4')) %>% 
    mutate(significance_post = paste0('#2: ', signif_2, 
                                      '\n#3: ', signif_3, 
                                      '\n#4: ', signif_4))

  clust_chara$result_tbl <- list(clust_chara$desc_stats, 
                                 clust_chara$main_results[c('variable', 'significance')], 
                                 clust_chara$result_tbl[c('variable', 'significance_post')]) %>% 
    reduce(left_join, by = 'variable') %>% 
    mutate(variable = translate_var(variable, type = 'plot_lab'), 
           variable = ifelse(is.na(variable), 'N COVID-19 patients', variable))
  
# Summary plots: inflammatory and iron parameters ------
  
  insert_msg('Inflammatory and iron paramaters')

  clust_chara$inflammation_plots <- list(variable = clust_chara$infl_variable, 
                                         plot_title = translate_var(clust_chara$infl_variable, type = 'plot_title'), 
                                         y_lab = translate_var(clust_chara$infl_variable, type = 'plot_lab'), 
                                         plot_subtitle = filter(clust_chara$main_results, 
                                                                variable %in% clust_chara$infl_variable)[['significance']] %>% 
                                           paste('Kruskal test:', .)) %>% 
    pmap(plot_variable, 
         clust_chara$analysis_tbl, 
         split_factor = 'clust_id', 
         type = 'box', 
         cust_theme = proj_globals$common_theme, 
         x_lab = 'Participant cluster') %>% 
    map(~.x + scale_fill_manual(values = clust_chara$clust_colors)) %>% 
    set_names(clust_chara$infl_variable)
    
    ## appending the plots with post-hoc results
    
    clust_chara$inflammation_plots <- list(plot = clust_chara$inflammation_plots, 
                                           clust_2 = filter(clust_chara$post_hoc_results$clust_2, 
                                                            variable %in% clust_chara$infl_variable)[['significance']], 
                                           clust_3 = filter(clust_chara$post_hoc_results$clust_3, 
                                                            variable %in% clust_chara$infl_variable)[['significance']], 
                                           clust_4 = filter(clust_chara$post_hoc_results$clust_4, 
                                                            variable %in% clust_chara$infl_variable)[['significance']]) %>% 
      pmap(add_post_hoc_clust)
    
# Disease severity in the clusters ------
  
  insert_msg('Participant clusters and disease severity')
  
  ## testing
  
  clust_chara$severity_testing <- compare_variables(clust_chara$analysis_tbl, 
                                                    variables = 'sev_paper', 
                                                    split_factor = 'clust_id', 
                                                    what = 'test', 
                                                    types = 'chisq_test')

  ## plotting table
  
  clust_chara$severity_plotting_tbl <- explore(data = clust_chara$analysis_tbl, 
                                              split_factor = 'clust_id', 
                                              variables = 'sev_paper') %>% 
    map(~.x$sev_paper$statistic) %>% 
    map(arrange, desc(category)) %>% 
    map2_dfr(., names(.), ~mutate(.x, clust_id = .y, txt_y = cumsum(percent) - percent * 0.5)) %>% 
    mutate(plot_lab = paste0(signif(percent, 2), '%'), 
           plot_lab = ifelse(txt_y == 100, NA, plot_lab))
  
  ## bar plot
  
  clust_chara$severity_plot <- clust_chara$severity_plotting_tbl %>% 
    ggplot(aes(x = clust_id, 
               y = percent, 
               fill = category)) + 
    geom_bar(stat = 'identity', 
             position = 'stack', 
             color = 'black') + 
    geom_text(aes(label = plot_lab, 
                  y = txt_y), 
              size = 2.75) +
    scale_fill_manual(values = proj_globals$sev_paper_colors, 
                      labels = proj_globals$sev_paper_labels, 
                      name = '') + 
    proj_globals$common_theme + 
    labs(title = 'COVID-19 severity', 
         subtitle = paste('\u03C7\u00B2 test:', clust_chara$severity_testing$significance), 
         tag = map2_chr(clust_chara$n_numbers$clust_id, 
                        clust_chara$n_numbers$n, 
                        paste, 
                        sep = ', n = ') %>% 
           paste(collapse = ', ') %>% 
           paste0('\n', .), 
         x = 'Participant cluster', 
         y = '% of the participant cluster')
  
# END -----
  
  insert_tail()