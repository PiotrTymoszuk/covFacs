# Compares expression of the selected/reviewer-requested backbone cytometry features
# between the monocyte populations and disease severity strata

  insert_head()
  
# container list -----
  
  back <- list()
  
# globals setup ------
  
  ## backbone markars
  
  back$mono_markers <- c('ccr2', 
                         'cd62l', 
                         'hladr', 
                         'cx3cr1')
  
  back$marker_labs <- c(ccr2 = 'CCR2', 
                        cd62l = 'CD62L', 
                        hladr = 'HLA-DR', 
                        cx3cr1 = 'CX3CR1')
  
  ## monocvyte population colors and labels
  
  back$mono_colors <- c(classical = 'firebrick4', 
                        intermediate = 'gray60', 
                        nonclassical = 'steelblue3')
  
  back$mono_labs <- c(classical = 'Classical', 
                      intermediate = 'Intermediate', 
                      nonclassical = 'Non-classical')
  
  ## analysis table
  
  back$analysis_tbl <- back$mono_markers %>% 
    map(~select(mono_data$data_tbl, id, sev_paper, ends_with(.x))) %>% 
    map(~filter(.x, complete.cases(.x)))
  
  back$analysis_tbl <- map2(back$analysis_tbl, 
                            back$mono_markers, 
                            ~gather(.x, 
                                    key = 'population', 
                                    value = !!.y, 
                                    ends_with(.y))) %>% 
    map(mutate, 
        population = stri_extract(population, 
                                  regex = 'classical|intermediate|nonclassical'), 
        population = factor(population, c('classical', 'intermediate', 'nonclassical'))) %>% 
    reduce(left_join, by = c('id', 'sev_paper', 'population'))
  
# Normality and EOV check -----
  
  insert_msg('Normality and EOV check')
  
  back$normality <- back$analysis_tbl %>% 
    ddply(.(sev_paper, population), 
          explore, 
          variables = back$mono_markers, 
          what = 'normality', 
          pub_styled = TRUE) %>% 
    as_tibble
  

  back$eov_population <- back$analysis_tbl %>% 
    ddply(.(sev_paper), 
          compare_variables, 
          variables = back$mono_markers, 
          split_factor = 'population', 
          what = 'variance', 
          pub_styled = TRUE) %>% 
    as_tibble
  
  back$eov_severity <- back$analysis_tbl %>% 
    ddply(.(population), 
          compare_variables, 
          variables = back$mono_markers, 
          split_factor = 'sev_paper', 
          what = 'variance', 
          pub_styled = TRUE) %>% 
    as_tibble
  
# Descriptive statistics -----
  
  insert_msg('Descriptive statistic')
  
  back$desc_stats <- back$analysis_tbl %>% 
    ddply(.(sev_paper, population), 
          explore, 
          variables = back$mono_markers, 
          what = 'table', 
          pub_styled = TRUE) %>% 
    as_tibble

# Comparison of the monocyte backbone markers between the monocyte populations with Friedman test -----
  
  insert_msg('Comparison of the monocyte backbone markers between the monocyte subsets')
  
  back$population_results <- back$analysis_tbl %>% 
    ddply(.(sev_paper), 
          compare_variables, 
          variables = back$mono_markers, 
          split_factor = 'population', 
          what = 'test', 
          types = 'friedman_test', 
          ci = FALSE) %>% 
    mutate(p_adjusted = p.adjust(p_value, 'BH'), 
           significance = ifelse(p_adjusted < 0.001, 
                                 'p < 0.001', 
                                 ifelse(p_adjusted >= 0.05, 
                                        paste0('ns (p = ', signif(p_adjusted, 2), ')'), 
                                        paste('p =', signif(p_adjusted, 2)))))
  
# Comparison of the marker expression between the disease severity strata with Kruskal-Wallis test -----
  
  insert_msg('Comparison of the backbone markers expression between the disease severity groups')
  
  back$severity_results <- back$analysis_tbl %>% 
    ddply(.(population), 
          compare_variables, 
          variables = back$mono_markers, 
          split_factor = 'sev_paper', 
          what = 'test', 
          types = 'kruskal_test', 
          ci = FALSE) %>% 
    mutate(p_adjusted = p.adjust(p_value, 'BH'), 
           significance = ifelse(p_adjusted < 0.001, 
                                 'p < 0.001', 
                                 ifelse(p_adjusted >= 0.05, 
                                        paste0('ns (p = ', signif(p_adjusted, 2), ')'), 
                                        paste('p =', signif(p_adjusted, 2)))))
  
# Paired plots -----
  
  insert_msg('Paired plots')
  
  back$paired_plots <- back$analysis_tbl %>% 
    dlply(.(sev_paper)) %>% 
    map2(., proj_globals$sev_paper_labels, 
         function(severity, sev_label) back$mono_markers %>% 
          map(~plot_variable(severity, 
                             variable = .x, 
                             split_factor = 'population', 
                             type = 'paired', 
                             plot_title = sev_label, 
                             y_lab = paste0(back$marker_labs[.x], ', MFI'), 
                             cust_theme = proj_globals$common_theme)) %>% 
           set_names(back$mono_markers))
  
  back$paired_plots <- back$paired_plots %>% 
    transpose %>% 
    map(set_cmm_scale) %>% 
    transpose %>% 
    unlist(recursive = FALSE)
  
  ## adding the Kruskal-Wallis p values to the plot sub-captions
  
  back$paired_plots <- map2(back$paired_plots, 
                            back$population_results$significance, 
                            ~.x + 
                              labs(subtitle = paste('Kruskal test:', .y), 
                                   tag = paste0('\n', stri_extract(get_tag(.x), regex = 'n = \\d{1,2}'))) + 
                              scale_x_discrete(labels = back$mono_labs) + 
                              scale_fill_manual(labels = back$mono_labs, 
                                                values = back$mono_colors))

# END ------
  
  insert_tail()