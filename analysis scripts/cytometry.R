# Compares the cytometry paramater values between healthy, moderate and severe CoV.
# Kruskal-Wallis test with eta-squared as an effect size statistic (eta < 0 are considered 0, i.e. no effect)
# Post-hoc test: Wilcoxon with the r effect size statistic

  insert_head()
  
# container list ------
  
  cyto <- list()
  
# globals -----
  
  insert_msg('Globals setup')
  
  ## variables
  
  cyto$variable <- proj_globals$var_tbl %>% 
    filter(type == 'response', 
           description == 'cytomtery') %>% 
    .$variable
  
  ## variable labels
  
  cyto$var_labs <- translate_var(cyto$variable, type = 'plot_lab') %>% 
    stri_replace(fixed = ' percent', replacement = 's') %>% 
    stri_replace(fixed = 'monocyte', replacement = 'mono') %>% 
    stri_replace(fixed = 'Monocyte', replacement = 'Mono') %>% 
    stri_replace(fixed = 'Monos', replacement = 'Mono') %>% 
    stri_replace(fixed = 'monos', replacement = 'mono') %>% 
    stri_replace(fixed = 'Classical', replacement = 'Class.') %>% 
    stri_replace(fixed = 'classical', replacement = 'Class.') %>% 
    stri_replace(fixed = 'Intermediate', replacement = 'Int.') %>% 
    stri_replace(fixed = 'Mono: Lymphocyte Ratio', replacement = 'MLR') %>% 
    stri_replace(fixed = 'Neutrophil: Lymphocyte Ratio', replacement = 'NLR') %>% 
    stri_replace(fixed = 'of ', replacement = '') %>% 
    stri_replace(regex = '^\\s{1}', replacement = '') %>% 
    stri_replace(fixed = ', \u0394MFI', replacement = '') %>% 
    set_names(cyto$variable)
  
  ## pairs for comparisons
  
  cyto$pairs <- list(healthy_moderate = c('healthy', 'moderate'), 
                     healthy_severe = c('healthy', 'severe'), 
                     moderate_severe = c('moderate', 'severe'))
  
# Normality and variance homogeneity check -------
  
  insert_msg('Normality and eov')
  
  cyto$normality <- explore(data = mono_data$data_tbl, 
                            variables = cyto$variable, 
                            what = 'normality')
  
  cyto$eov <- compare_variables(mono_data$data_tbl, 
                                variables = cyto$variable, 
                                split_factor = 'sev_paper', 
                                what = 'variance', 
                                pub_styled = TRUE)
  
# Descriptive statistics ------
  
  insert_msg('Descriptive statistic')
  
  cyto$desc_stats <- explore(data = mono_data$data_tbl, 
                             split_factor = 'sev_paper', 
                             variables = cyto$variable, 
                             what = 'table', 
                             pub_styled = TRUE)
  
# Testing with Kruskal-Wallis test------
  
  insert_msg('Kruskal-Wallis test')
  
  cyto$kruskal_results <- compare_variables(mono_data$data_tbl, 
                                            variables = cyto$variable, 
                                            split_factor = 'sev_paper', 
                                            what = 'eff_size', 
                                            types = 'kruskal_etasq', 
                                            ci = FALSE, 
                                            adj_method = 'BH')
  
# Identification of the significant features in Kruskal-Wallis test ------
  
  insert_msg('Ideptifying the significant features')
  
  cyto$kruskal_signif_features <- cyto$kruskal_results %>% 
    filter(p_adjusted < 0.05) %>% 
    .$variable
  
# Presenting the Kruskal-Wallis test results in a point plot ------
  
  insert_msg('Eta-squared and p plot')
  
  cyto$kruskal_plot <- plot(cyto$kruskal_results %>% 
                              mutate(variable = cyto$var_labs[variable]), 
                            show_labels = 'signif', 
                            point_hjitter = 0, 
                            point_wjitter = 0.001, 
                            cust_theme = proj_globals$common_theme, 
                            plot_title = 'Cytometry feature regulation', 
                            plot_subtitle = 'Healthy, moderate and severe COVID-19', 
                            force = 10, 
                            force_pull = 0.2, 
                            box.padding = 0.1, 
                            point.padding = 0.1, 
                            max.overlaps  = 20, 
                            hjust = 1, 
                            seed = 2245)
  
  cyto$kruskal_plot <- cyto$kruskal_plot + 
    geom_hline(yintercept = -log10(0.05), 
               linetype = 'dashed', 
               color = 'steelblue2') + 
    expand_limits(y = 2) + 
    labs(y = expression('-log'[10]*' pFDR'), 
         tag = paste0('\n', get_tag(cyto$kruskal_plot)))
  
# Post-hoc testing -----
  
  insert_msg('Post-hoc tests')
  
  cyto$post_hoc_results <- cyto$pairs %>% 
    map(~filter(mono_data$data_tbl, sev_paper %in% .x)) %>% 
    map(~compare_variables(.x, 
                           variables = cyto$variable, 
                           split_factor = 'sev_paper', 
                           what = 'eff_size', 
                           types = 'wilcoxon_r', 
                           ci = FALSE, 
                           adj_method = 'BH', 
                           pub_styled = FALSE))

# Identification of significant features in the post-hoc analysis ------
  
  insert_msg('Significant post-hoc testing results')
  
  cyto$post_hoc_signif_features <- cyto$post_hoc_results %>% 
    map(filter, p_adjusted < 0.05) %>% 
    map(~.x$variable) %>% 
    reduce(union)
    
# Plotting the results of post-hoc testing ------
  
  insert_msg('Plotting the post-hoc testing results')
  
  cyto$post_hoc_plots <- list(x = map(cyto$post_hoc_results, 
                                      mutate, 
                                      variable = cyto$var_labs[variable]), 
                              plot_title = c('Moderate C0VID-19 vs healthy', 
                                             'Severe COVID-19 vs healthy', 
                                             'Severe vs moderate COVID-19')) %>% 
    pmap(plot, 
         show_labels = 'signif', 
         point_hjitter = 0, 
         point_wjitter = 0.001, 
         cust_theme = proj_globals$common_theme, 
         plot_subtitle = 'Wilcoxon test', 
         force = 10, 
         force_pull = 0.2, 
         box.padding = 0.1, 
         point.padding = 0.1, 
         max.overlaps  = 20, 
         hjust = 1, 
         seed = 2245)
  
  cyto$post_hoc_plots <- cyto$post_hoc_plots %>% 
    map(~.x + 
          geom_hline(yintercept = -log10(0.05), 
                     linetype = 'dashed', 
                     color = 'steelblue2') + 
          labs(y = expression('-log'[10]*' pFDR'), 
               tag = paste0('\n', get_tag(.x))))

# Plotting single parameters as box plots ------
  
  insert_msg('Bar plots for single parameters')
  
  cyto$box_plots <- list(variable = cyto$kruskal_results$variable, 
                         plot_title = translate_var(cyto$kruskal_results$variable, type = 'plot_title'), 
                         plot_subtitle = paste('Kruskal test:', cyto$kruskal_results$significance), 
                         y_lab = translate_var(cyto$kruskal_results$variable, type = 'plot_lab')) %>% 
    pmap(plot_variable, 
         mono_data$data_tbl %>% 
           filter(!is.na(sev_paper)), 
         split_factor = 'sev_paper', 
         type = 'box', 
         cust_theme = proj_globals$common_theme) %>% 
    set_names(cyto$kruskal_results$variable)
  
  cyto$box_plots <- cyto$box_plots %>% 
    map(~.x +
          expand_limits(y = 0) + 
          scale_x_discrete(labels = proj_globals$sev_paper_labels) + 
          scale_fill_manual(values = proj_globals$sev_paper_colors, 
                            labels = proj_globals$sev_paper_labels) + 
          labs(tag = paste0('\n', get_tag(.x))))
  
 ## adding the post-hoc testing results
  
  cyto$box_plots <- list(plot = cyto$box_plots, 
                         healthy_moderate = cyto$post_hoc_results$healthy_moderate$significance, 
                         moderate_severe = cyto$post_hoc_results$moderate_severe$significance, 
                         healthy_severe = cyto$post_hoc_results$healthy_severe$significance) %>% 
    pmap(add_post_hoc)
  
# END -----
  
  insert_tail()