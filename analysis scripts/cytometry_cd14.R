# Checks differences in CD14 expression between the severity groups

  insert_head()
  
# container list -----
  
  cd14 <- list()
  
# globals -----
  
  insert_msg('Globals setup')
  
  cd14$pairs <- list(healthy_moderate = c('healthy', 'moderate'), 
                     healthy_severe = c('healthy', 'severe'), 
                     moderate_severe = c('moderate', 'severe'))
  
# normality and EOV check -----
  
  insert_msg('Normality and EOV check')
  
  cd14$normality <- explore(mono_data$data_tbl, 
                            variables = 'neutrophils_cd14', 
                            what = 'normality', 
                            pub_styled = TRUE)
  
  cd14$eov <- compare_variables(mono_data$data_tbl, 
                                variables = 'neutrophils_cd14', 
                                split_factor = 'sev_paper', 
                                what = 'variance', 
                                pub_styled = TRUE)
  
# descriptive statistic -----
  
  insert_msg('Descriptive statistic')
  
  cd14$desc_stats <- explore(mono_data$data_tbl, 
                             variables = 'neutrophils_cd14', 
                             split_factor = 'sev_paper', 
                             what = 'table', 
                             pub_styled = TRUE)
  
# Kruskal-Wallis test and post-hoc testing with Wilcoxon -----
  
  insert_msg('Kruskal-Wallis with Wilcoxon post-hoc test')
  
  cd14$kruskal_results <- compare_variables(mono_data$data_tbl, 
                                            variables = 'neutrophils_cd14', 
                                            split_factor = 'sev_paper', 
                                            what = 'test', 
                                            types = 'kruskal_test', 
                                            ci = FALSE, 
                                            pub_styled = FALSE)
  
  cd14$post_hoc_results <- cd14$pairs %>% 
    map(~filter(mono_data$data_tbl, sev_paper %in% .x)) %>% 
    map(~compare_variables(.x, 
                           variables = 'neutrophils_cd14', 
                           split_factor = 'sev_paper', 
                           what = 'eff_size', 
                           types = 'wilcoxon_r', 
                           ci = FALSE, 
                           pub_styled = FALSE))
  
# Plotting: box plot ------
  
  insert_msg('Box plot with the results')
  
  cd14$box_plot <- plot_variable(mono_data$data_tbl, 
                                 variable = 'neutrophils_cd14', 
                                 split_factor = 'sev_paper', 
                                 type = 'box',
                                 point_alpha = 0.8, 
                                 plot_title = 'Neutrophil CD14', 
                                 plot_subtitle = paste('Kruskal test:', cd14$kruskal_results$significance), 
                                 y_lab = 'Neutrophil CD14, MFI', 
                                 cust_theme = proj_globals$common_theme)
  
  cd14$box_plot <- cd14$box_plot + 
    scale_fill_manual(values = proj_globals$sev_paper_colors, 
                      labels = proj_globals$sev_paper_labels) + 
    scale_x_discrete(labels = proj_globals$sev_paper_labels) +
    labs(tag = paste0('\n', get_tag(cd14$box_plot)))
  
  ## post-hoc testing results
  
  cd14$box_plot <- add_post_hoc(cd14$box_plot, 
                                healthy_moderate = cd14$post_hoc_results$healthy_moderate$significance, 
                                moderate_severe = cd14$post_hoc_results$moderate_severe$significance, 
                                healthy_severe = cd14$post_hoc_results$healthy_severe$significance)
  
# END -----
  
  insert_tail()