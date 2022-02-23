# This script investigates correlations between the CD64, Cd40, Cd86 and monocyte subset levels
# and CRP and IL-6 in the CoV strata

  insert_head()
  
# tools -----
  
  library(cowplot)
  
# data container -----
  
  cyto_corr <- list()
  
# globals -----
  
  insert_msg('Globals setup')
  
  ## variables of interest, significant features identified by serial
  ## testing of the cytometry features with Kruskal test

  cyto_corr$cyto_var <- cyto$kruskal_signif_features
  
  cyto_corr$labor_vars <- c('crp', 'il6', 'neopterin', 'iron', 'ferritin')
  
  cyto_corr$pairs <- cyto_corr$cyto_var %>% 
    map(function(cyto_var) cyto_corr$labor_vars %>% 
          map(~c(cyto_var, .x))) %>% 
    unlist(recursive = FALSE)
  
  cyto_corr$pairs <- cyto_corr$pairs %>% 
    map(paste, collapse = '_') %>% 
    set_names(cyto_corr$pairs, .)

# Serial correlation with Spearman method (non-normal distribution of the features) -----
  
  insert_msg('Serial correlation analysis')
  
  cyto_corr$test_results <- cyto_corr$pairs %>% 
    map_dfr(~correlate_variables(filter(mono_data$data_tbl[.x], 
                                        complete.cases(mono_data$data_tbl[.x])), 
                                 variables = .x, 
                                 what = 'correlation', 
                                 type = 'spearman', 
                                 pub_styled = TRUE)) %>% 
    mutate(corr_id = names(cyto_corr$pairs), 
           p_adjusted = p.adjust(p_value, 'BH'), 
           significance = ifelse(p_adjusted < 0.001, 
                                 'p < 0.001', 
                                 ifelse(p_adjusted >= 0.05, 
                                        paste0('ns (p = ', signif(p_adjusted, 2), ')'), 
                                        paste('p =', signif(p_adjusted, 2)))), 
           plot_cap = paste(eff_size, significance, sep = ', '), 
           plot_cap = stri_replace(plot_cap, fixed = 'rho', replacement = '\u03C1'))
  
# Plotting the correlations ----
  
  insert_msg('Plotting the correlations')
  
  cyto_corr$plots <- cyto_corr$pairs %>% 
    map(~plot_correlation(mono_data$data_tbl, 
                          variables = .x, 
                          type = 'correlation', 
                          point_alpha = 0.8, 
                          point_hjitter = 0, 
                          point_wjitter = 0, 
                          show_trend = FALSE, 
                          cust_theme = proj_globals$common_theme, 
                          x_lab = translate_var(.x[1], type = 'plot_lab'), 
                          y_lab = translate_var(.x[2], type = 'plot_lab'), 
                          plot_title = translate_var(.x[1], type = 'plot_title')))
  
  ## adding the rho and p values to the plot sub-captions
  
  cyto_corr$plots <- map2(cyto_corr$plots, 
                          cyto_corr$test_results$plot_cap, 
                          ~.x + 
                            labs(subtitle = paste(.y, get_tag(.x), sep = ', ')) + 
                            theme(plot.tag = element_blank()) + 
                            scale_x_continuous(trans = 'pseudo_log') + 
                            scale_y_continuous(trans = 'pseudo_log') + 
                            geom_smooth(se = FALSE, 
                                        method = 'lm'))

# END -----
  
  insert_tail()