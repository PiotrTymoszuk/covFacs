# Clusters participant in respect to the cytometry features

  insert_head()
  
# container list -----
  
  cyto_clust <- list()
  
# globals: variables, normalized analysis table -----
  
  insert_msg('Globals setup')

  ## cytometry features
  
  cyto_clust$cyto_variables <- proj_globals$var_tbl %>% 
    filter(type == 'response', 
           description == 'cytomtery') %>% 
    .$variable
  
  cyto_clust$var_labs <- translate_var(cyto_clust$cyto_variables, type = 'plot_lab') %>% 
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
    set_names(cyto_clust$cyto_variables)
  
  ## clustering table
  
  cyto_clust$analysis_tbl <- mono_data$data_tbl %>% 
    select(id, all_of(cyto_clust$cyto_variables)) %>% 
    filter(complete.cases(.)) %>% 
    map_dfc(function(x) if(is.numeric(x)) scale(x)[, 1] else x) %>% 
    column_to_rownames('id')

# clustering of the cytometry features ------
  
  insert_msg('Clustering of the cytometry features')
  
  cyto_clust$ft_object <- combi_cluster(data = t(cyto_clust$analysis_tbl), 
                                        distance_som = 'cosine', 
                                        xdim = 4, 
                                        ydim = 5 , 
                                        topo = 'hexagonal', 
                                        neighbourhood.fct = 'gaussian', 
                                        toroidal = TRUE, 
                                        rlen = 2000,
                                        node_clust_fun = hcluster, 
                                        distance_nodes = 'cosine', 
                                        k = 3, 
                                        seed = 1234)
  
  ## variance and cross-validation error
  
  cyto_clust$ft_diagnostic <- list(diagn_plots = function(x) plot(x, cust_theme = proj_globals$common_theme), 
                                   variance = var, 
                                   cv = function(x) cv(x, nfolds = 10, nearest_n = 5)) %>% 
    map(~.x(cyto_clust$ft_object))
  
  ## renaming of the clusters: reader-friendly names
  
  cyto_clust$ft_object$clust_assignment <- extract(cyto_clust$ft_object, 'assignment') %>%
    mutate(clust_id = car::recode(clust_id, "'2' = 'Neutro'; '1' = 'CD40/64/71'; '3' = 'CD86/163'"), 
           clust_id = factor(clust_id))
  
  ## PCA plot
  
  cyto_clust$ft_pca <- plot(cyto_clust$ft_object, 
                            type = 'components', 
                            with = 'data', 
                            cust_theme = proj_globals$common_theme)
  
# clustering of the participants -----
  
  insert_msg('SOM of the participants')
  
  cyto_clust$part_object <- combi_cluster(data = cyto_clust$analysis_tbl, 
                                          distance_som = 'cosine', 
                                          xdim = 5, 
                                          ydim = 5, 
                                          topo = 'hexagonal', 
                                          neighbourhood.fct = 'gaussian', 
                                          toroidal = TRUE, 
                                          rlen = 3000,
                                          node_clust_fun = hcluster, 
                                          distance_nodes = 'cosine', 
                                          k = 4, 
                                          seed = 1234)
  
  ## variance and cross-validation error
  
  cyto_clust$part_diagnostic <- list(diagn_plots = function(x) plot(x, cust_theme = proj_globals$common_theme), 
                                   variance = var, 
                                   cv = function(x) cv(x, nfolds = 10, nearest_n = 5)) %>% 
    map(~.x(cyto_clust$part_object))
  
  ## renaming of the clusters
  
  cyto_clust$part_object$clust_assignment <- extract(cyto_clust$part_object, 'assignment') %>%
    mutate(clust_id = car::recode(clust_id, "'2' = '#1'; '1' = '#2'; '3' = '#3'; '4' = '#4'"), 
           clust_id = factor(clust_id))
  
  ## PCA plot
  
  cyto_clust$part_pca <- plot(cyto_clust$part_object, 
                              type = 'components', 
                              with = 'data', 
                              cust_theme = proj_globals$common_theme)
  
# Plotting the clustering of participants and cytometry features ------
  
  insert_msg('Plotting of the clustering results as a heat map')
  
  cyto_clust$heat_map <- plot_clust_hm(sample_clust_object = cyto_clust$part_object, 
                                       feature_clust_object = cyto_clust$ft_object, 
                                       cust_theme = proj_globals$common_theme, 
                                       plot_title = 'Participant and cytometry feature clustering', 
                                       plot_subtitle = 'Self-organizing map/HCl', 
                                       x_lab = 'Participant') + 
    scale_fill_gradient2(low = 'steelblue2', 
                         mid = 'black', 
                         high = 'firebrick2', 
                         name = 'Z-score') + 
    scale_y_discrete(labels = cyto_clust$var_labs) + 
    theme(plot.title.position = 'plot')
  
# END -----
  
  insert_tail()