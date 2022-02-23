# This script generates figures and supplementary figures for the paper

  insert_head()

# data containers -----
  
  paper_figures <- list()
  suppl_figures <- list()
  
# Figure 1: CONSORT ------
  
  insert_msg('Figure 1: CONSORT')
  
  paper_figures$consort <- ggdraw() + 
    draw_image('./aux figure files/consort.png')
  
  paper_figures$consort <- paper_figures$consort %>% 
    as_figure(label = 'figure_1_consort', 
              w = 180, 
              h = 150)
  
# Figure 2: cytometry features differentiating between healthy, moderate and severe COVID-19 -----
  
  insert_msg('Figure 2: cytometry features differentiating between healthy, moderate and severe COVID-19')
  
  paper_figures$cyto_screen <- cyto$kruskal_plot + 
    labs(y = expression('Kruskal test: -log'[10]*' pFDR'), 
         x = expression('Effect size: '*eta^2))
  
  paper_figures$cyto_screen <- paper_figures$cyto_screen %>% 
    as_figure(label = 'figure_2_cyto_screen', 
              w = 180, 
              h = 120)

# Figure 3: monocyte subset distribution in healthy and CoV strata ------
  
  insert_msg('Figure 3: Monocyte subset distribution')
  
  paper_figures$subset_distribution <- make_cyto_panel(plot_list = cyto$box_plots[c('panMono_classicalMono', 
                                                                                    'panMono_intermediateMono', 
                                                                                    'panMono_nonclassicalMono')], 
                                                       represent_path = './representative data/monocyte subsets/subsets_representative.png') %>% 
    as_figure(label = 'figure_3_monocyte_subsets', 
              w = 180, 
              h = 220)

# Figure 4: clustering of the cytometry features and study participants -----
  
  insert_msg('Figure 4: clustering of the participants')
  
  paper_figures$clustering <- plot_grid(cyto_clust$heat_map + 
                                          theme(plot.tag = element_blank(), 
                                                legend.position = 'bottom'), 
                                        plot_grid(clust_chara$severity_plot + 
                                                    theme(legend.position = 'bottom'), 
                                                  ggdraw(), 
                                                  nrow = 2, 
                                                  rel_heights = c(0.7, 0.3)), 
                                        ncol = 2, 
                                        rel_widths = c(0.6, 0.4), 
                                        labels = LETTERS, 
                                        label_size = 10) %>%
    as_figure(label = 'figure_4_participant_clustering', 
              w = 180, 
              h = 150)
  
# Figure 5: inflammatory and iron parameters in the participant clusters -------
  
  insert_msg('Figure 5: inflammatory paramaters in the participant clusters')
  
  paper_figures$clust_inflammation <- clust_chara$inflammation_plots %>% 
    map(~.x + 
          theme(plot.tag = element_blank(), 
                legend.position = 'none', 
                axis.title.x = element_blank(), 
                axis.text.x = element_blank(), 
                plot.margin = ggplot2::margin(2, 5, 2, 5, unit = 'mm')))
  
  paper_figures$clust_inflammation$neopterin <- paper_figures$clust_inflammation$neopterin + 
    scale_y_continuous(trans = 'log10')
  
  paper_figures$clust_inflammation <- paper_figures$clust_inflammation %>% 
    plot_grid(plotlist = ., 
              align = 'v', 
              ncol = 2) %>% 
    plot_grid(get_legend(clust_chara$inflammation_plots[[1]] + 
                           theme(legend.position = 'bottom') + 
                           labs(fill = 'Participant cluster')), 
              nrow = 2, 
              rel_heights = c(0.97, 0.03)) %>% 
    plot_grid(ggdraw() + 
                 draw_text(get_tag(clust_chara$inflammation_plots[[1]]) %>% 
                             stri_replace_all(fixed = '\n', replacement = ', '), 
                           size = 8, 
                           hjust = 0.5), 
              nrow = 2, 
              rel_heights = c(0.97, 0.03)) %>% 
    as_figure(label = 'figure_5_cluster_inflammation', 
              w = 180, 
              h = 220)

# Figure S1: gating strategy UMAP -------
  
  insert_msg('Figure S1: gating strategy')
  
  suppl_figures$gating_strategy <- ggdraw() + 
    draw_image('./aux figure files/gating_strategy.svg.png')
  
  suppl_figures$gating_strategy <- suppl_figures$gating_strategy %>% 
    as_figure(label = 'figure_s1_gating_strategy', 
              w = 180, 
              h = 186)
  
# Figure S2: gating strategy - identification of the monocyte subsets -----
  
  insert_msg('Figure S2: identification of the monocyte subsets')
  
  suppl_figures$monocyte_subsets <- ggdraw() + 
    draw_image('./aux figure files/gating_strategy_subsets.svg.png')
  
  suppl_figures$monocyte_subsets <- suppl_figures$monocyte_subsets %>% 
    as_figure(label = 'figure_s2_monocyte_subsets', 
              w = 180, 
              h = 103)
  
# Figure S3: sustained systemic inflammation in hospitalized COVID-19 subjects -----
  
  insert_msg('Figure S3: sustained inflammaiton in hospitalized COVID-19 patients')
  
  suppl_figures$systemic_inflammation <- inflammation$single_plots[c('il6', 
                                                                     'crp',
                                                                     'neopterin', 
                                                                     'ferritin', 
                                                                     'iron', 
                                                                     'tf_sat')] %>% 
    map(~.x + 
          theme(axis.text.x = element_blank(), 
                legend.position = 'none', 
                plot.tag = element_blank()))
  
  suppl_figures$systemic_inflammation$neopterin <- suppl_figures$systemic_inflammation$neopterin + 
    scale_y_continuous(trans = 'log10')
  
  suppl_figures$systemic_inflammation <- suppl_figures$systemic_inflammation %>% 
    plot_grid(plotlist = ., 
              ncol = 3, 
              align = 'hv', 
              labels = c('A', '', '', 'B'), 
              label_size = 10) %>% 
    plot_grid(., 
              plot_grid(ggdraw(), 
                        ggdraw(),
                        get_legend(inflammation$single_plots$crp), 
                        ggdraw() + 
                          draw_text(get_tag(inflammation$single_plots$crp), 
                                    x = 0.15, 
                                    size = 8, 
                                    hjust = 0), 
                        nrow = 6), 
              ncol = 2, 
              rel_widths = c(0.9, 0.2)) %>% 
    as_figure(label = 'figure_s3_systemic_inflammation', 
              w = 180, 
              h = 140)
  
# Figure S4: backbone markers in the disease severity groups -----
  
  insert_msg('Figure S4: backbone markers in the severity groups')
  
  suppl_figures$backbone <- back$paired_plots[c('healthy.hladr', 
                                                'moderate.hladr', 
                                                'severe.hladr', 
                                                'healthy.ccr2', 
                                                'moderate.ccr2', 
                                                'severe.ccr2', 
                                                'healthy.cx3cr1', 
                                                'moderate.cx3cr1', 
                                                'severe.cx3cr1')] %>% 
    map(~.x + 
          theme(legend.position = 'none', 
                plot.tag = element_blank(), 
                axis.text.x = element_blank())) %>% 
    plot_grid(plotlist = ., 
              ncol = 3, 
              align = 'hv', 
              labels = c('A', '', '', 'B', '', '', 'C'), 
              label_size = 10) %>% 
    plot_grid(plot_grid(ggdraw(), 
                        get_legend(back$paired_plots[[1]]), 
                        ggdraw() + 
                          draw_text(get_tag(cd14$box_plot), 
                                    size = 8, 
                                    hjust = 0, 
                                    x = 0.1), 
                        ggdraw(), 
                        nrow = 4), 
              ncol = 2, 
              rel_widths = c(0.85, 0.15)) %>% 
    as_figure(label = 'figure_s4_backbone_monocyte_markers', 
              w = 180, 
              h = 180)
  
# Figure S5: Neutrophil CD14 -----
  
  insert_msg('Figure S5: neutrophil CD14')
  
  suppl_figures$cd14 <- plot_grid(cd14$box_plot + 
                                    theme(plot.tag = element_blank(), 
                                          legend.position = 'none'),
                                  plot_grid(get_legend(cd14$box_plot), 
                                            ggdraw() + 
                                              draw_text(get_tag(cd14$box_plot), 
                                                        size = 8, 
                                                        hjust = 0, 
                                                        x = 0.1)), 
                                  ncol = 2, 
                                  rel_widths = c(0.6, 0.4)) %>% 
    as_figure(label = 'figure_s5_neutrophil_cd14', 
              w = 180, 
              h = 90)
  
# Figure S6: cellular readouts of inflammation -----
  
  insert_msg('Figure S6: cellular readouts of inflammation')
  
  suppl_figures$cell_inflammation <- cyto$box_plots[c('cd45_linneg', 
                                                      'cd45_neutrophils', 
                                                      'nlr')]
  
  suppl_figures$cell_inflammation$nlr <- suppl_figures$cell_inflammation$nlr + 
    scale_y_continuous(trans = 'log10')
    
  suppl_figures$cell_inflammation  <- make_cyto_panel(plot_list = suppl_figures$cell_inflammation , 
                                                      represent_path = './representative data/neutro percentages/lineage_representative.png', 
                                                      valign = 0.71) %>% 
    as_figure(label = 'figure_s6_cell_inflammation', 
              w = 180, 
              h = 220)
  
# Figure S7: Myeloid CD64 -----
  
  insert_msg('Figure S7: myeloid CD64')
  
  suppl_figures$myeloid_cd64 <- make_cyto_panel(plot_list = cyto$box_plots[c('neutrophils_CD64', 
                                                                             'classicalMono_CD64', 
                                                                             'intermediateMono_CD64')], 
                                                represent_path = './representative data/cd64 expression/CD64_expression.png', 
                                                valign = 0.55) %>% 
    as_figure(label = 'figure_s7_myeloid_cd64', 
              w = 180, 
              h = 220)
  
# Figure S8: Monocyte CD86 -----
  
  insert_msg('Figure S8: monocyte CD86')
  
  suppl_figures$myeloid_cd86 <- make_cyto_panel(plot_list = cyto$box_plots[c('classicalMono_CD86', 
                                                                             'intermediateMono_CD86')], 
                                                represent_path = './representative data/cd86 expression/CD86_expression.png', 
                                                valign = 0.47) %>% 
    as_figure(label = 'figure_s8_myeloid_cd86', 
              w = 180, 
              h = 145)
  
# Figure S9: Clustering QC ------ 
  
  insert_msg('Figure S9: clustering QC')
  
  suppl_figures$cluster_qc <- plot_grid(plot(cyto_clust$ft_object, 
                                             type = 'training', 
                                             cust_theme = proj_globals$common_theme)$observation + 
                                          labs(title = 'Cytometry features: SOM training') + 
                                          theme(plot.subtitle = element_blank(), 
                                                plot.tag = element_blank()), 
                                        plot(cyto_clust$part_object, 
                                             type = 'training', 
                                             cust_theme = proj_globals$common_theme)$observation + 
                                          labs(title = 'Cytometry features: SOM training') + 
                                          theme(plot.subtitle = element_blank(), 
                                                plot.tag = element_blank()), 
                                        cyto_clust$ft_diagnostic$diagn_plots$node$wss + 
                                          labs(title = 'Cytometry features: SOM node clustering') + 
                                          theme(plot.tag = element_blank(), 
                                                plot.subtitle = element_blank()), 
                                        cyto_clust$part_diagnostic$diagn_plots$node$wss + 
                                          labs(title = 'Participants: SOM node clustering') + 
                                          theme(plot.tag = element_blank(), 
                                                plot.subtitle = element_blank()), 
                                        cyto_clust$ft_diagnostic$diagn_plots$node$dendrogram + 
                                          labs(title = 'Cytometry features: SOM node clustering') + 
                                          theme(plot.tag = element_blank()) + 
                                          expand_limits(y = -0.3), 
                                        cyto_clust$part_diagnostic$diagn_plots$node$dendrogram + 
                                          labs(title = 'Participants: SOM node clustering') + 
                                          theme(plot.tag = element_blank()) + 
                                          expand_limits(y = -0.7), 
                                        ncol = 2, 
                                        align = 'hv', 
                                        labels = c('A', '', 'B', '', 'C'), 
                                        label_size = 10) %>% 
    as_figure(label = 'figure_s9_clustering_qc', 
              w = 180, 
              h = 220)
  
# saving the figures and supplements -----
  
  insert_msg('Saving the figures')
  
  paper_figures %>% 
    walk(save_figure, 
         path = './manuscript/figures', 
         device = cairo_pdf)
  
  suppl_figures %>% 
    walk(save_figure, 
         path = './manuscript/supplementary figures', 
         device = cairo_pdf)
  
# END -----
  
  insert_tail()