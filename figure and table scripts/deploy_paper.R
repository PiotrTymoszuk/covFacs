# this script renders word files with the main manuscript and supplements

  insert_head()
  
# Exporting the figure and supplementary figure code chunks ----
  
  insert_msg('Inserting the figures and supplementary figures')
  
  insert_figure(paper_figures$consort, 
                paper_figures$cyto_screen, 
                paper_figures$subset_distribution, 
                paper_figures$clustering, 
                paper_figures$clust_inflammation, 
                file = './manuscript/figure_chunks.Rmd', 
                ref_names = stri_replace_all(names(paper_figures), fixed = '_', replacement = '-'), 
                captions = c('Analysis inclusion scheme.', 
                             'Screening of the flow cytometry features for the differences between healthy controls, moderate and severe COVID-19.', 
                             'Monocyte subset distribution in healthy controls, moderate and severe COVID-19.', 
                             'Unsupervised clustering of healthy controls and COVID-19 individuals by myeloid leukocyte cytometry features.', 
                             'Differences in systemic inflammation status between the clusters of study participants.'))
  
  insert_figure(suppl_figures$gating_strategy, 
                suppl_figures$monocyte_subsets, 
                suppl_figures$systemic_inflammation, 
                suppl_figures$backbone, 
                suppl_figures$cd14, 
                suppl_figures$cell_inflammation, 
                suppl_figures$myeloid_cd64, 
                suppl_figures$myeloid_cd86, 
                suppl_figures$cluster_qc, 
                file = './manuscript/supplement_chunks.Rmd', 
                ref_names = stri_replace_all(names(suppl_figures), fixed = '_', replacement = '-'), 
                captions = c('Gating strategy and identification of blood neutrophils and monocytes.', 
                             'Identification of blood monocyte subsets.', 
                             'Systemic inflammation and iron turnover markers in hospitalized COVID-19 subjects.', 
                             'Regulation of the monocyte subset markers HLA-DR, CCR2 and CX3CR1 in healthy controls, moderate and severe COVID-19.', 
                             'Regulation of neutrophil CD14 in healthy controls, moderate and severe COVID-19.', 
                             'Cytometry markers of myeloid leukocyte expansion in healthy controls, moderate and severe COVID-19.', 
                             'Regulation of myeloid leukocyte CD64 in healthy controls, moderate and severe COVID-19.', 
                             'Regulation of monocyte CD86 in healthy controls, moderate and severe COVID-19.', 
                             'Training and clusterin of self-organizing maps.'))

# rendering the figures and tables ------

  insert_msg('Rendering the figures and tables')
  
  render('./manuscript/main_manuscript_revision.Rmd', 
         output_format = word_document2(number_sections = FALSE, 
                                        reference_docx = 'ms_template.docx', 
                                        fig_caption = TRUE))

# rendering the supplementary material -----

  insert_msg('Rendering the supplementary material')
  
  render('./manuscript/supplementary_material_revision.Rmd', 
         output_format = word_document2(number_sections = FALSE, 
                                        reference_docx = 'ms_template.docx', 
                                        fig_caption = TRUE))

# rendering the rebuttal letter ------
  
  insert_msg('Rendering the rebuttal letter')
  
  render('./manuscript/rebuttal_letter.Rmd', 
         output_format = word_document2(number_sections = FALSE)) 
  
# END ------

  insert_tail()