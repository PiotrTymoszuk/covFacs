# This script prepares three supplementary tables for the manuscript:
# cohort characteristic, cytometry staining antibodies, cytometry variables analyzed in the study
# and with all data records of the study for data distribution

  insert_head()
  
# data container -----
  
  suppl_tables <- list()
  
# cohort characteristic ------
  
  insert_msg('Table S1: Cohort characteristic')
  
  suppl_tables$cohort_chara <- cohort$result_tbl %>% 
    set_names(c('Variable', 
                'Healthy', 
                'Moderate COVID-19', 
                'Severe COVID-19', 
                'Comparison: all groups', 
                'Comparison: COVID-19'))
  
# cytometry antibodies -----
  
  insert_msg('Table S2: Cytometry antibodies')
  
  suppl_tables$cytom_antibodies <- read_excel('./clinics/antibodies.xlsx') %>% 
    set_names(c('Staining type', 
                'Antigen', 
                'Clone', 
                'Fluorophore'))
  
# cytometry variables analyzed in the study -----
  
  insert_msg('Table S3: cytometry variables')
  
  suppl_tables$cytometry_vars <- proj_globals$var_tbl %>% 
    filter(type != 'ignore', 
           description == 'cytomtery') %>% 
    .$variable
  
  suppl_tables$cytometry_vars <- tibble(Variable = translate_var(suppl_tables$cytometry_vars, type = 'label'), 
                                        Unit = translate_var(suppl_tables$cytometry_vars, type = 'unit'))

# characteristic of the participant clusters ------
  
  insert_msg('Clinical characteristic of the participant clusters')
  
  suppl_tables$cluster_chara <- clust_chara$result_tbl %>% 
    set_names(c('Variable', 
                'Cluster #1', 
                'Cluster #2', 
                'Cluster #3', 
                'Cluster #4', 
                'Comparison: all groups', 
                'Comaprison: Cluster #1'))
  
# study data -----
  
  insert_msg('Table S4: study data set')
  
  suppl_tables$study_variables <- c(cohort$variable, 
                                    inflammation$variable, 
                                    cyto$variable) %>% 
    unique
  
  suppl_tables$study_dataset <- mono_data$data_tbl %>% 
    filter(covid_status %in% c('healthy', 'COVID-19')) %>% 
    select(id, 
           sev_paper, 
           all_of(suppl_tables$study_variables), 
           ends_with(back$mono_markers[1]), 
           ends_with(back$mono_markers[2]), 
           ends_with(back$mono_markers[3]), 
           ends_with(back$mono_markers[4]), 
           ends_with('cd14'))
  
# saving on the disc -----
  
  insert_msg('saving the tables on the disc')
  
  suppl_tables[c('cohort_chara', 
                 'cytom_antibodies', 
                 'cytometry_vars', 
                 'cluster_chara', 
                 'study_dataset')] %>% 
    set_names(c('table_s1_cohort_characteristic', 
                'table_s2_cytometry_antibodies', 
                'table_s3_cytometry_variables', 
                'table_s4_cluster_characteristic', 
                'table_s5_study_dataset')) %>% 
    write_xlsx('./manuscript/supplementary tables/suppl_tables.xlsx')
    
# END -----
  
  insert_tail()