# This script reads and clears the cytometry, sample assignment and clinics data


# toolbox -----

  library(soucer)
  library(plyr)
  library(tidyverse)
  library(readxl)
  library(stringi)

  c('globals_setup.R', 
    './tools/project_tools.R') %>% 
    source_all(message = TRUE, crash = TRUE)

  insert_head()

# data container ----

  mono_data <- list()
  
# reading the file manifest, filtering out the samples with bad quality and follow-ups ------
  
  insert_msg('Reading the file manifest/analysis log')
  
  mono_data$analysis_log <- read_csv('./cytometry/analysis_log.csv') %>% 
    mutate(data_folder = paste('./cytometry/', sample, sep = '')) %>% 
    filter(quality != 'bad', 
           !stri_detect(sample, fixed = 'fup'))

# reading the cytometry data: frequency, CD274/CD279 expression and the test antibody panel signals -----

  insert_msg('reading the cytometry data')
  
  if(!'base_cyto.csv' %in% list.files('./cytometry')) {
    
    mono_data$base_cyto <- list(paste0(mono_data$analysis_log$data_folder, '/frequency_revision.txt'), 
                                paste0(mono_data$analysis_log$data_folder, '/pd_revision.txt'), 
                                paste0(mono_data$analysis_log$data_folder, '/testAb_revision.txt'), 
                                mono_data$analysis_log$data_folder) %>% 
      map2(., list(read_freq_data, 
                   read_pd_data, 
                   read_testab_data, 
                   read_revision), 
           ~.y(.x))
    
    mono_data$base_cyto <- mono_data$base_cyto %>% 
      reduce(full_join, by = 'id')
    
    write_tsv(mono_data$base_cyto, './cytometry/base_cyto.csv')
    
  } else {
    
    mono_data$base_cyto <- read_tsv('./cytometry/base_cyto.csv')
    
  }

# reading and clearing the sample assignment and clinical data ------
  
  insert_msg('reading the sample assigment table')
  
  mono_data$clinics <- read_excel('./clinics/clinical_data.xlsx')
  
  ## re-coding the COVID-19 status and disease severity
  
  mono_data$clinics <- mono_data$clinics %>% 
    mutate(covid_status = ifelse(infection %in% c('other', 'pulmonary'), 
                                 'other infection', 
                                 ifelse(infection == 'healthy', 
                                        'healthy', 
                                        'COVID-19')), 
           covid_status = factor(covid_status, 
                                 c('healthy', 
                                   'other infection', 
                                   'COVID-19')), 
           severity = ifelse(icu == 'yes', 
                             'critical', 
                             ifelse(oxygen == 'yes', 
                                    'severe', 
                                    ifelse(infection == 'healthy', 
                                           'healthy', 
                                           'moderate'))), 
           severity = factor(severity, 
                             c('healthy', 
                               'moderate', 
                               'severe', 
                               'critical'))) %>% 
    mutate(id = stri_replace(id, fixed = 'p', replacement = 'id'), 
           patient_ID = stri_replace(patient_ID, fixed = 'p', replacement = 'id'), 
           cluster_group = interaction(covid_status, severity), 
           cluster_group = droplevels(cluster_group)) %>% 
    select( - death, 
            - `PCR Einschluss`, 
            - ICU, 
            - Sauerstoffpflichtig)
  
  ## clearing the variables
  
  mono_data$clinics <- mono_data$clinics %>% 
    mutate(bmi = as.numeric(bmi), 
           wbc = as.numeric(wbc), 
           crp = as.numeric(crp), 
           pct = as.numeric(pct), 
           il6 = as.numeric(il6), 
           neopterin = as.numeric(neopterin), 
           sex = ifelse(sex == 'w', 'female', 'male') %>% 
             factor(c('female', 'male')), 
           mortality = ifelse(mortality == 0, 'no', 'yes') %>% 
             factor(c('no', 'yes')), 
           sev_paper = car::recode(severity,
                                   "'healthy' = 'healthy'; 
                                   'moderate' = 'moderate';
                                   'severe' = 'severe'; 
                                   'critical' = 'severe'") %>% 
             factor(c('healthy', 'moderate', 'severe')))
  
# stitching the tables together by sample id -----
  
  insert_msg('merging the tables together by sample id')
  
  mono_data$data_tbl <- mono_data[c('base_cyto', 'clinics')] %>% 
    reduce(left_join, 
           by = c('id'))

# adding the cell counts (based on WBC) an monocyte and neutrophil:lymphocyte ratio -----
  
  insert_msg('Adding the cell counts')
  
  mono_data$data_tbl <- mono_data$data_tbl %>% 
    mutate(count_neutrophils = cd45_neutrophils * wbc, 
           count_classicalMono = cd45_classicalMono * wbc, 
           count_intermediateMono = cd45_intermediateMono * wbc, 
           count_nonclassicalMono = cd45_nonclassicalMono * wbc, 
           count_Mono = cd45_panMono * wbc,
           cd45_linpos = 100 - cd45_linneg, 
           mlr = cd45_panMono/cd45_linpos, 
           nlr = cd45_neutrophils/cd45_linpos)
  
# adding a severity criterion applied in the paper analyses: moderate vs ICU or mech vent ----- 

  insert_msg('Additional severity criterion: mechvent or ICU, selecting the healthy and CoV cases')
  
  mono_data$data_tbl <- mono_data$data_tbl %>% 
    filter(covid_status %in% c('healthy', 'COVID-19'))

# END ----
  
  insert_tail()