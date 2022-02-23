# This script provides globals for plotting and analysis

  insert_head()
  
# toolbox -----
  
  library(tidyverse)
  library(readxl)

# data container -----
  
  proj_globals <- list()
  
# general graphics theme -----
  
  insert_msg('Theme setup')
  
  proj_globals$common_text <- element_text(size = 8, 
                                           face = 'plain', 
                                           color = 'black')
  
  proj_globals$common_margin <- ggplot2::margin(t = 5, 
                                                l = 5, 
                                                r = 5, 
                                                b = 5, 
                                                unit = 'mm')
  
  proj_globals$common_theme <- theme_classic() + theme(axis.text = proj_globals$common_text, 
                                                       axis.title = proj_globals$common_text, 
                                                       plot.title = element_text(size = 9, 
                                                                                 face = 'bold'), 
                                                       plot.subtitle = proj_globals$common_text, 
                                                       plot.tag = element_text(size = 8, 
                                                                               face = 'plain', 
                                                                               color = 'black', 
                                                                               hjust = 0, 
                                                                               vjust = 1), 
                                                       plot.tag.position = 'bottom', 
                                                       legend.text = proj_globals$common_text, 
                                                       legend.title = proj_globals$common_text, 
                                                       strip.text = proj_globals$common_text,
                                                       plot.margin = proj_globals$common_margin, 
                                                       panel.grid.major = element_line(color = 'gray90'))
  
# severity and COVID-19 colors ------
  
  insert_msg('Color setup')
  
  proj_globals$severity_colors <- c(healthy = 'cornflowerblue', 
                                    moderate = 'cornsilk', 
                                    severe = 'coral2', 
                                    critical = 'firebrick4')
  
  proj_globals$severity_labels <- c(healthy = 'Healthy', 
                                    moderate = 'Moderate', 
                                    severe = 'Severe', 
                                    critical = 'Critical')
  
  proj_globals$covid_labels <- c('healthy' = 'Healthy', 
                                 'other infection' = 'Non-COVID-19', 
                                 'COVID-19' = 'COVID-19')
  
  ## paper severity criterion: labels and colors
  
  proj_globals$sev_paper_colors <- c(healthy = 'cornflowerblue', 
                                    moderate = 'cornsilk3', 
                                    severe = 'coral3')
  
  proj_globals$sev_paper_labels <- c(healthy = 'Healthy', 
                                     moderate = 'Moderate', 
                                     severe = 'Severe')
  
# variable lexicon -----
  
  insert_msg('reading the variable lexicon')
  
  proj_globals$transf_lst <- list('identity' = function(x) x, 
                                  'log10' = log10, 
                                  'log10(x+1)' = function(x) log10(x + 1))
  
  proj_globals$var_tbl <- read_excel('./clinics/variable_legend.xlsx') %>% 
    mutate(unit = stri_replace(unit, fixed = 'dMFI', replacement = '\u0394MFI') %>% 
             stri_replace(fixed = 'm2', replacement = 'm\u00B2') %>% 
             stri_replace(fixed = '^3', replacement = '\u00B3')) %>% 
    mutate(prefix = ifelse(transf_fun == 'identity', 
                           '', 
                           ifelse(transf_fun %in% c('log10', 'log10(x+1)'), 
                                  'log10', 
                                  NA)), 
      plot_lab = ifelse(!is.na(unit), 
                             paste(label, unit, sep = ', '), 
                             label),
      plot_lab = paste(prefix, plot_lab), 
           plot_title = label, 
           transf_fun = proj_globals$transf_lst[transf_fun])

# END ----
  
  insert_tail()