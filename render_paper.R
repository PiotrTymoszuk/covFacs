# This is a mother script for rendering the paper figures, supplementary figures and tables ------

# tools -----

  insert_head()

  library(figur)
  library(cowplot)
  library(knitr)
  library(rmarkdown)
  library(bookdown)
  library(flextable)
  library(soucer)
  library(writexl)

  c('./tools/project_tools.R', 
    'globals_setup.R') %>% 
    source_all(message = TRUE, crash = TRUE)

# executable scripts ------

  insert_msg('Executing paper table, figure and publishing scripts')
  
  c('./figure and table scripts/paper_figures.R', ## figures and supplementary figures
    './figure and table scripts/cohort_characteristic.R', ## clinical and demographic characteristic of the cohort
    './figure and table scripts/paper_tables.R', ## supplement tables
    './figure and table scripts/deploy_paper.R') %>% ## renders the manuscript and the supplementary material
    source_all(message = TRUE, crash = TRUE)

# END ----

insert_tail()