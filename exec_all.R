# This program exectutes the KIAA project scripts

# libraries -----

  library(soucer)

# executing the scripts ----

  exec_log <- source_all(c('data_import.R', 
                           'paper_analyses.R', 
                           'render_paper.R'), 
                         message = TRUE, 
                         crash = FALSE)
  
  print(exec_log)
  
# END -----