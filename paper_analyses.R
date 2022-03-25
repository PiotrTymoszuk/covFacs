# This is a mother script executing the paper analyses ------

# tools -----

  insert_head()

  library(exda)
  library(somKernels)
  library(clustTools)

  c('globals_setup.R', 
    './tools/project_tools.R') %>% 
    source_all(message = TRUE, crash = TRUE)

# executable scripts ------
  
  insert_msg('Executing paper analysis scripts')
  
  c('./analysis scripts/inflammatory_params.R', ## comparison of the inflammatory and iron parameters
    './analysis scripts/cytometry.R', ## test cytometry features
    './analysis scripts/cytometry_cd14.R', ## CD14 in neutrophils, as requested by the reviewer
    './analysis scripts/cytometry_backbone.R', ## monocyte backbone markers as requested by the reviewer
    './analysis scripts/correlations.R', ## correlations between the cytometry features and inflammatory/iron parameter
    './analysis scripts/clustering.R', ## participant and cytometry feature clustering
    './analysis scripts/cluster_characteristic.R') %>% ## characteristic of the clusters
    source_all(message = TRUE, crash = TRUE)

# END ----
  
  insert_tail()