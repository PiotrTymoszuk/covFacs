# this script provides general functions for the project analyses

# other tools -----

  require(tidyverse)
  require(ggpubr)
  require(ggsignif)
  require(soucer)
  require(stringi)

# Data import and clearing -----

  read_freq_data <- function(path) {
    
    ## reads and clears a file with the population frequency data
    
    if(length(path) > 1) {
      
      data_lst <- path %>% 
        map(read_freq_data)
      
      status <- data_lst %>% 
        map_chr(function(x) if(is.null(x)) 'failure' else 'OK') %>% 
        tibble(path = path, 
               status = .) %>% 
        filter(status == 'failure')
      
      if(nrow(status) == 0) {
        
        message('No reading failures!')
        
      } else {
        
        print(status)
        
      }
      
      return(data_lst %>% 
               do.call('rbind', .))
      
      
    }
    
    freq_data <- try(read_tsv(path) %>% 
                       filter(!.data[['Sample:']] %in% c('Mean', 'SD')), 
                     silent = T)
    
    if(any(class(freq_data) == 'try-error')) {
      
      message(paste('Reading failure: ', path))
      
      return(NULL)
      
    }
    
    return(freq_data %>% 
             select(-`Sample:`))
    
  }
  
  read_pd_data <- function(path, exprs_only = TRUE) {
    
    ## reads and clears a file with the PD staining values
    ## returns delta MFI (median)
    
    if(length(path) > 1) {
      
      data_lst <- path %>% 
        map(read_pd_data)
      
      status <- data_lst %>% 
        map_chr(function(x) if(is.null(x)) 'failure' else 'OK') %>% 
        tibble(path = path, 
               status = .) %>% 
        filter(status == 'failure')
      
      if(nrow(status) == 0) {
        
        message('No reading failures!')
        
      } else {
        
        print(status)
        
      }
      
      return(data_lst %>% 
               do.call('rbind', .))
      
      
    }
    
    pd_tbl <- try(read_tsv(path)  %>% 
                    filter(!.data[['Sample:']] %in% c('Mean', 'SD')), 
                  silent = TRUE)
    
    if(any(class(pd_tbl) == 'try-error') | 
       length(pd_tbl$staining) < 2 | 
       !'ISO' %in% pd_tbl$staining) {
      
      message(paste('Reading failure: ', path))
      
      return(NULL)
      
    }
    
    ## calculating delta mfi
    
    desc_tbl <- pd_tbl %>% 
      select(`Sample:`, 
             id, 
             staining)
    
    suppressWarnings(expr_tbl <- pd_tbl %>% 
                       select(- `Sample:`,
                              - id, 
                              - staining) %>% 
                       map_dfc(as.numeric))
    
    suppressWarnings(ref_vals <-  pd_tbl %>% 
                       filter(staining == 'ISO') %>% 
                       select(- `Sample:`,
                              - id, 
                              - staining) %>% 
                       map_dfc(as.numeric))
    
    expr_tbl <- map2_dfc(expr_tbl, 
                         ref_vals, 
                         function(x, y) x - y) %>% 
      map_dfc(function(x) ifelse(x < 0, 0, x))
    
    pd_tbl <- cbind(desc_tbl, 
                    expr_tbl) %>% 
      filter(staining != 'ISO') %>%
      as_tibble
    
    if(exprs_only) {
      
      pd_tbl <- pd_tbl %>% 
        select( - `Sample:`, 
                - staining)
      
    }
    
    return(pd_tbl)
    
  }
  
  read_testab_data <- function(path) {
    
    ## reads and clears a file with the test Ab staining values
    ## returns delta MFI (median)

    if(length(path) > 1) {
      
      data_lst <- path %>% 
        map(safely(read_testab_data))
      
      status <- data_lst %>% 
        map_chr(function(x) if(is.null(x)) 'failure' else 'OK') %>% 
        tibble(path = path, 
               status = .) %>% 
        filter(status == 'failure')
      
      if(nrow(status) == 0) {
        
        message('No reading failures!')
        
      } else {
        
        print(status)
        
      }
      
      return(data_lst %>% 
               map(~.x$result) %>% 
               do.call('rbind', .))
      
      
    }
    
    test_tbl <- read_pd_data(path, exprs_only = FALSE)
    
    ## filling in the missing columns
    
    all_cols <- c('CD28', 
                  'CD40', 
                  'CD64', 
                  'CD71', 
                  'CD80', 
                  'CD86', 
                  'CD163', 
                  'FPN1', 
                  'TIM3')
    
    for(i in all_cols[!all_cols %in% test_tbl$staining]) {
      
      fill_record <- test_tbl[1, ] %>% 
        mutate(staining = i, 
               neutrophils_testAb = NA, 
               classicalMono_testAb = NA, 
               intermediateMono_testAb = NA, 
               nonclassicalMono_testAb = NA)
      
      test_tbl <- rbind(test_tbl, 
                        fill_record) %>% 
        as_tibble
      
    }
    
    ## switching to the wide format
    
    test_tbl <- test_tbl %>% 
      dlply(.(staining)) %>% 
      map(function(x) set_names(x, 
                                stri_replace(names(x), 
                                             fixed = 'testAb', 
                                             replacement = x$staining[1]))) %>% 
      map(select, 
          - `Sample:`, 
          - staining) %>% 
      reduce(left_join, 
             by = 'id') %>% 
      as_tibble
    
    return(test_tbl)
    
  }
  
  read_revision <- function(folder) {
    
    ## reads results of extra analyses for the paper revision
    
    if(length(folder) > 1) {
      
      data_lst <- folder %>% 
        map(safely(read_revision))
      
      status <- data_lst %>% 
        map_chr(function(x) if(is.null(x)) 'failure' else 'OK') %>% 
        tibble(path = folder, 
               status = .) %>% 
        filter(status == 'failure')
      
      if(nrow(status) == 0) {
        
        message('No reading failures!')
        
      } else {
        
        print(status)
        
      }
      
      return(data_lst %>% 
               map(~.x$result) %>% 
               do.call('rbind', .))
      
      
    }
    
    file_lst <- c('testAb_hladr.txt', 
                  'testAb_cd62l.txt', 
                  'testAb_cx3cr1.txt', 
                  'testAb_ccr2.txt') %>% 
      paste(folder, ., sep = '/')
    
    data_lst <- file_lst %>% 
      map(read_tsv) %>% 
      map(filter, !.data[['Sample:']] %in% c('Mean', 'SD')) %>% 
      map(select, - staining, - `Sample:`) %>% 
      reduce(left_join, by = 'id')
    
    suppressWarnings(data_lst <- data_lst[, -1] %>% 
                       map_dfc(as.numeric) %>% 
                       cbind(data_lst[, 1], .) %>% 
                       as_tibble)
    
    return(data_lst)
    
  }
  
# plotting functions -----
  
  add_norm_range <- function(plot, lower_limit, upper_limit) {
    
    plot + 
      geom_hline(yintercept = lower_limit, 
                 linetype = 'dashed', 
                 color = 'steelblue3') + 
      geom_hline(yintercept = upper_limit, 
                 linetype = 'dashed', 
                 color = 'steelblue3')
    
    
  }
  
  add_post_hoc <- function(plot, 
                           healthy_moderate, 
                           moderate_severe, 
                           healthy_severe, 
                           step_increase = 0.09, 
                           margin_top = 0.018, ...) {
    
    ## adds custom-calculated post-hoc results to the bar plot
    
    plot + 
      geom_signif(comparisons = list(c(1, 2), 
                                     c(2, 3), 
                                     c(1, 3)), 
                  step_increase = step_increase, 
                  annotations = c(healthy_moderate, 
                                  moderate_severe, 
                                  healthy_severe), 
                  textsize = 2.75, 
                  tip_length = 0, 
                  margin_top = margin_top)
    
  }
  
  add_post_hoc_clust <- function(plot, 
                                 clust_2, 
                                 clust_3, 
                                 clust_4, 
                                 step_increase = 0.09, 
                                 margin_top = 0.018, ...) {
    
    ## adds custom-calculated post-hoc results to the bar plot
    ## with the inclammatory paramaters in the participant clusters
    
    plot + 
      geom_signif(comparisons = list(c(1, 2), 
                                     c(1, 3), 
                                     c(1, 4)), 
                  step_increase = step_increase, 
                  annotations = c(clust_2, 
                                  clust_3, 
                                  clust_4), 
                  textsize = 2.75, 
                  tip_length = 0, 
                  margin_top = margin_top)
    
    
  }
  
  set_cmm_scale <- function(plot_list, min_y = 0) {
    
    ## sets common y scale for the given plots
    
    range_mtx <- plot_list %>% 
      map(~.x$data$variable) %>% 
      map(range) %>% 
      reduce(rbind)
    
    cmm_min <- min(range_mtx[, 1])
    cmm_max <- max(range_mtx[, 2])
    
    plot_list <- plot_list %>% 
      map(~.x + scale_y_continuous(limits = c(cmm_min, cmm_max)))
    
    if(!is.null(min_y)) {
      
      plot_list <- plot_list %>% 
        map(~.x + expand_limits(y = min_y))
      
    }
    
    plot_list
    
  }

  get_tag <- function(plot) {
    
    ## extracts the plot tag and changes it's format for the figure panel
    
    plot$labels$tag
    
  }
  
  add_representative_facs <- function(plot, image_path, valign = 0.67) {
    
    ## adds representative cytometry plots
    
    new_plot <- plot_grid(ggdraw() + 
                            draw_image(image_path, 
                                       vjust = 0, 
                                       x = 0, 
                                       valign = valign) + 
                            theme(plot.margin = ggplot2::margin(r = 3, l = 2, unit = 'mm')),
                          ggdraw(), 
                          plot, 
                          ncol = 3, 
                          rel_widths = c(0.7, 0.1, 1.2), 
                          labels = c('A', 'B'), 
                          label_size = 10)
    
    return(new_plot)
    
  }
  
  make_cyto_panel <- function(plot_list, represent_path, valign = 0.67) {
    
    cyto_panel <- plot_list %>% 
      map(~.x + 
            theme(legend.position = 'none', 
                  plot.tag = element_blank(), 
                  axis.text.x = element_blank())) %>% 
      plot_grid(plotlist = ., 
                nrow = length(plot_list)) %>% 
      plot_grid(plot_grid(ggdraw(), 
                          get_legend(plot_list[[1]]), 
                          ggdraw() + 
                            draw_text(get_tag(plot_list[[2]]), 
                                      size = 8,
                                      hjust = 0, 
                                      x = 0.1), 
                          nrow = 4), 
                ncol = 2, 
                rel_widths = c(0.75, 0.25))
    
    cyto_panel %>% 
      add_representative_facs(represent_path, valign = valign)
    
  }
  
  
# varia -----
  
  mm_inch <- function(input_mm) {
    
    return(0.0393700787 * input_mm)
    
  }
  
# variable to label conversion -----
  
  translate_var <- function(variable, dictionary = proj_globals$var_tbl, type = 'label') {
    
    # translates the variable name to the label of interest
    
    lab_vector <- set_names(dictionary[[type]], 
                            dictionary[['variable']])
    
    return(lab_vector[variable])
    
  }
  
# LATEX and report tools -----
  
  numbers <- c('one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine', 'ten')
  
  Numbers <- c('One', 'Two', 'Three', 'Four', 'Five', 'Six', 'Seven', 'Eight', 'Nine', 'Ten')
  
  my_count <- function(x, ...) {
    
    count_tbl <- count(x, ...)
    
    count_tbl %>% 
      mutate(percent = n/sum(n) * 100, 
             txt_lab = paste0(signif(percent, 2), '%'))
    
  }
  
  my_median <- function(data, variable) {
    
    quants <- quantile(data[[variable]], c(0.5, 0.25, 0.75), na.rm = TRUE)

    paste0(signif(quants[1], 2), 
           ' [IQR: ', 
           signif(quants[2], 2), 
           ' - ', 
           signif(quants[3], 2), ']')
        
  }
  
  add_span <- function(txt = NULL) {
    
    paste0("<span custom-style = 'revision'>", txt, "</span>")
    
  }
  
# text functions -----
  
  split_vec <- function(inp_vector, chunk_size) {
    
    return(split(inp_vector, ceiling(seq_along(inp_vector)/chunk_size)))
    
  }
  
  wrap_vector <- function(txt_vec, line_length = 5) {
    
    split_txt <- split_vec(txt_vec, 
                           line_length) %>% 
      map(paste, 
          collapse = ' ') %>%
      paste(collapse = '\n')
    
    return(split_txt)
    
  }
  
  
# END ------