##################################################.
### SNAP SRASA - Subset Monthly SMR01 extract  ###
##################################################.

# Bex Madden & Dylan Lewis
# 01/12/2025

subset_smr01_extract <- function(df, filter_to = c("cis", "patient")){
  
  #' Group and filter SRASA SMR01 extract to either CIS or whole patient record
  #' containing a RAS flag
  #'
  #' @description 
  #' 
  #' @param df - 
  #' @param filter_to -
  #' 
  #' @usage 
  #'
  #' @details 
  
  ### set grouping variables according to input value --------------------------
  if(filter_to == "cis"){
    filter_terms <- c("link_no", "cis_marker")
  } else {
    filter_terms <- "link_no"
  }
  
  ### group by selected variables and filter df --------------------------------
  ras_filtered_data <- df %>% 
    group_by(pick(all_of(filter_terms))) %>% 
    filter(any(ras_proc == TRUE))
  
  return(ras_filtered_data)
}