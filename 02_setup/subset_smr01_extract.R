##################################################.
### SNAP SRASA - Subset Monthly SMR01 extract  ###
##################################################.

# Bex Madden & Dylan Lewis
# 01/12/2025

subset_smr01_extract <- function(df, 
                                 group_to = c("cis", "patient"),
                                 filter_by = c("ras", "candidate")){
  
  #' Group and filter SRASA SMR01 extract to either CIS or whole patient record
  #' containing a RAS flag
  #'
  #' @description 
  #' 
  #' @param df - smr01 extract to filter
  #' @param group_to - group records to CIS (continuous inpatient stay) level, 
  #' or to retain all records pertaining to that patient
  #' 
  #' @usage - subset_smr01_extract(df, group+to = "cis"/"patient")
  #'
  #' @details - when 'cis' selected, groups by smr01 link_no and cis_marker, then 
  #' filters to retain only continuous inpatient stays where a RAS procedure was 
  #' performed. When 'patient' selected, groups by smr01 link_no only to retain 
  #' all records for all patients who have ever received a RAS procedure.
  
  ### set grouping variables according to input value --------------------------
  if(group_to == "cis"){
    grouping_terms <- c("link_no", "cis_marker")
  } else {
    grouping_terms <- "link_no"
  }
  
  if(filter_by == "ras"){
    filter_term <- "ras_proc"
  } else {
    filter_term <- "candidate_proc"
  }
  
  ### group by selected variables and filter df --------------------------------
  ras_filtered_data <- df %>%
    group_by(pick(all_of(grouping_terms))) %>% 
    filter(any(pick(all_of(filter_term)) == TRUE)) %>% 
    #add phase1_proc column and phase2_proc column then can dynamically filter to proc or ras as wanted
    ungroup()
  
  return(ras_filtered_data)
}