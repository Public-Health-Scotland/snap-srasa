##############################################.
### SNAP SRASA - Filter SMR01 extract down ###
##############################################.

# Bex Madden 
# 11/08/2026


filter_extract <- function(df){
  
  #' Filters cleaned extract to only patients with procedures/diagnoses of interest
  #'
  #' @description This function filters the part-processed SMR01 data extract to 
  #' keep patients who have had a RAS procedure or an oropharyngeal cancer diagnosis
  #' 
  #' @param df - the part-processed monthly SMR01 extract
  #' 
  #' @usage filter_extract(df)
  #'
  #' @details Groups and filters patient records based on presence of a main_op_type
  #'  (from identify_ras_procs) or a ras_oropharynx (from identify_oropharynx_diags)
  
  ### Filter -----------------------------------------------------------

filter_df <- df %>% 
  group_by(link_no) %>% 
  filter(any(!is.na(main_op_code)) | any(!is.na(ras_oropharynx))) %>%  #all records for any patient with a candidate or unlisted RAS proc
  ungroup()

### Return df ----------------------------------------------------

return(filter_df)
}