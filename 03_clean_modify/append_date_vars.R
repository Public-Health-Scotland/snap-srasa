###################################################################.
### SNAP SRASA - Append date variables to Monthly SMR01 extract ###
###################################################################.

# Bex Madden 
# 13/02/2026

append_date_vars <- function(df){
  
  #' Append various date variables to SRASA Monthly SMR01 extract
  #'
  #' @description wrangles existign date variables to give a range of record 
  #' date columns. And identifies whether an smr record has been amended post-insertion
  #' 
  #' @param df - an smr01 extract
  #' 
  #' @usage append_date_vars()
  
  cli_progress_step("Wrangling date columns...")
  
  ras_date_data <- df %>% 
  group_by(link_no, cis_marker) %>% # make this a function used in extraction process?
    mutate(first_admit_date = min(admission_date),
           last_discharge_date = max(discharge_date),
           cis_outcome = case_when(any(discharge == 2) ~ 'dead',
                                   .default = 'alive'), #get admission and discharge date for each stay plus outcome from cis
           op_mth = floor_date(op1_date, "month"), 
           op_year = format(as.Date(op_mth, format="%Y-%m-%d"),"%Y"),
           op_qt = lubridate::quarter(as.Date(op1_date, format="%Y-%m-%d"), with_year = T),
           .after = discharge_date) %>%
    ungroup() %>% 
    
    # add column related to record insert/amended data to see if rec has been amended
    #calculate difference between record inserted and record last amended dates
    mutate(date_record_inserted = as.Date(date_record_inserted),
           date_last_amended = as.Date(date_last_amended),
           record_amend_gap = as.numeric(date_last_amended-date_record_inserted),
           record_amended = ifelse(date_record_inserted != date_last_amended, TRUE, FALSE), 
           .after = date_last_amended)
  
  return(ras_date_data)
  
}

