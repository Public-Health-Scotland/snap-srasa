###################################################################.
### SNAP SRASA - Quarterly DQ check of emergency RAS procedures ###
###################################################################.

# Bex Madden 
# 06/07/2026


dq_emergency_procs <- function(df){
  
  #' Append compiled monthly data to master df of rolling monthly data 
  #' 
  #' @description - Compiles csv of OPCS codes associated with 'Unlisted' procedures
  #' appearing in SMR01 extract since the previous year to current date.
  #' 
  #' @usage - when message appears in monthly extract process, check output file 
  #' to identify any procedures which should be included on RAS list### test admission type distinction
  
last_year <- lubridate::year(Sys.Date()) - 1

emergency_procs <- df %>% 
  filter(op_year >= last_year & hosp_has_robot == "Yes") %>% 
  group_by(hosp_health_board, hospital_name, ras_proc, main_op_type, adm_type_grp) %>% 
  summarise(n=n()) %>% 
  filter(adm_type_grp == "emergency" & ras_proc == TRUE) %>% 
  group_by(hospital_name) %>% 
  mutate(hosp_total = sum(n))

# to investigate perodically

month_check <- lubridate::month(Sys.Date()) 

if(month_check %in% c(1,4,7,10)){
  write.csv(emergency_procs, paste0(data_dir, "monthly_extract/dq_emergency_procs.csv"))
  
  message("Quarterly check of emergency RAS procedures due - see (12) Data/monthly_extract/dq_emergency_procs.csv")
}
}