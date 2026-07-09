##################################################################.
### SNAP SRASA - Quarterly DQ check of unlisted RAS procedures ###
##################################################################.

# Bex Madden 
# 06/07/2026


dq_unlisted_procs <- function(df){
  
  #' Append compiled monthly data to master df of rolling monthly data 
  #' 
  #' @description - Compiles csv of OPCS codes associated with 'Unlisted' procedures
  #' appearing in SMR01 extract since the previous year to current date.
  #' 
  #' @usage - when message appears in monthly extract process, check output file 
  #' to identify any procedures which should be included on RAS list
  #'

last_year <- lubridate::year(Sys.Date()) - 1

unlisted_procs <- df %>% 
  filter(str_detect(main_op_type, 'Unlisted') &
           op_year >= last_year) %>% 
  group_by(main_op_type, main_op_code) %>% 
  summarise(n=n()) %>% 
  arrange(-n)

# to investigate perodically for review of inclusion list

month_check <- lubridate::month(Sys.Date()) 

if(month_check %in% c(3,6,9,12)){
  write.csv(unlisted_procs, paste0(data_dir, "monthly_extract/dq_unlisted_procs.csv"))
  
  message("Quarterly check of unlisted procedures due - see (12) Data/monthly_extract/dq_unlisted_procs.csv")
}
}