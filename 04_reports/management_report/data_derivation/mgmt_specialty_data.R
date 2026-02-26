################################################################.
#### SRASA Management Report - Data - Specialty-level usage ####
################################################################.

#Author: Bex Madden
#Date:26/02/2026

# Derivation of source data tables for the specialty-level utilisation measures 
# shown in the SRASA management report

### set min and max dates ------------------------------------------------------
# from last month of smr01 completeness (approx 6wks before middle of given month)
# to a year prior to that date
# use < latest date and >= start_date 
latest_date <- Sys.Date() %>% 
  lubridate::floor_date("month") %m-% months(2)

start_date <- latest_date %>% 
  lubridate::floor_date("month") %m-% months(12)

### Read in cleaned data from SMR01 --------------------------------------------
ras_cand_data <- read_parquet(paste0(data_dir, "monthly_extract/srasa_smr_extract_min.parquet")) %>% 
  filter(main_op_date >= start_date & 
           main_op_date < latest_date) %>% 
  
  #tidying
  mutate(main_op_approach = as.factor(main_op_approach), pt
         main_op_approach = fct_relevel(main_op_approach, c("NOS", "MIA", "RAS", 
                                                            "RAS conv open", "MIA conv open")),
         age_group = as.factor(age_group),
         age_group = fct_relevel(age_group, age_group_order),
         
         ras_proc = case_when(ras_proc == TRUE ~ "RAS",
                              .default = "Non-RAS")) %>% 
  
  #change hospital name to 'other' when a robotic surgery is listed against non-robotic site
  mutate(res_health_board = case_when(is.na(res_health_board) ~ "Unknown",
                                      .default = res_health_board))


### Hospital-level equity ------------------------------------------------------
# show at hospital level as more relevant to specialty than hb

##### Total monthly no. ras procs by specialty & location --------------------
spec_procsmth <- ras_cand_data %>%
  group_by(hospital_name_grp, hosp_health_board, op_mth, op_year, main_op_specialty, ras_proc) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(op_mth, op_year, main_op_specialty, ras_proc) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hospital_name_grp, ~"All"),
                      .groups = "drop")) %>% 
  ungroup() 

write_parquet(spec_procsmth, paste0(data_dir, "management_report/spec_procsmth.parquet"))

##### Weekly no. procs by specialty and location, shown monthly ----------------
#until i can figure out mean no per spec per day of the week per month
# equity_specsday <- long_data %>%
#   filter(proc_date >= start_date & 
#            proc_date < latest_date) %>% 
#   group_by(hospital_name, op_mth_year, op_year, proc_date, code_specialty, proc_approach_binary) %>% 
#   summarise(n = n()) %>% 
#   ungroup() %>% 
#   group_by(op_mth_year, op_year, code_specialty, proc_approach_binary) %>% 
#   bind_rows(summarise(.,
#                       across(where(is.numeric), sum),
#                       across(hospital_name, ~"All"),
#                       .groups = "drop")) %>% 
#   ungroup()
# 
# write_parquet(equity_specsday, paste0(data_dir, "management_report/equity_specsday.parquet"))

##### Mean number of surgeries per day of the week, monthly --------------------
equity_specsday <- ras_cand_data %>% # i don't think this is quite doing what I want it to
  filter(ras_proc == TRUE) %>%
  mutate(#op_week = floor_date(proc_date, "week"),
         dow = factor(format(as.Date(proc_date, format="%d/%m/%Y"),"%A"),
                      levels = c("Monday", "Tuesday", "Wednesday", "Thursday",
                                 "Friday", "Saturday", "Sunday"))) %>%
  group_by(hospital_name, op_year, op_mth_year, ?op_week, proc_date, dow, code_specialty) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  tidyr::complete(hospital_name, op_year, op_mth_year, ?op_week, dow, code_specialty) %>% #need to get zeroes for the days when no surgery happened for that spec at that location
  mutate(n = replace_na(n, 0)) %>%
  group_by(hospital_name, op_year, op_mth_year, dow, code_specialty) %>% #, .drop = FALSE
  summarise(mean_procs_pd = round(mean(n), 2)) %>%
  ungroup()

write_parquet(util_procsday, paste0(data_dir, "management_report/util_procsday.parquet"))


