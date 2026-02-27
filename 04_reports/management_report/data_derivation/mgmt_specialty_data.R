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
  lubridate::floor_date("month") %m-% months(3)

start_date <- latest_date %>% 
  lubridate::floor_date("month") %m-% months(12)

### Read in cleaned data from SMR01 --------------------------------------------
ras_cand_data <- read_parquet(paste0(data_dir, "monthly_extract/srasa_smr_extract_min.parquet")) %>% 
  filter(op_mth >= start_date & 
           op_mth < latest_date) %>% 
  
  #tidying
  mutate(main_op_approach = as.factor(main_op_approach), 
         main_op_approach = fct_relevel(main_op_approach, c("NOS", "MIA", "RAS", 
                                                            "RAS conv open", "MIA conv open")),
         age_group = as.factor(age_group),
         age_group = fct_relevel(age_group, age_group_order),
         
         ras_proc = case_when(ras_proc == TRUE ~ "RAS",
                              .default = "Non-RAS")) %>% 
  
  #change hospital name to 'other' when a robotic surgery is listed against non-robotic site
  mutate(res_health_board = case_when(is.na(res_health_board) ~ "Unknown",
                                      .default = res_health_board))

wrong_hosp <- ras_cand_data %>% #get list of hospital names that appear but do not have a robot
  filter(hosp_has_robot != "Yes") %>% 
  group_by(hospital_name) %>% 
  slice(1) %>% 
  dplyr::pull(., hospital_name)

ras_cand_data <- ras_cand_data %>% #some aberrant coding liekly due to transfers
  mutate(hospital_name_grp = case_when(hospital_name %in% wrong_hosp ~ "Other Hospital Listed", #contains non-RAS and private hospitals
                                       .default = hospital_name),
         hospital_name_grp = factor(hospital_name_grp, levels = hosp_order))

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

### Phases of conducted ras procs per specialty --------------------------------
spec_procphase <- ras_cand_data %>%
  group_by(hospital_name_grp, hosp_health_board, op_mth, op_year, main_op_specialty, main_op_phase, ras_proc) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(op_mth, op_year, main_op_specialty, main_op_phase, ras_proc) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hospital_name_grp, ~"All"),
                      .groups = "drop")) %>% 
  ungroup() 

write_parquet(spec_procphase, paste0(data_dir, "management_report/spec_procphase.parquet"))






##### Weekly no. procs by specialty and location, shown monthly ----------------
#until i can figure out mean no per spec per day of the week per month
# equity_specsday <- ras_cand_data %>%
#   filter(main_op_date >= start_date & 
#            main_op_date < latest_date) %>% 
#   group_by(hospital_name, op_mth, op_year, main_op_date, main_op_specialty, ras_proc) %>% 
#   summarise(n = n()) %>% 
#   ungroup() %>% 
#   group_by(op_mth, op_year, main_op_specialty, ras_proc) %>% 
#   bind_rows(summarise(.,
#                       across(where(is.numeric), sum),
#                       across(hospital_name, ~"All"),
#                       .groups = "drop")) %>% 
#   ungroup()
# 
# write_parquet(equity_specsday, paste0(data_dir, "management_report/equity_specsday.parquet"))

##### Mean number of surgeries per day of the week, monthly --------------------
# equity_specsday <- ras_cand_data %>% # i don't think this is quite doing what I want it to
#   filter(ras_proc == "RAS") %>%
#   mutate(dow = factor(format(as.Date(main_op_date, format="%d/%m/%Y"),"%A"),
#                       levels = c("Monday", "Tuesday", "Wednesday", "Thursday",
#                                  "Friday", "Saturday", "Sunday"))) %>%
#   group_by(hospital_name, op_year, op_mth, main_op_date, dow, main_op_specialty) %>%
#   summarise(n = n()) %>%
#   ungroup() %>%
#   tidyr::complete(hospital_name, op_year, op_mth, dow, main_op_specialty) %>% #need to get zeroes for the days when no surgery happened for that spec at that location
#   mutate(n = replace_na(n, 0)) %>%
#   group_by(hospital_name, op_year, op_mth, dow, main_op_specialty) %>% #, .drop = FALSE
#   summarise(mean_procs_pd = round(mean(n), 2)) %>%
#   ungroup()
# 
# write_parquet(util_procsday, paste0(data_dir, "management_report/util_procsday.parquet"))


