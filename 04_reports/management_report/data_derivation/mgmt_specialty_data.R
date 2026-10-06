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


# Simplify specialty categories for these graphs
ras_cand_data <- ras_cand_data %>% 
  mutate(main_op_specialty = str_remove(main_op_specialty, #collapse unlisted into main specialty
                                        " - unlisted"))

##### Total monthly no. ras procs by specialty & location ----------------------
spec_procsmth <- ras_cand_data %>%
  group_by(hospital_name_grp, hosp_health_board, op_mth, op_year, main_op_specialty, ras_proc) %>% 
  summarise(n = n()) %>% 
  ungroup() 
  # group_by(op_mth, op_year, main_op_specialty, ras_proc) %>% 
  # bind_rows(summarise(.,
  #                     across(where(is.numeric), sum),
  #                     across(hospital_name_grp, ~"All"),
  #                     .groups = "drop")) %>% 
  # ungroup() 

write_parquet(spec_procsmth, paste0(data_dir, "management_report/spec_procsmth.parquet"))

##### RAS and Non-RAS procedures for patients with cancer diagnoses ----
# whole year by specialty and location
# min data keeping non-ras procs

spec_appdiag <-  read_parquet(paste0(data_dir, "monthly_extract/srasa_smr_extract_min.parquet")) %>% 
  filter(op_mth >= yr_start & 
           op_mth < yr_end) %>%
  mutate(hospital_name_grp = case_when(hospital_name %in% wrong_hosp ~ "Other Hospital Listed", #contains non-RAS and private hospitals
                                       .default = hospital_name),
         hospital_name_grp = factor(hospital_name_grp, levels = hosp_order)) %>% 
  
  mutate(cancer_binary = case_when(!is.na(cancer_surgery) ~ "Cancer",
                                   .default = "Benign")) %>% 
  group_by(main_op_specialty, hosp_health_board, ras_proc, cancer_binary, hospital_name_grp) %>% 
  summarise(n=n()) %>% 
  group_by(main_op_specialty, hosp_health_board, cancer_binary, hospital_name_grp) %>% 
  mutate(total = sum(n),
         prop = round(n/total*100, 2),
         ras_proc = as.factor(ras_proc),
         ras_proc = case_when(ras_proc == "TRUE" ~ "RAS",
                              ras_proc == "FALSE" ~ "Non-RAS",
                              .default = NA)) %>% 
  ungroup() %>% 
  tidyr::complete(hospital_name_grp, # might as well do this here rather than in the report script
                  nesting(main_op_specialty, cancer_binary),
                  fill = list(n = 0, total = 0, prop = 100, ras_proc = "No procedures")) 

write_parquet(spec_appdiag, paste0(data_dir, "management_report/spec_appdiag.parquet"))

### Phases of conducted ras procs per specialty --------------------------------
# spec_procphase <- ras_cand_data %>%
#   group_by(hospital_name_grp, hosp_health_board, op_mth, op_year, main_op_specialty, main_op_phase, ras_proc) %>% 
#   summarise(n = n()) %>% 
#   ungroup()
#   # group_by(op_mth, op_year, main_op_specialty, main_op_phase, ras_proc) %>% 
#   # bind_rows(summarise(.,
#   #                     across(where(is.numeric), sum),
#   #                     across(hospital_name_grp, ~"All"),
#   #                     .groups = "drop")) %>% 
#   # ungroup() 
# 
# write_parquet(spec_procphase, paste0(data_dir, "management_report/spec_procphase.parquet"))



