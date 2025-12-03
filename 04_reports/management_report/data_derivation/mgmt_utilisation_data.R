######################################################.
#### SRASA Management Report - Data - Utilisation ####
######################################################.

#Author: Bex Madden
#Date:06/11/2025

# Derivation of source data tables for the utilisation-related measures shown in the 
# SRASA management report

#source functions
source("./02_setup/subset_smr01_extract.R")

### set min and max dates ------------------------------------------------------
# from last month of smr01 completeness (approx 6wks before middle of given month)
# to a year prior to that date
# use < latest date and >= start_date 
latest_date <- Sys.Date() %>% 
  lubridate::floor_date("month") %m-% months(2)

start_date <- latest_date %>% 
  lubridate::floor_date("month") %m-% months(12)

### Read in raw data from SMR01, no filtering for procedure --------------------
ras_clean_data <- read_parquet(paste0(data_dir, "monthly_extract/srasa_smr_extract_", 
                                      format(Sys.Date(), "%Y-%m"), ".parquet")) %>%
  subset_smr01_extract(filter_to = "cis") %>% 
  filter(date_of_main_operation >= start_date & 
           date_of_main_operation < latest_date)

all_ras_procs <- ras_clean_data %>% 
  ungroup()  %>%
  filter(!specialty_desc %in% c("Trauma and Orthopaedic Surgery", "Neurosurgery", "Neurology"), #ortho & neuro not cavity, other procs shouldn't be filtered by smr specialty 
         ras_proc == TRUE) %>%  #only want rows with actual procedure 
  mutate(specialty_desc = case_when(specialty_desc == "General Surgery (excludes Vascular)" ~ "General Surgery",
                                    specialty_desc == "Paediatrics" ~ "Paediatric Surgery",
                                    specialty_desc == "Cardiology" ~ "Cardiac Surgery",
                                    specialty_desc == "Renal Medicine" ~ "Urology",
                                    specialty_desc == "Respiratory Medicine" ~ "Thoracic Surgery",
                                    specialty_desc %in% c("Gastroenterology","Geriatric Medicine","Anaesthetics","Clinical Oncology", "Acute Medicine") ~ "Other Specialty",
                                    is.na(specialty_desc) ~ "Other Specialty",
                                    .default = specialty_desc))

wrong_hosp <- all_ras_procs %>% #get list of hospital names that appear but do not have a robot
  filter(hosp_has_robot != "Yes") %>% 
  group_by(hospital_name) %>% 
  slice(1) %>% 
  dplyr::pull(., hospital_name)

all_ras_procs <- all_ras_procs %>% #some aberrant coding liekly due to transfers
  mutate(hospital_name_grp = case_when(hospital_name %in% wrong_hosp ~ "Other Hospital Listed", #contains non-RAS and private hospitals
                                   .default = hospital_name),
         hospital_name_grp = factor(hospital_name_grp, levels = hosp_order))

### Number of robotics procedures per month by hospital location ---------------
ppm <- all_ras_procs %>% 
  mutate(op_mth = floor_date(date_of_main_operation, "month"),
         op_year = format(as.Date(date_of_main_operation, format="%d/%m/%Y"),"%Y")) %>% 
  group_by(hospital_name_grp, op_mth, op_year) %>% 
  summarise(n = n()) %>% 
  ungroup()

write_parquet(ppm, paste0(data_dir,  "management_report/util_procsmth.parquet"))

### Mean daily number of robotics procedures by day per month ------------------
ppd <- all_ras_procs %>% 
  mutate(op_mth = floor_date(date_of_main_operation, "month"),
         op_year = format(as.Date(date_of_main_operation, format="%d/%m/%Y"),"%Y"),
         dow = factor(format(as.Date(date_of_main_operation, format="%d/%m/%Y"),"%A"),
                      levels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                 "Friday", "Saturday", "Sunday"))) %>%
  group_by(hospital_name_grp, op_year, op_mth, date_of_main_operation, dow) %>% 
  summarise(n = n()) %>% 
  group_by(hospital_name_grp, op_year, op_mth, dow, .drop = FALSE) %>% 
  summarise(mean_procs_pd = round(mean(n), 2)) %>% 
  mutate(mean_procs_pd = ifelse(is.nan(mean_procs_pd), 0, mean_procs_pd)) %>% 
  ungroup() 

write_parquet(ppd, paste0(data_dir, "management_report/util_procsday.parquet"))

### Specialty of surgeries number by month -------------------------------------
pps <- all_ras_procs %>% 
  mutate(op_mth = floor_date(date_of_main_operation, "month"),
         op_year = format(as.Date(date_of_main_operation, format="%d/%m/%Y"),"%Y")) %>% 
  group_by(hospital_name_grp, op_year, op_mth, specialty_desc) %>% 
  summarise(n = n()) %>% 
  ungroup()

write_parquet(pps, paste0(data_dir, "management_report/util_procspec.parquet"))

### Sessions per day by specialty?

