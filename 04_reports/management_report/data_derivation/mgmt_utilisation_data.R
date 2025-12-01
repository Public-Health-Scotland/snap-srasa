######################################################.
#### SRASA Management Report - Data - Utilisation ####
######################################################.

#Author: Bex Madden
#Date:06/11/2025

# Derivation of source data tables for the utilisation-related measures shown in the 
# SRASA management report

### Read in raw data from SMR01, no filtering for procedure --------------------
ras_clean_data <- read_parquet(paste0(data_dir, "srasa_smr_extract_2025-12.parquet")) %>%
  subset_smr01_extract(filter_to = "cis")
  
# source("./02_setup/extract_smr01_data.R")
# smr01_extract <- extract_smr01_data()

all_ras_procs <- ras_clean_data %>% 
  ungroup()  %>%
  filter(!specialty_desc %in% c("Trauma and Orthopaedic Surgery", "Neurosurgery", "Neurology")) %>%  #ortho & neuro not cavity 
## cardiac? da vinci say only coronary bypass graft and mitral valve, here listed aortic valve, pacemaker insertion?
# may need to refine by code e.g. 1 paediatric knee op, biopsy of lesions not resections?
# a handful of cases just coded unusually e.g. prostatectomy under renal specialty not surgery, kidney excision coded as gastro, can't rely on specialty alone for refining
  mutate(specialty_desc = case_when(specialty_desc == "General Surgery (excludes Vascular)" ~ "General Surgery",
                                    specialty_desc == "Paediatrics" ~ "Paediatric Surgery",
                                    specialty_desc == "Cardiology" ~ "Cardiac Surgery",
                                    specialty_desc == "Renal Medicine" ~ "Urology",
                                    specialty_desc == "Respiratory Medicine" ~ "Thoracic Surgery",
                                    specialty_desc %in% c("Gastroenterology","Geriatric Medicine","Anaesthetics","Clinical Oncology", "Acute Medicine") ~ "Other Specialty",
                                    is.na(specialty_desc) ~ "Other Specialty",
                                    .default = specialty_desc))

wrong_hosp <- all_ras_procs %>% #get list of hospital names that appear but do not have a robot (by number, replace with list of hospitals that DO have robot)
  group_by(hospital_name) %>% 
  summarise(n = n()) %>% 
  filter(n <= 10) %>% 
  dplyr::pull(., hospital_name)

all_ras_procs <- all_ras_procs %>% #some aberrant coding liekly due to transfers
  mutate(hospital_name = case_when(hospital_name %in% wrong_hosp ~ "Other Hospital Listed", 
                                   .default = hospital_name))

checks <- all_ras_procs %>% 
  mutate(op_year = format(as.Date(date_of_main_operation, format="%d/%m/%Y"),"%Y")) %>% 
  group_by(op_year, specialty_desc) %>% 
  summarise(n = n())


### Number of robotics procedures per month by hospital location ---------------
ppm <- all_ras_procs %>% 
  mutate(proc_mth_yr = format(as.Date(date_of_main_operation), "%Y-%m"),
         op_year = format(as.Date(date_of_main_operation, format="%d/%m/%Y"),"%Y")) %>% 
  group_by(hospital_name, proc_mth_yr, op_year) %>% 
  summarise(n = n()) 

write_parquet(ppm, paste0("../../../(06) Testing/Bex_testing/",  "mgmt_data/util_procsmth.parquet"))

### Mean daily number of robotics procedures by day per month ------------------
ppd <- all_ras_procs %>% 
  mutate(proc_mth_yr = format(as.Date(date_of_main_operation), "%Y-%m"),
         op_year = format(as.Date(date_of_main_operation, format="%d/%m/%Y"),"%Y"),
         dow = factor(format(as.Date(date_of_main_operation, format="%d/%m/%Y"),"%A"))) %>%
  group_by(hospital_name, proc_mth_yr, date_of_main_operation, op_year, dow, .drop = FALSE) %>% #need a fill to capture all days?
  summarise(n = n()) %>% 
  group_by(hospital_name, proc_mth_yr, op_year, dow, .drop = FALSE) %>% 
  summarise(mean_procs_pd = round(mean(n), 2))

write_parquet(ppd, paste0("../../../(06) Testing/Bex_testing/", "mgmt_data/util_procsday.parquet"))

### Specialty of surgeries number by month -------------------------------------
pps <- all_ras_procs %>% 
  mutate(proc_mth_yr = format(as.Date(date_of_main_operation), "%Y-%m")) %>% 
  group_by(proc_mth_yr, specialty_desc, hospital_name) %>% 
  summarise(n = n())

write_parquet(pps, paste0("../../../(06) Testing/Bex_testing/", "mgmt_data/util_procspec.parquet"))

### Sessions per day by specialty?

