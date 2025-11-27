######################################################.
#### SRASA Management Report - Data - Utilisation ####
######################################################.

#Author: Bex Madden
#Date:06/11/2025

# Derivation of source data tables for the utilisation-related measures shown in the 
# SRASA management report

### Load packages --------------------------------------------------------------
data_dir <- "../../../(06) Testing/Bex_testing/" # temporary location

### Read in raw data from SMR01, no filtering for procedure --------------------
smr01_extract <- read_parquet(paste0(data_dir, "smr01_test_data2.parquet")) %>% 
  clean_names()
# source("./02_setup/extract_smr01_data.R")
# smr01_extract <- extract_smr01_data()

ras_clean_data <- smr01_extract %>% 
  
  # split procedure codes by a and b position
  separate_wider_position(main_operation, c(op1a = 4, op1b = 4), too_few = "align_start")  %>% # always 4 digit codes?
  separate_wider_position(other_operation_1, c(op2a = 4, op2b = 4), too_few = "align_start") %>% 
  separate_wider_position(other_operation_2, c(op3a = 4, op3b = 4), too_few = "align_start") %>% 
  separate_wider_position(other_operation_3, c(op4a = 4, op4b = 4), too_few = "align_start") %>% 
  
  mutate(op1_approach = case_when(op1b %in% approach_list ~ op1b, #make new column for approach codes only
                                  !(op1b %in% approach_list) ~ "NOS"), #if no matching approach code call it 'NOS'
         op2_approach = case_when(op2b %in% approach_list ~ op2b,
                                  !(op2b %in% approach_list) ~ "NOS"),
         op3_approach = case_when(op3b %in% approach_list ~ op3b,
                                  !(op3b %in% approach_list) ~ "NOS"),
         op4_approach = case_when(op4b %in% approach_list ~ op4b,
                                  !(op4b %in% approach_list) ~ "NOS")) # need to add in is.na = "NOS" too

approach_vec <- c("op1_approach", "op2_approach", "op3_approach", "op4_approach") #label ras/minimally invasive/NoS instead of codes
ras_clean_data <- ras_clean_data %>% 
  mutate(across(all_of(approach_vec), ~ case_when(. %in% robotics_list ~ "RAS",
                                                  . %in% minimal_list ~ "MIA",
                                                  . == "NOS" ~ "NOS",
                                                  #is.na(.) ~ "NOS", # need to add in is.na = "NOS" too test tthis
                                                  . == "Y721" ~ "RAS conv open",
                                                  . %in% c("Y714", "Y722") ~ "MIA conv open",
                                                  .default = NA_character_)),
         ras_proc = case_when(op1_approach == "RAS" | op1_approach == "RAS conv open" ~ "RAS",
                              op1_approach == "RAS" | op1_approach == "RAS conv open" ~ "RAS",
                              op1_approach == "RAS" | op1_approach == "RAS conv open" ~ "RAS",
                              op1_approach == "RAS" | op1_approach == "RAS conv open" ~ "RAS",
                              .default = NA_character_)) %>% 
  group_by(link_no, cis_marker) %>% 
  filter(any(ras_proc == "RAS"))

specialty_lookup <- read.csv("../../../(12) Data/Lookups/smr01_specialty_lookup.csv") %>% 
  rename(specialty_desc = Value)
hosp_lookup <- read.csv("../../../(12) Data/Lookups/NHSScotland_hospitals.csv") %>% 
  select(hospital_code, hospital_name)

all_ras_procs <- ras_clean_data %>% 
  ungroup() %>% 
  filter(ras_proc == "RAS") %>% # 1 row per proc
  left_join(specialty_lookup, by = join_by(specialty == Code)) %>% 
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
                                    .default = specialty_desc)) %>% 
  left_join(hosp_lookup, by = join_by(location == hospital_code))

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

# gastro <- all_ras_procs %>%
#   filter(specialty_desc == "Gastroenterology")# | specialty_desc == "Paediatric Surgery")

ut_data <- all_ras_procs %>% #only counting each stay once? or should we count each operation?
  group_by(link_no, cis_marker) %>% 
  arrange(date_of_main_operation) %>% 
  slice(1)





### Number of robotics procedures per month by hospital location ---------------
ppm <- all_ras_procs %>% 
  mutate(proc_mth_yr = format(as.Date(date_of_main_operation), "%Y-%m")) %>% 
  group_by(hospital_name, proc_mth_yr) %>% 
  summarise(n = n()) 

### Mean daily number of robotics procedures per month -------------------------
ppd <- all_ras_procs %>% 
  mutate(proc_date = format(as.Date(date_of_main_operation), "%Y-%m-%d"),
         proc_mth_yr = format(as.Date(date_of_main_operation), "%Y-%m")) %>%
  group_by(hospital_name, proc_mth_yr, proc_date) %>% 
  summarise(n = n()) %>% 
  group_by(hospital_name, proc_mth_yr) %>% 
  summarise(mean_procs_pd = round(mean(n), 2))

### Specialty of surgeries number by month -------------------------------------
pps <- all_ras_procs %>% 
  mutate(proc_mth_yr = format(as.Date(date_of_main_operation), "%Y-%m")) %>% 
  group_by(proc_mth_yr, specialty_desc, hospital_name) %>% 
  summarise(n = n())
  
### Sessions per day by specialty?

