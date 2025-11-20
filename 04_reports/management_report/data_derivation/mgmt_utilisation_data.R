######################################################.
#### SRASA Management Report - Data - Utilisation ####
######################################################.

#Author: Bex Madden
#Date:06/11/2025

# Derivation of source data tables for the utilisation-related measures shown in the 
# SRASA management report

### Load packages --------------------------------------------------------------
data_dir <- "../../../../../../(06) Testing/Bex_testing/" # temporary location
source(paste0(data_dir, "load_packages_srasa.R"))

### Read in cleaned data from Theatres -----------------------------------------
# This may change to use Intuitive's data only
#or time + date-based linkage between theatres and intuitive

ut_data <- read.csv(paste0(data_dir, "theatres_data.csv")) %>% 
  clean_names() %>% 
  arrange(case_id) %>% 
  group_by(case_id, nhs_board, procedure_start_date) %>% 
  filter(any(procedure_code %in% robotics_list)) %>% # want all robotic surgery not only that in the candidate list i think?
  mutate(
    #    approach = case_when(any(procedure_code %in% robotics_list) ~ "RAS",
    #                           any(procedure_code %in% minimal_list) ~ "MIA",
    #                           any(procedure_code %in% robotic_conv_list) ~ "RAS conv open",
    #                           any(procedure_code %in% minimal_conv_list) ~ "MIA conv open",
    #                           .default = "NOS"),
    #    approach = as.factor(approach),
         candidate_proc = case_when(procedure_code %in% candidate_list ~ procedure_code,
                                    .default = NA_character_),
         candidate_desc = case_when(procedure_code %in% candidate_list ~ procedure,
                                    .default = NA_character_)) %>% 
  #fill(candidate_proc, .direction = "downup") %>%  # what about when there are 2 valid procedures?
  mutate(procedure_start_date = as.Date(procedure_start_date),
         op_year = format(as.Date(min(procedure_start_date), format="%Y-%m-%d"),"%Y"))

# candidate_codes <- read_csv("../../../../../../(12) Data/Lookups/ras_procedure_codes.csv") %>%  #move all this to r profile
#   rename(op_specialty = specialty)
# candidate_list <- dplyr::pull(candidate_codes, code)
# 
# approach_codes <- read_csv("../../../../../../(12) Data/Lookups/approach_codes.csv") 
# approach_list <- dplyr::pull(approach_codes, approach_code)
# 
# robotics_list <- approach_codes$approach_code[!is.na(approach_codes$robotic)]
# minimal_list <- approach_codes$approach_code[!is.na(approach_codes$minimal)]
# robotic_conv_list <- approach_codes$approach_code[!is.na(approach_codes$robotic_conv)]
# minimal_conv_list <- approach_codes$approach_code[!is.na(approach_codes$minimal_conv)]

freq_codes <- ut_data %>% 
  group_by(procedure, procedure_code) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq)) %>% #245 different procedures coded
  mutate(candidate = case_when(procedure_code %in% candidate_list ~ "proc",
                               procedure_code %in% approach_list ~ "app",
                               startsWith(procedure_code, "Z") ~ "suppl",
                               .default = NA_character_)) %>% 
  group_by(candidate) %>% 
  mutate(cand_tot = sum(freq))

freq <- freq_codes %>% 
  ungroup() %>% 
  select(candidate, cand_tot) %>% 
  distinct()
#  candidate  cand_tot
#  app           1917
#  NA            1583
#  suppl          590
#  proc           684

### Number of robotics procedures per month by hospital location ---------------
ppm <- ut_data %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(proc_mth_yr = format(as.Date(procedure_start_date), "%Y-%B")) %>% 
  group_by(hospital, proc_mth_yr) %>% 
  summarise(n = n()) #looks really bad because robotics so poorly coded, but format will be accurate for better data later on

### Mean daily number of robotics procedures per month -------------------------
ppd <- ut_data %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(proc_date = format(as.Date(procedure_start_date), "%Y-%m-%d"),
         proc_mth_yr = format(as.Date(procedure_start_date), "%Y-%B")) %>%
  group_by(hospital, proc_mth_yr, proc_date) %>% 
  summarise(n = n()) %>% 
  group_by(hospital, proc_mth_yr) %>% 
  summarise(mean_procs_pd = round(mean(n), 2))

### Mean no. hours robot in use per month --------------------------------------
use_hrs <- ut_data %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(proc_date = format(as.Date(procedure_start_date), "%Y-%m-%d"),
         proc_mth_yr = format(as.Date(procedure_start_date), "%Y-%B")) %>% 
  group_by(hospital, proc_mth_yr, proc_date) %>% 
  summarise(daily_hrs = sum(det_procedural_hours)) %>% 
  group_by(hospital, proc_mth_yr) %>% 
  summarise(mean_hrs_pd <- round(mean(daily_hrs), 2))

