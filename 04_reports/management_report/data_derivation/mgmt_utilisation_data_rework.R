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
ras_min_data <- read_parquet(paste0(data_dir, "monthly_extract/srasa_smr_extract_min_", #min data is only 1 row per proc
                                      format(Sys.Date(), "%Y-%m"), ".parquet")) %>%
  filter(op1_date >= start_date & 
           op1_date < latest_date) %>% 
  ungroup() 

wrong_hosp <- ras_min_data %>% #get list of hospital names that appear but do not have a robot
  filter(hosp_has_robot != "Yes") %>% 
  group_by(hospital_name) %>% 
  slice(1) %>% 
  dplyr::pull(., hospital_name)

ras_util_data <- ras_min_data %>% #some aberrant coding liekly due to transfers
  mutate(hospital_name_grp = case_when(hospital_name %in% wrong_hosp ~ "Other Hospital Listed", #contains non-RAS and private hospitals
                                   .default = hospital_name),
         hospital_name_grp = factor(hospital_name_grp, levels = hosp_order))

### Number of robotics procedures per month by hospital location ---------------
util_procsmth <- ras_util_data %>% 
  mutate(op_mth = floor_date(op1_date, "month"),
         op_year = format(as.Date(op1_date, format="%d/%m/%Y"),"%Y")) %>% 
  group_by(hospital_name_grp, op_mth, op_year) %>% 
  summarise(n = n()) %>% 
  ungroup() #compares ok with previous approach - one or 2 more or less per month

write_parquet(util_procsmth, paste0(data_dir,  "management_report/util_procsmth.parquet"))

### Mean daily number of robotics procedures by day per month ------------------
util_procsday <- ras_util_data %>% 
  mutate(op_mth = floor_date(op1_date, "month"),
         op_year = format(as.Date(op1_date, format="%d/%m/%Y"),"%Y"),
         dow = factor(format(as.Date(op1_date, format="%d/%m/%Y"),"%A"),
                      levels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                 "Friday", "Saturday", "Sunday"))) %>%
  group_by(hospital_name_grp, op_year, op_mth, op1_date, dow) %>% 
  summarise(n = n()) %>% 
  group_by(hospital_name_grp, op_year, op_mth, dow, .drop = FALSE) %>% #is drop=F working fully? it should be possible to have a mean per day <1 e.g. if surgery not done every monday
  summarise(mean_procs_pd = round(mean(n), 2)) %>% 
  mutate(mean_procs_pd = ifelse(is.nan(mean_procs_pd), 0, mean_procs_pd)) %>% 
  ungroup() 

write_parquet(util_procsday, paste0(data_dir, "management_report/util_procsday.parquet"))

