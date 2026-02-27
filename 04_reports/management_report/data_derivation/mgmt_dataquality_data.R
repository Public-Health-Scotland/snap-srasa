#######################################################.
#### SRASA Management Report - Data - Data Quality ####
#######################################################.

#Author: Bex Madden
#Date:10/12/2025

# Comparison of aggregated smr01 utilisation data to Intuitive data

### read in intuitive data -----------------------------------------------------
intuitive_data <- read_parquet(paste0(data_dir, "intuitive/intuitive_rolling_data.parquet"))

# and lookup for adjusting specialty labelling to match SRASA labels
intuitive_lookup <- read.csv(paste0(data_dir, "lookups/intuitive_spec_lookup.csv")) 

### make monthly total for intuitive data ------------------------------------
intuitive_comp <- intuitive_data %>% 
  left_join(intuitive_lookup, by = join_by(procedure_type == int_procedure)) %>% 
  group_by(hospital_name, srasa_specialty, op_month) %>% 
  summarise(monthly_n = n()) %>% 
  ungroup() 

# aggregate to hospital level
intuitive_hospwide <- intuitive_comp %>% 
  group_by(hospital_name, op_month) %>% 
  summarise(int_n = sum(monthly_n))

### make comparison of intuititve and smr01 utilisation stats -----------------
#monthly, hospital-level

# read in aggregated smr01 data 
smr01_procsmth <- read_parquet(paste0(data_dir, "management_report/util_procsmth.parquet")) 

#combine smr01 and intutiive monthly data
dq_comp <- smr01_procsmth %>% 
  left_join(intuitive_hospwide, by = join_by(hospital_name_grp == hospital_name, 
                                             op_mth == op_month)) %>% 
  mutate(diff = int_n-n,
         prop_missed = round(diff/int_n*100, 2)) %>% 
  group_by(hospital_name_grp) %>% 
  mutate(mean_diff = round(mean(diff, na.rm = TRUE), 2),
         mean_prop_missed = round(mean(prop_missed, na.rm = TRUE), 2))

#save out data
write_parquet(dq_comp, paste0(data_dir, "management_report/dq_comp.parquet"))


### Specialty-level comparison of intuitive and smr01 utilisation stats --------

#set min and max dates for intutivie data to match smr01
latest_date <- Sys.Date() %>% 
  lubridate::floor_date("month") %m-% months(3)

start_date <- latest_date %>% 
  lubridate::floor_date("month") %m-% months(12)

# tidy intuitive data
int_spec <- intuitive_comp %>% 
  rename(intuitive = monthly_n) %>% 
  filter(op_month >= start_date & 
           op_month < latest_date)

# read in hospital lookup 
hosp_lookup <- read_csv(paste0(lookup_dir, "NHSScotland_hospitals.csv")) %>% 
  select(hospital_name, health_board)

#read in smr01 specialty-level data and join in intuitive
dq_compspec <- read_parquet(paste0(data_dir, "management_report/spec_procsmth.parquet")) %>% 
  filter(ras_proc == "RAS",
         hospital_name_grp != "All") %>% 
  select(-ras_proc, -op_year, -hosp_health_board) %>% 
  rename(smr01 = n) %>% 
  full_join(int_spec, by = join_by(hospital_name_grp == hospital_name, 
                                   op_mth == op_month, 
                                   main_op_specialty == srasa_specialty)) %>% 
  mutate(intuitive = replace_na(intuitive, 0),
         smr01 = replace_na(smr01, 0)) %>% 
  left_join(hosp_lookup, by = join_by(hospital_name_grp == hospital_name)) %>% 
  pivot_longer(smr01:intuitive, names_to = "dataset", values_to = "n_procs")

#save out data
write_parquet(dq_compspec, paste0(data_dir, "management_report/dq_compspec.parquet"))

