#######################################################.
#### SRASA Management Report - Data - Data Quality ####
#######################################################.

#Author: Bex Madden
#Date:10/12/2025

# Comparison of aggregated smr01 utilisation data to DUMMY Intuitive data

### read in aggregated smr01 data ----------------------------------------------
smr01_procsmth <- read_parquet(paste0(data_dir, "management_report/util_procsmth.parquet")) 

### read in intuitive data -----------------------------------------------------
intuitive_data <- read_parquet(paste0(data_dir, "intuitive/intuitive_rolling_data.parquet"))

# and lookup for adjusting specialty labelling to match SRASA labels
intuitive_lookup <- read.csv(paste0(data_dir, "lookups/intuitive_spec_lookup.csv")) 

### make aggregate stats for intuitive data ------------------------------------
intuitive_comp <- intuitive_data %>% 
  left_join(intuitive_lookup, by = join_by(procedure_type == int_procedure)) %>% 
  group_by(hospital_name, srasa_specialty, op_month) %>% 
  summarise(monthly_n = n()) %>% 
  ungroup() 

# aggregate to hospital level
intuitive_hospwide <- intuitive_comp %>% 
  group_by(hospital_name, op_month) %>% 
  summarise(int_n = sum(monthly_n))

### merge smr01 and intutiive aggregated data ----------------------------------
dq_compprocs <- smr01_procsmth %>% 
  left_join(intuitive_hospwide, by = join_by(hospital_name_grp == hospital_name, op_mth == op_month)) %>% 
  mutate(diff = int_n-n,
         prop_missed = round(diff/int_n*100, 2)) %>% 
  group_by(hospital_name_grp) %>% 
  mutate(mean_diff = round(mean(diff, na.rm = TRUE), 2),
         mean_prop_missed = round(mean(prop_missed, na.rm = TRUE), 2))

### save out data --------------------------------------------------------------
write_parquet(dq_compprocs, paste0(data_dir, "management_report/dq_compprocs.parquet"))
 
