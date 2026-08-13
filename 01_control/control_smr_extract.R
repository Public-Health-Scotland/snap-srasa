#########################################################.
### SNAP SRASA - Monthly SMR01 extract control script ###
#########################################################.

# Bex Madden & Dylan Lewis
# 01/12/2025
# Requires 15gb session

### source scripts -------------------------------------------------------------
list.files("./02_setup/", full.names = TRUE) %>% 
  walk(source)
list.files("./03_clean_modify/", full.names = TRUE) %>% 
  walk(source)

### extract smr01 data ---------------------------------------------------------
extract_smr01_data() %>% 
  identify_ras_procs() %>%
  idenfity_oropharynx_diags() %>% 
  filter_extract() %>% 
  adjust_phase_diag() %>%
  identify_cancer_diag() %>%
  append_date_vars() %>% 
  append_lookups(which_lookups = "all") %>% 
  save_monthly_data()
  
# periodical DQ checks - look out for messages in console
monthly_extract_min <- read_parquet(paste0(data_dir, "monthly_extract/srasa_smr_extract_min.parquet"))

  dq_unlisted_procs(monthly_extract_min) 
  dq_emergency_procs(monthly_extract_min)
  

