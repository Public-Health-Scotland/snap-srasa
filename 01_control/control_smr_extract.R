#########################################################.
### SNAP SRASA - Monthly SMR01 extract control script ###
#########################################################.

# Bex Madden & Dylan Lewis
# 01/12/2025

### source scripts -------------------------------------------------------------
list.files("./02_setup/", full.names = TRUE) %>% 
  walk(source)
list.files("./03_clean_modify/", full.names = TRUE) %>% 
  walk(source)

### extract smr01 data ---------------------------------------------------------
extract_smr01_data() %>% 
  identify_ras_procs() %>%
  adjust_phase_diag() %>%
  identify_cancer_diag() %>%
  append_date_vars() %>% 
  append_lookups(which_lookups = "all") %>% 
  save_monthly_data()
  

