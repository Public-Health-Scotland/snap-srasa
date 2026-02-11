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
  #adjust_phase_diag() %>% 
  #identify_cancer_diag() %>% 
  append_lookups(which_lookups = "all") %>% 
  
  # save extract
  write_parquet(paste0(data_dir, "monthly_extract/srasa_smr_extract_", 
                       format(Sys.Date(), "%Y-%m"), ".parquet")) #%>% 
  # 
  # filter(!is.na(main_op_code)) %>% 
  # write_parquet(paste0(data_dir, "monthly_extract/srasa_smr_extract_min_", 
  #                      format(Sys.Date(), "%Y-%m"), ".parquet"))


### compile and save intuitive data --------------------------------------------
compile_intuitive_data()
