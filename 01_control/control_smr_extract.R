#########################################################.
### SNAP SRASA - Monthly SMR01 extract control script ###
#########################################################.

# Bex Madden & Dylan Lewis
# 01/12/2025

### source scripts -------------------------------------------------------------
list.files("./02_setup/", full.names = TRUE) %>% 
  walk(source)

### extract smr01 data ---------------------------------------------------------
extract_smr01_data() %>% 
  append_lookups(which_lookups = "all") %>% 
  
  # save extract
  write_parquet(paste0(data_dir, "monthly_extract/srasa_smr_extract_", 
                       format(Sys.Date(), "%Y-%m"), ".parquet"))

### ----------------------------------------------------------------------------