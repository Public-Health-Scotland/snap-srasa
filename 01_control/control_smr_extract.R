#########################################################.
### SNAP SRASA - Monthly SMR01 extract control script ###
#########################################################.

# Bex Madden & Dylan Lewis
# 01/12/2025

### source scripts -------------------------------------------------------------
list.files("./02_setup/", full.names = TRUE) %>% 
  map(source)

### extract smr01 data ---------------------------------------------------------
extract_smr01_data() %>% 
  append_lookups() %>% 
  
  # save extract
  write_parquet(paste0("../../../(12) Data/Monthly extract/srasa_smr_extract_", 
                       format(Sys.Date(), "%Y-%m"), ".parquet"))

### ----------------------------------------------------------------------------