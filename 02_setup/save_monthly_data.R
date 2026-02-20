###################################################.
### SNAP SRASA - Save out Monthly SMR01 extract ###
###################################################.

# Bex Madden 
# 13/02/2026

save_monthly_data <- function(df){
  
  #' Save out SRASA Monthly SMR01 extract
  #'
  #' @description 
  #' 
  #' @param df - an smr01 extract
  #' 
  #' @usage 
  
  cli_progress_step("Saving out data...")
  
  save_dfs <- df %>% 
  write_parquet(paste0(data_dir, "monthly_extract/srasa_smr_extract_", 
                       format(Sys.Date(), "%Y-%m"), ".parquet")) %>% 

  filter(!is.na(main_op_code)) %>% #one row per procedure
  select(date_record_inserted:ras_proc, main_op_code:main_op_approach, cancer_flag:cancer_unidentified) %>%  #only keep data columns required for majority of processes
  
    write_parquet(paste0(data_dir, "monthly_extract/srasa_smr_extract_min.parquet"))
  
  cli_progress_step("Monthly data extract complete!")
  cli_progress_done()
  
}