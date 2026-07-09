#####################################################################.
### SNAP SRASA - Append Intuitive monthly extract to rolling data ###
#####################################################################.

# Bex Madden 
# 10/02/2026


append_intuitive_data <- function(df){
  
  #' Append compiled monthly data to master df of rollign monthly data 
  #' 
  #' @description 
  #' 
  #' @usage 
  #'
  #' @details 
  
  intuitive_rolling <- read_parquet(paste0(data_dir, "intuitive/intuitive_rolling_data.parquet")) 
 
  if(max(intuitive_rolling$start_date) >= max(df$start_date)){
    message("This month's data is already present")
  } else {
    intuitive_rolling <- bind_rows(intuitive_rolling, df)
    
    int_dup_check <- intuitive_rolling %>% 
      group_by(start_date, start_time, system_serial_number) %>% 
      summarise(n=n()) %>% 
      filter(n > 1)
    
    if(nrow(int_dup_check) >= 1){
      message("Duplicate rows present in data, saving as intuitive_dup_data.csv")
      write.csv(int_dup_check, paste0(data_dir, "intuitive/intuitive_dup_data.csv"))
      }else{
        write_parquet(intuitive_rolling, paste0(data_dir, "intuitive/intuitive_rolling_data.parquet"))
        
        cli_progress_step("Intuitive data saved.")
        }

  }
  
  cli_progress_done()

}


