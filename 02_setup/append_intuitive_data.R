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
  
  intuitive_rolling <- read_parquet(paste0(data_dir, "intuitive/intuitive_rolling_data.parquet")) %>% 
    bind_rows(df)
  
  ## INSERT SOME CHECKS FOR DUPLICATION HERE
  
  write_parquet(intuitive_rolling, paste0(data_dir, "intuitive/intuitive_rolling_data.parquet"))
  
  cli_progress_step("Intuitive data saved.")
  cli_progress_done()
  
}


