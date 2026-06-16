#################################.
### Get latest data (minimal) ###
#################################.

# Author:
# Bex Madden
# Date: 16/06/2026


get_latest_data <- function(){
  read_parquet(paste0(data_dir, "monthly_extract/srasa_smr_extract_min.parquet"))
}