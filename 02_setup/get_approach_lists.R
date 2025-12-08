################################################.
### SNAP SRASA - Read in approach code lists ###
################################################.

# Bex Madden
# 04/12/2025

  
  approach_codes <- read_csv(paste0(lookup_dir, "approach_codes.csv"), 
                             show_col_types = FALSE)
  
  approach_list <- dplyr::pull(approach_codes, approach_code)
  robotics_list <- approach_codes$approach_code[!is.na(approach_codes$robotic)]
  minimal_list <- approach_codes$approach_code[!is.na(approach_codes$minimal)]
  robotic_conv_list <- approach_codes$approach_code[!is.na(approach_codes$robotic_conv)]
  minimal_conv_list <- approach_codes$approach_code[!is.na(approach_codes$minimal_conv)]
  
 rm(approach_codes)
