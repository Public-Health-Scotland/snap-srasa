###################################################################################.
### SNAP SRASA - Identify Oropharyngeal cancer cases from Monthly SMR01 extract ###
###################################################################################.

# Bex Madden 
# 11/08/2026


identify_oropharynx_diags <- function(df){
  
  #' Identifies oropharyngeal cancer patients from raw monthly extract of SMR01 
  #' data for SRASA and flags whether they have had any RAS procedure to address their diagnosis
  #'
  #' @description This function wrangles the raw SMR01 data extract to identify 
  #' oropharyngeal cancer patients with/without RAS surgeries for ENT key procedure.
  #' 
  #' @param df - the raw monthly SMR01 extract
  #' 
  #' @usage identify_oropharynx_diags(df)
  #'
  #' @details Splitting operation codes into 4-digit a & b codes, identifying 
  #' which appear int he candidate list. labelling approach as RAS, MIA or NOS. 
  #' Finding RAS procedures which are not in the candidate list. Compiling the 
  #' first RAS rpocedures into the 'main_op_xxx' columns for usage
 
  
  # Get lookup -----------------------------------------------------------------
  oropharynx_lookup <- read_csv(paste0(lookup_dir, "diagnostics/diag_codes_ent_oropharyngeal.csv")) %>% 
    pull(icd10)
  
  # Identify diagnoses ---------------------------------------------------------
  cli_progress_step("Identifying Oropharyngeal cancer patients...")
  
  oropharynx_data <- df %>% 
   group_by(upi_number, link_no, cis_marker) %>% 
    mutate(cancer_oropharynx = case_when(diag1 %in% oropharynx_lookup ~ TRUE,
                                         diag2 %in% oropharynx_lookup ~ TRUE, #only diag 1 and 2 as it should be primary diagnosis for that CIS to get treatment
                                         .default = NA),
           ras_oropharynx = case_when(any(ras_proc == TRUE & cancer_oropharynx == TRUE) ~ "RAS",
                                      any(cancer_oropharynx == TRUE) ~ "Non-RAS",
                                      .default = NA))
  
  # Return df -----------
  return(oropharynx_data)
}