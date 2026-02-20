##################################################.
### SNAP SRASA - Adjust phasing with diagnosis ###
##################################################.

# Bex Madden 
# 09/02/2026


adjust_phase_diag <- function(df){
  
  #' Adjust cleaned extract of SMR01 data for SRASA, to update phase label
  #' applied to candidate procedures according to certain diagnoses
  #'
  #' @description 
  #' 
  #' @usage 
  #'
  #' @details 
  
  ### Flag necessary diagnoses and adjust main_op_phase label ------------------
  cli_progress_step("Reapplying phase labels for certain diagnoses...")
  
  #read in lists of codes
  diag_codes_ent <- read.csv(paste0(lookup_dir, "diagnostics/diag_codes_ent.csv")) %>% 
    dplyr::pull(icd10_code)
  obesity_codes_gastro <- read.csv(paste0(lookup_dir, "diagnostics/obesity_codes_gastro.csv")) %>% 
    dplyr::pull(icd10_code)
  
  diag_dependent_ent <- read.csv(paste0(lookup_dir, "diagnostics/diag_dependent_ent.csv")) %>% 
    dplyr::pull(opcs_code)
  diag_dependent_gynae <- read.csv(paste0(lookup_dir, "diagnostics/diag_dependent_gynae.csv")) %>% 
    dplyr::pull(opcs_code)
  diag_dependent_gastro <- read.csv(paste0(lookup_dir, "diagnostics/diag_dependent_gastro.csv")) %>% 
    dplyr::pull(opcs_code)
  
  diag_adj_data <- df %>% 
    mutate(phasing_diagnostics = case_when(diag1 %in% diag_codes_ent |
                                             diag2 %in% diag_codes_ent |
                                             diag3 %in% diag_codes_ent |
                                             diag4 %in% diag_codes_ent |
                                             diag5 %in% diag_codes_ent |
                                             diag6 %in% diag_codes_ent ~ "cancer_ent",
                                           
                                           diag1 == "C541" |
                                             diag2 == "C541" |
                                             diag3 == "C541" |
                                             diag4 == "C541" |
                                             diag5 == "C541" |
                                             diag6 == "C541" ~ "cancer_endometrial",
                                           
                                           diag1 == "C56X" |
                                             diag2 == "C56X" |
                                             diag3 == "C56X" |
                                             diag4 == "C56X" |
                                             diag5 == "C56X" |
                                             diag6 == "C56X" ~ "cancer_ovarian", # no need for other gynae cancer diagnostics as benign+non-ovarian non-endo cancers make up phase 2 
                                           
                                           diag1 %in% obesity_codes_gastro |
                                             diag2 %in% obesity_codes_gastro |
                                             diag3 %in% obesity_codes_gastro |
                                             diag4 %in% obesity_codes_gastro |
                                             diag5 %in% obesity_codes_gastro |
                                             diag6 %in% obesity_codes_gastro ~ "obesity_gastro",
                                           
                                           .default = NA),
           
           main_op_phase = case_when(main_op_code %in% diag_dependent_ent & ## For some reason this wasn't working when in a single case_when, retry another time for neatness
                                       is.na(phasing_diagnostics) | 
                                       phasing_diagnostics != "cancer_ent" ~ "phase2", 
                                     .default = main_op_phase), #if ent and NOT flagged as cancer = phase 2, otherwise phase 1
           main_op_phase = case_when(main_op_code %in% diag_dependent_gynae &
                                       phasing_diagnostics == "cancer_ovarian" ~ "non-priority",
                                     main_op_code %in% diag_dependent_gynae &
                                       phasing_diagnostics == "cancer_endometrial" ~ "phase1", 
                                     .default = main_op_phase),#hysterectomy default is phase2 for any cancer (except ovarian) and benign cases, if endometrial cancer make it phase 1
           main_op_phase = case_when(main_op_code %in% diag_dependent_gastro &
                                       phasing_diagnostics == "obesity_gastro" ~ "non-priority", #if gastro and obesity = non-priority, otherwise phase 2
                                     .default = main_op_phase))
  
  return(diag_adj_data)
  
}