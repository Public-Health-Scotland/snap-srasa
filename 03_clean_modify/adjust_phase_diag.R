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
           
           main_op_phase = replace_when(main_op_phase, 
                                        #is.na(phasing_diagnostics) & main_op_phase == "non-priority" ~ "non-priority",
                                        phasing_diagnostics == "cancer_ent" &
                                          main_op_code %in% diag_dependent_ent ~ "phase1", #changed lookup to make ent phase 2 by default, so now if dig = ent cancer then switch it to phase 1
                                        phasing_diagnostics == "cancer_ovarian" &
                                          main_op_code %in% diag_dependent_gynae ~ "other",
                                        phasing_diagnostics == "cancer_endometrial" &
                                          main_op_code %in% diag_dependent_gynae ~ "phase1", #hysterectomy default is phase2 for any cancer (except ovarian) and benign cases, if endometrial cancer make it phase 1
                                        phasing_diagnostics == "obesity_gastro" &
                                          main_op_code %in% diag_dependent_gastro ~ "other")) #if gastro and obesity = non-priority, otherwise default is phase 2
 
  return(diag_adj_data)
  
}