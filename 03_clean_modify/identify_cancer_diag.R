##############################################.
### SNAP SRASA - Identify cancer diagnoses ###
##############################################.

# Bex Madden 
# 09/02/2026


identify_cancer_diag <- function(df){
  
  #' Adjust cleaned extract of SMR01 data for SRASA, to identify relevant 
  #' cancer diagnoses in records where candidate procs have been performed
  #'
  #' @description 
  #' 
  #' @usage 
  #'
  #' @details 
  
  ### Flag ANY (malignant) cancer diagnosis ------------------------------------
  cli_progress_step("Identifying cancer-related surgeries...")
  
  flagged_data <- df %>% 
  mutate_at(vars(starts_with("diag")), 
            list(flag = ~ if_else(substr(., 1, 3) %in% #d37-d48 neoplasms of unknown behaviour - include?
                                    c("C00","C01","C02","C03","C04","C05","C06","C07","C08","C09",
                                      "C10","C11","C12","C13","C14","C15","C16","C17","C18","C19",
                                      "C20","C21","C22","C23","C24","C25","C26",
                                      "C30","C31","C32","C33","C34","C37","C38","C39",
                                      "C40","C41","C43","C44","C45","C46","C47","C48","C49",
                                      "C50","C51","C52","C53","C54","C55","C56","C57","C58",
                                      "C60","C61","C62","C63","C64","C65","C66","C67","C68","C69",
                                      "C70","C71","C72","C73","C74","C75","C76","C77","C78", "C79",
                                      "C80","C81","C82","C83","C84","C85","C86","C88",
                                      "C90","C91","C92","C93","C94","C95","C96","C97"), #non opcs from joe: "140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","174","175","176","177","178","179","180","181","182","183","184","185","186","187","188","189","190","191","192","193","194","195","200","201","202","203","204","205","206","207","208",
                                  1, 0))) %>% 
    #group_by(link_no, cis_marker) %>% 
    mutate(cancer_flag = rowSums(across(diag1_flag:diag6_flag), na.rm = T),
           cancer_flag = case_when(cancer_flag > 0 ~ TRUE,
                                   cancer_flag == 0 ~ FALSE, #if doing on grouped data add any()
                                   .default = NA)) #%>% 
    #ungroup()

### Identify cancer diagnoses specific to the specialty of procedure -----------
  
  #read in lists of cancer (malignant) codes
  diag_codes_colorectal <- read.csv(paste0(lookup_dir, "diagnostics/diag_codes_colorectal.csv")) %>% 
    dplyr::pull(icd10_code)
  diag_codes_ent <- read.csv(paste0(lookup_dir, "diagnostics/diag_codes_ent.csv")) %>% 
    dplyr::pull(icd10_code)
  diag_codes_gastro <- read.csv(paste0(lookup_dir, "diagnostics/diag_codes_gastro.csv")) %>% 
    dplyr::pull(icd10_code)
  diag_codes_gynae <- read.csv(paste0(lookup_dir, "diagnostics/diag_codes_gynae.csv")) %>% 
    dplyr::pull(icd10_code)
  diag_codes_hepatic <- read.csv(paste0(lookup_dir, "diagnostics/diag_codes_hepatic.csv")) %>% 
    dplyr::pull(icd10_code)
  diag_codes_thoracic <- read.csv(paste0(lookup_dir, "diagnostics/diag_codes_thoracic.csv")) %>% 
    dplyr::pull(icd10_code)
  diag_codes_urology <- read.csv(paste0(lookup_dir, "diagnostics/diag_codes_urology.csv")) %>% 
    dplyr::pull(icd10_code)
  
  #read in lists of non-malignant neoplasm codes, as a neoplasm may be treated as if it were cancer until proven otherwise
  # nonmal_codes_colorectal <- read.csv(paste0(lookup_dir, "diagnostics/diag_codes_colorectal_nonmal.csv")) %>% 
  #   dplyr::pull(icd10_code)
  # nonmal_codes_ent <- read.csv(paste0(lookup_dir, "diagnostics/diag_codes_ent_nonmal.csv")) %>% 
  #   dplyr::pull(icd10_code)
  # nonmal_codes_gastro <- read.csv(paste0(lookup_dir, "diagnostics/diag_codes_gastro_nonmal.csv")) %>% 
  #   dplyr::pull(icd10_code)
  # nonmal_codes_gynae <- read.csv(paste0(lookup_dir, "diagnostics/diag_codes_gynae_nonmal.csv")) %>% 
  #   filter(cancer_type != "gynae_fibroid") %>% 
  #   dplyr::pull(icd10_code)
  # nonmal_codes_hepatic <- read.csv(paste0(lookup_dir, "diagnostics/diag_codes_hepatic_nonmal.csv")) %>% 
  #   dplyr::pull(icd10_code)
  # nonmal_codes_thoracic <- read.csv(paste0(lookup_dir, "diagnostics/diag_codes_thoracic_nonmal.csv")) %>% 
  #   dplyr::pull(icd10_code)
  # nonmal_codes_urology <- read.csv(paste0(lookup_dir, "diagnostics/diag_codes_urology_nonmal.csv")) %>% 
  #   dplyr::pull(icd10_code)
  
  diag_data <- flagged_data %>% 
    mutate(cancer_surgery = case_when(main_op_specialty == "Colorectal" & 
                                     (diag1 %in% diag_codes_colorectal |
                                        diag2 %in% diag_codes_colorectal |
                                        diag3 %in% diag_codes_colorectal |
                                        diag4 %in% diag_codes_colorectal |
                                        diag5 %in% diag_codes_colorectal |
                                        diag6 %in% diag_codes_colorectal) ~ "Colorectal",
                                   
                                   main_op_specialty == "ENT" & 
                                     (diag1 %in% diag_codes_ent |
                                        diag2 %in% diag_codes_ent |
                                        diag3 %in% diag_codes_ent |
                                        diag4 %in% diag_codes_ent |
                                        diag5 %in% diag_codes_ent |
                                        diag6 %in% diag_codes_ent) ~ "ENT",
                                   
                                   main_op_specialty == "Gastrointestinal" & 
                                     (diag1 %in% diag_codes_gastro |
                                        diag2 %in% diag_codes_gastro |
                                        diag3 %in% diag_codes_gastro |
                                        diag4 %in% diag_codes_gastro |
                                        diag5 %in% diag_codes_gastro |
                                        diag6 %in% diag_codes_gastro) ~ "Gastrointestinal",
                                   
                                   main_op_specialty == "Gynaecology" & 
                                     (diag1 %in% diag_codes_gynae |
                                        diag2 %in% diag_codes_gynae |
                                        diag3 %in% diag_codes_gynae |
                                        diag4 %in% diag_codes_gynae |
                                        diag5 %in% diag_codes_gynae |
                                        diag6 %in% diag_codes_gynae) ~ "Gynaecological",
                                   
                                   main_op_specialty == "Hepatobiliary" & 
                                     (diag1 %in% diag_codes_hepatic |
                                        diag2 %in% diag_codes_hepatic |
                                        diag3 %in% diag_codes_hepatic |
                                        diag4 %in% diag_codes_hepatic |
                                        diag5 %in% diag_codes_hepatic |
                                        diag6 %in% diag_codes_hepatic) ~ "Hepatobiliary",
                                   
                                   main_op_specialty == "Thoracic" & 
                                     (diag1 %in% diag_codes_thoracic |
                                        diag2 %in% diag_codes_thoracic |
                                        diag3 %in% diag_codes_thoracic |
                                        diag4 %in% diag_codes_thoracic |
                                        diag5 %in% diag_codes_thoracic |
                                        diag6 %in% diag_codes_thoracic) ~ "Thoracic",
                                   
                                   main_op_specialty == "Urology" & 
                                     (diag1 %in% diag_codes_urology |
                                        diag2 %in% diag_codes_urology |
                                        diag3 %in% diag_codes_urology |
                                        diag4 %in% diag_codes_urology |
                                        diag5 %in% diag_codes_urology |
                                        diag6 %in% diag_codes_urology) ~ "Urological", #any op with specialty 'General surgery' is hernia repair so cancer not relevant
                                   
                                   main_op_type == "Unlisted - colorectal" & 
                                     (diag1 %in% diag_codes_colorectal |
                                        diag2 %in% diag_codes_colorectal |
                                        diag3 %in% diag_codes_colorectal |
                                        diag4 %in% diag_codes_colorectal |
                                        diag5 %in% diag_codes_colorectal |
                                        diag6 %in% diag_codes_colorectal) ~ "Colorectal", 
                                   
                                   main_op_type == "Unlisted - gastrointestinal" & 
                                     (diag1 %in% diag_codes_gastro |
                                        diag2 %in% diag_codes_gastro |
                                        diag3 %in% diag_codes_gastro |
                                        diag4 %in% diag_codes_gastro |
                                        diag5 %in% diag_codes_gastro |
                                        diag6 %in% diag_codes_gastro) ~ "Gastrointestinal",
                                   
                                   main_op_type == "Unlisted - gynaecological" & 
                                     (diag1 %in% diag_codes_gynae |
                                        diag2 %in% diag_codes_gynae |
                                        diag3 %in% diag_codes_gynae |
                                        diag4 %in% diag_codes_gynae |
                                        diag5 %in% diag_codes_gynae |
                                        diag6 %in% diag_codes_gynae) ~ "Gynaecological",
                                   
                                   main_op_type == "Unlisted - hepatobiliary" & 
                                     (diag1 %in% diag_codes_hepatic |
                                        diag2 %in% diag_codes_hepatic |
                                        diag3 %in% diag_codes_hepatic |
                                        diag4 %in% diag_codes_hepatic |
                                        diag5 %in% diag_codes_hepatic |
                                        diag6 %in% diag_codes_hepatic) ~ "Hepatobiliary",
                                   
                                   .default = NA),
           
           
           cancer_unidentified = case_when(is.na(cancer_surgery) & cancer_flag == TRUE ~ TRUE,
                                           !is.na(cancer_surgery) & cancer_flag == TRUE ~ FALSE,
                                           .default = NA)) %>%  #this works but there are quite a lot of cases (~1.5%) - presumably where secondary has not been coded as such so primary cancer location is not relevant to spec of surgery performed
  #Need to exclude tonsillectomies unless cancer diagnosis is present (in which case it will be phase 2)
  filter_out(main_op_type %in% c("Tonsillectomy", "Other operations on tonsil") &
               main_op_phase == "phase2") 
  # #Could add to this -  exclude M65.3 unless cancer diagnosis is present?
  
return(diag_data)
  
}
