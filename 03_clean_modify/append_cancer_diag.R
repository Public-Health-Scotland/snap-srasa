###############################################################.
### SNAP SRASA - Identify cancer diagnoses - lookups method ###
###############################################################.

# Bex Madden 
# 17/09/2026


append_cancer_diag <- function(df){
  
  #' Extract cancer diagnosis codes and merge in list of speciality-relevant diagnoses
  #'
  #' @description This function identifies cancer diagnosis codes relevant to the 
  #' speciality of surgery and merges in information relating to that diagnosis
  #' 
  #' @usage append_cancer_diag(df)
  #'
  #' @details Creating speciality-specific columns in which icd-10 codes matching
  #' those in lookup lists are pasted, which are then united into a single column
  #' and used to merge in diagnosis-related information for further use
  
  cli_progress_step("Identifying cancer-related surgeries...")
  
### Load in diagnosis lists by speciality ------
  cancer_colorectal_df <- read.csv(paste0(data_dir, "lookups/diagnostics/cancer_codes_colorectal.csv"))
  cancer_colorectal <- cancer_colorectal_df %>% 
    pull(icd10_code)
  
  cancer_ent_df <- read.csv(paste0(data_dir, "lookups/diagnostics/cancer_codes_ent.csv"))
  cancer_ent <- cancer_ent_df %>% 
    pull(icd10_code)
  
  cancer_gynae_df <- read.csv(paste0(data_dir, "lookups/diagnostics/cancer_codes_gynae.csv")) # distinguishes endometriala nd ovarian neoplasms and fibroids - will require grep() on 'cancer_type'
  cancer_gynae <- cancer_gynae_df %>% 
    pull(icd10_code) 
  
  cancer_urology_df <- read.csv(paste0(data_dir, "lookups/diagnostics/cancer_codes_urology.csv"))
  cancer_urology <- cancer_urology_df %>% 
    pull(icd10_code)
  
  cancer_thoracic_df <- read.csv(paste0(data_dir, "lookups/diagnostics/cancer_codes_thoracic.csv"))
  cancer_thoracic <- cancer_thoracic_df %>% 
    pull(icd10_code)
  
  cancer_hepatic_df <- read.csv(paste0(data_dir, "lookups/diagnostics/cancer_codes_hepatic.csv"))
  cancer_hepatic <- cancer_hepatic_df %>% 
    pull(icd10_code)
  
  cancer_gastro_df <- read.csv(paste0(data_dir, "lookups/diagnostics/cancer_codes_gastro.csv"))
  cancer_gastro <- cancer_gastro_df %>% 
    pull(icd10_code)
  
  cancer_codes <- rbind(cancer_colorectal_df, cancer_ent_df, cancer_gynae_df, 
                        cancer_urology_df, cancer_thoracic_df, 
                        cancer_hepatic_df, cancer_gastro_df) %>% 
    distinct() %>% 
    mutate(icd10_desc = str_to_lower(icd10_desc))
  
  
  ### Pull out speciality-relevant cancer and non-malignant neoplasm ICD10 codes -----
  
  diag_data <- df %>% 
    mutate(diag_colorectal = case_when(main_op_specialty == "Colorectal" &
                                         diag1 %in% cancer_colorectal ~ diag1,
                                       main_op_specialty == "Colorectal" &
                                         diag2 %in% cancer_colorectal ~ diag2,
                                       main_op_specialty == "Colorectal" &
                                         diag3 %in% cancer_colorectal ~ diag3,
                                       main_op_specialty == "Colorectal" &
                                         diag4 %in% cancer_colorectal ~ diag4,
                                       main_op_specialty == "Colorectal" &
                                         diag5 %in% cancer_colorectal ~ diag5,
                                       main_op_specialty == "Colorectal" &
                                         diag6 %in% cancer_colorectal ~ diag6,
                                       .default = NA),
           diag_ent = case_when(main_op_specialty == "ENT" &
                                  diag1 %in% cancer_ent ~ diag1,
                                main_op_specialty == "ENT" &
                                  diag2 %in% cancer_ent ~ diag2,
                                main_op_specialty == "ENT" &
                                  diag3 %in% cancer_ent ~ diag3,
                                main_op_specialty == "ENT" &
                                  diag4 %in% cancer_ent ~ diag4,
                                main_op_specialty == "ENT" &
                                  diag5 %in% cancer_ent ~ diag5,
                                main_op_specialty == "ENT" &
                                  diag6 %in% cancer_ent ~ diag6,
                                .default = NA),
           diag_gynae = case_when(main_op_specialty == "Gynaecology" & # Identifies endometrial cancer as the priority
                                    diag1 == "C541" ~ diag1,
                                  main_op_specialty == "Gynaecology" &
                                    diag2 == "C541" ~ diag2,
                                  main_op_specialty == "Gynaecology" &
                                    diag3 == "C541" ~ diag3,
                                  main_op_specialty == "Gynaecology" &
                                    diag4 == "C541" ~ diag4,
                                  main_op_specialty == "Gynaecology" &
                                    diag5 == "C541" ~ diag5,
                                  main_op_specialty == "Gynaecology" &
                                    diag6 == "C541" ~ diag6,
                                  main_op_specialty == "Gynaecology" &
                                    diag1 %in% cancer_gynae ~ diag1,
                                  main_op_specialty == "Gynaecology" &
                                    diag2 %in% cancer_gynae ~ diag2,
                                  main_op_specialty == "Gynaecology" &
                                    diag3 %in% cancer_gynae ~ diag3,
                                  main_op_specialty == "Gynaecology" &
                                    diag4 %in% cancer_gynae ~ diag4,
                                  main_op_specialty == "Gynaecology" &
                                    diag5 %in% cancer_gynae ~ diag5,
                                  main_op_specialty == "Gynaecology" &
                                    diag6 %in% cancer_gynae ~ diag6,
                                  .default = NA),
           diag_urology = case_when(main_op_specialty == "Urology" &
                                      diag1 %in% cancer_urology ~ diag1,
                                    main_op_specialty == "Urology" &
                                      diag2 %in% cancer_urology ~ diag2,
                                    main_op_specialty == "Urology" &
                                      diag3 %in% cancer_urology ~ diag3,
                                    main_op_specialty == "Urology" &
                                      diag4 %in% cancer_urology ~ diag4,
                                    main_op_specialty == "Urology" &
                                      diag5 %in% cancer_urology ~ diag5,
                                    main_op_specialty == "Urology" &
                                      diag6 %in% cancer_urology ~ diag6,
                                    .default = NA),
           diag_thoracic = case_when(main_op_specialty == "Thoracic" &
                                       diag1 %in% cancer_thoracic ~ diag1,
                                     main_op_specialty == "Thoracic" &
                                       diag2 %in% cancer_thoracic ~ diag2,
                                     main_op_specialty == "Thoracic" &
                                       diag3 %in% cancer_thoracic ~ diag3,
                                     main_op_specialty == "Thoracic" &
                                       diag4 %in% cancer_thoracic ~ diag4,
                                     main_op_specialty == "Thoracic" &
                                       diag5 %in% cancer_thoracic ~ diag5,
                                     main_op_specialty == "Thoracic" &
                                       diag6 %in% cancer_thoracic ~ diag6,
                                     .default = NA),
           diag_hepatic = case_when(main_op_specialty == "Hepatobiliary" &
                                      diag1 %in% cancer_hepatic ~ diag1,
                                    main_op_specialty == "Hepatobiliary" &
                                      diag2 %in% cancer_hepatic ~ diag2,
                                    main_op_specialty == "Hepatobiliary" &
                                      diag3 %in% cancer_hepatic ~ diag3,
                                    main_op_specialty == "Hepatobiliary" &
                                      diag4 %in% cancer_hepatic ~ diag4,
                                    main_op_specialty == "Hepatobiliary" &
                                      diag5 %in% cancer_hepatic ~ diag5,
                                    main_op_specialty == "Hepatobiliary" &
                                      diag6 %in% cancer_hepatic ~ diag6,
                                    .default = NA),
           diag_gastro = case_when(main_op_specialty == "Gastrointestinal" &
                                     diag1 %in% cancer_gastro ~ diag1,
                                   main_op_specialty == "Gastrointestinal" &
                                     diag2 %in% cancer_gastro ~ diag2,
                                   main_op_specialty == "Gastrointestinal" &
                                     diag3 %in% cancer_gastro ~ diag3,
                                   main_op_specialty == "Gastrointestinal" &
                                     diag4 %in% cancer_gastro ~ diag4,
                                   main_op_specialty == "Gastrointestinal" &
                                     diag5 %in% cancer_gastro ~ diag5,
                                   main_op_specialty == "Gastrointestinal" &
                                     diag6 %in% cancer_gastro ~ diag6,
                                   .default = NA)) %>% 
    
    ### Pull into one column and join in diagnosis list by speciality -----
    
    unite(cancer_surgery_diag, diag_colorectal:diag_gastro, sep="", na.rm = TRUE) %>%  
    left_join(cancer_codes, by = join_by(cancer_surgery_diag == icd10_code, main_op_specialty == join_speciality)) 
    
    # # Exclude tonsillectomies unless cancer diagnosis is present - work out better strategies for dealing with ENT
    # filter_out(main_op_type %in% c("Tonsillectomy", "Other operations on tonsil") &
    #              is.na(cancer_surgery)) 
  
  return(diag_data)
}