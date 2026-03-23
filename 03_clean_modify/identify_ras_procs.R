#####################################################################################.
### SNAP SRASA - Identify RAS and candidate procedures from Monthly SMR01 extract ###
#####################################################################################.

# Bex Madden 
# 13/02/2026


identify_ras_procs <- function(df){
  
  #' Identifies RAS and candidate procedures from raw monthly extract of SMR01 data for SRASA
  #'
  #' @description This function wrangles the raw SMR01 data extract to identify 
  #' candidate procedures for RAS and the surgical approach taken.
  #' 
  #' @param df - the raw monthly SMR01 extract
  #' 
  #' @usage identify_ras_procs(df)
  #'
  #' @details Splitting operation codes into 4-digit a & b codes, identifying 
  #' which appear int he candidate list. labelling approach as RAS, MIA or NOS. 
  #' Finding RAS procedures which are not in the candidate list. Compiling the 
  #' first RAS rpocedures into the 'main_op_xxx' columns for usage
  
### Data wrangling -----------------------------------------------------------
cli_progress_step("Identifying RAS procedures...")

ras_clean_data <- df %>%

  # split procedure codes by a and b position
  separate_wider_position(main_operation, c(op1a = 4, op1b = 4), too_few = "align_start")  %>% # always 4 digit codes?
  separate_wider_position(other_operation_1, c(op2a = 4, op2b = 4), too_few = "align_start") %>% 
  separate_wider_position(other_operation_2, c(op3a = 4, op3b = 4), too_few = "align_start") %>% 
  separate_wider_position(other_operation_3, c(op4a = 4, op4b = 4), too_few = "align_start")  %>% 
  
  # make new columns to extract and label approach codes
  mutate(op1_approach = case_when(!is.na(op1a) & op1b %in% approach_list ~ op1b, 
                                  !is.na(op1a) & !(op1b %in% approach_list) ~ "NOS"), #if no matching approach code call it 'NOS'
         op2_approach = case_when(!is.na(op2a) & op2b %in% approach_list ~ op2b,
                                  !is.na(op2a) & !(op2b %in% approach_list) ~ "NOS"),
         op3_approach = case_when(!is.na(op3a) & op3b %in% approach_list ~ op3b,
                                  !is.na(op3a) & !(op3b %in% approach_list) ~ "NOS"),
         op4_approach = case_when(!is.na(op4a) & op4b %in% approach_list ~ op4b,
                                  !is.na(op4a) & !(op4b %in% approach_list) ~ "NOS")) # need to add in is.na = "NOS" too

approach_vec <- c("op1_approach", "op2_approach", "op3_approach", "op4_approach") #label ras/minimally invasive/NoS instead of codes
all_procs_list <- read_csv(paste0(lookup_dir, "all_ras_procs.csv")) %>%  # Lookup for candidate procedures
  select(-code3)
# spec_lookup <- read_csv(paste0(lookup_dir, "smr01_specialty_lookup.csv")) %>% #lookup for smr01 specialty - bespoke column to provide desired specialty categories
#   select(-Value, -specialty_desc) %>% 
#   rename(unlisted_ras_proc_spec = specialty_for_unlisted)

ras_clean_data <- ras_clean_data %>% 
  mutate(across(all_of(approach_vec), ~ case_when(. %in% robotics_list ~ "RAS",
                                                  . %in% minimal_list ~ "MIA",
                                                  . == "NOS" ~ "NOS",
                                                  #is.na(.) ~ "NOS", # need to add in is.na = "NOS" too test tthis
                                                  . %in% robotic_conv_list ~ "RAS conv open",
                                                  . %in% minimal_conv_list ~ "MIA conv open",
                                                  .default = NA_character_)),
         
         # make logical column indicating whether RAS has been used
         ras_proc = case_when(op1_approach == "RAS" | op1_approach == "RAS conv open" ~ TRUE,
                              op2_approach == "RAS" | op2_approach == "RAS conv open" ~ TRUE,
                              op3_approach == "RAS" | op3_approach == "RAS conv open" ~ TRUE,
                              op4_approach == "RAS" | op4_approach == "RAS conv open" ~ TRUE,
                              is.na(op1_approach) & is.na(op2_approach) & is.na(op3_approach) & is.na(op4_approach) ~ NA,
                              .default = FALSE)) %>% 
  
  left_join(all_procs_list, by = join_by(op1a == code)) %>% #find all records with any candidate procedure in any procedure a position
  rename("op1_opcs_desc" = "opcs_desc", 
         "op1_proc_type" = "proc_type",
         "op1_specialty" = "proc_specialty",
         "op1_phase" = "phase") %>% 
  left_join(all_procs_list, by = join_by(op2a == code)) %>% 
  rename("op2_opcs_desc" = "opcs_desc", 
         "op2_proc_type" = "proc_type", 
         "op2_specialty" = "proc_specialty",
         "op2_phase" = "phase") %>% 
  left_join(all_procs_list, by = join_by(op3a == code)) %>% 
  rename("op3_opcs_desc" = "opcs_desc", 
         "op3_proc_type" = "proc_type", 
         "op3_specialty" = "proc_specialty",
         "op3_phase" = "phase") %>% 
  left_join(all_procs_list, by = join_by(op4a == code)) %>% 
  rename("op4_opcs_desc" = "opcs_desc", 
         "op4_proc_type" = "proc_type",
         "op4_specialty" = "proc_specialty",
         "op4_phase" = "phase") %>% 
  
  filter(!specialty %in% c("C6", "AH", "C8", "A2", "C41")) %>% #remove any records with smr1 ortho, cardiac, neuro specialty codes
  mutate(no_match = case_when(ras_proc == TRUE & is.na(op1_opcs_desc) & is.na(op2_opcs_desc) & is.na(op3_opcs_desc) & is.na(op4_opcs_desc) ~ TRUE,
                              .default = FALSE), #only look for unlisted procs if NO listed proc in episode record
         unlisted_ras_proc = case_when(op1_approach == "RAS" & no_match == TRUE ~ op1a,
                                       op2_approach == "RAS" & no_match == TRUE ~ op2a,
                                       op3_approach == "RAS" & no_match == TRUE ~ op3a,
                                       op4_approach == "RAS" & no_match == TRUE ~ op4a)) %>% 
  select(-no_match) %>% #remove obsolete column
  mutate(unlisted_opcs_type = case_when(is.na(unlisted_ras_proc) ~ NA, #use fits letter of OPCS code (corresponds to anatomical region) to assign ballpark specialty for proc type and spec
                                        str_starts(unlisted_ras_proc, "H") ~ "Unlisted - colorectal",
                                        str_starts(unlisted_ras_proc, "M") ~ "Unlisted - urology",
                                        str_starts(unlisted_ras_proc, "Q") ~ "Unlisted - gynaecology",
                                        str_starts(unlisted_ras_proc, "J") ~ "Unlisted - hepatobiliary",
                                        str_starts(unlisted_ras_proc, "G") ~ "Unlisted - gastrointestinal",
                                        specialty %in% c("AQ", "C42") & #smr specialty codes for thoracic specialties
                                          str_starts(unlisted_ras_proc, "E") ~ "Unlisted - thoracic",
                                        specialty == "C5" & #smr specialty codes for ent specialties
                                          str_starts(unlisted_ras_proc, "E") ~ "Unlisted - ENT",
                                        str_starts(unlisted_ras_proc, "F") ~ "Unlisted - ENT",
                                        .default = "Unlisted - other"),
         unlisted_spec = case_when(is.na(unlisted_ras_proc) ~ NA,
                                  str_starts(unlisted_ras_proc, "H") ~ "Colorectal",
                                  str_starts(unlisted_ras_proc, "M") ~ "Urology",
                                  str_starts(unlisted_ras_proc, "Q") ~ "Gynaecology",
                                  str_starts(unlisted_ras_proc, "J") ~ "Hepatobiliary",
                                  str_starts(unlisted_ras_proc, "G") ~ "Gastrointestinal",
                                  specialty %in% c("AQ", "C42") & 
                                    str_starts(unlisted_ras_proc, "E") ~ "Thoracic",
                                  specialty == "C5" & 
                                    str_starts(unlisted_ras_proc, "E") ~ "ENT",
                                  str_starts(unlisted_ras_proc, "F") ~ "ENT",
                                  .default = "Unlisted")) %>% #unlisted_spec = case_when(!is.na(unlisted_ras_proc) ~ specialty,.default = NA) #left_join(spec_lookup, by = join_by(unlisted_spec == Code)) previously used smr spcialty for this but updated to use opcs code first letter
  
  #then make 1 column with 1st candidate proc that appears - main_ras_proc, plus proc type, specialty, phase etc - pull the unlisted procs into that so all are in one place. 
  mutate(main_op_code = case_when(!is.na(op1_opcs_desc) ~ op1a,
                                  !is.na(op2_opcs_desc) ~ op2a,
                                  !is.na(op3_opcs_desc) ~ op3a,
                                  !is.na(op4_opcs_desc) ~ op4a,
                                  !is.na(unlisted_ras_proc) ~ unlisted_ras_proc,
                                  .default = NA),
         main_op_desc = case_when(!is.na(op1_opcs_desc) ~ op1_opcs_desc,
                                  !is.na(op2_opcs_desc) ~ op2_opcs_desc,
                                  !is.na(op3_opcs_desc) ~ op3_opcs_desc,
                                  !is.na(op4_opcs_desc) ~ op4_opcs_desc,
                                  !is.na(unlisted_ras_proc) ~ "Unlisted procedure",
                                  .default = NA),
         main_op_type = case_when(!is.na(op1_opcs_desc) ~ op1_proc_type,
                                  !is.na(op2_opcs_desc) ~ op2_proc_type,
                                  !is.na(op3_opcs_desc) ~ op3_proc_type,
                                  !is.na(op4_opcs_desc) ~ op4_proc_type,
                                  !is.na(unlisted_ras_proc) ~ unlisted_opcs_type,
                                  .default = NA),
         main_op_specialty = case_when(!is.na(op1_opcs_desc) ~ op1_specialty,
                                       !is.na(op2_opcs_desc) ~ op2_specialty,
                                       !is.na(op3_opcs_desc) ~ op3_specialty,
                                       !is.na(op4_opcs_desc) ~ op4_specialty,
                                       !is.na(unlisted_ras_proc) ~ unlisted_spec,
                                       .default = NA),
         main_op_phase = case_when(!is.na(op1_opcs_desc) ~ op1_phase,
                                   !is.na(op2_opcs_desc) ~ op2_phase,
                                   !is.na(op3_opcs_desc) ~ op3_phase,
                                   !is.na(op4_opcs_desc) ~ op4_phase,
                                   !is.na(unlisted_ras_proc) ~ "other",
                                   .default = NA),
         main_op_date = case_when(!is.na(op1_opcs_desc) ~ op1_date,
                                  !is.na(op2_opcs_desc) ~ op2_date,
                                  !is.na(op3_opcs_desc) ~ op3_date,
                                  !is.na(op4_opcs_desc) ~ op4_date,
                                  !is.na(unlisted_ras_proc) ~ op1_date, #this will do for unlisted cases
                                  .default = NA),
         main_op_approach = case_when(!is.na(op1_opcs_desc) ~ op1_approach,
                                      !is.na(op2_opcs_desc) ~ op2_approach,
                                      !is.na(op3_opcs_desc) ~ op3_approach,
                                      !is.na(op4_opcs_desc) ~ op4_approach,
                                      !is.na(unlisted_ras_proc) ~ "RAS",
                                      .default = NA)) %>% 
  group_by(link_no) %>% 
  filter(any(!is.na(main_op_code))) %>%  #all records for any patient with a candidate or unlisted RAS proc
  ungroup() ##DO WE WANT TO ADD A FLAG FOR INDEX PROCEDURES?

### Return df ----------------------------------------------------

return(ras_clean_data)

}