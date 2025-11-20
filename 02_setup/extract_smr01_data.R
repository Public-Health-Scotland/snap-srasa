##########################################.
### SNAP SRASA - Monthly SMR01 extract ###
##########################################.

# Bex Madden & Dylan Lewis
# 13/11/2025


extract_smr01_data <- function(start_date = "01-January-2023", 
                               end_date = Sys.Date() %>% 
                                 lubridate::floor_date("month") %m-% months(2) %>% 
                                 format("%d-%B-%Y")){
  
  #' Monthly extract of SMR01 data for SRASA
  #'
  #' @description This function connects to SMR01 and extracts the desired 
  #' variables for the date range specified. This function should be run at 
  #' the start of each month to update the SRASA dataset.
  #' 
  #' @param start_date - format as 01-January-2023. Defaults to this date.
  #' @param end_date - format as above. Defaults to 2 months before 1st of 
  #' sys.date month e.g. if running on 14th November defaults to 1st September
  #' 
  #' @usage extract_smr01_data(start_date, end_date)
  #'
  #' @details Connects to SMR01; Extracts data according to start and end date 
  #' inputs; Performs preliminary wrangling- splitting operation codes into 
  #' 4-digit a & b codes, then filtering df to leave full records for patients
  #' who have at some point had a candidate procedure. Resulting data needs to
  #' be grouped by link_no to work within patient record or grouped by link_no
  #' and cis_marker to work within each individual patient stay.
  
  
  ### Set up connection
  smr01_connect <-   dbConnect(odbc(),
                               dsn = "SMRA",
                               uid = Sys.getenv("USER"),
                               pwd = .rs.askForPassword(
                                 "SMRA Password: "))
  
  ### Set up query 
  query_smr01 <- paste0("SELECT 
                      CI_CHI_NUMBER, 
                      UPI_NUMBER, 
                      PATIENT_IDENTIFIER, 
                      EPISODE_RECORD_KEY,
                      CIS_MARKER,
                      GLS_CIS_MARKER, 
                      LINK_NO, 
                      ADMISSION, 
                      DISCHARGE, 
                      URI,
                      DOB,
                      AGE_IN_YEARS,
                      SEX,
                      POSTCODE,
                      ADMISSION_DATE,
                      DISCHARGE_DATE,
                      LENGTH_OF_STAY,
                      SPECIALTY,
                      SIGNIFICANT_FACILITY,
                      ADMISSION_TYPE,
                      LOCATION,
                      MAIN_CONDITION,
                      OTHER_CONDITION_1,
                      OTHER_CONDITION_2,
                      OTHER_CONDITION_3,
                      OTHER_CONDITION_4,
                      OTHER_CONDITION_5,
                      CLINICIAN_MAIN_OPERATION,
                      MAIN_OPERATION,
                      DATE_OF_MAIN_OPERATION,
                      OTHER_OPERATION_1,
                      DATE_OF_OTHER_OPERATION_1,
                      OTHER_OPERATION_2,
                      DATE_OF_OTHER_OPERATION_2,
                      OTHER_OPERATION_3,
                      DATE_OF_OTHER_OPERATION_3


                      FROM ANALYSIS.SMR01_PI
                      WHERE ADMISSION_DATE >= ", start_date, "
                          AND ADMISSION_DATE <= ", end_date, "
                        ORDER BY LINK_NO, ADMISSION_DATE, 
                        DISCHARGE_DATE, ADMISSION, DISCHARGE, URI")
  
  ### Run query 
  smr01_data_extract <- dbGetQuery(smr01_connect, query_smr01) %>% 
    as_data_frame() %>% 
    clean_names()
    
  ### Data wrangling
  candidate_codes <- read_csv("../../../(12) Data/Lookups/ras_procedure_codes.csv") %>%  #not full list as appears on discovery site, list sent by craig
    rename(op_specialty = specialty)
  candidate_list <- dplyr::pull(candidate_codes, code)
  
  # approach_codes <- read_csv("../../../(12) Data/Lookups/approach_codes.csv") # move this to next function
  # approach_list <- dplyr::pull(approach_codes, approach_code)
  # 
  # robotics_list <- approach_codes$approach_code[!is.na(approach_codes$robotic)]
  # minimal_list <- approach_codes$approach_code[!is.na(approach_codes$minimal)]
  # robotic_conv_list <- approach_codes$approach_code[!is.na(approach_codes$robotic_conv)]
  # minimal_conv_list <- approach_codes$approach_code[!is.na(approach_codes$minimal_conv)]
  
  
  ras_clean_data <- smr01_data_extract %>% 
    
    # split procedure codes by a and b position
    separate_wider_position(main_operation, c(op1a = 4, op1b = 4), too_few = "align_start")  %>% # always 4 digit codes?
    separate_wider_position(other_operation_1, c(op2a = 4, op2b = 4), too_few = "align_start") %>% 
    separate_wider_position(other_operation_2, c(op3a = 4, op3b = 4), too_few = "align_start") %>% 
    separate_wider_position(other_operation_3, c(op4a = 4, op4b = 4), too_few = "align_start") %>% 

    mutate(which_candidate1 = case_when(op1a %in% candidate_list ~ 1, #make column indicating position of candidate procedures
                                        op2a %in% candidate_list ~ 2,
                                        op3a %in% candidate_list ~ 3,
                                        op4a %in% candidate_list ~ 4,
                                        .default = NA_integer_),
           which_candidate2 = case_when(
             which_candidate1 < 2 & op2a %in% candidate_list ~ 2,
             which_candidate1 < 3 & op3a %in% candidate_list ~ 3,
             which_candidate1 < 4 & op4a %in% candidate_list ~ 4,
             .default = NA_integer_),
           # which_candidate3 = case_when(  ## include regular checks on list of 3+ candidate procedures and consider creating 3rd column if it grows
           #   which_candidate2 < 3 & op3a %in% candidate_list ~ 3,
           #   which_candidate2 < 4 & op4a %in% candidate_list ~ 4,
           #   .default = NA_integer_),
           # which_candidate4 = case_when(
           #   which_candidate3 < 4 & op4a %in% candidate_list ~ 4,
           #   .default = NA_integer_),
           
           candidate_proc1 = case_when(which_candidate1 == 1 ~ op1a,
                                       which_candidate1 == 2 ~ op2a,
                                       which_candidate1 == 3 ~ op3a,
                                       which_candidate1 == 4 ~ op4a,
                                       .default = NA_character_),
           candidate_proc2 = case_when(which_candidate2 == 2 ~ op2a,
                                       which_candidate2 == 3 ~ op3a,
                                       which_candidate2 == 4 ~ op4a,
                                       .default = NA_character_),
           candidate_proc1_date = case_when(which_candidate1 == 1 ~ date_of_main_operation,
                                            which_candidate1 == 2 ~ date_of_other_operation_1,
                                            which_candidate1 == 3 ~ date_of_other_operation_2,
                                            which_candidate1 == 4 ~ date_of_other_operation_3,
                                            .default = NA_Date_),
           candidate_proc2_date = case_when(which_candidate2 == 2 ~ date_of_other_operation_1,
                                            which_candidate2 == 3 ~ date_of_other_operation_2,
                                            which_candidate2 == 4 ~ date_of_other_operation_3,
                                            .default = NA_Date_)) 
  
  
  
  ### Disconnect and clean environment
  dbDisconnect(smr01_connect)
  rm(smr01_connect)
  gc()
  
  return(ras_clean_data)
}

