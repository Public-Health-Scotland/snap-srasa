##########################################.
### SNAP SRASA - Monthly SMR01 extract ###
##########################################.

# Bex Madden & Dylan Lewis
# 13/11/2025


extract_smr01_data <- function(start_date = "'01-January-2023'", 
                               end_date = NULL){
  
  #' Monthly extract of SMR01 data for SRASA
  #'
  #' @description This function connects to SMR01 and extracts the desired 
  #' variables for the date range specified. This function should be run at 
  #' the start of each month to update the SRASA dataset.
  #' 
  #' @param start_date - format as '01-January-2023' (including single quotes). Defaults to this date.
  #' @param end_date - format as above. Defaults to 2 months before 1st of 
  #' sys.date month e.g. if running on 14th November defaults to 1st September
  #' 
  #' @usage extract_smr01_data(start_date, end_date)
  #'
  #' @details Connects to SMR01; Extracts data according to start and end date 
  #' inputs; Performs preliminary wrangling- splitting operation codes into 
  #' 4-digit a & b codes, then adding a flag for RAS procedure.
  #' Resulting data needs to be grouped by link_no to work within patient record
  #' or grouped by link_no and cis_marker to work within each individual patient stay.
  
  ### Set default end date if none provided ------------------------------------
  
  if(is.null(end_date)){
    end_date <- Sys.Date() %>% 
      lubridate::floor_date("month") %m-% months(2) %>% 
      format("'%d-%B-%Y'")
  }
  
  
  ### Set up connection --------------------------------------------------------
  cli_progress_step("Connecting to database...")
  smr01_connect <-   dbConnect(odbc(),
                               dsn = "SMRA",
                               uid = Sys.getenv("USER"),
                               pwd = .rs.askForPassword(
                                 "SMRA Password: "))
  
  ### Set up query -------------------------------------------------------------
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
                          AND ADMISSION_DATE < ", end_date, "
                        ORDER BY LINK_NO, ADMISSION_DATE, 
                        DISCHARGE_DATE, ADMISSION, DISCHARGE, URI")
  
  ### Run query ----------------------------------------------------------------
  cli_progress_step("Fetching SMR-01 data from database...")
  smr01_data_extract <- dbGetQuery(smr01_connect, query_smr01) %>% 
    as.data.frame() %>% 
    clean_names()

  ### Data wrangling -----------------------------------------------------------
  cli_progress_step("Preparing data extract...")

  ras_clean_data <- smr01_data_extract %>% 
    
    # split procedure codes by a and b position
    separate_wider_position(main_operation, c(op1a = 4, op1b = 4), too_few = "align_start")  %>% # always 4 digit codes?
    separate_wider_position(other_operation_1, c(op2a = 4, op2b = 4), too_few = "align_start") %>% 
    separate_wider_position(other_operation_2, c(op3a = 4, op3b = 4), too_few = "align_start") %>% 
    separate_wider_position(other_operation_3, c(op4a = 4, op4b = 4), too_few = "align_start")  %>% 
    
    # make new column for approach codes only
    mutate(op1_approach = case_when(!is.na(op1a) & op1b %in% approach_list ~ op1b, 
                                    !is.na(op1a) & !(op1b %in% approach_list) ~ "NOS"), #if no matching approach code call it 'NOS'
           op2_approach = case_when(!is.na(op2a) & op2b %in% approach_list ~ op2b,
                                    !is.na(op2a) & !(op2b %in% approach_list) ~ "NOS"),
           op3_approach = case_when(!is.na(op3a) & op3b %in% approach_list ~ op3b,
                                    !is.na(op3a) & !(op3b %in% approach_list) ~ "NOS"),
           op4_approach = case_when(!is.na(op4a) & op4b %in% approach_list ~ op4b,
                                    !is.na(op4a) & !(op4b %in% approach_list) ~ "NOS")) # need to add in is.na = "NOS" too
  
  approach_vec <- c("op1_approach", "op2_approach", "op3_approach", "op4_approach") #label ras/minimally invasive/NoS instead of codes
  ras_clean_data <- ras_clean_data %>% 
    mutate(across(all_of(approach_vec), ~ case_when(. %in% robotics_list ~ "RAS",
                                                    . %in% minimal_list ~ "MIA",
                                                    . == "NOS" ~ "NOS",
                                                    #is.na(.) ~ "NOS", # need to add in is.na = "NOS" too test tthis
                                                    . == "Y721" ~ "RAS conv open",
                                                    . %in% c("Y714", "Y722") ~ "MIA conv open",
                                                    .default = NA_character_)),
           ras_proc = case_when(op1_approach == "RAS" | op1_approach == "RAS conv open" ~ TRUE,
                                op2_approach == "RAS" | op2_approach == "RAS conv open" ~ TRUE,
                                op3_approach == "RAS" | op3_approach == "RAS conv open" ~ TRUE,
                                op4_approach == "RAS" | op4_approach == "RAS conv open" ~ TRUE,
                                is.na(op1_approach) & is.na(op2_approach) & is.na(op3_approach) & is.na(op4_approach) ~ NA,
                                .default = FALSE))
  
  ### Disconnect and clean environment -----------------------------------------
  dbDisconnect(smr01_connect)
  rm(smr01_connect)
  gc()
  
  ### Return df ----------------------------------------------------

  return(ras_clean_data)

}

