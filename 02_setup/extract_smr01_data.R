#################################################.
### SNAP SRASA - Monthly SMR01 extract REWORK ###
#################################################.

# Bex Madden & Dylan Lewis
# 13/11/2025
# AMENDED 09/02/2026


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
  #' inputs; Performs preliminary data tidying.
  
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
                      DATE_RECORD_INSERTED,
                      DATE_LAST_AMENDED,
                      UPI_NUMBER, 
                      EPISODE_RECORD_KEY,
                      CIS_MARKER, 
                      LINK_NO, 
                      DOB,
                      AGE_IN_YEARS,
                      SEX,
                      POSTCODE,
                      ADMISSION, 
                      DISCHARGE, 
                      ADMISSION_DATE,
                      DISCHARGE_DATE,
                      LENGTH_OF_STAY,
                      SPECIALTY,
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
                        DISCHARGE_DATE, ADMISSION, DISCHARGE, URI")#removed gls_cis_marker (17 obs of 88000 where is is not the same as cis_marker), ci_chi_number (because we should use upi), patient_identifier (never used), significant facility (never used). ADDED record inserted + record_last_amended
  
  ### Run query ----------------------------------------------------------------
  cli_progress_step("Fetching SMR-01 data from database...")
  smr01_data_extract <- dbGetQuery(smr01_connect, query_smr01) %>% 
    as.data.frame() %>% 
    clean_names() %>% 
    rename(diag1 = main_condition, diag2 = other_condition_1, diag3 = other_condition_2, #rename diagnosis code columns
           diag4 = other_condition_3, diag5 = other_condition_4, diag6 = other_condition_5,
           op1_date = date_of_main_operation, op2_date = date_of_other_operation_1, #rename date of procedure cols
           op3_date = date_of_other_operation_2, op4_date = date_of_other_operation_3) 
  
  ### Disconnect and clean environment -----------------------------------------
  dbDisconnect(smr01_connect)
  rm(smr01_connect)
  gc()
  
  return(smr01_data_extract)
}

