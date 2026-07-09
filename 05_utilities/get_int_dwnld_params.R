######################################################################.
### SNAP SRASA - Intuitive data download parameters and file names ###
######################################################################.

# Bex Madden 
# 17/02/2025

get_int_dwnld_params <- function(){

  #' Print date parameters and file names for monthly Intuitive data download
  #'
  #' @description This function prints a list including start and end dates for
  #' manual download of intuitive data, plus file name strings (important to 
  #' keep these consistent as they are read in by name elsewhere)
  #' 
  #' @usage get_int_dwnld_params()
  #' 
  #' @details Prints the parameters in the console for copy-pasting
  #' 

  start_date <- Sys.Date() %>% 
    lubridate::floor_date("month") %m-% months(3) %>% 
    format("%m-%d-%Y")
  end_date <- as.Date(start_date, format = "%m-%d-%Y") %>% 
    lubridate::ceiling_date("month") - days(1)
  end_date <- format(end_date, format = "%m-%d-%Y")
  
  file_date <- as.Date(start_date, format = "%m-%d-%Y") %>% 
    format("%b%y") %>% 
    tolower()
  
  ARI <- paste0("ARI_", file_date)
  DGRI <- paste0("DGRI_", file_date)
  FVRH <- paste0("FVRH_", file_date)
  GRI <- paste0("GRI_", file_date)
  GJNH <- paste0("GJNH_", file_date)
  UHH <- paste0("UHH_", file_date)
  NWD <- paste0("NWD_", file_date)
  QEUH <- paste0("QEUH_", file_date)
  RHI <- paste0("RHI_", file_date)
  RAH <- paste0("RAH_", file_date)
  RIE <- paste0("RIE_", file_date)
  SJH <- paste0("SJH_", file_date)
  UHC <- paste0("UHC_", file_date)
  VHK <- paste0("VHK_", file_date)
  WGH <- paste0("WGH_", file_date)
  
  return(list(start_date = start_date, 
              end_date = end_date,
              ARI = ARI,
              DGRI = DGRI,
              FVRH = FVRH,
              GRI = GRI,
              GoldenJubilee = GJNH,
              Hairmyres = UHH,
              Ninewells = NWD,
              QEUH = QEUH,
              Raigmore = RHI,
              RAH = RAH,
              RIE = RIE,
              SJH = SJH,
              UHC = UHC,
              VHK = VHK,
              WGH = WGH))
}
