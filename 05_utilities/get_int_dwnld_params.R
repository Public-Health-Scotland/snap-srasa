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

  end_date <- Sys.Date() %>% 
    lubridate::floor_date("month") %m-% months(2) %>% 
    format("%m-%d-%Y")
  start_date <- as.Date(end_date, format = "%m-%d-%Y") %m-% months(1) %>% 
    format("%m-%d-%Y")
  
  file_sdate <- as.Date(start_date, format = "%m-%d-%Y") %>% 
    format("%b%y") %>% 
    tolower()
  file_edate <- as.Date(end_date, format = "%m-%d-%Y") %>% 
    format("%b%y") %>% 
    tolower()
  
  ARI <- paste0("ARI_", file_sdate, "-", file_edate)
  GRI <- paste0("GRI_", file_sdate, "-", file_edate)
  GoldenJubilee <- paste0("GJNH_", file_sdate, "-", file_edate)
  Hairmyres <- paste0("UHH_", file_sdate, "-", file_edate)
  Ninewells <- paste0("NWD_", file_sdate, "-", file_edate)
  QEUH <- paste0("QEUH_", file_sdate, "-", file_edate)
  Raigmore <- paste0("RHI_", file_sdate, "-", file_edate)
  RIE <- paste0("RIE_", file_sdate, "-", file_edate)
  SJH <- paste0("SJH_", file_sdate, "-", file_edate)
  UHC <- paste0("UHC_", file_sdate, "-", file_edate)
  VHK <- paste0("VHK_", file_sdate, "-", file_edate)
  WGH <- paste0("WGH_", file_sdate, "-", file_edate)
  
  return(list(start_date = start_date, 
              end_date = end_date,
              ARI = ARI,
              GRI = GRI,
              GJNH = GJNH,
              NWD = NWD,
              QEUH = QEUH,
              RHI = RHI,
              RIE = RIE,
              SJH = SJH,
              UHC = UHC,
              UHH = UHH,
              VHK = VHK,
              WGH = WGH))
}
