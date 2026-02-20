######################################################.
### SNAP SRASA - Compile Intuitive monthly extract ###
######################################################.

# Bex Madden 
# 10/02/2026


compile_intuitive_data <- function(){
  
  #' Compile together hospital-level monthly extracts of RAS utilisation data from Intuitive
  #'
  #' @description 
  #' 
  #' @usage 
  #'
  #' @details 
  
  ### Read in data files and append hospital identifier ------------------------
  hosp_lookup <- read.csv(paste0(lookup_dir, "NHSScotland_hospitals.csv")) %>% 
    select(hospital_name, health_board)

  filenames <- list.files(path = paste0(data_dir, "intuitive"), pattern = '\\.csv$', full.names = TRUE) #need to make generic filepath for forward compatibility
  datalist = list()
  
  cli_progress_step("Reading and wrangling hospital data...")
  
  for (i in seq_along(filenames)) {
    
    df <- read.csv(filenames[[i]]) %>% 
      clean_names() %>% 
      select(system_start_time:additional_procedure_type) %>% #remove unnecessary columns
      distinct() %>% 
      mutate(name = substr(filenames[[i]], 30, 32), #this is specific tot he filepath above so needs change if filepath changes
             hospital_name = case_when(name == "ARI" ~ "Aberdeen Royal Infirmary",
                                       name == "GRI" ~ "Glasgow Royal Infirmary",
                                       name == "GJN" ~ "Golden Jubilee University National Hospital",
                                       name == "UHH" ~ "University Hospital Hairmyres",
                                       name == "NWD" ~ "Ninewells Hospital",
                                       name == "QEU" ~ "Queen Elizabeth University Hospital",
                                       name == "RHI" ~ "Raigmore Hospital",
                                       name == "RIE" ~ "Royal Infirmary of Edinburgh at Little France",
                                       name == "SJH" ~ "St John's Hospital",
                                       name == "UHC" ~ "University Hospital Crosshouse",
                                       name == "VHK" ~ "Victoria Hospital",
                                       name == "WGH" ~ "Western General Hospital",
                                       .default = NA))
    
    datalist[[i]] <- df
  }
  
  cli_progress_step("Compiling hospital data...")
  
  intuitive_data <- do.call(dplyr::bind_rows, datalist) %>% 
    left_join(hosp_lookup, by = join_by(hospital_name)) %>% 
    separate_wider_delim(system_start_time, delim = " ", names = c("start_date", "start_time")) %>% 
    separate_wider_delim(system_end_time, delim = " ", names = c("end_date", "end_time")) %>% 
    mutate(start_date = as.Date(start_date, format = "%m-%d-%Y"),
           end_date = as.Date(end_date, format = "%m-%d-%Y"),
           start_time = as.POSIXct(start_time, format="%H:%M:%S"),
           end_time = as.POSIXct(end_time, format="%H:%M:%S"),
           op_month = floor_date(start_date, "month"))
  
  return(intuitive_data)  
  
  cli_progress_step("Hospital data files compiled...")
}