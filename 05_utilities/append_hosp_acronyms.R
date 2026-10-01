################################.
### Append hospital acronyms ###
################################.

# Author:
# Bex Madden
# Date: 14/08/2026



append_hosp_acronyms <- function(df){
  
  #' Appends hospital name acronyms for labelling in plots etc
  #'
  #' @description This function uses column name 'hospital_name' to append a 
  #' new column called 'hosp_acronym' allowing hospitals to be labelled in 
  #' short-form in plots etc
  #' 
  #' @usage append_hosp_acronyms()
  #' 

    df_acronym <- df %>% 
      mutate(hosp_acronym = case_when(hospital_name == "Aberdeen Royal Infirmary" ~ "ARI",
                                      hospital_name == "Dumfries & Galloway Royal Infirmary" ~ "DGRI",
                                      hospital_name == "Forth Valley Royal Hospital" ~ "FVRH",
                                      hospital_name == "Glasgow Royal Infirmary" ~ "GRI",
                                      hospital_name == "Golden Jubilee University National Hospital" ~ "GJNH",
                                      hospital_name == "Ninewells Hospital" ~ "NWD",
                                      hospital_name == "Queen Elizabeth University Hospital" ~ "QEUH",
                                      hospital_name == "Raigmore Hospital" ~ "RHI",
                                      hospital_name == "Royal Infirmary of Edinburgh at Little France" ~ "RIE",
                                      hospital_name == "Royal Infirmary of Edinburgh" ~ "RIE",
                                      hospital_name == "St John's Hospital" ~ "SJH",
                                      hospital_name == "University Hospital Crosshouse" ~ "UHC",
                                      hospital_name == "University Hospital Hairmyres" ~ "UHH",
                                      hospital_name == "Victoria Hospital" ~ "VHK",
                                      hospital_name == "Western General Hospital" ~ "WGH",
                                      .default = NA))
    
    return(df_acronym)
  }