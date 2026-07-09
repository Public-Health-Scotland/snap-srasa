####################################################.
### SNAP SRASA - SMR01:Intuitive record matching ###
####################################################.

# Bex Madden & Dylan Lewis
# 01/05/2026

# Purpose: Using Intuitive records, try to match RAS surgeries row-to-row with SMR01 data.
# In a heirarchical manner, beginning with matching based on op type and date - A
# Then check for surgeries without RAS tag, using op type and date - B
# Then using op type and a fuzzy margin around date (due to known smr date inaccuracy) - C
# Then using surgical speciality and op date (in case of coding misinterpretation) - D
# Then using surgical specialty and fuzzy marging around date - E
# Then checking surgeries without RAS tag, using op type and fuzzy date - F

record_matching_int_smr <- function(op_month = Sys.Date() %>% 
                                      lubridate::floor_date("month") %m-% months(5) %>% 
                                      format("%d-%m-%Y")) {
  
  ### Load in data ----'
  # Intuitive
  int_procs_lookup <- read.csv("../testing/intuitive_all_procs.csv") %>% #lookup to relate Int procedure labels to SMR ones
    select(procedure_type, srasa_type, srasa_spec) %>% 
    distinct()
  
  int_minimal <- read_parquet(paste0(data_dir, "intuitive/intuitive_rolling_data.parquet")) %>% 
    filter(op_month == op_month) %>% 
    select(op_date = start_date, 
           hospital_name, 
           spec = specialty, 
           int_desc = procedure_type) %>% 
    left_join(int_procs_lookup, by = join_by(int_desc == procedure_type))
  
  # SMR01
  smr_data <- read_parquet(paste0(data_dir, "monthly_extract/srasa_smr_extract_min.parquet")) 
  
  smr_minimal <- smr_data %>% 
    filter(op_mth == op_month & 
             ras_proc == TRUE) %>% 
    select(op_date = main_op_date, 
           hospital_name, 
           smr_spec = smr_specialty_desc, 
           main_op_type, 
           main_op_specialty, 
           upi_number) 
  
  ## Write loop -----
  hosp_list <- hosp_order[hosp_order !="Other Hospital Listed"]
  
  for (i in hosp_list) {
    
    hosp <- i
    ### Prepare data for joining -----
    # Intuitive
    int_gri <- int_minimal %>% 
      filter(hospital_name == hosp) %>% 
      group_by(op_date, srasa_type) %>% 
      mutate(id_no = seq_along(srasa_type), # to uniquely identifying identical same-day surgeries
             surgery_id = str_c(as.character(op_date), srasa_type, id_no, sep = " ")) %>% # make a unique id for each procedure
      ungroup()
    
    # SMR01
    smr_gri <- smr_minimal %>% 
      filter(hospital_name == hosp) %>% 
      mutate(main_op_type = replace_when(main_op_type, 
                                         main_op_type == "Abdominal hysterectomy" | 
                                           main_op_type == "Vaginal hysterectomy" ~ "Hysterectomy")) %>%
      group_by(op_date, main_op_type) %>% 
      mutate(id_no = seq_along(main_op_type),
             surgery_id = str_c(as.character(op_date), main_op_type, id_no, sep = " "), # make a unique id for each procedure to match than in int_gri
             main_op_specialty = replace_when(main_op_specialty, 
                                              main_op_specialty == "Unlisted" & #get more specialty info for unlisted procedures if it can be gleaned from smr_spec
                                                smr_spec != "General Surgery" ~ smr_spec)) %>% 
      ungroup()
    
    smr_joining <- smr_gri %>% 
      select(surgery_id, upi_number)
    
    # Define hospital acronym for use in filenames
    # hosp <- "Glasgow Royal Infirmary"
    
    hb_acronym <- if(hosp == "Glasgow Royal Infirmary") {paste0("GRI")
    }else if(hosp == "Aberdeen Royal Infirmary") {paste0("ARI")
    }else if(hosp == "Royal Infirmary of Edinburgh at Little France") {paste0("RIE")
    }else if(hosp == "University Hospital Crosshouse") {paste0("UHC")
    }else if(hosp == "St John's Hospital") {paste0("SJH")
    }else if(hosp == "Queen Elizabeth University Hospital") {paste0("QEUH")
    }else if(hosp == "Golden Jubilee University National Hospital") {paste0("GJNH")
    }else if(hosp == "University Hospital Hairmyres") {paste0("UHH")
    }else if(hosp == "Ninewells Hospital") {paste0("NWD")
    }else if(hosp == "Victoria Hospital") {paste0("VHK")
    }else if(hosp == "Western General Hospital") {paste0("WGH")
    }else if(hosp == "Raigmore Hospital") {paste0("RHI")
    }else{paste0("Other")}
    
    
    
    ### Join A -----
    # op type and op date
    gri_merged <- int_gri %>% 
      left_join(smr_joining, by = join_by(surgery_id)) %>% 
      rename(join_a = upi_number)
    
    ##### Make lists of unmatched ops in each dataset -----
    success_list <- gri_merged %>% # list of ids for the procedures we have matched
      filter(!is.na(join_a)) %>% 
      dplyr::pull(surgery_id)
    
    failed_smr <- smr_gri %>% 
      filter(!surgery_id %in% success_list) %>% 
      mutate(day_in_year = yday(op_date))
    
    failed_int <- int_gri %>% 
      filter(!surgery_id %in% success_list) %>% 
      mutate(day_in_year = yday(op_date))
    
    ### Join B -----
    # take SMR procs not tagged as RAS and search for matches there
    # Prep non-RAS data
    non_ras_smr <- smr_data %>% 
      filter(op_mth == op_month & 
               ras_proc == FALSE &
               hospital_name == hosp) %>% 
      mutate(main_op_type = replace_when(main_op_type, 
                                         main_op_type == "Abdominal hysterectomy" | 
                                           main_op_type == "Vaginal hysterectomy" ~ "Hysterectomy"),
             main_op_type = replace_when(main_op_type, 
                                         main_op_type == "Cholecysctectomy" ~ "Cholecystectomy"),
             main_op_specialty = replace_when(main_op_specialty, 
                                              main_op_specialty == "Unlisted" & #get more specialty info for unlisted procedures if it can be gleaned from smr_spec
                                                smr_specialty_desc != "General Surgery" ~ smr_specialty_desc),
             day_in_year = yday(main_op_date)) %>% 
      select(op_date = main_op_date, 
             hospital_name, 
             smr_spec = smr_specialty_desc, 
             main_op_type, 
             main_op_specialty, 
             upi_number, 
             day_in_year) 
    
    # Join on op type and date
    join_b_data <- failed_int %>% 
      inner_join(non_ras_smr, by = join_by(srasa_type == main_op_type, op_date)) %>% 
      select(surgery_id_int = surgery_id,
             upi_number, 
             smr_date = op_date, 
             main_op_type = srasa_type, 
             main_op_specialty) %>% 
      mutate(join = "join_b")
    
    ##### Join B results into main data -----
    gri_merged <- join_b_data %>%  
      group_by(surgery_id_int) %>% 
      summarise(join_b = n()) %>% 
      right_join(gri_merged, by = join_by(surgery_id_int == surgery_id)) %>% 
      relocate(join_b, .after = join_a)
    
    ### Join C -----
    # op type and fuzzy date margin
    join_c_data <- failed_int %>% 
      difference_inner_join(failed_smr, by = "day_in_year", max_dist = 2) %>% 
      filter(srasa_type == main_op_type) %>% 
      select(surgery_id_int = surgery_id.x, 
             upi_number, 
             smr_date = op_date.y, 
             main_op_type, 
             main_op_specialty) %>% 
      mutate(join = "join_c")
    
    ##### Join C results into main data -----
    gri_merged <- join_c_data %>%  
      group_by(surgery_id_int) %>% 
      summarise(join_c = n()) %>% 
      right_join(gri_merged, by = join_by(surgery_id_int)) %>% 
      relocate(join_c, .after = join_b)
    
    ### Join D -----
    # op date and surgical specialty 
    join_d_data <- failed_int %>% 
      inner_join(failed_smr, by = join_by(op_date, srasa_spec == main_op_specialty)) %>% 
      select(surgery_id_int = surgery_id.x, 
             upi_number, 
             smr_date = op_date, 
             main_op_type, 
             main_op_specialty = srasa_spec) %>% 
      mutate(join = "join_d")
    
    ##### Join D results into main data -----
    gri_merged <- join_d_data %>% 
      group_by(surgery_id_int) %>% 
      summarise(join_d = n()) %>%  
      right_join(gri_merged, by = join_by(surgery_id_int)) %>% 
      relocate(join_d, .after = join_c)
    
    ### Join E -----
    # surgical specialty and fuzzy date
    join_e_data <- failed_int %>% 
      difference_inner_join(failed_smr, by = "day_in_year", max_dist = 2) %>% 
      filter(srasa_spec == main_op_specialty) %>% 
      select(surgery_id_int = surgery_id.x, 
             upi_number, 
             smr_date = op_date.y, 
             main_op_type, 
             main_op_specialty = srasa_spec) %>% 
      mutate(join = "join_e")
    
    ##### Join E results into main data -----
    gri_merged <- join_e_data %>%  
      group_by(surgery_id_int) %>% 
      summarise(join_e = n()) %>% 
      right_join(gri_merged, by = join_by(surgery_id_int)) %>% 
      relocate(join_e, .after = join_d)
    
    ### Join F -----
    # take SMR procs not tagged as RAS and search for matches with fuzzy date
    join_f_data <- failed_int %>% 
      difference_inner_join(non_ras_smr, by = "day_in_year", max_dist = 1) %>% #harsher fuzziness in dates
      filter(srasa_type == main_op_type) %>% 
      select(surgery_id_int = surgery_id, 
             upi_number, 
             smr_date = op_date.y, 
             main_op_type, 
             main_op_specialty) %>% 
      mutate(join = "join_f")
    
    ##### Join F results into main data -----
    gri_merged <- join_f_data %>%  
      group_by(surgery_id_int) %>% 
      summarise(join_f = n()) %>% 
      right_join(gri_merged, by = join_by(surgery_id_int)) %>% 
      relocate(join_f, .after = join_e)
    
    ### Update lists of unmatched ops from each dataset -----
    # Intuitive
    failed_int_info <- gri_merged %>% 
      filter(!surgery_id_int %in% success_list) %>% 
      mutate(match_found = case_when(!is.na(join_b) | !is.na(join_c) | !is.na(join_d) | !is.na(join_e) | !is.na(join_f) ~ NA,
                                     .default = "no match"))
    
    unmatched_records_int <- failed_int_info %>% 
      filter(match_found == "no match") %>% 
      write_csv(paste0(data_dir, "monthly_data_query/unmatched_records_int_", hb_acronym, "_", op_month, ".csv"))
    
    # SMR01
    failed_smr_info <- failed_smr %>% 
      group_by(upi_number) %>% 
      mutate(join_a = NA,
             join_b = sum(upi_number %in% join_b_data$upi_number),
             join_c = sum(upi_number %in% join_c_data$upi_number),
             join_d = sum(upi_number %in% join_d_data$upi_number),
             join_e = sum(upi_number %in% join_e_data$upi_number),
             join_f = sum(upi_number %in% join_f_data$upi_number),
             
             match_found = case_when(join_b > 0 | join_c > 0 | join_d > 0 | join_e > 0 | join_f > 0 ~ NA,
                                     .default = "no match"))
    
    unmatched_records_smr <- failed_smr_info %>% 
      filter(match_found == "no match") %>% 
      write_csv(paste0(data_dir, "monthly_data_query/unmatched_records_smr_", hb_acronym, "_", op_month, ".csv"))
    
    # Make single list of join result details
    join_results <- rbind(join_b_data, join_c_data, join_d_data, join_e_data, join_f_data) %>% 
      group_by(surgery_id_int, upi_number) %>% 
      mutate(join = case_when(any(join == "join_b") ~ "join_b", # any duplicate matches, label with first join in which they appear
                              any(join == "join_c") ~ "join_c",
                              any(join == "join_d") ~ "join_d",
                              any(join == "join_e") ~ "join_e",
                              any(join == "join_f") ~ "join_f"),
             smr_date = as.Date(smr_date),
             mismatch_reason = case_when(join == "join_b" ~ "No RAS code",
                                         join == "join_c" ~ "Incorrect date (+/- 2 days)",
                                         join == "join_d" ~ "Incorrect procedure code",
                                         join == "join_e" ~ "Incorrect date AND incorrect procedure code",
                                         join == "join_f" ~ "No RAS code AND incorrect date",
                                         .default = NA),
             location = hosp) %>% 
      group_by(surgery_id_int) %>% 
      distinct() %>% 
      arrange(surgery_id_int, join) %>% 
      write_csv(paste0(data_dir, "monthly_data_query/join_result_detail_", hb_acronym, "_", op_month, ".csv"))
    
  }
}

