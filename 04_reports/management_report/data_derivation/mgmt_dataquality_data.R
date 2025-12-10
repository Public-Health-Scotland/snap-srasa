#######################################################.
#### SRASA Management Report - Data - Data Quality ####
#######################################################.

#Author: Bex Madden
#Date:10/12/2025

# Comparison of aggregated smr01 utilisation data to DUMMY Intuitive data

### read in aggregated smr01 data ----------------------------------------------
smr01_procsmth <- read_parquet(paste0(data_dir, "management_report/util_procsmth.parquet")) 

### create dummy data as if from Intuitive -------------------------------------
dq_compprocs <- smr01_procsmth %>% 
  mutate(intuitive_robot1 = case_when(hospital_name_grp == "Ninewells Hospital" |
                                hospital_name_grp == "St John's Hospital" |
                                hospital_name_grp == "University Hospital Crosshouse" ~ round(n+n*0.075,0),
                              hospital_name_grp == "Raigmore Hospital" |
                                hospital_name_grp == "Royal Infirmary of Edinburgh at Little France" |
                                hospital_name_grp == "Victoria Hospital" |
                                hospital_name_grp == "Glasgow Royal Infirmary" ~ round(n+n*0.09,0),
                              hospital_name_grp == "University Hospital Hairmyres" ~ round(n*0.95),
                              hospital_name_grp == "Western General Hospital" |
                                hospital_name_grp == "Queen Elizabeth University Hospital" |
                                hospital_name_grp == "Golden Jubilee University National Hospital" ~ round(n/2, 0),
                              hospital_name_grp == "Aberdeen Royal Infirmary" ~ round(n/3, 0),
                              hospital_name_grp == "Other Hospital Listed" ~ NA,
                              .default = NA),
         intuitive_robot2 = case_when(hospital_name_grp == "Western General Hospital" |
                                hospital_name_grp == "Queen Elizabeth University Hospital" |
                                hospital_name_grp == "Golden Jubilee University National Hospital" |
                                hospital_name_grp == "Aberdeen Royal Infirmary" ~ round(intuitive_robot1+
                                                                                          intuitive_robot1*0.08, 0),
                              .default = NA),
         intuitive_robot3 = case_when(hospital_name_grp == "Aberdeen Royal Infirmary" ~ round(intuitive_robot1+
                                                                                                intuitive_robot1*0.04, 0),
                              .default = NA),
         intuitive_tot = case_when(is.na(intuitive_robot1) ~ NA,
                                   is.na(intuitive_robot2) ~ intuitive_robot1,
                                   is.na(intuitive_robot3) ~ intuitive_robot1 + intuitive_robot2,
                                   !is.na(intuitive_robot3) ~ intuitive_robot1 + intuitive_robot2 + intuitive_robot3),
         prop_smr_found = round(n/intuitive_tot*100, 2)) %>% 
  rename(smr01 = n) %>%  
  pivot_longer(smr01:intuitive_robot3, names_to = "robot", values_to = "n") %>% 
  mutate(hospital_name_grp = case_when(robot != "smr01" ~ paste0(hospital_name_grp, " - Intuitive"),
                                       .default = hospital_name_grp))

### save out data --------------------------------------------------------------
write_parquet(dq_compprocs, paste0(data_dir, "management_report/dq_compprocs.parquet"))
         
# ,
# robot2_n = case_when(hospital_name_grp == "Western General Hospital" |
#                        hospital_name_grp == "Queen Elizabeth University Hospital" |
#                        hospital_name_grp == "Golden Jubilee University National Hospital" |
#                        ~ robot1_n,
#                      .default = NA),
# robot3_n = case_when(hospital_name_grp == "Aberdeen Royal Infirmary" ~ robot1_n,
#                      .default = NA)
