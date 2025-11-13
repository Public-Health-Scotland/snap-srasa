#################################################.
#### SRASA Management Report - Data - Equity ####
#################################################.

#Author: Bex Madden
#Date:06/11/2025

# Derivation of source data tables for the equity-related measures shown in the 
# SRASA management report

### Load packages --------------------------------------------------------------
data_dir <- "../../../../../../(6) Testing/Bex_testing/" # temporary location
source(paste0(data_dir, "load_packages_srasa.R"))

### Read in cleaned data from SMR01 --------------------------------------------
RAS_data <- read_parquet(paste0(data_dir, "RAS_data_2023-25.parquet")) 

postcode_lookup <- get_simd_postcode() %>%
  select(pc7, simd_quintile = simd2020v2_sc_quintile)
urban_rural_lookup <- get_spd() %>% 
  select(pc7, urban_rural_6 = ur6_2022_name)
hosp_lookup <- read_xlsx("/conf/quality/sap/Active/(09) Implementation/Data & Validation/Extract_Scripts/Lookup/SSCA_HOSPITALS_LOOKUP.xlsx") %>% 
  select(LOCNAME, LOCATION, HEALTH_BOARD) %>% 
  rename_with(tolower) %>% 
  mutate(region = case_when(health_board %in% c("Grampian", "Tayside", "Highland", 
                                                "Orkney", "Shetland", "Western Isles") ~ "North",
                            health_board %in% c("Lothian", "Fife", "Forth Valley", 
                                                "Borders") ~ "East",
                            health_board %in% c("Lanarkshire", "Golden Jubilee University National Hospital", 
                                                "Greater Glasgow & Clyde", "Ayrshire & Arran",
                                                "Dumfries & Galloway") ~ "West",
                            .default = NA_character_),
         health_board = fct_recode(health_board, "GJNH" = "Golden Jubilee University National Hospital")) %>% 
  unique()

#add postcode and hospital information (this should happen before saving out the data file)
RAS_data <- RAS_data %>% 
  mutate(postcode = format_postcode(postcode, format = "pc7"),
         age_group = create_age_groups(age_in_years),
         sex = as.factor(sex),
         sex = fct_recode(sex, "Male" = "1", "Female" = "2")) %>% 
  left_join(postcode_lookup, by = join_by(postcode == pc7)) %>% 
  left_join(hosp_lookup, by = join_by(location)) %>% 
  left_join(urban_rural_lookup, by = join_by(postcode == pc7))

### Filter so only showing RAS procedures, pivot long so showing all procs -----
long_data <- RAS_data %>% 
  ungroup() %>% 
  filter(!is.na(op1_approach)) %>% 
  pivot_longer(cols = candidate_proc1:candidate_proc2,
               names_to = "op_no",
               values_to = "proc_code",
               names_prefix = "candidate_proc",
               values_drop_na = TRUE) %>% 
  
  mutate(proc_approach = case_when(proc_code == op1a ~ op1_approach,
                                   proc_code == op2a ~ op2_approach,
                                   proc_code == op3a ~ op3_approach,
                                   proc_code == op4a ~ op4_approach,
                                   .default = NA_character_),
         proc_type = case_when(op_no == 1 ~ type_proc1,
                               op_no == 2 ~ type_proc2,
                               .default = NA_character_),
         proc_spec = case_when(op_no == 1 ~ spec_proc1,
                               op_no == 2 ~ spec_proc2,
                               .default = NA_character_),
         
         proc_approach = as.factor(proc_approach),
         proc_approach = fct_relevel(proc_approach, c("NOS", "MIA", "RAS", 
                                                      "RAS conv open", "MIA conv open")),
         age_group = as.factor(age_group),
         age_group = fct_relevel(age_group, c("0-4", "5-9", "10-14", "15-19", 
                                              "20-24", "25-29", "30-34", "35-39",
                                              "40-44", "45-49", "50-54", "55-59", 
                                              "60-64", "65-69", "70-74", "75-79",
                                              "80-84", "85-89", "90+"))) %>% 
  
  mutate(approach_binary = case_when(proc_approach == "RAS" | proc_approach == "RAS conv open" ~ "RAS",
                                     .default = "Other approach")) %>% 
  select(upi_number, cis_marker, link_no, sex, stay_number, first_admit_date,
         last_discharge_date, cis_outcome, op_year, op_mth_year, op_qt, 
         stay_length, postop_los, age_group, simd_quintile, urban_rural_6,
         locname, health_board, region, op_no, proc_code, proc_type, proc_spec, 
         proc_approach, approach_binary)

### Age and Sex of patients accessing robotics ---------------------------------
equity_agesex <- long_data %>% 
  filter(proc_approach == "RAS" | proc_approach == "RAS conv open") %>% #only looking at numbers of people accessing RAS not other modalities here
  group_by(age_group, sex, op_year) %>%
  summarise(n_age_sex = n()) %>% 
  ungroup()

write_parquet(equity_agesex, paste0(data_dir, "mgmt_data/equity_agesex.parquet"))

### SIMD of patients accessing robotics ----------------------------------------
equity_simd <- long_data %>% 
  group_by(simd_quintile, op_year, approach_binary, sex) %>%
  summarise(n_simd = n()) %>% 
  group_by(simd_quintile, op_year, sex) %>% 
  mutate(tot_simd = sum(n_simd),
         app_prop = round(n_simd/tot_simd*100, 2)) %>% 
  ungroup()

write_parquet(equity_simd, paste0(data_dir, "mgmt_data/equity_simd.parquet"))

### Urban/Rural status of patients accessing robotics --------------------------
equity_urbrural <- long_data %>% 
  group_by(urban_rural_6, op_year, approach_binary) %>% #sex
  summarise(n_urb = n()) %>% 
  group_by(urban_rural_6, op_year) %>% #sex
  mutate(tot_urb = sum(n_urb),
         app_prop = round(n_urb/tot_urb*100, 2)) %>% 
  ungroup() %>% 
  filter(op_year == latest_year & !is.na(urban_rural_6))

write_parquet(equity_urbrural, paste0(data_dir, "mgmt_data/equity_urbrural.parquet"))

### Pre-existing medical conditions --------------------------------------------
source("../../../../../../(6) Testing/Bex_testing/smr01_comorb_test.R") #move the function elsewhere once finalised

RAS_comorbs <- RAS_data %>% 
  rename(diag1 = main_condition, 
         diag2 = other_condition_1, 
         diag3 = other_condition_2,
         diag4 = other_condition_3, 
         diag5 = other_condition_4, 
         diag6 = other_condition_5) %>% 
  calculate_cci() %>%  #function from smr01 comorb test script
  select(link_no, cci_score) %>% 
  right_join(long_data, by = join_by(link_no))

equity_pemc <- RAS_comorbs %>% 
  mutate(cci_score_bin = case_when(cci_score >= 10 ~ "10+",
                                    .default = as.character(cci_score)),
         cci_score_bin = factor(cci_score_bin, levels = c("0", "1", "2", "3", 
                                                          "4", "5", "6", "7", 
                                                          "8", "9", "10+"))) %>% 
  group_by(cci_score_bin, op_year, approach_binary) %>% 
  summarise(n_pemc = n()) %>% 
  group_by(cci_score_bin, op_year) %>% 
  mutate(tot_pemc = sum(n_pemc),
         app_prop = round(n_pemc/tot_pemc*100, 2))

write_parquet(equity_pemc, paste0(data_dir, "mgmt_data/equity_pemc.parquet"))




