### Data Query for A&A
# Urology cases for March and June 2025
# Following first Mgmt Report on 03/03/2026, to check any issues with codes used e.g. prostatectomies (but A&A data generally looks quite good)

ayrshire_data <- read_parquet(paste0(data_dir, "monthly_extract/srasa_smr_extract_min.parquet")) %>% 
  filter(hosp_health_board == "Ayrshire & Arran" &
           (op_mth == "2025-07-01" | op_mth == "2025-08-01" | op_mth == "2025-09-01") &
           (main_op_specialty == "Urology" | main_op_specialty == "Urology - unlisted")) %>% 
  mutate(main_op_phase = as.factor(main_op_phase),
         main_op_phase = fct_recode(main_op_phase, other = "non-priority")) %>% 
  select(upi_number, hospital_name, hosp_health_board, first_admit_date, 
         last_discharge_date, op_mth, op_year, main_op_code, main_op_desc, 
         main_op_type, main_op_specialty, main_op_phase, main_op_date, 
         main_op_approach, ras_proc, op1a, op1b, op1_date, op1_approach, 
         op2a, op2b, op2_date, op2_approach, op3a, op3b, op3_date, op3_approach, 
         op4a, op4b, op4_date, op4_approach, diag1, diag2, diag3, diag4, diag5, 
         diag6, cancer_flag, cancer_surgery)

write.csv(ayrshire_data, "../../../(08) Communications/Data Query/2026-03-12_ayrshire_data_query_srasa.csv")

# Send out data query with 'data_dictionary_for_data_query', 'diag_codes_other_spec'/gynae/ent, and 'RAS procedure codes and phasing' docs