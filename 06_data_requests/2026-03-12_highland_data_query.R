### Data Query for Highland
# Colorectal and 'General Surgery' cases for March and June 2025
# Following first Mgmt Report on 03/03/2026, to check hihg numbers for 'General' and any issues with codes used e.g. benign cases

highland_data <- read_parquet(paste0(data_dir, "monthly_extract/srasa_smr_extract_min.parquet")) %>% 
  filter(hosp_health_board == "Highland" &
           (op_mth == "2025-03-01" | op_mth == "2025-06-01") &
           (main_op_specialty == "Colorectal" | main_op_specialty == "General surgery" | main_op_specialty == "General surgery - unlisted")) %>% 
  mutate(main_op_phase = as.factor(main_op_phase),
         main_op_phase = fct_recode(main_op_phase, other = "non-priority")) %>% 
  select(upi_number, hospital_name, hosp_health_board, first_admit_date, 
         last_discharge_date, op_mth, op_year, main_op_code, main_op_desc, 
         main_op_type, main_op_specialty, main_op_phase, main_op_date, 
         main_op_approach, ras_proc, op1a, op1b, op1_date, op1_approach, 
         op2a, op2b, op2_date, op2_approach, op3a, op3b, op3_date, op3_approach, 
         op4a, op4b, op4_date, op4_approach, diag1, diag2, diag3, diag4, diag5, 
         diag6, cancer_flag, cancer_surgery)

write.csv(highland_data, "../../../(08) Communications/Data Query/2026-03-12_highland_data_query_srasa.csv")

# Send out data query with 'data_dictionary_for_data_query', 'diag_codes_other_spec'/gynae/ent, and 'RAS procedure codes and phasing' docs

# havign made fix to specialty for unlisted procs, checking same dates to see if gynae procedures have correctly made it out of 'general'
highland_data <- read_parquet(paste0(data_dir, "monthly_extract/srasa_smr_extract_min.parquet")) %>% #extract made 19/2/26 with new specialty coding
  filter(hosp_health_board == "Highland" &
           (op_mth == "2025-03-01") &
           (main_op_specialty == "Colorectal" | main_op_specialty == "General surgery" | main_op_specialty == "General surgery - unlisted" | main_op_specialty == "Gynaecology"))
#gynae procedures have succesfully been recoded to the right specialty

t <- highland_data %>% 
  filter(upi_number %in% c("1705595421", "0103483578", "2601469021")) #only finds 1st 2 - i RAS conv open 1 NOS (MIA conv open in op2a position doesn't make it to main_op_approach)
 # filter(main_op_approach == "RAS conv open" | main_op_approach == "MIA conv open")
 # filter(op_mth == "2025-12-01" & main_op_specialty == "Colorectal")

big_data <- read_parquet("../testing/part_processed_extract_18-3-26.parquet") %>% 
  filter(upi_number == "2601469021") #not present in data at all... wrong chi or record not uploaded yet? surgery from december
