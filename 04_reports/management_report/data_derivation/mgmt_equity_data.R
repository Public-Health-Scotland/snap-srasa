#################################################.
#### SRASA Management Report - Data - Equity ####
#################################################.

#Author: Bex Madden
#Date:06/11/2025

# Derivation of source data tables for the equity-related measures shown in the 
# SRASA management report

### set min and max dates ------------------------------------------------------
# from last month of smr01 completeness (approx 6wks before middle of given month)
# to a year prior to that date
# use < latest date and >= start_date 
latest_date <- Sys.Date() %>% 
  lubridate::floor_date("month") %m-% months(2)

start_date <- latest_date %>% 
  lubridate::floor_date("month") %m-% months(12)

### Read in cleaned data from SMR01 --------------------------------------------
ras_cand_data <- read_parquet(paste0(data_dir, "monthly_extract/srasa_smr_extract_", 
                                      format(Sys.Date(), "%Y-%m"), ".parquet")) %>% 
  filter(date_of_main_operation >= start_date & 
           date_of_main_operation < latest_date) %>% 
  
  #only want phase1 and phase2 procs for equity. Unfair to compare equity for non-priority surgeries
  subset_smr01_extract(group_to = "cis", filter_by = "candidate") %>% 
  ungroup() %>%
  filter(!specialty_desc %in% c("Trauma and Orthopaedic Surgery", "Neurosurgery", "Neurology")) %>%  #ortho & neuro not cavity, other procs shouldn't be filtered by smr specialty 
  
  # add in some date variables
  group_by(link_no, cis_marker) %>% 
  mutate(first_admit_date = min(admission_date),
         last_discharge_date = max(discharge_date),
         cis_outcome = case_when(any(discharge == 2) ~ 'dead',
                                 .default = 'alive'), #get admission and discharge date for each stay plus outcome from cis
         op_year = format(as.Date(min(date_of_main_operation), format="%Y-%m-%d"),"%Y"),
         op_mth_year = floor_date(date_of_main_operation, "month"), #as.Date(format(as.Date(min(date_of_main_operation), format="%Y-%m-%d"),"%Y-%m")),
         op_qt = lubridate::quarter(as.Date(min(date_of_main_operation), format="%Y-%m-%d"),with_year = T)) %>% 
  ungroup()

# for now, equity should only look at phase 1 procedures as most fair to compare sites on priority procs
ras_ph1_data <- ras_cand_data %>% 
  filter(!is.na(phase1_proc1)) %>% 
  mutate(approach_ph1_proc1 = case_when(phase1_proc1 == op1a ~ op1_approach,
                                        phase1_proc1 == op2a ~ op2_approach,
                                        phase1_proc1 == op3a ~ op3_approach,
                                        phase1_proc1 == op4a ~ op4_approach,
                                        .default = NA),
         approach_ph1_proc2 = case_when(phase1_proc2 == op2a ~ op2_approach,
                                        phase1_proc2 == op3a ~ op3_approach,
                                        phase1_proc2 == op4a ~ op4_approach,
                                        .default = NA)) 
  
  
### Pivot long so showing all phase 1 procs in 1 column ------------------------
phase1_procs <- read_csv(paste0(lookup_dir, "phase1_procedure_codes.csv"), 
                         show_col_types = FALSE)

long_data <- ras_ph1_data %>% 
  pivot_longer(cols = c("phase1_proc1","phase1_proc2"),
               names_to = "op_no",
               values_to = "ph1_proc_code",
               names_prefix = "phase1_",
               values_drop_na = TRUE) %>% 
  
  #make good info relating to procedures 1 and 2, tidy age group
  mutate(proc_approach = case_when(op_no == "proc1" ~ approach_ph1_proc1,
                                   op_no == "proc2" ~ approach_ph1_proc2,
                                   .default = NA),
         proc_date = case_when(op_no == "proc1" ~ phase1_proc1_date,
                               op_no == "proc2" ~ phase1_proc2_date,
                               .default = NA),
         proc_approach = as.factor(proc_approach),
         proc_approach = fct_relevel(proc_approach, c("NOS", "MIA", "RAS", 
                                                      "RAS conv open", "MIA conv open")),
         proc_approach_binary = case_when(proc_approach == "RAS" | proc_approach == "RAS conv open" ~ "RAS",
                                          .default = "Non-RAS"),
         age_group = as.factor(age_group),
         age_group = fct_relevel(age_group, age_group_order)) %>% 
  
  #join in information relating to procedure codes
  left_join(phase1_procs, by = join_by(ph1_proc_code == code)) %>% 
  select(upi_number, cis_marker, link_no, sex, op_year, op_mth_year, op_qt, 
         first_admit_date, last_discharge_date, age_in_years, age_group, 
         res_health_board, simd_quintile, urban_rural_6, hospital_name, 
         hosp_health_board, hosp_has_robot, op_no, ph1_proc_code, proc_date,
         proc_approach, proc_approach_binary, proc_type, code_specialty) %>% 
  
  #change hospital name to 'other' when a robotic surgery is listed against non-robotic site
  mutate(hospital_name = case_when(hosp_has_robot != "Yes" & 
                                     proc_approach_binary == "RAS" ~ "Other Hospital Listed",
                                   .default = hospital_name)) %>% 
  filter(op_no == "proc1") #only want to count each surgery once not each proc within surgery - for now

### Hospital-level equity ------------------------------------------------------
# show at hospital level as more relevant to specialty than hb
##### Total monthly no. phas1 procs by specialty & location --------------------
spec_procsmth <- long_data %>%
  filter(proc_date >= start_date & 
           proc_date < latest_date) %>% 
  group_by(hospital_name, op_mth_year, op_year, code_specialty, proc_approach_binary) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(op_mth_year, op_year, code_specialty, proc_approach_binary) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hospital_name, ~"All"),
                      .groups = "drop")) %>% 
  ungroup()

write_parquet(spec_procsmth, paste0(data_dir, "management_report/spec_procsmth.parquet"))

##### Weekly no. procs by specialty and location, shown monthly ----------------
#until i can figure out mean no per spec per day of the week per month
# equity_specsday <- long_data %>%
#   filter(proc_date >= start_date & 
#            proc_date < latest_date) %>% 
#   group_by(hospital_name, op_mth_year, op_year, proc_date, code_specialty, proc_approach_binary) %>% 
#   summarise(n = n()) %>% 
#   ungroup() %>% 
#   group_by(op_mth_year, op_year, code_specialty, proc_approach_binary) %>% 
#   bind_rows(summarise(.,
#                       across(where(is.numeric), sum),
#                       across(hospital_name, ~"All"),
#                       .groups = "drop")) %>% 
#   ungroup()
# 
# write_parquet(equity_specsday, paste0(data_dir, "management_report/equity_specsday.parquet"))

##### Mean number of surgeries per day of the week, monthly --------------------
# equity_specsday <- long_data %>% # i don't think this is quite doing what I want it to
#   filter(proc_approach_binary == "RAS") %>% 
#   mutate(#op_week = floor_date(proc_date, "week"), 
#          dow = factor(format(as.Date(proc_date, format="%d/%m/%Y"),"%A"),
#                       levels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
#                                  "Friday", "Saturday", "Sunday"))) %>%
#   group_by(hospital_name, op_year, op_mth_year, ?op_week, proc_date, dow, code_specialty) %>% 
#   summarise(n = n()) %>% 
#   ungroup() %>% 
#   tidyr::complete(hospital_name, op_year, op_mth_year, ?op_week, dow, code_specialty) %>% #need to get zeroes for the days when no surgery happened for that spec at that location
#   mutate(n = replace_na(n, 0)) %>% 
#   group_by(hospital_name, op_year, op_mth_year, dow, code_specialty) %>% #, .drop = FALSE
#   summarise(mean_procs_pd = round(mean(n), 2)) %>% 
#   ungroup() 
# 
# write_parquet(util_procsday, paste0(data_dir, "management_report/util_procsday.parquet"))

### Patient characteristics ----------------------------------------------------
# represent by residence health board as reflects geographic equaity better
##### Age and Sex of patients accessing robotics -------------------------------
equity_agesex <- long_data %>% 
  filter(proc_date >= start_date & 
           proc_date < latest_date,
         !is.na(res_health_board)) %>% # last 12 months
  group_by(res_health_board, code_specialty, age_group, sex, proc_approach_binary) %>% # add in ability to filter by specialty and hospital with 'all' options for both
  summarise(n_age_sex = n()) %>% 
  group_by(code_specialty, age_group, sex, proc_approach_binary) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(res_health_board, ~"All"),
                      .groups = "drop")) %>% 
  ungroup() %>% 
  group_by(res_health_board, age_group, sex, proc_approach_binary) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(code_specialty, ~"All"),
                      .groups = "drop")) %>% 
  group_by(res_health_board, code_specialty, age_group, sex) %>% 
  mutate(tot_procs = sum(n_age_sex),
         app_prop = round(n_age_sex/tot_procs*100, 2)) %>% 
  ungroup()

write_parquet(equity_agesex, paste0(data_dir, "management_report/equity_agesex.parquet"))

##### Mean age of patients accessing robotics by location ----------------------
equity_agemean <- long_data %>% 
  filter(proc_date >= start_date & 
           proc_date < latest_date,
         !is.na(res_health_board)) %>% # last 12 months
  group_by(res_health_board, code_specialty, sex, proc_approach_binary) %>% 
  summarise(mean_age = mean(age_in_years),
            sd_age = sd(age_in_years)) %>% 
  mutate(sd_age = replace_na(sd_age, 0)) %>% 
  group_by(res_health_board, code_specialty, proc_approach_binary) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), mean),
                      across(sex, ~"All"),
                      .groups = "drop")) %>% 
  group_by(code_specialty, sex, proc_approach_binary) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), mean),
                      across(res_health_board, ~"All"),
                      .groups = "drop")) %>%
  group_by(res_health_board, sex, proc_approach_binary) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), mean),
                      across(code_specialty, ~"All"),
                      .groups = "drop")) %>% 
  ungroup() %>% 
  mutate(mean_age = round(mean_age, 2),
         sd_age = round(sd_age, 2))

write_parquet(equity_agemean, paste0(data_dir, "management_report/equity_agemean.parquet"))

# ##### Pre-existing medical conditions ------------------------------------------
# source("../../../../../../(6) Testing/Bex_testing/smr01_comorb_test.R") #move the function elsewhere once finalised
# 
# ras_comorbs <- ras_data %>% 
#   rename(diag1 = main_condition, 
#          diag2 = other_condition_1, 
#          diag3 = other_condition_2,
#          diag4 = other_condition_3, 
#          diag5 = other_condition_4, 
#          diag6 = other_condition_5) %>% 
#   calculate_cci() %>%  #function from smr01 comorb test script
#   select(link_no, cci_score) %>% 
#   right_join(long_data, by = join_by(link_no))
# 
# equity_pemc <- ras_comorbs %>% 
#   mutate(cci_score_bin = case_when(cci_score >= 10 ~ "10+",
#                                    .default = as.character(cci_score)),
#          cci_score_bin = factor(cci_score_bin, levels = c("0", "1", "2", "3", 
#                                                           "4", "5", "6", "7", 
#                                                           "8", "9", "10+"))) %>% 
#   group_by(cci_score_bin, op_year, approach_binary) %>% 
#   summarise(n_pemc = n()) %>% 
#   group_by(cci_score_bin, op_year) %>% 
#   mutate(tot_pemc = sum(n_pemc),
#          app_prop = round(n_pemc/tot_pemc*100, 2))
# 
# write_parquet(equity_pemc, paste0(data_dir, "management_report/equity_pemc.parquet"))

### Geography ------------------------------------------------------------------
##### SIMD of patients accessing robotics --------------------------------------
equity_simd <- long_data %>% 
  filter(proc_date >= start_date & 
           proc_date < latest_date,
         !is.na(res_health_board)) %>% # last 12 months
  group_by(res_health_board, code_specialty, simd_quintile, sex, proc_approach_binary) %>%
  summarise(n_simd = n()) %>%  
  group_by(code_specialty, simd_quintile, sex, proc_approach_binary) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(res_health_board, ~"All"),
                      .groups = "drop")) %>% 
  ungroup() %>% 
  group_by(res_health_board, simd_quintile, sex, proc_approach_binary) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(code_specialty, ~"All"),
                      .groups = "drop")) %>% 
  group_by(res_health_board, code_specialty, simd_quintile, sex) %>% 
  mutate(tot_simd = sum(n_simd),
         app_prop = round(n_simd/tot_simd*100, 2)) %>% 
  ungroup() 

write_parquet(equity_simd, paste0(data_dir, "management_report/equity_simd.parquet"))
 
# ##### Urban/Rural status of patients accessing robotics ------------------------
# equity_urbrural <- long_data %>% 
#   group_by(urban_rural_6, op_year, approach_binary) %>% #sex
#   summarise(n_urb = n()) %>% 
#   group_by(urban_rural_6, op_year) %>% #sex
#   mutate(tot_urb = sum(n_urb),
#          app_prop = round(n_urb/tot_urb*100, 2)) %>% 
#   ungroup() %>% 
#   filter(op_year == latest_year & !is.na(urban_rural_6))
# 
# write_parquet(equity_urbrural, paste0(data_dir, "management_report/equity_urbrural.parquet"))
# 
# ##### Residence HB vs treatment HB ---------------------------------------------
# # sankey/alluvial plot - maybe map flow?
# res_treat_hb <- ras_data %>% 
#   filter(op1_approach == "RAS" | op1_approach == "RAS conv open") %>% 
#   select(op_year, res_health_board, hosp_health_board) %>% 
#   group_by(res_health_board, hosp_health_board) %>% 
#   summarise(freq = n()) %>% 
#   mutate(same_diff = case_when(res_health_board != hosp_health_board ~ "different",
#                                .default = "same"))
# 
# library(ggalluvial)
# ggplot(data = res_treat_hb,
#        aes(axis1 = str_wrap(res_health_board, 15), axis2 = str_wrap(hosp_health_board, 15), y = freq)) +
#   geom_alluvium(aes(fill = res_health_board), #or could colour by same/different hb 
#                 curve_type = "sigmoid") +
#   geom_stratum(aes(fill = res_health_board), alpha = 0, reverse = TRUE, color = "grey50", linewidth = 0.25) +
#   geom_text(stat = "stratum",
#             aes(label = after_stat(stratum)))+
#   scale_x_discrete(limits = c("Residence", "Treatment"),
#                    expand = c(0.15, 0.05)) +
#   scale_fill_viridis_d(option = "turbo", begin = 0.1, end = 1) + 
#   theme_minimal() +
#   theme(legend.position = "none")
# 
# ggsave("../../../../../../(06) Testing/Bex_testing/Plots/res_treat_hb.png", width = 35, height = 50, units = "cm")
##### Residence HB and prop of robotic procedures ------------------------------

##### Treatment HB and prop of robotic procedures ------------------------------