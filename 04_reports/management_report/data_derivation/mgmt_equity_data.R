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
ras_cand_data <- read_parquet(paste0(data_dir, "monthly_extract/srasa_smr_extract_min.parquet")) %>% 
  filter(main_op_date >= start_date & 
           main_op_date < latest_date,
         main_op_phase == "phase1" | 
           main_op_phase == "phase2") %>% 
  #only want phase1 and phase2 procs for equity? fair to compare equity for non-priority surgeries? otherwise get all non-ras cases for non-priority procs too so hard to justify
  #tidying
  mutate(main_op_approach = as.factor(main_op_approach), 
         main_op_approach = fct_relevel(main_op_approach, c("NOS", "MIA", "RAS", 
                                                      "RAS conv open", "MIA conv open")),
         age_group = as.factor(age_group),
         age_group = fct_relevel(age_group, age_group_order),
         
         ras_proc = case_when(ras_proc == TRUE ~ "RAS",
                              .default = "Non-RAS")) %>% 
  
  #change hospital name to 'other' when a robotic surgery is listed against non-robotic site
  mutate(res_health_board = case_when(is.na(res_health_board) ~ "Unknown",
                                               .default = res_health_board))

wrong_hosp <- ras_cand_data %>% #get list of hospital names that appear but do not have a robot
  filter(hosp_has_robot != "Yes") %>% 
  group_by(hospital_name) %>% 
  slice(1) %>% 
  dplyr::pull(., hospital_name)

ras_cand_data <- ras_cand_data %>% #some aberrant coding liekly due to transfers
  mutate(hospital_name_grp = case_when(hospital_name %in% wrong_hosp ~ "Other Hospital Listed", #contains non-RAS and private hospitals
                                       .default = hospital_name),
         hospital_name_grp = factor(hospital_name_grp, levels = hosp_order))

### Geographical equity -----------------------------------------------------
#### Proportion robotic of phas1 procs by specialty and patient residence HB
equity_resprop <- ras_cand_data %>% 
  group_by(res_health_board, op_mth, op_year, main_op_specialty, ras_proc) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(res_health_board, op_mth, op_year, ras_proc) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(main_op_specialty, ~"All"),
                      .groups = "drop")) %>%
  ungroup() %>%
  pivot_wider(values_from = n,
              names_from = ras_proc,
              values_fill = 0) %>%
  mutate(n = RAS + `Non-RAS`,
         prop = round(RAS/n*100, 2)) %>%
  ungroup()

write_parquet(equity_resprop, paste0(data_dir, "management_report/equity_resprop.parquet"))


### Patient characteristics ----------------------------------------------------
# represent by residence health board as reflects geographic equaity better

##### Age and Sex of patients accessing robotics -------------------------------
equity_agesex <- ras_cand_data %>% 
  group_by(res_health_board, main_op_specialty, age_group, sex, ras_proc) %>% # add in ability to filter by specialty and hospital with 'all' options for both
  summarise(n_age_sex = n()) %>% 
  group_by(main_op_specialty, age_group, sex, ras_proc) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(res_health_board, ~"All"),
                      .groups = "drop")) %>% 
  ungroup() %>% 
  group_by(res_health_board, age_group, sex, ras_proc) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(main_op_specialty, ~"All"),
                      .groups = "drop")) %>% 
  group_by(res_health_board, main_op_specialty, age_group, sex) %>% 
  mutate(tot_procs = sum(n_age_sex)) %>% 
  ungroup() %>% 
  mutate(app_prop = round(n_age_sex/tot_procs*100, 2)) 

write_parquet(equity_agesex, paste0(data_dir, "management_report/equity_agesex.parquet"))

##### Mean age of patients accessing robotics by location ----------------------
equity_agemean <- ras_cand_data %>% 
  filter(!is.na(res_health_board)) %>% # or relabel as 'unknown' or something
  group_by(res_health_board, main_op_specialty, sex, ras_proc) %>% 
  summarise(mean_age = mean(age_in_years),
            sd_age = sd(age_in_years)) %>% 
  mutate(sd_age = replace_na(sd_age, 0)) %>% 
  group_by(res_health_board, main_op_specialty, ras_proc) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), mean),
                      across(sex, ~"All"),
                      .groups = "drop")) %>% 
  group_by(main_op_specialty, sex, ras_proc) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), mean),
                      across(res_health_board, ~"All"),
                      .groups = "drop")) %>%
  group_by(res_health_board, sex, ras_proc) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), mean),
                      across(main_op_specialty, ~"All"),
                      .groups = "drop")) %>% 
  ungroup() %>% 
  mutate(mean_age = round(mean_age, 2),
         sd_age = round(sd_age, 2))

write_parquet(equity_agemean, paste0(data_dir, "management_report/equity_agemean.parquet"))

# ##### Pre-existing medical conditions ------------------------------------------
# source("../../../../../../(6) Testing/Bex_testing/smr01_comorb_test.R") #move the function elsewhere once finalised
# 
# ras_comorbs <- ras_data %>% 
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
equity_simd <- ras_cand_data %>% 
  filter(!is.na(res_health_board)) %>% # or relabel as 'unknown' or something
  group_by(res_health_board, main_op_specialty, simd_quintile, sex, ras_proc) %>%
  summarise(n_simd = n()) %>%  
  group_by(main_op_specialty, simd_quintile, sex, ras_proc) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(res_health_board, ~"All"),
                      .groups = "drop")) %>% 
  ungroup() %>% 
  group_by(res_health_board, simd_quintile, sex, ras_proc) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(main_op_specialty, ~"All"),
                      .groups = "drop")) %>% 
  group_by(res_health_board, main_op_specialty, simd_quintile, sex) %>% 
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
# sankey/alluvial plot - maybe map flow?
res_treat_hb <- ras_cand_data %>%
  filter(ras_proc == "RAS",
         op_year == "2025") %>%#these 2 filters for now while figuring out how unlisted & hernia rpocs have been labelled phase 2 - obesity
  group_by(res_health_board, hosp_health_board, main_op_specialty) %>%
  summarise(freq = n()) %>%
  mutate(same_diff = case_when(res_health_board != hosp_health_board ~ "different",
                               .default = "same"))

library(ggalluvial)
ggplot(data = res_treat_hb,
       aes(axis1 = str_wrap(res_health_board, 15), axis2 = str_wrap(hosp_health_board, 15), y = freq)) +
  geom_alluvium(aes(fill = res_health_board), #or could colour by same/different hb
                curve_type = "sigmoid") +
  geom_stratum(aes(fill = res_health_board), alpha = 0, reverse = TRUE, color = "grey50", linewidth = 0.25) +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)))+
  scale_x_discrete(limits = c("Residence", "Treatment"),
                   expand = c(0.15, 0.05)) +
  scale_fill_viridis_d(option = "turbo", begin = 0.1, end = 1) +
  facet_wrap(~main_op_specialty, scales = "free_y")+
  theme_minimal() +
  theme(legend.position = "none")

ggsave("../../../(06) Testing/Bex_testing/res_treat_hb_sankey.png", width = 35, height = 50, units = "cm")

##### Residence HB and prop of robotic procedures ------------------------------

##### Treatment HB and prop of robotic procedures ------------------------------