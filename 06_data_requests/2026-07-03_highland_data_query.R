### Rectal Resections Jan and Feb 2026 requested by Colin Richards

highland_data <- read_parquet(paste0(data_dir, "monthly_extract/srasa_smr_extract_min.parquet")) %>% 
  filter(hosp_health_board == "Highland" &
           main_op_type == "Rectal resection" &
           op_year == "2026") %>% 
  select(upi_number, hospital_name, hosp_health_board, first_admit_date, 
         last_discharge_date, op_mth, op_year, main_op_code, main_op_desc, 
         main_op_type, main_op_specialty, main_op_phase, main_op_date, 
         main_op_approach, ras_proc, op1a, op1b, op1_date, op1_approach, 
         op2a, op2b, op2_date, op2_approach, op3a, op3b, op3_date, op3_approach, 
         op4a, op4b, op4_date, op4_approach, diag1, diag2, diag3, diag4, diag5, 
         diag6, cancer_flag, cancer_surgery, admission_type)


write.csv(highland_data, "../../../(08) Communications/Data Query/2026-07-01_highland_data_query_srasa.csv")


# investigating emergency admission - 9/7/26
adm_lookup <- read.csv("../../../(12) Data/lookups/smr_admtype_lookup.csv") %>% 
  select(-definition)

check <- highland_data %>% 
  left_join(adm_lookup, by = join_by(admission_type == code)) %>% 
  group_by(adm_type_grp, ras_proc) %>% 
  summarise(n=n()) # would lose all but 1 (?) non-ras proc by excluding emergencies from index procs/from non-ras cohort altogether?


### Generate index proc plot with added filter against emergency procedures for illustration ----
# use < latest date and >= start_date 
latest_date <- Sys.Date() %>% 
  lubridate::floor_date("month") %m-% months(3)

start_date <- latest_date %>% 
  lubridate::floor_date("month") %m-% months(12)


### Read in cleaned data from SMR01 --------------------------------------------
ras_proc_data <- read_parquet(paste0(data_dir, "monthly_extract/srasa_smr_extract_min.parquet")) %>% 
  filter(op_mth >= start_date & 
           op_mth < latest_date) %>% 
  
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

wrong_hosp <- ras_proc_data %>% #get list of hospital names that appear but do not have a robot
  filter(hosp_has_robot != "Yes") %>% 
  group_by(hospital_name) %>% 
  slice(1) %>% 
  dplyr::pull(., hospital_name)

ras_proc_data <- ras_proc_data %>% #some aberrant coding liekly due to transfers
  mutate(hospital_name_grp = case_when(hospital_name %in% wrong_hosp ~ "Other Hospital Listed", #contains non-RAS and private hospitals
                                       is.na(hospital_name) ~ "Other Hospital Listed",
                                       .default = hospital_name),
         hospital_name_grp = factor(hospital_name_grp, levels = hosp_order)) %>% 
  left_join(adm_lookup, by = join_by(admission_type == code)) #new


index_proc_list <- c("Rectal resection", "Hysterectomy", "Lobectomy of lung", "Prostatectomy", "Pharyngectomy")

proc_index <- ras_proc_data %>% 
  mutate(main_op_type = case_when(main_op_type == "Abdominal hysterectomy" ~ "Hysterectomy", #vaginal/abdominal approach to hysterectomy not relevant... consider changign in all_ras_procs?
                                  main_op_type == "Vaginal hysterectomy" ~ "Hysterectomy",
                                  .default = main_op_type)) %>% 
  filter(main_op_type %in% index_proc_list &
           (main_op_phase == "phase1" &
              adm_type_grp == "routine")) %>% # new
  group_by(main_op_type, main_op_specialty, ras_proc, op_mth, hospital_name_grp) %>% 
  summarize(n = n()) %>% 
  group_by(main_op_type, main_op_specialty, op_mth, hospital_name_grp) %>% 
  mutate(tot_n = sum(n),
         prop = round(n/tot_n*100, 2)) 


highland_rectal <- proc_index %>% 
  filter(hospital_name_grp == "Raigmore Hospital" &
           main_op_specialty == "Colorectal")

# plot
ggplot(highland_rectal, 
       aes(x = op_mth, y = prop, fill = ras_proc, 
           tooltip = paste0("Hospital Location: ", hospital_name_grp,
                            "\n ", ras_proc,
                            "\n % of procedures: ", prop, "%",
                            "\n No. of procedures: ", n,
                            "\n Month: ", op_mth),
           data_id = op_mth)) + 
  geom_bar_interactive(stat = "identity", width = 20, hover_nearest = TRUE) +
  facet_wrap(~ hospital_name_grp)+ 
  #geom_hline(aes(yintercept = hline), colour = "orange", linetype="dashed")+ #threshold
  labs(x = "Month", 
       y = "% procedures performed using RAS",
       fill = "Surgical approach",
       caption = "Data from SMR01, all surgical approaches to index procedure (phase 1 only)",
       subtitle = paste0("Index procedure: Rectal resection"))+ 
  scale_fill_manual(values = c("Non-RAS" = "#94AABD", "RAS" = "#12436D", "No procedures" = "#b1b1b1"))+
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y"
  ) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
