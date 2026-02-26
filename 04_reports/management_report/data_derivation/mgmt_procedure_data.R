################################################################.
#### SRASA Management Report - Data - Procedure-level usage ####
################################################################.

#Author: Bex Madden
#Date:26/02/2026

# Derivation of source data tables for the procedure-level utilisation measures 
# shown in the SRASA management report

### set min and max dates ------------------------------------------------------
# from last month of smr01 completeness (approx 6wks before middle of given month)
# to a year prior to that date
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
         hospital_name_grp = factor(hospital_name_grp, levels = hosp_order))



# Index proc, % ras vs non-ras

index_proc_list <- c("Rectal resection", "Hysterectomy", "Lobectomy of lung", "Prostatectomy", "Tonsillectomy")

proc_index <- ras_proc_data %>% 
  mutate(main_op_type = case_when(main_op_type == "Abdominal hysterectomy" ~ "Hysterectomy", #vaginal/abdominal approach to hysterectomy not relevant... consider changign in all_ras_procs?
                                  main_op_type == "Vaginal hysterectomy" ~ "Hysterectomy",
                                  .default = main_op_type)) %>% 
  filter(main_op_type %in% index_proc_list &
         (main_op_phase == "phase1" | 
             main_op_phase == "phase2")) %>% 
  group_by(main_op_type, main_op_specialty, ras_proc, op_mth, hospital_name_grp) %>% 
  summarize(n = n()) %>% 
  group_by(main_op_type, main_op_specialty, op_mth, hospital_name_grp) %>% 
  mutate(tot_n = sum(n),
         prop = round(n/tot_n*100, 2)) #%>% 
  # mutate(hline = case_when(main_op_specialty == "Gynaecology" ~ 60,
  #                          main_op_specialty == "Thoracic" ~ 35,
  #                          main_op_specialty == "Urology" ~ 95,
  #                          main_op_specialty == "Colorectal" ~ 50,
  #                          main_op_specialty == "ENT" ~ 50),
  #        main_op_type = as.factor(main_op_type)) #thresholds

write_parquet(proc_index, paste0(data_dir, "management_report/proc_index.parquet"))

index_proc_plot <- ggplot(proc_index, aes(x = op_mth, y = prop, fill = ras_proc, #and this need to be split across specialty tabs
                                          tooltip = paste0("Hospital Location: ", hospital_name_grp,
                                                           "\n % phase 1 and 2 procedures; ", prop, "%",
                                                           "\n No. phase 1 and 2 procedures: ", n,
                                                           "\n Month: ", op_mth),
                                          data_id = ras_proc)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~ hospital_name_grp)+ 
  #geom_hline(aes(yintercept = hline), colour = "orange", linetype="dashed")+ #threshold
  labs(x = "Month", 
       y = "% procedures performed using RAS",
       fill = "Surgical approach",
       caption = "Data from SMR01",
       subtitle = paste0("Index procedure: ", main_op_type))+ 
  scale_fill_manual(values = c("#94AABD","#12436D"))+
  #theme_phs_ylines() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))
index_proc_plot

### All RAS procs by specialty split by procedure type 
proc_spec <- ras_proc_data %>% 
  filter(ras_proc == "RAS") %>% 
  group_by(main_op_type, main_op_specialty, op_mth, hospital_name_grp) %>% 
  summarize(n = n()) %>% 
  group_by(main_op_specialty, op_mth, hospital_name_grp) %>% 
  mutate(tot_n = sum(n),
         prop = round(n/tot_n*100, 2)) 

write_parquet(proc_spec, paste0(data_dir, "management_report/proc_spec.parquet"))

ggplot(proc_spec, aes(x = op_mth, y = prop, fill = main_op_type, #and this need to be split across specialty tabs
                       tooltip = paste0("Hospital Location: ", hospital_name_grp,
                                        "\n % procedure type performed by RAS; ", prop, "%",
                                        "\n No. procedure type conducted: ", n,
                                        "\n Month: ", op_mth),
                       data_id = main_op_type)) +
  geom_bar(stat = "identity") +
  facet_wrap(.~hospital_name_grp) +
  labs(x = "Month", 
       y = "% procedure type performed by RAS",
       fill = "Procedure type",
       caption = "Data from SMR01")+ 
  scale_fill_manual(values = c("#12436D","#28A197", "#801650", "#F46A25",
                               "#3E8ECC", "#3F085C", "#3D3D3D","#94AABD", 
                               "#B4DEDB", "#CCA2B9", "#FBC3A8", "#A8CCE8", 
                               "#A285D1", "#A8A8A8"))+
  #theme_phs_ylines() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))
