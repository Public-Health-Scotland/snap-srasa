######################################################.
#### SRASA Management Report - Data - Utilisation ####
######################################################.

#Author: Bex Madden
#Date:06/11/2025

# Derivation of source data tables for the utilisation-related measures shown in the 
# SRASA management report

#source functions
source("./02_setup/subset_smr01_extract.R")

### set min and max dates ------------------------------------------------------
# from last month of smr01 completeness (approx 6wks before middle of given month)
# to a year prior to that date
# use < latest date and >= start_date 
yr_end <- Sys.Date() %>% 
  lubridate::floor_date("month") %m-% months(3)

yr_start <- yr_end %>% 
  lubridate::floor_date("month") %m-% months(12)

### Read in raw data from SMR01, no filtering for procedure --------------------
ras_min_data <- read_parquet(paste0(data_dir, "monthly_extract/srasa_smr_extract_min.parquet")) %>% #min data is 1 row per cis
  filter(op_mth >= yr_start & 
           op_mth < yr_end,
         ras_proc == TRUE) %>% #only counting ras procs for utilisation
  ungroup() 

wrong_hosp <- ras_min_data %>% #get list of hospital names that appear but do not have a robot
  filter(hosp_has_robot != "Yes") %>% 
  group_by(hospital_name) %>% 
  slice(1) %>% 
  dplyr::pull(., hospital_name)

ras_util_data <- ras_min_data %>% #some aberrant coding liekly due to transfers
  mutate(hospital_name_grp = case_when(hospital_name %in% wrong_hosp ~ "Other Hospital Listed", #contains non-RAS and private hospitals
                                   .default = hospital_name),
         hospital_name_grp = factor(hospital_name_grp, levels = hosp_order))

### Number of robotics procedures per month by hospital location ---------------
util_procsmth <- ras_util_data %>% 
  group_by(hospital_name_grp, op_mth, op_year) %>% 
  summarise(n = n()) %>% 
  ungroup() #compares ok with previous approach - one or 2 more or less per month

write_parquet(util_procsmth, paste0(data_dir,  "management_report/util_procsmth.parquet"))

### Mean daily number of robotics procedures by day per month ------------------
# from intuitive data - new method: year to date

#bank holiday and hospital lookups
bank_hols <- read.csv("../../../(12) Data/lookups/bank_hols.csv") %>% #how to access local holidays info? council-level unlikely to be reflected in true workign practice?
  mutate(date = as.Date(date, "%Y-%m-%d")) %>% 
  pull(date)

hospitals <- read.csv("../../../(12) Data/lookups/NHSScotland_hospitals.csv") %>% 
  filter(hosp_has_robot == "Yes") %>% 
  select(hospital_name, health_board)

# make df of all dates between yr_start and yr_end 
dates <- as.data.frame(seq(yr_start, yr_end, by = 1)) %>% 
  rename(date = `seq(yr_start, yr_end, by = 1)`) %>% 
  mutate(weekday = strftime(date, format = "%A"),
         working_day = case_when(weekday == "Saturday" | weekday == "Sunday" ~ "weekend",
                                 date %in% bank_hols ~ "holiday",
                                 .default = "working"))

# read in intuitive data
int_data <- read_parquet(paste0(data_dir, "intuitive/intuitive_rolling_data.parquet")) %>% 
  filter((start_date >= yr_start & start_date <= yr_end)) %>% # !hospital_name %in% new_hosps
  mutate(hosp_device = paste0(hospital_name, " - ", system_serial_number)) %>% 
  group_by(start_date, hosp_device) %>% 
  summarise(n = n()) 

# loop to extract % workign days used for each weekday for each device
device_list <- int_data %>% 
  ungroup()  %>% 
  select(hosp_device) %>% 
  distinct() %>% 
  dplyr::pull(hosp_device)

weekday_results <- NULL

for (i in device_list) {
  
  device <- i
  
  weekday_data <- int_data %>% 
    filter(hosp_device == device) %>% 
    right_join(dates, by = join_by(start_date == date)) %>% 
    arrange(start_date) %>% 
    mutate(monday = case_when(weekday == "Monday" & working_day == "working" ~ 1,
                              .default = 0),
           used_monday = case_when(weekday == "Monday" & working_day == "working"
                                   & n >= 1 ~ 1,
                                   .default = 0),
           tuesday = case_when(weekday == "Tuesday" & working_day == "working" ~ 1,
                               .default = 0),
           used_tuesday = case_when(weekday == "Tuesday" & working_day == "working"
                                    & n >= 1 ~ 1,
                                    .default = 0),
           wednesday = case_when(weekday == "Wednesday" & working_day == "working" ~ 1,
                                 .default = 0),
           used_wednesday = case_when(weekday == "Wednesday" & working_day == "working"
                                      & n >= 1 ~ 1,
                                      .default = 0),
           thursday = case_when(weekday == "Thursday" & working_day == "working" ~ 1,
                                .default = 0),
           used_thursday = case_when(weekday == "Thursday" & working_day == "working"
                                     & n >= 1 ~ 1,
                                     .default = 0),
           friday = case_when(weekday == "Friday" & working_day == "working" ~ 1,
                              .default = 0),
           used_friday = case_when(weekday == "Friday" & working_day == "working"
                                   & n >= 1 ~ 1,
                                   .default = 0)) %>% 
    
    ungroup() %>% 
    summarise(monday = sum(monday),
              used_monday = sum(used_monday),
              tuesday = sum(tuesday),
              used_tuesday = sum(used_tuesday),
              wednesday = sum(wednesday),
              used_wednesday = sum(used_wednesday),
              thursday = sum(thursday),
              used_thursday = sum(used_thursday),
              friday = sum(friday),
              used_friday = sum(used_friday),
              total_ops = sum(n, na.rm = TRUE)) %>% 
    mutate(prop_Monday = round(used_monday/monday*100,2),
           prop_Tuesday = round(used_tuesday/tuesday*100,2),
           prop_Wednesday = round(used_wednesday/wednesday*100,2),
           prop_Thursday = round(used_thursday/thursday*100,2),
           prop_Friday = round(used_friday/friday*100,2),
           hosp_device = device) %>% 
    select(prop_Monday:hosp_device)
  
  weekday_results <- rbind(weekday_results, weekday_data) %>% 
    arrange(hosp_device)
} 

# cleaning up df ready for plotting
util_procsday <- weekday_results %>% 
  mutate(hospital_name_grp = word(hosp_device, 1, sep = "\\ -"),
         start_date = yr_start,
         end_date = yr_end) %>% 
  left_join(hospitals, by = join_by(hospital_name_grp == hospital_name)) %>% 
  pivot_longer(prop_Monday:prop_Friday, names_to = "weekday", values_to = "prop", names_prefix = "prop_") %>% 
  mutate(weekday = as.factor(weekday),
         weekday = factor(weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")))

# save out
write_parquet(util_procsday, paste0(data_dir, "management_report/util_procsday.parquet"))


# util_procsday <- ras_util_data %>% 
#   mutate(dow = factor(format(as.Date(main_op_date, format="%d/%m/%Y"),"%A"),
#                       levels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
#                                  "Friday", "Saturday", "Sunday")),
#          week = floor_date(main_op_date, "week")) %>%
#   group_by(hospital_name_grp, week, dow) %>% 
#   summarise(n = n()) %>% 
#   ungroup() %>% 
#   tidyr::complete(hospital_name_grp, week, dow, 
#                   fill = list(n=0)) %>%
#   # then need to re-derive date vars for plotting. Complete does not work well with multiple date cols
#   mutate(op_mth = floor_date(week, "month"), 
#          op_year = format(as.Date(week, format="%Y-%m-%d"),"%Y"),
#          op_qt = lubridate::quarter(as.Date(week, format="%Y-%m-%d"), with_year = T)) %>% 
#   #and make mean per month (qt?)
#   group_by(hospital_name_grp, op_year, op_mth, dow, .drop = FALSE) %>% #is drop=F working fully? it should be possible to have a mean per day <1 e.g. if surgery not done every monday
#   summarise(mean_procs_pd = round(mean(n), 2)) %>% 
#   mutate(mean_procs_pd = ifelse(is.nan(mean_procs_pd), 0, mean_procs_pd)) %>% 
#   ungroup() 


