##################################################################.
### SNAP SRASA - Append lookup values to Monthly SMR01 extract ###
##################################################################.

# Bex Madden & Dylan Lewis
# 01/12/2025

append_lookups <- function(df, which_lookups = c("postcode", "urban_rural", "hospital", "specialty")){
  
  #' Append lookup values to SRASA Monthly SMR01 extract according to inputs
  #'
  #' @description 
  #' 
  #' @param df - 
  #' @param which_lookups -
  #' 
  #' @usage 
  #'
  #' @details 
  
  ras_lookup_data <- df %>% 
    mutate(postcode = format_postcode(postcode, format = "pc7"),
           age_group = create_age_groups(age_in_years),
           sex = as.factor(sex),
           sex = fct_recode(sex, "Male" = "1", "Female" = "2"))
  
  if("postcode" %in% which_lookups){
    cli_progress_step("Adding postcode information...")
    
  postcode_lookup <- phslookups::get_simd_postcode() %>%
    select(pc7, simd_quintile = simd2020v2_sc_quintile, res_health_board = hb2019name) %>% 
    mutate(res_health_board = str_replace_all(res_health_board, pattern = c(" and " = " & ", "NHS " = "")))
  
  ras_lookup_data <- ras_lookup_data %>% 
    left_join(postcode_lookup, by = join_by(postcode == pc7)) 
}
  
  if("urban_rural" %in% which_lookups){
    cli_progress_step("Adding urban/rural status...")
    
  urban_rural_lookup <- get_spd() %>% 
    select(pc7, urban_rural_6 = ur6_2022_name)
  
  ras_lookup_data <- ras_lookup_data %>% 
    left_join(urban_rural_lookup, by = join_by(postcode == pc7))
  }
  
  if("hospital" %in% which_lookups){
    cli_progress_step("Adding hospital information...")
    
  hosp_lookup <- read_csv(paste0(lookup_dir, "NHSScotland_hospitals.csv")) %>% 
    select(hospital_code, hospital_name, hosp_health_board = health_board) 
  
  ras_lookup_data <- ras_lookup_data %>% 
    left_join(hosp_lookup, by = join_by(location == hospital_code))
  }
  
  if("specialty" %in% which_lookups){
    cli_progress_step("Adding specialty information...")
    
    specialty_lookup <- read.csv(paste0(lookup_dir, "smr01_specialty_lookup.csv")) %>% 
      rename(specialty_desc = Value)
      
  ras_lookup_data <- ras_lookup_data %>% 
    left_join(specialty_lookup, by = join_by(specialty == Code))
  }
  
  return(ras_lookup_data)
  
}
