###############################################.
#### SRASA Management Report - Equity page ####
###############################################.

#Author: Bex Madden
#Date:06/11/2025

### Define page layout ---------------------------------------------------------

p_equity <- page_sidebar(
  title = "Equity",
  sidebar = sidebar(
    title = "Page controls"),
  value = "mgmt_equity",
  h1("Equity"),
  
  # equity_text1,
  # HTML("<br>"),
  
  #navset_card_underline(
  # title = "Robotics utilisation by patient demographics",
  # nav_panel(paste0("Figure X: Total utilisation of surgical robots by 
  # age group and sex of patients (", min(equity_agesex$op_year), ")"),
  #     # layout_sidebar(
  #     #   sidebar = sidebar(
  #     #     title = "Local controls")), #have sidebar in-card rather than over the whole page - filter all plots on page at once or indivudally?
  #     equity_agesex_ui("equity_agesex"))) #to get tabs in one card rather than individual cards per plot
  
  card(full_screen = TRUE,
      card_header(paste0("Figure X: Total utilisation of surgical robots by 
              age group and sex of patients (", latest_year, ")")),
      # layout_sidebar(
      #   sidebar = sidebar(
      #     title = "Local controls")), #have sidebar in-card rather than over the whole page - filter all plots on page at once or indivudally?
      equity_agesex_ui("equity_agesex")),
  
  card(full_screen = TRUE,
      card_header(paste0("Figure X: Total utilisation of surgical robots by 
              SIMD quintile of patients (", latest_year, ")")),
  
      equity_simd_ui("equity_simd")),
  
  card(full_screen = TRUE,
       card_header(paste0("Figure X: Total utilisation of surgical robots by 
              urban/rural status of patients (", latest_year, ")")),
       
       equity_urbrural_ui("equity_urbrural")),
  
  card(full_screen = TRUE,
       card_header(paste0("Figure X: Total utilisation of surgical robots by patient's
              number of pre-existing medical conditions (", latest_year, ")")),
       
       equity_pemc_ui("equity_pemc"))
)