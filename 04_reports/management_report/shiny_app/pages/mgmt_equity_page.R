###############################################.
#### SRASA Management Report - Equity page ####
###############################################.

#Author: Bex Madden
#Date:06/11/2025

### Define page layout ---------------------------------------------------------

p_equity <- nav_panel( #page_sidebar for controls in sidebar
  title = "Patient Equity",
  sidebar = sidebar(
    title = "Page controls"),
  value = "mgmt_equity",
  h2("Patient Equity of Access to RAS"),
  
  HTML("Equity data is shown for Phase 1 procedures only, comparing patients who received RAS to those who did not"),
 
  layout_columns(
    col_widths = breakpoints(xs = c(-2,8,-2), xxl = c(-3,6,-3)),
    navset_card_tab(full_screen = FALSE,
                    title = paste0("Figure X: Total number of procedures performed robotically by 
                                 age group and sex of patients (", start_date, " - ", latest_date, ")"),
                    nav_panel(
                      "Number",
                      card_title("Number of patients undergoing a phase 1 RAS procedure"),
                      equity_agesex_ui("equity_agesex")),
                    
                    nav_panel(
                      "Proportion",
                      card_title("Proportion of patients undergoing a phase 1 procedure who received RAS"),
                      equity_agesexprop_ui("equity_agesexprop")),
                    
                    nav_panel(
                      "Mean Age",
                      card_title("Mean age of patients undergoing a phase 1 RAS procedure by location"),
                      equity_agemean_ui("equity_agemean"))
    ), 
    
    card(full_screen = FALSE,
         fill = FALSE,
         card_header(paste0("Figure X: Total utilisation of surgical robots by 
              SIMD quintile and sex of patients (", start_date, " - ", latest_date, ")")),
         
         equity_simd_ui("equity_simd")),
    
    # card(full_screen = FALSE,
    #      fill = FALSE,
    #      card_header(paste0("Figure X: Total utilisation of surgical robots by 
    #             urban/rural status of patients (", ")")),
    #      
    #      equity_urbrural_ui("equity_urbrural")),
    # 
    # card(full_screen = FALSE,
    #      fill = FALSE,
    #      card_header(paste0("Figure X: Total utilisation of surgical robots by patient's
    #             number of pre-existing medical conditions (", ")")),
    #      
    #      equity_pemc_ui("equity_pemc")),
  )
)


#navset_card_underline(
# title = "Robotics utilisation by patient demographics",
# nav_panel(paste0("Figure X: Total utilisation of surgical robots by 
# age group and sex of patients (", min(equity_agesex$op_year), ")"),
#     # layout_sidebar(
#     #   sidebar = sidebar(
#     #     title = "Local controls")), #have sidebar in-card rather than over the whole page - filter all plots on page at once or indivudally?
#     equity_agesex_ui("equity_agesex"))) #to get tabs in one card rather than individual cards per plot
