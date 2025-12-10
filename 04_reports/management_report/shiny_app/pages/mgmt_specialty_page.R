#########################################################.
#### SRASA Management Report - Specialty Access page ####
#########################################################.

#Author: Bex Madden
#Date:08/12/2025

### Define page layout ---------------------------------------------------------

p_specialty <- nav_panel( #page_sidebar for controls in sidebar
  title = "Specialty Access",
  sidebar = sidebar(
    title = "Page controls"),
  value = "mgmt_specialty",
  h2("Utilisation of Surgical Robots by Specialty"),
  
  HTML("Specialty data is shown for Phase 1 RAS procedures only"),
  
  layout_columns(
    col_widths = breakpoints(xs = c(-2,8,-2), xxl = c(-3,6,-3)),
    
    card(full_screen = FALSE,
         fill = FALSE,
         card_header(paste0("Figure X: Total utilisation of surgical robots per 
              surgical specialty, by hospital (", start_date, " - ", latest_date, ")")),
         
         spec_procsmth_ui("spec_procsmth")),
    
  )
)

