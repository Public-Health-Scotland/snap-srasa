####################################################.
#### SRASA Management Report - Utilisation page ####
####################################################.

#Author: Bex Madden
#Date:06/11/2025

### Define page layout ---------------------------------------------------------

p_utilisation <- nav_panel(
  title = "Utilisation",
  sidebar = sidebar(
    title = "Page controls"),
  value = "mgmt_utilisation",
  h2("Utilisation of Surgical Robots"),
  
  layout_columns(
    col_widths = breakpoints(xs = c(-2,8,-2), xxl = c(-3,6,-3)),
    card(full_screen = FALSE,
         fill = FALSE,
         card_header(paste0("Figure X: Total number of RAS procedures monthly by 
                          hospital (", start_date, " - ", latest_date, ")")),
         util_procsmth_ui("util_procsmth")),
    
    card(full_screen = FALSE,
         fill = FALSE,
         card_header(paste0("Figure X: Mean no. RAS procedures performed per
                          day, by hospital (", start_date, " - ", latest_date, ")")),
         util_procsday_ui("util_procsday"))
  
    
  )
)

