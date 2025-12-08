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
  h1("Utilisation"),
  
  layout_columns(
    card(full_screen = FALSE,
         fill = FALSE,
         card_header(paste0("Figure X: Total number of RAS procedures monthly by 
                          hospital (", start_date, " - ", latest_date, ")")),
         util_procsmth_ui("util_procsmth")),
    
    card(full_screen = FALSE,
         fill = FALSE,
         card_header(paste0("Figure X: Mean no. RAS procedures performed per
                          day, by hospital (", start_date, " - ", latest_date, ")")),
         util_procsday_ui("util_procsday")),
    card(full_screen = FALSE,
         fill = FALSE,
         card_header(paste0("Figure X: Proportion of Phase 1 procedures performed robotically per
                          month, by specialty (", start_date, " - ", latest_date, ")")),
         util_procsfunnel_ui("util_procsfunnel")),
    
    col_widths = c(-2,8,-2,-2,8,-2)
  )
)

