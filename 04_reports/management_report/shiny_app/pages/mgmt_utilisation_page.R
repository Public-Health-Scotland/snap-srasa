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
  
  card(full_screen = TRUE,
       card_header(paste0("Figure X: Total number of RAS procedures monthly by 
                          hospital (", ")")),
       
       util_procsmth_ui("util_procsmth")),
  
  card(full_screen = TRUE,
       card_header(paste0("Figure X: Mean no. RAS procedures performed per
                          day, by hospital (", ")")),
       
       util_procsday_ui("util_procsday"))
)
