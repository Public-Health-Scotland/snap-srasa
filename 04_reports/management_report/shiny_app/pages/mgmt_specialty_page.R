#########################################################.
#### SRASA Management Report - Specialty Access page ####
#########################################################.

#Author: Bex Madden
#Date:08/12/2025

### Define page layout ---------------------------------------------------------

p_specialty <- nav_panel( #page_sidebar for controls in sidebar
  title = "Utilisation by Specialty",
  sidebar = sidebar(
    title = "Page controls"),
  value = "mgmt_specialty",
  h2("Utilisation of Surgical Robots by Specialty")
  )

