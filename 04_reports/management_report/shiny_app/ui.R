######################################.
#### SRASA Management Report - UI ####
######################################.

#Author: Bex Madden
#Date:06/11/2025

ui <- page_navbar(
  title = "Scottish Robotic-Assisted Surgery Audit (SRASA) - Management Information",
  navbar_options = navbar_options(
    bg = "#80BCEA",
    theme = "light"
  ),
  fillable = FALSE,
  
  p_utilisation,
  p_specialty,
  p_equity,
  p_dataquality,

)