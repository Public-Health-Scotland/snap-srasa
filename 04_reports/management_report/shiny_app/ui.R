######################################.
#### SRASA Management Report - UI ####
######################################.

#Author: Bex Madden
#Date:06/11/2025

ui <- page_navbar(
  title = "Scottish Robotic-Assisted Surgery Audit (SRASA) - Management Information",
  bg = "#80BCEA",
  inverse = TRUE,
  fillable = FALSE,
  
  #nav_panel(title = "Introduction", p_introduction),
  p_utilisation,
  p_specialty,
  p_equity,
  #nav_panel(title = "Outcome", p_outcome)
  # 
  # navset_pill_list(
  # 
  #     p_introduction,
  #     p_utilisation,
  #     p_equity,
  #     p_outcome
  #     
  #   )
)