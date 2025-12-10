#####################################################.
#### SRASA Management Report - Data Quality page ####
#####################################################.

#Author: Bex Madden
#Date:10/12/2025

### Define page layout ---------------------------------------------------------

p_dataquality <- nav_panel( #page_sidebar for controls in sidebar
  title = "SMR01 Data Quality",
  sidebar = sidebar(
    title = "Page controls"),
  value = "mgmt_dataquality",
  h2("Quality of SMR01 data compared to Intuitive's DUMMY records of robot utilisation"),
  
  layout_columns(
    card(full_screen = FALSE,
         fill = FALSE,
         card_header(paste0("Figure X: Comparison of monthly utilisation figures
                            from SMR01 and Intuitive datasets (", start_date, " - ", latest_date, ")")),
         
         dq_compprocs_ui("dq_compprocs")),
  
    col_widths = c(-2,8,-2)
  )
  
)