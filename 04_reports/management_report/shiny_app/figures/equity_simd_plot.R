######################################################.
#### SRASA Management Report - Equity - SIMD Plot ####
######################################################.

#Author: Bex Madden
#Date:06/11/2025

# Simple bar chart showing SIMD quintile (add sex?)

### UI -------------------------------------------------------------------------

equity_simd_ui <- function(id) {
  ns <- NS(id)
  tagList(
    withSpinner(girafeOutput(ns("equity_simd"),height=450))
  )
}

### Server ---------------------------------------------------------------------

equity_simd_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$equity_simd <- renderGirafe({
        
        chart_data <- equity_simd %>% 
          filter(op_year == latest_year) 
        
        equity_simd_plot <- ggplot(data = chart_data, 
                                   aes(x = simd_quintile, y = app_prop, fill = approach_binary,
                                       tooltip = paste0("SIMD quintile: ", simd_quintile,
                                                        "\n Sex: ", sex,
                                                        "\n Number of patients: ", n_simd),
                                       data_id = approach_binary)) +
          geom_bar_interactive(stat = "identity")+
          labs(x = "SIMD quintile", 
               y = "", 
               fill = "Surgical Approach",
               caption = "Data from SMR01",
               subtitle = paste0(latest_year))+ 
          theme_minimal() +
          theme(legend.position = 'bottom')+
          scale_fill_manual(values = c("#CAC6D1", "#AF69A9")) +
          scale_y_continuous(labels = function(x) paste0(x,"%")) +
          facet_wrap(.~ sex)
        
        girafe(ggobj = equity_simd_plot)
      })
    }
  )
}