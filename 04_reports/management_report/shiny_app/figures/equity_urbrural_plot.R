#############################################################.
#### SRASA Management Report - Equity - Urban/Rural Plot ####
#############################################################.

#Author: Bex Madden
#Date:11/11/2025

# Simple bar chart showing urban/rural status of patients

### UI -------------------------------------------------------------------------

equity_urbrural_ui <- function(id) {
  ns <- NS(id)
  tagList(
    withSpinner(girafeOutput(ns("equity_urbrural"),height=450))
  )
}

### Server ---------------------------------------------------------------------

equity_urbrural_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$equity_urbrural <- renderGirafe({
        
        chart_data <- equity_urbrural %>% 
          filter(op_year == latest_year) 
        
        equity_urbrur_plot <- ggplot(data = chart_data,
                                     aes(x = str_wrap(urban_rural_6, 15), y = app_prop, fill = approach_binary,
                                         tooltip = paste0("Urban/Rural status: ", urban_rural_6,
                                                          "\n Surgical Approach: ", approach_binary,
                                                          "\n Number of patients: ", n_urb),
                                         data_id = approach_binary))+
          geom_bar_interactive(stat = "identity")+
          labs(x = "Urban/Rural status",
               y = "",
               fill = "Surgical Approach",
               caption = "Data from SMR01",
               subtitle = paste0(latest_year))+ 
          theme_minimal() +
          theme(legend.position = 'bottom')+
          scale_fill_manual(values = c("#CAC6D1", "#AF69A9")) +
          scale_y_continuous(labels = function(x) paste0(x,"%")) 
        
        girafe(ggobj = equity_urbrur_plot)
      })
    }
  )
}
