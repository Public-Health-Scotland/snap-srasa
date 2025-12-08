######################################################.
#### SRASA Management Report - Equity - PEMC Plot ####
######################################################.

#Author: Bex Madden
#Date:06/11/2025

# Simple bar chart showing number of pre-existing medical conditions according to CCI

### UI -------------------------------------------------------------------------

equity_pemc_ui <- function(id) {
  ns <- NS(id)
  tagList(
    withSpinner(girafeOutput(ns("equity_pemc"),
                             width = "auto", height = "auto"))
  )
}

### Server ---------------------------------------------------------------------

equity_pemc_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$equity_pemc <- renderGirafe({
        
        chart_data <- equity_pemc #%>% 
          #filter(op_year == latest_year) 
        
        equity_pemc_plot <- ggplot(data = chart_data, 
                                   aes(x = cci_score_bin, y = app_prop, fill = approach_binary,
                                       tooltip = paste0("Number of PEMCs: ", cci_score_bin,
                                                        "\n Surgical Approach: ", approach_binary,
                                                        "\n Number of patients: ", n_pemc),
                                       data_id = approach_binary)) +
          geom_bar_interactive(stat = "identity")+
          labs(x = "No. pre-existing medical conditions", 
               y = "", 
               fill = "Surgical Approach",
               caption = "Data from SMR01",
               subtitle = paste0())+ 
          theme_minimal() +
          theme(legend.position = 'bottom')+
          scale_fill_manual(values = c("#CAC6D1", "#AF69A9")) +
          scale_y_continuous(labels = function(x) paste0(x,"%")) 
        
        girafe(ggobj = equity_pemc_plot, 
               options = list(
                 opts_tooltip(css = "border-radius:5px;padding:5px", opacity = 0.8, use_fill = TRUE),
                 opts_hover(css = "opacity:1"), #makes translucent hover, or
                 opts_hover_inv(css = "opacity:0.4") # makes non-selected translucent. format options https://r-graph-gallery.com/412-customize-css-in-interactive-ggiraph.html
                 ))
      })
    }
  )
}