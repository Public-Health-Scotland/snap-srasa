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
    withSpinner(girafeOutput(ns("equity_simd"),
                             width = "auto", height = "auto"))
  )
}

### Server ---------------------------------------------------------------------

equity_simd_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$equity_simd <- renderGirafe({
        
        chart_data <- equity_simd# %>% 
          #filter(op_year == latest_year) 
        
        equity_simd_plot <- ggplot(data = chart_data, 
                                   aes(x = simd_quintile, y = app_prop, fill = proc_approach_binary,
                                       tooltip = paste0("SIMD quintile: ", simd_quintile,
                                                        "\n Surgical approach: ", proc_approach_binary,
                                                        "\n Number of patients: ", n_simd,
                                                        "\n % of patients: ", app_prop),
                                       data_id = simd_quintile)) +
          geom_bar_interactive(stat = "identity")+
          labs(x = "SIMD quintile", 
               y = "", 
               fill = "Surgical Approach",
               caption = "Data from SMR01",
               subtitle = paste0())+ 
          theme_minimal() +
          theme(legend.position = 'bottom')+
          scale_fill_manual(values = c("#CAC6D1", "#AF69A9")) +
          scale_y_continuous(labels = function(x) paste0(x,"%")) +
          facet_wrap(.~ sex)
        
        girafe(ggobj = equity_simd_plot,
               options = list(
                 opts_tooltip(css = "border-radius:5px; padding:5px", opacity = 1, use_fill = TRUE),
                 opts_hover(css = "opacity:0.8"), #makes translucent hover, or
                 opts_hover_inv(css = "opacity:0.4")), # makes non-selected translucent. format options https://r-graph-gallery.com/412-customize-css-in-interactive-ggiraph.html
               height_svg = 6,
               width_svg = 9)
      })
    }
  )
}