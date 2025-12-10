###############################################.
#### SRASA Management Report - Funnel Plot ####.
###############################################.

#Author: dylanl01
#Date: 10/12/2025

# Description

### UI -------------------------------------------------------------------------

funnel_ui <- function(id) {
  ns <- NS(id)
  tagList(
    withSpinner(girafeOutput(ns("plot"),
                             width = "auto", height = "auto"))
  )
}

### Server ---------------------------------------------------------------------

funnel_server <- function(id,
                          data,
                          by,
                          ylab = "Prop. performed robotically",
                          n = n,
                          prop = prop) {
  moduleServer(
    id,
    function(input, output, session) {
      
      if(!is.reactive(data)){
        data <- reactive(data)
      }
      
      output$plot <- renderGirafe({
        req(data(), cancelOutput = TRUE)
        funnel <- data() |>
          ggplot(
            aes(x = {{n}},
                y = {{prop}},
                tooltip = paste0({{by}},
                                 "\nNumber of procedures: ", {{n}},
                                 "\n", ylab, ": ", format({{prop}}, nsmall=2, digits = 2)),
                data_id = {{by}})) +
          geom_funnel_lines() +
          phs_funnel_style() +
          geom_point_interactive(size = 3) +
          theme_minimal() +
          theme(legend.position = "bottom",
                legend.title = element_blank()) +
          labs(x="Number of procedures",
               y=ylab)
        
        girafe(ggobj = funnel, 
               options = list(
                 opts_selection(type="none"),
                 opts_tooltip(css = "border-radius:5px; padding:5px; background-color: #9F9BC2", opacity = 1, use_fill = FALSE),
                 opts_hover(css = "opacity:1"), #makes translucent hover, or
                 opts_hover_inv(css = "opacity:0.4")), # makes non-selected translucent. format options https://r-graph-gallery.com/412-customize-css-in-interactive-ggiraph.html
               height_svg = 6,
               width_svg = 9)
      })
    }
  )
}


### Usage ----------------------------------------------------------------------

# Document proper usage below (required arguments, etc)
# funnel_ui(id = "funnel")
# funnel_server(id = "funnel", data = equity_resprop, by = res_health_board)
