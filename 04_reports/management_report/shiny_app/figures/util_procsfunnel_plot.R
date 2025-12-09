util_procsfunnel_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("month"),
                label = "Month",
                choices = unique(equity_procspec$op_mth_year),
                selected = max(equity_procspec$op_mth_year)),
    selectInput(ns("specialty"),
                label = "Specialty",
                choices = unique(equity_procspec$code_specialty)),
    withSpinner(girafeOutput(ns("plot"),
                             width = "auto", height = "auto"))
  )
}

util_procsfunnel_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      # move below into pre-processing
      procs_prop <- equity_procspec |>
        pivot_wider(values_from = n,
                    names_from = proc_approach_binary,
                    values_fill = 0) |>
        mutate(n = RAS + `Non-RAS`,
               prop = RAS / n,
               hospital_name = str_remove(hospital_name, "'")) |>
        filter(hospital_name != "All")
      
      plot_data <- reactive({
        procs_prop |>
          filter(op_mth_year == input$month,
                 code_specialty == input$specialty)
      })
      
      output$plot <- renderGirafe({
        funnel <- plot_data() |>
          ggplot(
            aes(x = n,
                y = prop,
                tooltip = hospital_name,
                data_id = hospital_name)) +
          geom_funnel_lines() +
          phs_funnel_style() +
          geom_point_interactive() +
          geom_point() +
          theme_minimal() +
          labs(x="Number of procedures",
               y="Proportion of procedures performed robotically")
        
        girafe(ggobj = funnel, 
               options = list(
                 opts_tooltip(css = "border-radius:5px; padding:5px", opacity = 1, use_fill = FALSE),
                 opts_hover(css = "opacity:0.8"), #makes translucent hover, or
                 opts_hover_inv(css = "opacity:0.4")), # makes non-selected translucent. format options https://r-graph-gallery.com/412-customize-css-in-interactive-ggiraph.html
               height_svg = 6,
               width_svg = 9)
      })
    }
  )
}


