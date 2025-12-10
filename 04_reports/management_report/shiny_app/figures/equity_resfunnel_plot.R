###########################################################.
#### SRASA Management Report - Equity - Resfunnel Plot ####.
###########################################################.

#Author: dylanl01
#Date: 10/12/2025

# Description

### UI -------------------------------------------------------------------------

equity_resfunnel_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("month"),
                label = "Month",
                choices = unique(equity_resprop$op_mth_year),
                selected = max(equity_resprop$op_mth_year)),
    selectInput(ns("specialty"),
                label = "Specialty",
                choices = unique(equity_resprop$code_specialty),
                selected = "All"),
    funnel_ui(id = ns("funnel"))
  )
}

### Server ---------------------------------------------------------------------

equity_resfunnel_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      plot_data <- reactive({
        equity_resprop |>
          filter(op_mth_year == input$month,
                 code_specialty == input$specialty)
      })
      
      funnel_server(id = "funnel", data = plot_data, by = res_health_board)
    }
  )
}


### Usage ----------------------------------------------------------------------

# Document proper usage below (required arguments, etc)
# equity_resfunnel_ui(id = "resfunnel")
# equity_resfunnel_server(id = "resfunnel")
