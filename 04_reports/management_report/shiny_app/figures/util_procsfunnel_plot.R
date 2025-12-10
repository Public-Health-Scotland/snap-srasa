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
    funnel_ui(id = ns("funnel"))
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
      
      funnel_server(id = "funnel", data = plot_data, by = hospital_name)
      
    }
  )
}


