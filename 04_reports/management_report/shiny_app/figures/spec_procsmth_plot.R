#####################################################################################.
#### SRASA Management Report - Utilisation - Specialty Procedures per Month Plot ####
#####################################################################################.

#Author: Bex Madden
#Date:28/11/2025

# Simple bar chart showing phase 1 procedure volume by month, filter by location and specialty

### UI -------------------------------------------------------------------------

spec_procsmth_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("hospital"),
                label = "Hospital",
                choices = unique(subset(equity_procspec, 
                                        equity_procspec$proc_approach_binary == "RAS")$hospital_name),
                selected = "All"),
    withSpinner(girafeOutput(ns("spec_procsmth"),
                             width = "auto", height = "auto"))
  )
}

### Server ---------------------------------------------------------------------

spec_procsmth_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$spec_procsmth <- renderGirafe({
        
        chart_data <- equity_procspec %>% 
          filter(proc_approach_binary == "RAS") %>% 
          mutate(op_mth_year = format(op_mth_year, "%Y-%m")) %>% 
          filter(hospital_name == input$hospital) 
        
        spec_procsmth_plot <- ggplot(data = chart_data, 
                                     aes(x = op_mth_year, y = n, fill = code_specialty,
                                         tooltip = paste0("Hospital Location: ", hospital_name,
                                                          "\n Surgical Specialty; ", code_specialty,
                                                          "\n No. RAS procedures: ", n,
                                                          "\n Month: ", op_mth_year),
                                         data_id = op_mth_year)) +
          geom_bar_interactive(stat = "identity")+
          labs(x = "Month", 
               y = "Number of cases", 
               fill = "Surgical Specialty",
               caption = "Data from SMR01",
               subtitle = paste0("Patients receiving RAS only"))+ 
          scale_fill_manual(values = spec_colours)+
          theme_minimal() 
        
        girafe(ggobj = spec_procsmth_plot, 
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
