###########################################################################.
#### SRASA Management Report - Utilisation - Procedures per Month Plot ####
###########################################################################.

#Author: Bex Madden
#Date:28/11/2025

# Simple bar chart showing total procedure volume by month, filter by location

### UI -------------------------------------------------------------------------

util_procsmth_ui <- function(id) {
  ns <- NS(id)
  tagList(
    withSpinner(girafeOutput(ns("util_procsmth"),height=450))
  )
}

### Server ---------------------------------------------------------------------

util_procsmth_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$util_procsmth <- renderGirafe({
        
        chart_data <- util_procsmth %>% 
          filter(op_year == latest_year) # ad dynamic filter by hospital location 
        
        util_procsmth_plot <- ggplot(data = chart_data, 
                                   aes(x = proc_mth_yr, y = n, fill = hospital_name,
                                       tooltip = paste0("Hospital Location: ", hospital_name,
                                                        "\n No. RAS procedures: ", n,
                                                        "\n Month: ", proc_mth_yr),
                                   data_id = proc_mth_yr)) +
          geom_bar_interactive(stat = "identity")+
          labs(x = "Month", 
             y = "Number of cases", 
             caption = "Data from SMR01",
             subtitle = paste0())+ 
          #facet_wrap(.~ health_board) +
          theme_minimal() +
          theme(legend.position = 'none') 
      
      girafe(ggobj = util_procsmth_plot, 
             options = list(
               opts_tooltip(css = "border-radius:5px;padding:5px", opacity = 0.8, use_fill = TRUE),
               opts_hover(css = "opacity:1"), #makes translucent hover, or
               opts_hover_inv(css = "opacity:0.4") # makes non-selected translucent. format options https://r-graph-gallery.com/412-customize-css-in-interactive-ggiraph.html
             ))
      })
    }
  )
}
