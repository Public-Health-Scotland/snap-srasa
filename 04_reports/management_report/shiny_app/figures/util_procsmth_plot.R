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
     selectInput(ns("hospital"),
                label = "Hospital",
                choices = unique(util_procsmth$hospital_name_grp)),
    withSpinner(girafeOutput(ns("util_procsmth"),
                             width = "auto", height = "auto"))
  )
}

### Server ---------------------------------------------------------------------

util_procsmth_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$util_procsmth <- renderGirafe({
        
        chart_data <- util_procsmth
        
        y_limit <- ceiling(max(chart_data$n))+1
        
        chart_data <- chart_data %>% 
          filter(hospital_name_grp == input$hospital) %>% 
          mutate(op_mth = format(op_mth, "%Y-%m"))
        
        util_procsmth_plot <- ggplot(data = chart_data, 
                                   aes(x = op_mth, y = n, fill = hospital_name_grp,
                                       tooltip = paste0("Hospital Location: ", hospital_name_grp,
                                                        "\n No. RAS procedures: ", n,
                                                        "\n Month: ", op_mth),
                                   data_id = op_mth)) +
          geom_bar_interactive(stat = "identity")+
          labs(x = "Month", 
             y = "Number of cases", 
             caption = "Data from SMR01",
             subtitle = paste0())+ 
          ylim(0, y_limit)+
          scale_fill_manual(values = hosp_colours)+
          #  facet_wrap(.~ hospital_name_grp) +
          theme_minimal() +
          theme(legend.position = 'none') 
      
      girafe(ggobj = util_procsmth_plot, 
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
