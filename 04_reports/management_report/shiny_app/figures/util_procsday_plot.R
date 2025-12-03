##################################################################################.
#### SRASA Management Report - Utilisation - Mean no. Procedures per Day Plot ####
##################################################################################.

#Author: Bex Madden
#Date:28/11/2025

# Simple bar chart showing mean number of procedures recorded per ACTIVE day, by location

### UI -------------------------------------------------------------------------

util_procsday_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("month"),
                label = "Month",
                choices = unique(util_procsday$op_mth),
                selected = max(util_procsday$op_mth)),
    selectInput(ns("hospital"),
                label = "Hospital",
                choices = unique(util_procsday$hospital_name_grp)),
    withSpinner(girafeOutput(ns("util_procsday"),height=450))
  )
}

### Server ---------------------------------------------------------------------

util_procsday_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$util_procsday <- renderGirafe({
        
        chart_data <- util_procsday 
        
        y_limit <- ceiling(max(chart_data$mean_procs_pd))+0.1
        
        chart_data <- chart_data %>% 
        filter(op_mth == input$month,
               hospital_name_grp == input$hospital) %>% 
          mutate(op_mth = format(op_mth, "%Y-%m"))
        
        y_limit <- ceiling(max(chart_data$mean_procs_pd))+0.5
        
        util_procsday_plot <- ggplot(data = chart_data, 
                                     aes(x = dow, y = mean_procs_pd, fill = hospital_name_grp,
                                         tooltip = paste0("Hospital Location: ", hospital_name_grp,
                                                          "\n Mean no. RAS procedures on ", dow,"s: ", mean_procs_pd,
                                                          "\n Month: ", op_mth),
                                         data_id = dow)) +
          geom_bar_interactive(stat = "identity")+
          geom_hline_interactive(yintercept = 1.5, linetype = "dashed", color = "red")+
          labs(x = "Month", 
               y = "Mean no. RAS procedures per day", 
               caption = "Data from SMR01",
               subtitle = paste0())+ 
          ylim(0, y_limit)+
          scale_fill_manual(values = hosp_colours)+
         # facet_wrap(.~ hospital_name_grp) +
          theme_minimal() +
          theme(legend.position = 'none') 
        
        girafe(ggobj = util_procsday_plot, 
               options = list(
                 opts_tooltip(css = "border-radius:5px;padding:5px", opacity = 0.8, use_fill = TRUE),
                 opts_hover(css = "opacity:1"), #makes translucent hover, or
                 opts_hover_inv(css = "opacity:0.4") # makes non-selected translucent. format options https://r-graph-gallery.com/412-customize-css-in-interactive-ggiraph.html
               ))
      })
    }
  )
}
