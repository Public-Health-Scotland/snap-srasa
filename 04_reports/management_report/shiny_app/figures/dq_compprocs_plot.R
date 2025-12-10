#############################################################################.
#### SRASA Management Report - Data Quality - SMR01/Intuitive Comparison ####
#############################################################################.

#Author: Bex Madden
#Date:10/12/2025

# Stacked bar chart comparing DUMMY Intuitive data to SMR01 data on monthly utilisation

### UI -------------------------------------------------------------------------

dq_compprocs_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("month"),
                label = "Month",
                choices = unique(dq_compprocs$op_mth),
                selected = max(dq_compprocs$op_mth)),
    withSpinner(girafeOutput(ns("dq_compprocs"),
                             width = "auto", height = "auto"))
  )
}

### Server ---------------------------------------------------------------------

dq_compprocs_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$dq_compprocs <- renderGirafe({
        
        chart_data <- dq_compprocs  %>%
          mutate(hospital_name_grp = factor(hospital_name_grp,
                                            levels = c("Aberdeen Royal Infirmary", "Aberdeen Royal Infirmary - Intuitive",
                                                       "Glasgow Royal Infirmary", "Glasgow Royal Infirmary - Intuitive",
                                                       "Golden Jubilee University National Hospital", "Golden Jubilee University National Hospital - Intuitive",
                                                       "Ninewells Hospital", "Ninewells Hospital - Intuitive",
                                                       "Queen Elizabeth University Hospital", "Queen Elizabeth University Hospital - Intuitive",
                                                       "Raigmore Hospital", "Raigmore Hospital - Intuitive",
                                                       "Royal Infirmary of Edinburgh at Little France", "Royal Infirmary of Edinburgh at Little France - Intuitive",
                                                       "St John's Hospital", "St John's Hospital - Intuitive",
                                                       "University Hospital Crosshouse", "University Hospital Crosshouse - Intuitive",
                                                       "University Hospital Hairmyres", "University Hospital Hairmyres - Intuitive",
                                                       "Victoria Hospital", "Victoria Hospital - Intuitive",
                                                       "Western General Hospital", "Western General Hospital - Intuitive",
                                                       "Other Hospital Listed", "Other Hospital Listed - Intuitive"))) %>% 
          filter(op_mth == input$month)
        
        dq_compprocs_plot <- ggplot(data = chart_data, 
                                    aes(x = factor(hospital_name_grp), y = n, fill = robot,
                                        tooltip = paste0("Hospital: ", hospital_name_grp,
                                                         "\n SMR01 or Intuitive: ", robot,
                                                         "\n No. Procedures: ", n,
                                                         "\n % Intuitive procs found in SMR01: ", prop_smr_found, "%"),
                                        data_id = robot)) +
          geom_bar_interactive(stat = "identity", position = "stack") +
          labs(x = "Hospital", 
               y = "Number of Procedures", 
               fill = "Robot", 
               caption = "Data from SMR01",
               subtitle = paste0("All patients receiving RAS"))+
          coord_flip() +
          theme_minimal() +
          theme(legend.position = 'bottom') +
          scale_fill_manual(values = c("#0078D4", "#B3D7F2","#E6F2FB", "#6B5C85"))
        
        girafe(ggobj = dq_compprocs_plot,
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