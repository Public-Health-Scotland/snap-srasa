#############################################################.
#### SRASA Management Report - Equity - Average Age Plot ####
#############################################################.

#Author: Bex Madden
#Date:06/11/2025

# Mirrored bar chart showing average age of those accessign RAS by location

### UI -------------------------------------------------------------------------

equity_agemean_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("sex"),
                label = "Sex",
                choices = unique(equity_agemean$sex),
                selected = "All"),
    withSpinner(girafeOutput(ns("equity_agemean"),
                             width = "auto", height = "auto"))
  )
}

### Server ---------------------------------------------------------------------

equity_agemean_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$equity_agemean <- renderGirafe({
        
        chart_data <- equity_agemean %>% 
          mutate(hospital_name = factor(hospital_name, levels = hosp_order)) %>% 
          filter(proc_approach_binary == "RAS",
                 sex == input$sex)
        
        sex_mean <- round(mean(chart_data$mean_age))
       
        equity_agemean_plot <- ggplot(data = chart_data) +
          geom_pointrange_interactive(aes(x = hospital_name, y = mean_age, 
                                          ymin = mean_age-sd_age, ymax = mean_age+sd_age,
                                           colour = sex,
                                           tooltip = paste0("Sex: ", sex,
                                                      "\n Mean age: ", round(mean_age, 2),
                                                      "\n Hospital: ", hospital_name),
                                     data_id = sex)) +
          geom_hline_interactive(yintercept = sex_mean, linetype = "dashed", 
                                 color = "grey30") +
          geom_text_interactive(aes(x = "Other Hospital Listed", y = sex_mean, 
                                    label = "mean", hjust = -0.1, vjust = -1, 
                                    colour = "grey30")) +
          scale_y_continuous(limits = c(20, 90)) +
          labs(x = "Hospital", 
               y = "Patient Mean Age", 
               colour = "Sex", 
               caption = "Data from SMR01",
               subtitle = paste0("Patients receiving RAS only"))+
          coord_flip() +
          theme_minimal() +
          theme(legend.position = 'bottom') +
          scale_colour_manual(values = c("All" = "#1E7F84", "Female" = "#AF69A9", "Male" = "#655E9D"))
        
        girafe(ggobj = equity_agemean_plot,
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