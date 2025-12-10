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
    selectInput(ns("specialty"),
                label = "Surgical Specialty",
                choices = unique(equity_agemean$code_specialty),
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
          filter(proc_approach_binary == "RAS") %>% 
          mutate(res_health_board = factor(res_health_board, levels = hb_order))
        
        y_limit <- round(max(chart_data$mean_age+chart_data$sd_age)+2, 0)
        y_limit_min <- round(min(chart_data$mean_age+chart_data$sd_age)-3, 0)
        
        sex_mean <- mean(subset(chart_data, 
                                chart_data$res_health_board == "All" & 
                                  chart_data$code_specialty == "All" &
                                  chart_data$sex == "All")$mean_age)
        
        chart_data <- chart_data %>% 
          filter(sex == input$sex,
                 code_specialty == input$specialty)
        
        equity_agemean_plot <- ggplot(data = chart_data) +
          geom_pointrange_interactive(aes(x = res_health_board, y = mean_age, 
                                          ymin = mean_age-sd_age, ymax = mean_age+sd_age,
                                           colour = sex,
                                           tooltip = paste0("Sex: ", sex,
                                                      "\n Mean age: ", round(mean_age, 2),
                                                      "\n Health Board of Residence: ", res_health_board),
                                     data_id = sex)) +
          geom_hline_interactive(yintercept = sex_mean, linetype = "dashed", 
                                 color = "grey30") +
          geom_text_interactive(aes(x = "All", y = sex_mean, 
                                    label = "mean", hjust = -0.1, vjust = -1, 
                                    colour = "grey30")) +
          scale_y_continuous(limits = c(y_limit_min, y_limit)) +
          labs(x = "Health Board of Residence", 
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