###########################################################.
#### SRASA Management Report - Equity - Age & Sex Plot ####
###########################################################.

#Author: Bex Madden
#Date:06/11/2025

# Mirrored bar chart showing all robotics cases by age and sex

### UI -------------------------------------------------------------------------

equity_agesex_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("hospital"),
                label = "Hospital",
                choices = unique(subset(equity_agesex, 
                                        equity_agesex$proc_approach_binary == "RAS")$hospital_name),
                selected = "All"),
    selectInput(ns("specialty"),
                label = "Surgical Specialty",
                choices = unique(equity_agesex$code_specialty),
                selected = "All"),
    withSpinner(girafeOutput(ns("equity_agesex"),
                             width = "auto", height = "auto"))
  )
}

### Server ---------------------------------------------------------------------

equity_agesex_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$equity_agesex <- renderGirafe({
        
        chart_data <- equity_agesex %>% 
          filter(proc_approach_binary == "RAS") %>% 
          filter(hospital_name == input$hospital,
                 code_specialty == input$specialty)
        
        y_limit <- ifelse(nrow(chart_data)>= 1, ceiling(max(chart_data$n_age_sex))+1, 0) #could move before dynamic filters to keep consistent across filters
        y_limit_neg <- ifelse(nrow(chart_data)>= 1, -1*(ceiling(max(chart_data$n_age_sex))+1), 0)
        
        chart_data <- chart_data %>% 
          mutate(y = ifelse(sex == "Male", y_limit_neg, y_limit),
                 x = "90+") 
        
        equity_agesex_plot <- ggplot(data = chart_data) +
          geom_bar_interactive(aes(x = age_group, y = n_age_sex, fill = sex,
                                   tooltip = paste0("Sex: ", sex,
                                                    "\n Age Group: ", age_group,
                                                    "\n Number of patients: ", n_age_sex),
                                   data_id = age_group), 
                               stat = "identity", width=0.9, #colour="black",
                               subset(chart_data, chart_data$sex == "Female"))+
          geom_bar_interactive(aes(x = age_group, y = -n_age_sex, fill = sex,
                                   tooltip = paste0("Sex: ", sex,
                                                    "\n Age Group: ", age_group,
                                                    "\n Number of patients: ", n_age_sex),
                                   data_id = age_group), 
                               stat = "identity", width=0.9, #colour="black",
                               subset(chart_data, chart_data$sex == "Male"))+
          geom_text_interactive(aes(x = x, y = y, label = sex, hjust = 1)) +
          scale_y_continuous(breaks = seq(y_limit_neg, y_limit, round(y_limit/5, 0)),
                             limits = c(y_limit_neg, y_limit),
                             labels = seq(y_limit_neg, y_limit, round(y_limit/5, 0))) +
          labs(x = "Age Group", 
               y = "Number of Patients", 
               fill = "Sex", 
               caption = "Data from SMR01",
               subtitle = paste0("Patients receiving RAS only"))+
          theme_minimal() +
          theme(legend.position = 'bottom')+
          scale_fill_manual(values = c("#AF69A9", "#655E9D"))
        
        girafe(ggobj = equity_agesex_plot,
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