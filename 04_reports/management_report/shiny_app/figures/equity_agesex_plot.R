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
    withSpinner(girafeOutput(ns("equity_agesex"),height=450))
  )
}

### Server ---------------------------------------------------------------------

equity_agesex_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$equity_agesex <- renderGirafe({
        
        chart_data <- equity_agesex# %>% 
          #filter(op_year == latest_year) #tried moving this outside renderGirafe({}) but still didn't render in shiny

        equity_agesex_plot <- ggplot(data = chart_data) +
            geom_bar_interactive(aes(x = age_group, y = n_age_sex, fill = sex,
                                     tooltip = paste0("Sex: ", sex,
                                                      "\n Age Group: ", age_group,
                                                      "\n Number of patients: ", n_age_sex),
                                     data_id = sex), 
                                 stat = "identity", width=1, colour="black",
                     subset(chart_data, chart_data$sex == "Female"))+
            geom_bar_interactive(aes(x = age_group, y = -n_age_sex, fill = sex,
                                     tooltip = paste0("Sex: ", sex,
                                                      "\n Age Group: ", age_group,
                                                      "\n Number of patients: ", n_age_sex),
                                     data_id = sex), 
                                 stat = "identity", width=1, colour="black",
                     subset(chart_data, chart_data$sex == "Male"))+
            scale_y_continuous(breaks = seq(round(max(chart_data$n_age_sex)/100)* -100,
                                            round(max(chart_data$n_age_sex)/100)* 100, 100),
                               limits = c(round(max(chart_data$n_age_sex)/100)* -100,
                                          round(max(chart_data$n_age_sex)/100)* 100),
                               labels = format(abs(seq(round(max(chart_data$n_age_sex)/100)* -100,
                                                       round(max(chart_data$n_age_sex)/100)* 100, 100)), big.mark = ",")) +
            labs(x = "Age Group", 
                 y = "Number of Patients", 
                 fill = "Sex", 
                 caption = "Data from SMR01",
                 subtitle = paste0())+
            theme_minimal() +
            theme(legend.position = 'bottom')+
            scale_fill_manual(values = c("#AF69A9", "#655E9D"))
        
        girafe(ggobj = equity_agesex_plot)
      })
    }
  )
}