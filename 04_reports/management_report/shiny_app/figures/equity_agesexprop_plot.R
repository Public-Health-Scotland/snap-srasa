#############################################################.
#### SRASA Management Report - Equity - Age & Sex % Plot ####
#############################################################.

#Author: Bex Madden
#Date:06/11/2025

# Mirrored bar chart showing % of all phase 1 procedures performed robotically,
# by age and sex

### UI -------------------------------------------------------------------------

equity_agesexprop_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("healthboard"),
                label = "Health Board of Residence",
                choices = unique(equity_agesex$res_health_board),
                selected = "All"),
    selectInput(ns("specialty"),
                label = "Surgical Specialty",
                choices = unique(equity_agesex$code_specialty),
                selected = "All"),
    withSpinner(girafeOutput(ns("equity_agesexprop"),
                             width = "auto", height = "auto"))
  )
}

### Server ---------------------------------------------------------------------

equity_agesexprop_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$equity_agesexprop <- renderGirafe({
       
        y_limit <- 110
        y_limit_neg <- -110
        
        chart_data <- equity_agesex %>% 
          mutate(y = ifelse(sex == "Male", y_limit_neg, y_limit),
                 x = "90+",
                 res_health_board = factor(res_health_board, levels = hb_order),
                 app_prop = case_when(sex == "Male" ~ -1*app_prop,
                                       .default = app_prop)) %>% 
          filter(res_health_board == input$healthboard,
                 code_specialty == input$specialty)
        
        equity_agesexprop_plot <- ggplot(data = chart_data) +
          geom_bar_interactive(aes(x = factor(age_group), y = app_prop, fill = proc_approach_binary,
                                   tooltip = paste0("Sex: ", sex,
                                                    "\n Surgical approach: ", proc_approach_binary,
                                                    "\n Age Group: ", age_group,
                                                    "\n Number of patients: ", n_age_sex,
                                                    "\n % of patients: ", abs(app_prop), "%"),
                                   data_id = age_group), 
                               stat = "identity", width=0.9) +
          geom_text_interactive(aes(x = x, y = y, label = sex, hjust = 1)) +
          scale_y_continuous(breaks = seq(y_limit_neg+10, y_limit-10, 20),
                             limits = c(y_limit_neg, y_limit),
                             labels = format(abs(seq(y_limit_neg+10, y_limit-10, 20)), big.mark = ",")) +#labels = function(x) paste0(x,"%")
          labs(x = "Age Group", 
               y = "% of Patients", 
               fill = "Surgical Approach", 
               caption = "Data from SMR01",
               subtitle = paste0("All phase 1 procedures"))+
          theme_minimal() +
          theme(legend.position = 'bottom')+
          scale_fill_manual(values = c("#CAC6D1", "#AF69A9"))
        
        girafe(ggobj = equity_agesexprop_plot,
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