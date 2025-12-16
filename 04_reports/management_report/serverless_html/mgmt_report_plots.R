
make_plot_util_procsmth <- function(hospitals, hosp_colours){
  chart_data <- util_procsmth %>% 
    filter(hospital_name_grp %in% hospitals) %>% 
    mutate(op_mth = format(op_mth, "%Y-%m"))
  
  util_procsmth_plot <- ggplot(data = chart_data, 
                               aes(x = op_mth, y = n, fill = hospital_name_grp,
                                   tooltip = paste0("Hospital Location: ", hospital_name_grp,
                                                    "\n No. RAS procedures: ", n,
                                                    "\n Month: ", op_mth),
                                   data_id = hospital_name_grp)) +
    geom_col_interactive() +
    labs(x = "Month", 
         y = "Number of cases", 
         fill = NULL,
         caption = "Data from SMR01",
         subtitle = paste0())+ 
    scale_fill_manual(values = hosp_colours)+
    theme_phs()
  
  return(util_procsmth_plot)
}