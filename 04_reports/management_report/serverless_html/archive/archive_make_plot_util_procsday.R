### Archived: make_util_procsday old method (mean activity over 3 months)

make_plot_util_procsday <- function(hospitals, hosp_colours){
  latest_month <- max(util_procsday$op_mth)
  three_month <- as.Date(latest_month %m-% months(2))
  
  chart_data <- util_procsday %>% 
    filter(op_mth >= three_month & op_mth <= latest_month, #last 3 months in data
           hospital_name_grp %in% hospitals) %>% 
    mutate(hospital_name_grp = fct_drop(hospital_name_grp)) %>%
    group_by(hospital_name_grp, dow, .drop = FALSE) %>%
    summarise(mean_3m = round(mean(mean_procs_pd), 2)) %>% #re-do mean to make mean per day over last 3 months
    mutate(#op_mth = format(op_mth, "%Y-%m"),
      hospital_name_grp = str_replace(hospital_name_grp, "'", "’"))
  
  plot <- ggplot(data = chart_data, 
                 aes(x = dow, y = mean_3m, fill = hospital_name_grp,
                     tooltip = paste0("Hospital Location: ", hospital_name_grp,
                                      "\n Mean no. RAS procedures on ", dow,"s: ", mean_3m,
                                      "\n 3 month average: ", format(three_month, "%Y-%m"), " to ", format(latest_month, "%Y-%m")),
                     data_id = dow)) +
    geom_bar_interactive(stat = "identity", hover_nearest = TRUE)+
    geom_hline_interactive(yintercept = 1, linetype = "dashed", color = "grey30")+
    labs(x = "Day of the Week", 
         y = "Monthly mean no. RAS procedures", 
         caption = "Data from SMR01, RAS procedures only",
         subtitle = paste0()) + 
    scale_fill_manual(values = hosp_colours )+
    facet_wrap(.~ hospital_name_grp) +
    theme_phs_ylines() +
    theme(legend.position = 'none',
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  
  plot_out <- ggiraph_default(plot)
  
  return(plot_out)
}

