theme_phs_ylines <- function(){
  list(
    theme_phs(),
    theme(panel.grid.major.y = ggplot2::element_line(color = grDevices::rgb(190 / 255, 190 / 255, 190 / 255)),
          panel.grid.major.x = ggplot2::element_blank())
  )
}


make_plot_util_procsmth <- function(hospitals, hosp_colours){
  chart_data <- util_procsmth %>% 
    filter(hospital_name_grp %in% hospitals) %>% 
    mutate(op_mth = format(op_mth, "%Y-%m"),
           hospital_name_grp = str_replace(hospital_name_grp, "'", "’"))
  
  util_procsmth_plot <- ggplot(data = chart_data, 
                               aes(x = op_mth, y = n, fill = hospital_name_grp,
                                   tooltip = paste0("Hospital Location: ", hospital_name_grp,
                                                    "\n No. RAS procedures: ", n,
                                                    "\n Month: ", op_mth),
                                   data_id = hospital_name_grp)) +
    geom_col_interactive(hover_nearest = TRUE) +
    labs(x = "Month", 
         y = "Number of cases", 
         fill = NULL,
         caption = "Data from SMR01",
         subtitle = paste0())+ 
    scale_fill_manual(values = hosp_colours) +
    guides(
      fill = guide_legend(nrow = ceiling(length(unique(chart_data$hospital_name_grp))/2)) 
    ) +
    theme_phs_ylines()
  
  return(util_procsmth_plot)
}

make_plot_util_procsday <- function(hospitals, month, hosp_colours){
  chart_data <- util_procsday %>% 
    filter(op_mth == month,
           hospital_name_grp %in% hospitals) %>% 
    mutate(op_mth = format(op_mth, "%Y-%m"))
  
  util_procsday_plot <- ggplot(data = chart_data, 
                               aes(x = dow, y = mean_procs_pd, fill = hospital_name_grp,
                                   tooltip = paste0("Hospital Location: ", hospital_name_grp,
                                                    "\n Mean no. RAS procedures on ", dow,"s: ", mean_procs_pd,
                                                    "\n Month: ", op_mth),
                                   data_id = dow)) +
    geom_bar_interactive(stat = "identity", hover_nearest = TRUE)+
    geom_hline_interactive(yintercept = 1.5, linetype = "dashed", color = "grey30")+
    labs(x = "Day of the Week", 
         y = "Monthly mean no. RAS procedures", 
         caption = "Data from SMR01",
         subtitle = paste0())+ 
    scale_fill_manual(values = hosp_colours)+
    facet_wrap(.~ hospital_name_grp) +
    theme_phs_ylines() +
    theme(legend.position = 'none',
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  
  return(util_procsday_plot)
}

## Spec access

make_plot_spec_procsmth <- function(hospitals, spec_colours){
  chart_data <- spec_procsmth %>% 
    filter(proc_approach_binary == "RAS") %>% 
    mutate(op_mth_year = format(op_mth_year, "%Y-%m")) %>% 
    filter(hospital_name %in% hospitals) 
  
  spec_procsmth_plot <- ggplot(data = chart_data, 
                               aes(x = op_mth_year, y = n, fill = code_specialty,
                                   tooltip = paste0("Hospital Location: ", hospital_name,
                                                    "\n Surgical Specialty; ", code_specialty,
                                                    "\n No. RAS procedures: ", n,
                                                    "\n Month: ", op_mth_year),
                                   data_id = op_mth_year)) +
    geom_bar_interactive(stat = "identity", hover_nearest = TRUE)+
    labs(x = "Month", 
         y = "Number of cases", 
         fill = "Surgical Specialty",
         caption = "Data from SMR01",
         #subtitle = paste0("Patients receiving RAS only")
         )+ 
    scale_fill_manual(values = spec_colours) +
    theme_phs_ylines() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
    facet_wrap(~hospital_name)
  
  return(spec_procsmth_plot)
}

make_plot_spec_funnel <- function(month, specialty, hosp_colours){
  chart_data <- spec_procsmth |>
    pivot_wider(values_from = n,
                names_from = proc_approach_binary,
                values_fill = 0) |>
    mutate(n = RAS + `Non-RAS`,
           prop = RAS / n,
           hospital_name = str_remove(hospital_name, "'")) |>
    filter(hospital_name != "All") |>
    filter(op_mth_year == month,
           code_specialty == specialty)
  
  funnel <- chart_data |>
    ggplot(
      aes(x = n,
          y = prop,
          fill = hospital_name,
          tooltip = paste0(hospital_name,
                           "\nNumber of procedures: ", n,
                           "\nProp. performed robotically", ": ", format(prop, nsmall=2, digits = 2)),
          data_id = hospital_name)) +
    geom_funnel_lines() +
    scale_funnel_phs() +
    geom_point_interactive(size = 3, shape = 21, hover_nearest = TRUE) +
    scale_fill_manual(values = hosp_colours,
                      na.value = "#3D3D3D",
                      guide = NULL) +
    labs(x="Number of procedures",
         y="Prop. performed robotically") +
    theme_phs()
  
  return(funnel)
}