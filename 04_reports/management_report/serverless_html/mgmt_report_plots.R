### Setup ----------------------------------------------------------------------

# theme_phs for coord flip
theme_phs_ylines <- function(){
  list(
    theme_phs(),
    theme(panel.grid.major.y = ggplot2::element_line(color = grDevices::rgb(190 / 255, 190 / 255, 190 / 255)),
          panel.grid.major.x = ggplot2::element_blank())
  )
}

# default settings for ggiraph plots
ggiraph_default <- function(plot){
  girafe(ggobj = plot,
         options = list(
           opts_tooltip(
             opacity = 0.6,
             use_fill = TRUE),
           opts_hover(css = "opacity:0.8", nearest_distance = 10),
           opts_hover_inv(css = "opacity:0.4")),
         height_svg = 6,
         width_svg = 9,
         fonts = list(sans = "Open Sans")
  )
}

### Utilisation tab ------------------------------------------------------------
make_plot_util_procsmth <- function(hospitals, hosp_colours){
  chart_data <- util_procsmth %>% 
    filter(hospital_name_grp %in% hospitals) %>% 
    mutate(op_mth = as.Date(op_mth),
           hospital_name_grp = str_replace(hospital_name_grp, "'", "’"))
  
  plot <- ggplot(data = chart_data, 
                               aes(x = op_mth, y = n, fill = hospital_name_grp,
                                   tooltip = paste0("Hospital Location: ", hospital_name_grp,
                                                    "\n No. RAS procedures: ", n,
                                                    "\n Month: ", op_mth),
                                   data_id = hospital_name_grp)) +
    geom_bar_interactive(stat = "identity", width = 20, hover_nearest = TRUE) +
    labs(x = "Month", 
         y = "Number of cases", 
         fill = NULL,
         caption = "Data from SMR01, RAS procedures only",
         subtitle = paste0())+ 
    scale_fill_manual(values = hosp_colours) +
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b %Y"
    ) +
    guides(
      fill = guide_legend(nrow = ceiling(length(unique(chart_data$hospital_name_grp))/2)) 
    ) +
    theme_phs_ylines() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  
  plot_out <- ggiraph_default(plot)
  
  return(plot_out)
}

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

### Specialty level tab -------------------------------------------------------

make_plot_spec_procsmth <- function(hospitals, spec_colours){
  chart_data <- spec_procsmth %>% 
    filter(ras_proc == "RAS")  %>%
    mutate(op_mth = as.Date(op_mth)) %>% 
    filter(hospital_name_grp %in% hospitals) 
  
  plot <- ggplot(data = chart_data, 
                               aes(x = op_mth, y = n, fill = main_op_specialty,
                                   tooltip = paste0("Hospital Location: ", hospital_name_grp,
                                                    "\n Surgical Specialty: ", main_op_specialty,
                                                    "\n No. RAS procedures: ", n,
                                                    "\n Month: ", op_mth),
                                   data_id = op_mth)) +
    geom_bar_interactive(stat = "identity", width = 20, hover_nearest = TRUE)+
    labs(x = "Month", 
         y = "Number of cases", 
         fill = "Surgical Specialty",
         caption = "Data from SMR01, RAS procedures only",
         #subtitle = paste0("Patients receiving RAS only")
         )+ 
    scale_fill_manual(values = spec_colours) +
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b %Y"
    ) +
    theme_phs_ylines() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
    facet_wrap(~hospital_name_grp)
  
  plot_out <- ggiraph_default(plot)
  
  return(plot_out)
}

make_plot_spec_procphase <- function(hospitals, specialty){ #this one needs specialty tabs
  #"Number of procedures performed by RAS per month by procedure phase, by specialty ({start_date} - {latest_date})", (already in there)
  
  all_months <- spec_procphase$op_mth |> unique() |> sort()
  
  chart_data <- spec_procphase %>% 
    filter(ras_proc == "RAS",
           hospital_name_grp %in% hospitals,
           main_op_specialty == specialty) %>%
    complete(op_mth = all_months,
             main_op_phase = c("phase1", "phase2", "other"),
             hospital_name_grp = hospitals,
             fill = list(n = 0)) %>% 
    mutate(main_op_phase = factor(main_op_phase, 
                                  levels = c("phase1", "phase2", "other"),
                                  labels = c("Phase 1", "Phase 2", "Other")),
           op_mth = as_date(op_mth))
  
  plot <- ggplot(chart_data, 
                                aes(x = op_mth, y = n, fill = fct_rev(main_op_phase),
                                    tooltip = paste0("Hospital Location: ", hospital_name_grp,
                                                     "\n Procedure phase: ", main_op_phase,
                                                     "\n No. RAS procedures: ", n,
                                                     "\n Month: ", op_mth),
                                    data_id = op_mth)) +
    geom_bar_interactive(stat = "identity", width = 20, hover_nearest = TRUE) +
    labs(x = "Month", 
         y = "Total RAS procedures", 
         fill = "Procedure phase",
         caption = "Data from SMR01, RAS procedures only",
         subtitle = paste0())+ 
    scale_fill_manual(values = c("Other" = "#b1b1b1","Phase 1" = "#3F085C", "Phase 2" = "#3E8ECC")) + 
    scale_y_continuous(
      breaks = scales::breaks_width(5),
    ) +
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b %Y"
    ) +
    expand_limits(y = 5) +
    facet_wrap(~hospital_name_grp) +
    theme_phs_ylines() +
    theme(legend.position = 'bottom',
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  
  plot_out <- ggiraph_default(plot)
  
  return(plot_out)
}

### Procedure level tab --------------------------------------------------------

make_plot_proc_index <- function(hospitals, specialty){ #this one needs specialty tabs
  #"Proportion of each specialty's index procedure performed by RAS, by specialty ({start_date} - {latest_date})", #add threshold after steering grp
  
  all_months <- proc_index$op_mth |> unique() |> sort()
  proc <- proc_index %>% filter(main_op_specialty == specialty) |> pull(main_op_type) |> unique()
  
  chart_data <- proc_index %>% 
    filter(hospital_name_grp %in% hospitals,
           main_op_specialty == specialty) %>%
    ungroup() %>%
    complete(op_mth = all_months,
             main_op_type = proc,
             hospital_name_grp = hospitals,
             fill = list(n = 0,
                         prop = 100,
                         ras_proc = "No procedures")) %>%
    mutate(op_mth = as.Date(op_mth))
  
  proc_label <- replace_when(proc,
                             proc == "Hysterectomy" ~ "Hysterectomy (endometrial cancer only)",
                             proc == "Pharyngectomy" ~ "Pharyngectomy (cancer only)")
  
  plot <- ggplot(chart_data, 
                            aes(x = op_mth, y = prop, fill = ras_proc, 
                                tooltip = paste0("Hospital Location: ", hospital_name_grp,
                                                 "\n ", ras_proc,
                                                 "\n % of procedures: ", prop, "%",
                                                 "\n No. of procedures: ", n,
                                                 "\n Month: ", op_mth),
                                data_id = op_mth)) + 
    geom_bar_interactive(stat = "identity", width = 20, hover_nearest = TRUE) +
    facet_wrap(~ hospital_name_grp)+ 
    #geom_hline(aes(yintercept = hline), colour = "orange", linetype="dashed")+ #threshold
    labs(x = "Month", 
         y = "% procedures performed using RAS",
         fill = "Surgical approach",
         caption = "Data from SMR01, all surgical approaches to index procedure (phase 1 only)",
         subtitle = paste0("Index procedure: ", proc_label))+ 
    scale_fill_manual(values = c("Non-RAS" = "#94AABD", "RAS" = "#12436D", "No procedures" = "#b1b1b1"))+
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b %Y"
    ) +
    theme_phs_ylines() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  
  plot_out <- ggiraph_default(plot)
  
  return(plot_out)
}

# make_plot_proc_spec <- function(hospitals, specialty){ #this one needs specialty tabs
#   #"Procedures carried out in each specialty as a proportion of total procedures ({start_date} - {latest_date})",
#   
#   chart_data <- proc_spec %>% 
#     filter(hospital_name_grp %in% hospitals &
#              main_op_specialty == specialty)
#   
#   proc_spec_plot <-  ggplot(chart_data,
#                             aes(x = op_mth, y = prop, fill = main_op_type, 
#                                 tooltip = paste0("Hospital Location: ", hospital_name_grp,
#                                                  "\n % procedure type performed by RAS; ", prop, "%",
#                                                  "\n No. procedure type conducted: ", n,
#                                                  "\n Month: ", op_mth),
#                                 data_id = main_op_type)) +
#     geom_bar(stat = "identity") +
#     facet_wrap(.~hospital_name_grp) +
#     labs(x = "Month", 
#          y = "% procedure type performed by RAS",
#          fill = "Procedure type",
#          caption = "Data from SMR01, all candidate procedures")+ 
#     scale_fill_manual(values = c("#12436D","#28A197", "#801650", "#F46A25",
#                                  "#3E8ECC", "#3F085C", "#3D3D3D","#94AABD", 
#                                  "#B4DEDB", "#CCA2B9", "#FBC3A8", "#A8CCE8", 
#                                  "#A285D1", "#A8A8A8"))+
#     scale_x_datetime(
#       date_breaks = "1 month",
#       date_labels = "%b %Y"
#     ) +
#     theme_phs_ylines() +
#     theme(legend.position = "bottom",
#           axis.text.x = element_text(angle = 45, hjust = 1))
#   
#   return(proc_spec_plot)
# }

make_table_proc_spec <- function(hospitals){
  proc_spec %>% 
    filter(hospital_name_grp %in% hospitals) %>%
    select(
      Month = op_mth,
      Hospital = hospital_name_grp,
      Specialty = main_op_specialty,
      Procedure = main_op_type,
      `Number of procedures` = n,
      `% within specialty` = prop) %>%
    arrange(Month, Hospital, Specialty) %>%
    datatable(extensions = c('Select', 'SearchPanes'),
              selection = 'none',
              rownames = FALSE,
              height = "100%",
              options = list(dom = 'Prtip',
                             searchPanes = list(
                               controls = FALSE,
                               viewCount = FALSE
                             ),
                             columnDefs = list(
                               list(searchPanes = list(show = FALSE),
                                    targets = 3:5)
                             )
              )) %>%
    formatDate(
      "Month", 
      method = "toLocaleString",
      params = list(
        "en-GB",
        list(month = "long", year = "2-digit")
      )
    )
}

### Data Quality/comparison tab ------------------------------------------------

make_plot_dq_comp <- function(hospitals){ #this one does NOT need specialty tabs
  #"Comparison of total RAS procedure numbers recorded by SMR01 and Intuitive, by hospital ({start_date} - {latest_date})",
  
  chart_data <- dq_comp %>% 
    rename(SMR01 = n, Intuitive = int_n) %>% 
    pivot_longer(SMR01:Intuitive, names_to = "dataset", values_to = "n_procs") %>% 
    filter(hospital_name_grp %in% hospitals) %>%
    mutate(op_mth = as.Date(op_mth))
  
  plot <- ggplot(chart_data, 
                         aes(x = op_mth, y = n_procs, fill = dataset,
                             tooltip = paste0("Hospital Location: ", hospital_name_grp,
                                              "\n Data source: ", dataset,
                                              "\n No. RAS procedures recorded: ", n_procs,
                                              "\n Month: ", op_mth),
                             data_id = dataset)) +
    geom_bar_interactive(stat = "identity", position = "dodge", width = 20, hover_nearest = TRUE) +
    labs(x = "Month", 
         y = "No. recorded RAS procedures", 
         fill = "Data source",
         caption = "Data from SMR01 and Intuitive, RAS procedures only",
         subtitle = paste0())+ 
    scale_fill_manual(values = c("#3E8ECC","#3F085C")) +
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b %Y"
    ) +
    facet_wrap(~hospital_name_grp)+
    theme_phs_ylines() +
    theme(legend.position = 'bottom',
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  
  plot_out <- ggiraph_default(plot)
  
  return(plot_out)
}

make_plot_dq_compspec <-function(hospitals, specialty){ #this one needs specialty tabs
  #"Specialty-level comparison of RAS procedure numbers recorded by SMR01 and Intuitive, by specialty ({start_date} - {latest_date})",
  
  all_months <- dq_compspec$op_mth |> unique() |> sort() |> as.Date()
  
  chart_data <- dq_compspec %>% 
    filter(hospital_name_grp %in% hospitals &
             main_op_specialty == specialty) %>%
    mutate(dataset = replace_values(dataset,
                                    "intuitive" ~ "Intuitive",
                                    "smr01" ~ "SMR01"),
           op_mth = as.Date(op_mth)) %>%
    ungroup() %>%
    complete(op_mth = all_months,
             main_op_specialty = specialty,
             hospital_name_grp = hospitals,
             dataset = c("Intuitive", "SMR01"),
             fill = list(n_procs = 0))

  plot <- ggplot(chart_data, 
                 aes(x = op_mth, y = n_procs, fill = dataset, 
                     tooltip = paste0("Hospital Location: ", hospital_name_grp,
                                      "\n Data source: ", dataset,
                                      "\n No. RAS procedures recorded: ", n_procs,
                                      "\n Month: ", op_mth),
                     data_id = dataset)) +
    geom_bar_interactive(stat = "identity", position = "dodge", width = 20, hover_nearest = TRUE) +
    labs(x = "Month", 
         y = "No. recorded RAS procedures", 
         fill = "Data source",
         caption = "Data from SMR01 and Intuitive, RAS procedures only",
         subtitle = paste0())+ 
    scale_fill_manual(values = c("#3E8ECC","#3F085C")) + 
    scale_y_continuous(
      breaks = scales::breaks_width(5),
    ) +
    expand_limits(y = 5) +
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b %Y"
    ) +
    facet_wrap(~hospital_name_grp)+
    theme_phs_ylines() +
    theme(legend.position = 'bottom',
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  
  plot_out <- ggiraph_default(plot)
  
  return(plot_out)
}

# 
# make_plot_candidate_procs <- function(hospitals, specialty){
#   chart_data <- spec_procsmth |>
#     mutate(op_mth = format(op_mth, "%Y-%m"),
#            hospital_name_grp = str_remove(hospital_name_grp, "'")) |>
#     complete(
#       hospital_name_grp,
#       op_mth,
#       main_op_specialty,
#       ras_proc,
#       fill = list(n = 0)
#     )  |>
#     filter(hospital_name_grp %in% hospitals,
#            main_op_specialty == specialty)
#   
#   candidate_procs_plot <- ggplot(data = chart_data, 
#                                  aes(x = op_mth, y = n, fill = ras_proc,
#                                      tooltip = paste0("Hospital Location: ", hospital_name_grp,
#                                                       "\n Surgical Specialty; ", main_op_specialty,
#                                                       "\n Approach: ", ras_proc,
#                                                       "\n No. procedures: ", n,
#                                                       "\n Month: ", op_mth),
#                                      data_id = op_mth)) +
#     geom_bar_interactive(stat = "identity", hover_nearest = TRUE)+
#     labs(x = "Month", 
#          y = "Number of procedures", 
#          fill = "Approach",
#          caption = "Data from SMR01") + 
#     scale_fill_manual(values = setNames(phs_colour_values[c(1, 11)], c("RAS", "Non-RAS"))) +
#     #coord_cartesian(ylim = c(0, max(5, max(chart_data$n, na.rm = TRUE)))) +
#     scale_y_continuous(
#       breaks = scales::breaks_width(5),
#     ) +
#     expand_limits(y = 5) +
#     theme_phs_ylines() +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
#     facet_wrap(~hospital_name_grp)
#   
#   return(candidate_procs_plot)
# }
# 
# make_plot_spec_funnel <- function(month, specialty, hosp_colours){
#   chart_data <- spec_procsmth |>
#     pivot_wider(values_from = n,
#                 names_from = ras_proc,
#                 values_fill = 0) |>
#     mutate(n = RAS + `Non-RAS`,
#            prop = RAS / n,
#            hospital_name_grp = str_remove(hospital_name_grp, "'")) |>
#     filter(hospital_name_grp != "All") |>
#     filter(op_mth == month,
#            main_op_specialty == specialty)
#   
#   funnel <- chart_data |>
#     ggplot(
#       aes(x = n,
#           y = prop,
#           fill = hospital_name_grp,
#           tooltip = paste0(hospital_name_grp,
#                            "\nNumber of procedures: ", n,
#                            "\nProp. performed robotically", ": ", format(prop, nsmall=2, digits = 2)),
#           data_id = hospital_name_grp)) +
#     geom_funnel_lines() +
#     scale_funnel_phs() +
#     geom_point_interactive(size = 3, shape = 21, hover_nearest = TRUE) +
#     scale_fill_manual(values = hosp_colours,
#                       na.value = "#3D3D3D",
#                       guide = NULL) +
#     labs(x="Number of procedures",
#          y="Prop. performed robotically") +
#     theme_phs()
#   
#   return(funnel)
# }

# make_res_equity_funnel <- function(month, specialty, hb_colours){
#   chart_data <- equity_resprop |>
#     mutate(prop = RAS / n) |>
#     filter(res_health_board != "All") |>
#     filter(op_mth == month,
#            main_op_specialty == specialty)
#   
#   funnel <- chart_data |>
#     ggplot(
#       aes(x = n,
#           y = prop,
#           fill = res_health_board,
#           tooltip = paste0(res_health_board,
#                            "\nNumber of procedures: ", n,
#                            "\nProp. performed robotically", ": ", format(prop, nsmall=2, digits = 2)),
#           data_id = res_health_board)) +
#     geom_funnel_lines() +
#     scale_funnel_phs() +
#     geom_point_interactive(size = 3, shape = 21, hover_nearest = TRUE) +
#     scale_fill_manual(values = hb_colours,
#                       na.value = "#3D3D3D",
#                       guide = NULL) +
#     labs(x="Number of patients",
#          y="Prop. receiving RAS") +
#     theme_phs()
#   
#   return(funnel)
# }