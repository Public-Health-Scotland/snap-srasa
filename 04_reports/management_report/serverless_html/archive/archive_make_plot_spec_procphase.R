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


## UI
#### procs by phase (per specialty)
card(
  card_header(str_glue("2.2 - Number of procedures performed by RAS monthly according to procedure prioritisation phase, by specialty ({date_string})")),
  do.call(navset_pill,
          args = map(
            sort(unique(spec_procsmth$main_op_specialty)),
            ~ggiraph_nav(capitalise_first(.x),
                         make_plot_spec_procphase(hosps, .x)
            )
          )
  ),
  card_body(
    "Note: For detail on which prioritisation phase each procedure belongs to, see the supplementary file downloadable from the 'About SRASA' page.",
    br(),
    "Note: All known candidate procedures are assigned to surgical specialty as per the supplementary file downloadable from the 'About SRASA' tab. Procedures performed by RAS that are not listed here have been assigned to the correct specialty where possible, but those that could not be satisfactorily matched are designated 'unlisted'.")
)
)
),  