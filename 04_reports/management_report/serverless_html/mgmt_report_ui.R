report_ui <- page_navbar(
  
    ### Page options
    window_title = "Scottish Robotic-Assisted Surgery Audit",
    navbar_options = navbar_options(
      bg = "white",
      theme = "light"
    ),
    fillable = FALSE,
    
    ### Header
    title = img_base64("/conf/quality/srasa/(11) Scripts/Dylan/snap-srasa/04_reports/management_report/serverless_html/resources/phs-logo.png",
                       style = "width:120px;"),
    nav_item("Scottish Robotic-Assisted Surgery Audit"),
    
    ### Page one - about SRASA
    nav_panel(
      "About SRASA",
      layout_columns(
        col_widths = breakpoints(xs = 12, lg = c(-2,8,-2), xxl = c(-3,6,-3)),
        
        #### Data protection warning
        includeHTML(paste0(script_dir, "/resources/mgmt-info-distribution-warning.html")),
        
        #### About text
        card(card_header("About this report"),
             includeHTML(paste0(script_dir, "/resources/about.html"))),
       
        #### Download classifications
        card(card_header("Supplementary information"),
             span("The classification of procedures into 'phase 1', 'phase 2' and 'other' can be accessed here:"),
             downloadthis::download_file(paste0(script_dir, "/resources/SRASA procedure codes and phasing.xlsx"),
                                         button_label = "Priority procedure classification table",
                                         self_contained = TRUE))
      )
    ),
    
    ### Page two - utilisation
    nav_panel(
      "1. Total utilisation",
      layout_columns(
        col_widths = breakpoints(xs = 12, lg = c(-2,8,-2), xxl = c(-3,6,-3)),
        
        #### Total procs
        ggiraph_card(
          title = str_glue("1.1 - Total number of procedures performed by RAS monthly ({date_string})"),
          plot = make_plot_util_procsmth(hosps, hosp_colours),
          includeHTML(paste0(script_dir, "/resources/smr-note.html"))
        ),
        
        #### Procs by day
        ggiraph_card(
          title = str_glue("1.2 - Mean daily utilisation of RAS system in the latest three months ({date_string_3m})"),
          plot = make_plot_util_procsday(hosps, hosp_colours),
          "Note: This plot shows the number of procedures performed robotically on each day of the week, averaged over the most recent three months. A threshold line at 1 indicates the goal of daily utilisation of each robotic system. Please see figure 1.1 to examine total utilisation figures for the latest 3 months, and take notice of the information relating to SMR01 data completeness."
        )
      )
    ),
    
    ### Page three - specialties
    nav_panel(
      "2. By specialty",
      layout_columns(
        col_widths = breakpoints(xs = 12, lg = c(-2,8,-2), xxl = c(-3,6,-3)),
        
        #### procs by specialty
        ggiraph_card(
          title = str_glue("2.1 - Number of procedures performed by RAS monthly under each specialty ({date_string})"),
          plot = make_plot_spec_procsmth(hosps, spec_colours)
        ),
        
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
    
    ### Page four - procedures
    nav_panel(
      "3. By procedure",
      layout_columns(
        col_widths = breakpoints(xs = 12, lg = c(-2,8,-2), xxl = c(-3,6,-3)),
        
        #### index procs
        card(
          card_header(str_glue("3.1 - Proportion of the index procedure performed by RAS monthly, by specialty ({date_string})")),
          do.call(navset_pill,
                  args = map(
                    sort(unique(proc_index$main_op_specialty)),
                    ~ggiraph_nav(capitalise_first(.x),
                                 make_plot_proc_index(hosps, .x)
                    )
                  )
          ),
          card_body("Note: The index procedure is the main priority procedure for each specialty's transition to RAS")
        ),
        
        #### procs table
        card(
          card_header(str_glue("3.2 - Table of procedures performed by RAS monthly, with proportion of specialty utilisation attributable to each procedure type ({date_string})")),
          make_table_proc_spec(hosps),
          "Note: All known candidate procedures are assigned to surgical specialty as per the supplementary file downloadable from the 'About SRASA' tab. Procedures performed by RAS that are not listed here have been assigned to the correct specialty where possible, but those that could not be satisfactorily matched are designated 'unlisted'.",
          full_screen = T,
          fillable = F
        )
      )
    ),
    
    ### Page five - data quality
    nav_panel(
      "4. Data quality",
      layout_columns(
        col_widths = breakpoints(xs = 12, lg = c(-2,8,-2), xxl = c(-3,6,-3)),
        
        #### Intuitive comparison
        ggiraph_card(str_glue("4.1 - Comparison of RAS utilisation figures as recorded in SMR01 and Intuitive monthly ({date_string})"),
                     make_plot_dq_comp(hosps)),
        
        #### Intuitive comparison by specialty
        card(
          card_header(str_glue("4.2 - Comparison of RAS utilisation figures as recorded in SMR01 and Intuitive monthly, by specialty ({date_string})")),
          do.call(navset_pill,
                  args = map(
                    sort(unique(dq_compspec$main_op_specialty)),
                    ~ggiraph_nav(capitalise_first(.x),
                                 make_plot_dq_compspec(hosps, .x)
                    )
                  )
          ),
          card_body("Note: Records labelled 'Unspecified' here are those submitted to Intuitive without procedure information.",
                    br(),
                    "Note: All known candidate procedures are assigned to surgical specialty as per the supplementary file downloadable from the 'About SRASA' tab. Procedures performed by RAS that are not listed here have been assigned to the correct specialty where possible, but those that could not be satisfactorily matched are designated 'unlisted'.")
        )
      )
    ),
    
    # addtional header items
    nav_spacer(),
    nav_item(hb)
  ) |>
  
  ## theme options
    page_fluid(theme = bs_theme(primary = phs_colour_values[1],
                                secondary = phs_colour_values[2],
                                danger = "#B93A46",
                                base_font = "Open Sans",
                                "font-size-lg" = "1.1rem" ),
               gdtools::addGFontHtmlDependency(family = "Open Sans"))
