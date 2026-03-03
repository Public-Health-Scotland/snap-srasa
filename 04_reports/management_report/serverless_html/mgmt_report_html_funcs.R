produce_report <- function(hb, start_date = NULL, latest_date = NULL){
 
  ##### Hospitals & Health Board
  hb <- str_replace(hb, " and", "&") #coerce to ampersand
  
  hospitals <- hospitals |> filter(health_board == hb, hosp_has_robot == "Yes") |> pull(hospital_name)
  
  ##### Dates ----
  if(is.null(latest_date)){
    latest_date <- Sys.Date() %>% 
      floor_date("month") %m-% months(2)
  } else {
    latest_date <- as_date(latest_date)
  }
  
  latest_month <- latest_date %>% 
    floor_date("month") %m-% months(1)
  
  if(is.null(start_date)){
    start_date <- latest_date %>% 
      floor_date("month") %m-% months(12)
  } else {
    start_date <- as_date(start_date)
  }
  
  date_string <- paste0(format(start_date, "%B %Y"), " - ", format(latest_month, "%B %Y"))
  
  hosp_colours <- phs_colour_values[1:length(hospitals)] |>
    setNames(str_replace(hospitals, "'", "’"))
  
  spec_colours <- c("Colorectal" = "#12436D",
                    "ENT" = "#28A197",
                    "Gynaecology" = "#ae1e6d",
                    "Thoracic" = "#F46A25",
                    "Urology" = "#A285D1",
                    "Gastrointestinal" = "#3E8ECC",
                    "Hepatobiliary" = "#94AABD",
                    "General surgery" = "#b1b1b1")
  
  ##### html
  report_html <- page_navbar(
    title = span(
      img_base64("/conf/quality/srasa/(11) Scripts/Dylan/snap-srasa/04_reports/management_report/serverless_html/resources/phs-logo.png",
                 style = "width:120px;"),
      "Scottish Robotic-Assisted Surgery Audit") ,
    navbar_options = navbar_options(
      bg = "white",
      theme = "light"
    ),
    fillable = FALSE,
    nav_panel(
      "About SRASA",
      layout_columns(
        col_widths = breakpoints(xs = c(-2,8,-2), xxl = c(-3,6,-3)),
        includeHTML(paste0(script_dir, "/resources/mgmt-info-distribution-warning.html")),
        card(card_header("About this report"),
             includeHTML(paste0(script_dir, "/resources/about.html"))),
        card(card_header("Supplementary information"),
             span("The classification of procedures into 'phase 1', 'phase 2' and 'non-priority' can be accessed here:"),
             downloadthis::download_file(paste0(script_dir, "/resources/SRASA procedure codes and phasing.xlsx"),
                                         button_label = "Priority procedure classification table",
                                         self_contained = TRUE))
      )
    ),
    nav_panel(
      "Total utilisation",
      layout_columns(
        col_widths = breakpoints(xs = c(-2,8,-2), xxl = c(-3,6,-3)),
        ggiraph_card(
          title = str_glue("Total number of RAS procedures monthly by hospital ({date_string})"),
          plot = make_plot_util_procsmth(hospitals, hosp_colours)
        ),
        ggiraph_card(
          title = str_glue("Mean no. RAS procedures performed per day, by hospital ({format(latest_month, '%B %Y')})"),
          plot = make_plot_util_procsday(hospitals, month = latest_month, hosp_colours),
          "Note: This plot shows the number of procedures performed robotically on each day of the week, averaged over the most recent month. A threshold line at 1 indicates the goal of daily utilisation of each robotic system."
        )
      )
    ),
    nav_panel(
      "By specialty",
      layout_columns(
        col_widths = breakpoints(xs = c(-2,8,-2), xxl = c(-3,6,-3)),
        ggiraph_card(
          title = str_glue("Total utilisation of surgical robots per surgical specialty, by hospital ({date_string})"),
          plot = make_plot_spec_procsmth(hospitals, spec_colours)
        ),
        card(
          card_header(str_glue("Number of procedures performed by RAS per month by procedure phase, by specialty ({date_string})")),
          do.call(navset_pill,
                  args = map(
                    sort(unique(spec_procsmth$main_op_specialty)),
                    ~ggiraph_nav(capitalise_first(.x),
                                 make_plot_spec_procphase(hospitals, .x)
                    )
                  )
          ),
          card_body(
            "Note: For detail on which prioritisation phase each procedure belongs to, see the supplementary file downloadable from the 'About SRASA' page.",
            br(),
            "Note: All known candidate procedures are assigned to surgical specialty as per the supplementary file downloadable from the 'About SRASA' tab. Procedures performed by RAS that are not listed here have been assigned to the correct specialty where possible, but those that could not be satisfactorily matched are designated 'unlisted' and assigned to 'General surgery'")
        )
      )
    ),
    nav_panel(
      "By procedure",
      layout_columns(
        col_widths = breakpoints(xs = c(-2,8,-2), xxl = c(-3,6,-3)),
        card(
          card_header(str_glue("Proportion of each specialty's index procedure performed by RAS, by specialty ({date_string})")),
          do.call(navset_pill,
                args = map(
                  sort(unique(proc_index$main_op_specialty)),
                  ~ggiraph_nav(capitalise_first(.x),
                               make_plot_proc_index(hospitals, .x)
                  )
                )
          ),
          card_body("Note: The index procedure is the main priority procedure for each specialty's transition to RAS")
        ),
        card(
          card_header(str_glue("Number and proportion of procedure types performed by RAS per month, by specialty ({date_string})")),
          make_table_proc_spec(hospitals),
          "Note: All known candidate procedures are assigned to surgical specialty as per the supplementary file downloadable from the 'About SRASA' tab. Procedures performed by RAS that are not listed here have been assigned to the correct specialty where possible, but those that could not be satisfactorily matched are designated 'unlisted' and assigned to 'General surgery'",
          full_screen = T,
          fillable = F
        )
      )
    ),
    nav_panel(
      "Data quality",
      layout_columns(
        col_widths = breakpoints(xs = c(-2,8,-2), xxl = c(-3,6,-3)),
        ggiraph_card(str_glue("Comparison of total RAS procedure numbers recorded by SMR01 and Intuitive, by hospital ({date_string})"),
                     make_plot_dq_comp(hospitals),
                     "Note: Records labelled 'Unspecified' here are those submitted to Intuitive without procedure information."),
        card(
          card_header(str_glue("Specialty-level comparison of RAS procedure numbers recorded by SMR01 and Intuitive, by specialty ({date_string})")),
          do.call(navset_pill,
                args = map(
                  sort(unique(dq_compspec$main_op_specialty)),
                  ~ggiraph_nav(capitalise_first(.x),
                               make_plot_dq_compspec(hospitals, .x)
                  )
                )
          ),
          card_body("Note: All known candidate procedures are assigned to surgical specialty as per the supplementary file downloadable from the 'About SRASA' tab. Procedures performed by RAS that are not listed here have been assigned to the correct specialty where possible, but those that could not be satisfactorily matched are designated 'unlisted' and assigned to 'General surgery'")
        )
      )
    ),
    
    nav_spacer(),
    nav_item(hb)
  ) |>
    page_fluid(theme = bs_theme(primary = phs_colour_values[1],
                               secondary = phs_colour_values[2],
                               danger = "#B93A46",
                               base_font = "Open Sans",
                               "font-size-lg" = "1.1rem" ),
              gdtools::addGFontHtmlDependency(family = "Open Sans"))
  
  return(report_html)
}

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

ggiraph_card <- function(title, plot, ...){
  card(
    fill = FALSE,
    card_header(title),
    card_body(
      fillable=FALSE,
      ggiraph_default(plot)
    ),
    card_body(...)
  )
}

ggiraph_nav <- function(tab_name, plot, ...){
  nav_panel(
    tab_name,
    card_body(
      fillable=FALSE,
      ggiraph_default(plot)
    ),
    card_body(...)
  )
}


capitalise_first <- function(x) {
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}


save_self_contained_html <- function(taglist, output_filepath) {
  # Normalize output path
  output_filepath <- xfun::normalize_path(output_filepath)
  
  # Create a temporary directory for intermediate files
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  # Temporary HTML file inside temp_dir
  temp_html <- file.path(temp_dir, "temp.html")
  
  # Save the taglist as HTML (with /libs folder if needed)
  htmltools::save_html(taglist, file = temp_html, libdir = file.path(temp_dir, "libs"))
  
  # Remove <!DOCTYPE> lines for pandoc
  input_lines <- readLines(temp_html, warn = FALSE)
  writeLines(input_lines[!grepl("<!DOCTYPE", input_lines, fixed = TRUE)], temp_html)
  
  # Create a minimal template for pandoc
  template <- tempfile(fileext = ".html")
  on.exit(unlink(template), add = TRUE)
  xfun::write_utf8("$body$", template)
  
  # Determine pandoc format
  from <- if (rmarkdown::pandoc_available("1.17")) "markdown_strict" else "markdown"
  
  # Convert to self-contained HTML
  rmarkdown::pandoc_convert(
    input = temp_html,
    from = from,
    output = output_filepath,
    options = c("--embed-resources", "--standalone", "--template", template)
  )
  
  invisible(output_filepath)
}

batch_reports <- function(health_boards, date_from, date_to, output_dir) {
  #create output directory if it doesn't yet exist
  if(!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  health_boards |>
    walk(~produce_report(.x, date_from, date_to) |>
          save_self_contained_html(paste0(output_dir,
                                          "/srasa_mgmt_report_",
                                          str_to_snake(
                                            paste0(.x,
                                                   format(date_from, "%b%y"),
                                                   format(date_to %m-% months(1), "%b%y")
                                                   )
                                          ),
                                          ".html"
          ))
    )
}


# base64 encode images to embed
img_base64 <- function(path, ...) {
  base64 <- base64enc::dataURI(file = path, mime = "image/png")
  htmltools::tags$img(src = base64, ...)
}
