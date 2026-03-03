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
    setNames(hospitals)
  
  spec_colours <- c("Colorectal" = "#12436D",
                    "ENT" = "#28A197",
                    "Gynaecology" = "#801650",
                    "Thoracic" = "#F46A25",
                    "Urology" = "#A285D1",
                    "Gastroenterology" = "#3E8ECC",
                    "Hepatobiliary" = "#3F085C",
                    "General surgery (other)" = "#3D3D3D",
                    "General surgery - unlisted" = "#3D3D3D",
                    "ENT - unlisted" = "#3D3D3D",
                    "Thoracic - unlisted" = "#3D3D3D",
                    "Urology - unlisted" = "#3D3D3D",
                    "Other specialty - unlisted" = "#3D3D3D")
  
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
      "1. Total utilisation",
      layout_columns(
        col_widths = breakpoints(xs = c(-2,8,-2), xxl = c(-3,6,-3)),
        ggiraph_card(
          title = str_glue("1.1 - Total number of procedures performed by RAS monthly ({date_string})"),
          plot = make_plot_util_procsmth(hospitals, hosp_colours)
        ),
        ggiraph_card(
          title = str_glue("1.2 - Mean daily utilisation of RAS system in the latest month ({format(latest_month, '%B %Y')})"),
          plot = make_plot_util_procsday(hospitals, month = latest_month, hosp_colours)
        )
      )
    ),
    nav_panel(
      "2. By specialty",
      layout_columns(
        col_widths = breakpoints(xs = c(-2,8,-2), xxl = c(-3,6,-3)),
        ggiraph_card(
          title = str_glue("2.1 - Number of procedures performed by RAS monthly under each specialty ({date_string})"),
          plot = make_plot_spec_procsmth(hospitals, spec_colours)
        ),
        do.call(navset_card_tab,
                args = map(
                  sort(unique(spec_procsmth$main_op_specialty)),
                  ~ggiraph_nav(capitalise_first(.x),
                               title = str_glue(
                                 "2.2 - Number of procedures performed by RAS monthly according to procedure prioritisation phase, by specialty ({date_string})",
                                 spec = .x),
                               make_plot_spec_procphase(hospitals, .x)
                  )
                )
        )
      )
    ),
    nav_panel(
      "3. By procedure",
      layout_columns(
        col_widths = breakpoints(xs = c(-2,8,-2), xxl = c(-3,6,-3)),
        do.call(navset_card_tab,
                args = map(
                  sort(unique(proc_index$main_op_specialty)),
                  ~ggiraph_nav(capitalise_first(.x),
                               title = str_glue(
                                 "3.1 - Proportion of the index procedure performed by RAS monthly, by specialty ({date_string})",
                                 spec = .x),
                               make_plot_proc_index(hospitals, .x)
                  )
                )
        ),
        card(
          card_header(str_glue("3.2 - Table of procedures performed by RAS monthly, with proportion of specialty utilisation attributable to each procedure type ({date_string})")),
          make_table_proc_spec(hospitals),
          full_screen = T,
          fillable = F
        )
      )
    ),
    nav_panel(
      "4. Data quality",
      layout_columns(
        col_widths = breakpoints(xs = c(-2,8,-2), xxl = c(-3,6,-3)),
        ggiraph_card(str_glue("4.1 - Comparison of RAS utilisation figures as recorded in SMR01 and Intuitive monthly ({date_string})"),
                     make_plot_dq_comp(hospitals)
                     ),
        do.call(navset_card_tab,
                args = map(
                  sort(unique(dq_compspec$main_op_specialty)),
                  ~ggiraph_nav(capitalise_first(.x),
                               title = str_glue(
                                 "4.2 - Comparison of RAS utilisation figures as recorded in SMR01 and Intuitive monthly, by specialty ({date_string})",
                                 spec = .x),
                               make_plot_dq_compspec(hospitals, .x)
                  )
                )
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

ggiraph_card <- function(title, plot){
  card(
    fill = FALSE,
    card_header(title),
    card_body(
      fillable=FALSE,
      ggiraph_default(plot)
    )
  )
}

ggiraph_nav <- function(tab_name, title, plot){
  nav_panel(
    tab_name,
    card_title(title),
    card_body(
      fillable=FALSE,
      ggiraph_default(plot)
    )
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
                                                   format(date_to, "%b%y")
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
