produce_report <- function(hb, start_date = NULL, latest_date = NULL){
 
  ##### Hospitals & Health Board
  health_board <- str_replace(hb, "and", "&") #coerce to ampersand
  hospcodes <- phsopendata::get_resource("c698f450-eeed-41a0-88f7-c1e40a568acc") |>
    mutate(HealthBoardName = phsmethods::match_area(HealthBoard) |> str_replace(" and ", " & "))
  
  hospitals <- hospcodes$HospitalName[hospcodes$HealthBoardName == health_board]
  hospitals <- hospitals[hospitals %in% util_procsday$hospital_name_grp]
  
  
  ##### Dates ----
  if(is.null(latest_date)){
    latest_date <- Sys.Date() %>% 
      floor_date("month") %m-% months(2)
  } else {
    latest_date <- as_date(latest_date)
  }
  
  latest_month <- latest_date %m-% months(1)
  
  if(is.null(start_date)){
    start_date <- latest_date %>% 
      floor_date("month") %m-% months(12)
  } else {
    start_date <- as_date(latest_date)
  }
  
  ##### Colours ----
  # hosp_colours <- c("Aberdeen Royal Infirmary" = "#12436D",
  #                   "Glasgow Royal Infirmary" = "#94AABD",
  #                   "Golden Jubilee University National Hospital" = "#28A197",
  #                   "Ninewells Hospital" = "#B4DEDB",
  #                   "Queen Elizabeth University Hospital" = "#801650",
  #                   "Raigmore Hospital" = "#CCA2B9",
  #                   "Royal Infirmary of Edinburgh at Little France" = "#F46A25",
  #                   "St John's Hospital" = "#FBC3A8",
  #                   "University Hospital Crosshouse" = "#3E8ECC",
  #                   "University Hospital Hairmyres" = "#A8CCE8",
  #                   "Victoria Hospital" = "#3F085C",
  #                   "Western General Hospital" = "#A285D1",
  #                   "Other Hospital Listed" = "#3D3D3D")
  
  hosp_colours <- phs_colour_values[1:length(hospitals)] |>
    setNames(hospitals)
  
  spec_colours <- c("colorectal" = "#12436D",
                    "ENT" = "#28A197",
                    "gynaecology" = "#801650",
                    "thoracic" = "#F46A25",
                    "urology" = "#A285D1",
                    "gastroenterology" = "#3E8ECC",
                    "hepatobiliary" = "#3F085C",
                    "other surgical specialty" = "#3D3D3D")
  
  
  ##### html
  report_html <- page_navbar(
    title = "Scottish Robotic-Assisted Surgery Audit - Management Report",
    navbar_options = navbar_options(
      bg = "#80BCEA",
      theme = "light"
    ),
    fillable = FALSE,
    nav_panel(
      "Utilisation",
      layout_columns(
        col_widths = breakpoints(xs = c(-2,8,-2), xxl = c(-3,6,-3)),
        ggiraph_card(
          title = str_glue("Total number of RAS procedures monthly by hospital ({start_date} - {latest_date})"),
          plot = make_plot_util_procsmth(hospitals, hosp_colours)
        ),
        ggiraph_card(
          title = str_glue("Mean no. RAS procedures performed per day, by hospital ({format(latest_month, '%B %Y')})"),
          plot = make_plot_util_procsday(hospitals, month = latest_month, hosp_colours)
        )
      )
    ),
    nav_panel(
      "Specialty access",
      layout_columns(
        col_widths = breakpoints(xs = c(-2,8,-2), xxl = c(-3,6,-3)),
        ggiraph_card(
          title = str_glue("Total utilisation of surgical robots per surgical specialty, by hospital ({start_date} - {latest_date})"),
          plot = make_plot_spec_procsmth(hospitals, spec_colours)
        ),
        do.call(navset_card_tab,
          args = map(
            sort(unique(spec_procsmth$code_specialty)),
            ~ggiraph_nav(capitalise_first(.x),
                         title = str_glue(
                           "Proportion of Phase 1 {spec} procedures performed robotically per hospital ({format(latest_month, '%B %Y')})",
                           spec = .x),
                         make_plot_spec_funnel(month = latest_month,
                                               specialty = .x,
                                               hosp_colours = hosp_colours[names(hosp_colours) %in% hospitals])
            )
          )
        )
      )
    ),
    nav_spacer(),
    nav_item(hb)
  )
  
  return(report_html)
}


ggiraph_card <- function(title, plot){
  card(
    fill = FALSE,
    card_header(title),
    card_body(
      fillable=FALSE,
      girafe(ggobj = plot,
             options = list(
               opts_tooltip(css = "
                              border-radius:5px; 
                              padding:5px;
                              text-shadow: 0 0 1px white, 0 0 1px white;
                              ",
                            opacity = 1, use_fill = TRUE),
               opts_hover(css = "opacity:0.8", nearest_distance = 10),
               opts_hover_inv(css = "opacity:0.4")),
             height_svg = 6,
             width_svg = 9)
    )
  )
}

ggiraph_nav <- function(tab_name, title, plot){
  nav_panel(
    tab_name,
    card_title(title),
    card_body(
      fillable=FALSE,
      girafe(ggobj = plot,
             options = list(
               opts_tooltip(css = "
                              border-radius:5px; 
                              padding:5px;
                              text-shadow: 0 0 1px white, 0 0 1px white;
                              ",
                            opacity = 1, use_fill = TRUE),
               opts_hover(css = "opacity:0.8", nearest_distance = 10),
               opts_hover_inv(css = "opacity:0.4")),
             height_svg = 6,
             width_svg = 9)
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
