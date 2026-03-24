# Produce report functions ------------------------------------------------

produce_report <- function(hb, start_date = NULL, latest_date = NULL){
 
  ##### Hospitals & Health Board
  hb <- str_replace(hb, " and", "&") #coerce to ampersand
  
  hosps <- hospitals |> filter(health_board == hb, hosp_has_robot == "Yes") |> pull(hospital_name)
  
  ##### Dates
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
  date_string_3m <- paste0(format(latest_month %m-% months(2), "%B %Y"), " - ", format(latest_month, "%B %Y"))
  
  ##### colours
  hosp_colours <- phs_colour_values[1:length(hosps)] |>
    setNames(str_replace(hosps, "'", "’"))
  
  spec_colours <- c("Colorectal" = "#12436D",
                    "ENT" = "#28A197",
                    "Gynaecology" = "#ae1e6d",
                    "Thoracic" = "#F46A25",
                    "Urology" = "#A285D1",
                    "Gastrointestinal" = "#3E8ECC",
                    "Hepatobiliary" = "#94AABD",
                    "General surgery" = "#b1b1b1")
  
  ##### source report UI (within function env)
  source(paste0(script_dir, "mgmt_report_ui.R"), local = TRUE)
  
  return(report_ui)
}

batch_reports <- function(health_boards, date_from, date_to, output_dir) {
  
  #### create output directory if it doesn't yet exist
  if(!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  #### iterate across health boards and save out
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



# html utility functions --------------------------------------------------

# bslib containers that will display ggiraph plots nicely
ggiraph_card <- function(title, plot, ...){
  card(
    fill = FALSE,
    card_header(title),
    card_body(
      fillable=FALSE,
      plot
    ),
    card_body(...)
  )
}

ggiraph_nav <- function(tab_name, plot, ...){
  nav_panel(
    tab_name,
    card_body(
      fillable=FALSE,
      plot
    ),
    card_body(...)
  )
}

# useful for titles
capitalise_first <- function(x) {
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}


# save out html as single file
save_self_contained_html <- function(taglist, output_filepath) {
  #### Normalize output path
  output_filepath <- xfun::normalize_path(output_filepath)
  
  #### Create a temporary directory for intermediate files
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  #### Temporary HTML file inside temp_dir
  temp_html <- file.path(temp_dir, "temp.html")
  
  #### Save the taglist as HTML (with /libs folder if needed)
  htmltools::save_html(taglist, file = temp_html, libdir = file.path(temp_dir, "libs"))
  
  #### Remove <!DOCTYPE> lines for pandoc
  input_lines <- readLines(temp_html, warn = FALSE)
  writeLines(input_lines[!grepl("<!DOCTYPE", input_lines, fixed = TRUE)], temp_html)
  
  #### Create a minimal template for pandoc
  template <- tempfile(fileext = ".html")
  on.exit(unlink(template), add = TRUE)
  xfun::write_utf8("$body$", template)
  
  #### Determine pandoc format
  from <- if (rmarkdown::pandoc_available("1.17")) "markdown_strict" else "markdown"
  
  #### Convert to self-contained HTML
  rmarkdown::pandoc_convert(
    input = temp_html,
    from = from,
    output = output_filepath,
    options = c("--embed-resources", "--standalone", "--template", template)
  )
  
  invisible(output_filepath)
}

# base64 encode images to embed in single html file
img_base64 <- function(path, ...) {
  base64 <- base64enc::dataURI(file = path, mime = "image/png")
  htmltools::tags$img(src = base64, ...)
}
