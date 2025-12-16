produce_report <- function(hb, start_date = NULL, latest_date = NULL){
 
  ##### Hospitals & Health Board
  health_board <- str_replace(hb, "and", "&") #coerce to ampersand
  hospcodes <- phsopendata::get_resource("c698f450-eeed-41a0-88f7-c1e40a568acc") |>
    mutate(HealthBoardName = phsmethods::match_area(HealthBoard) |> str_replace("and", "&"))
  
  hospitals <- hospcodes$HospitalName[hospcodes$HealthBoardName == health_board]
  
  
  ##### Dates ----
  if(is.null(latest_date)){
    latest_date <- Sys.Date() %>% 
      floor_date("month") %m-% months(2)
  } else {
    latest_date <- as_date(latest_date)
  }
  
  
  if(is.null(start_date)){
    start_date <- latest_date %>% 
      floor_date("month") %m-% months(12)
  } else {
    start_date <- as_date(latest_date)
  }
  
  ##### Colours ----
  hosp_colours <- c("Aberdeen Royal Infirmary" = "#12436D",
                    "Glasgow Royal Infirmary" = "#94AABD",
                    "Golden Jubilee University National Hospital" = "#28A197",
                    "Ninewells Hospital" = "#B4DEDB",
                    "Queen Elizabeth University Hospital" = "#801650",
                    "Raigmore Hospital" = "#CCA2B9",
                    "Royal Infirmary of Edinburgh at Little France" = "#F46A25",
                    "St John's Hospital" = "#FBC3A8",
                    "University Hospital Crosshouse" = "#3E8ECC",
                    "University Hospital Hairmyres" = "#A8CCE8",
                    "Victoria Hospital" = "#3F085C",
                    "Western General Hospital" = "#A285D1",
                    "Other Hospital Listed" = "#3D3D3D")
  
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
    title = "Scottish Robotic-Assisted Surgery Audit (SRASA) - Management Information",
    navbar_options = navbar_options(
      bg = "#80BCEA",
      theme = "light"
    ),
    fillable = FALSE,
    nav_panel(
      "P1",
      ggiraph_card(
        title = str_glue("Total number of RAS procedures monthly by hospital ({start_date} - {latest_date})"),
        plot = make_plot_util_procsmth(hospitals, hosp_colours)
      )
    )
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
               opts_tooltip(css = "border-radius:5px; padding:5px", opacity = 1, use_fill = TRUE),
               opts_hover(css = "opacity:0.8", nearest_distance = 100),
               opts_hover_inv(css = "opacity:0.4")))
    )
  )
}