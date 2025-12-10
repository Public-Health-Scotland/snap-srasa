#' Create Shiny module with boilerplate code and title block
#'
#' @param name Module name.
#' @param page (Optional) Dashboard page.
#' @param type Module type, for picking correct folder and file name. Default is "plot".
#' @param folder Where to save the new module script. Default determined by type.
#' @param overwrite Overwrite existing module scripts?
#'
#' @examples add_module("example", page = "utilisation")
add_module <- function(name,
                       page = NULL,
                       type = "plot",
                       folder = NULL,
                       overwrite = FALSE){
  # default folder
  if(is.null(folder)){
    if(type == "plot"){
      folder <- "./04_reports/management_report/shiny_app/figures"
    } else {
      stop(str_glue("No default folder for module type {type}, please provide one"))
    }
  }
  
  if(!file.exists(folder)){
    stop(str_glue("No folder found at {folder}, please check the filepath"))
  }
  
  # template inputs
  author <- Sys.getenv("USER")
  date <- Sys.Date() |> format("%d/%m/%Y")
  modname <- ifelse(is.null(page),
                    name,
                    str_glue("{page}_{name}"))
  title <- ifelse(is.null(page),
                  str_glue("{name} {type}"), 
                  str_glue("{page} - {name} {type}")) |>
    str_to_title()
  
  # add padding for title block
  titlepadding <- str_c(rep("#", times = str_width(title)), collapse = "")
  
  # read in template
  template <- read_lines("./05_utilities/module_template.txt")
  
  # apply inputs
  out <- map(template, ~str_glue(.x)) |>
    unlist()
  
  # write out
  filepath <- str_glue("{folder}/{modname}_{type}.R")
   
  if(file.exists(filepath) & overwrite == FALSE){
    stop(str_glue("File already exists at {filepath}"))
  }

  writeLines(out, con = filepath)
  message(str_glue("New module created at {filepath}"))
}