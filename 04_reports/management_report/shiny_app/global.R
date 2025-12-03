##########################################.
#### SRASA Management Report - Global ####
##########################################.

#Author: Bex Madden
#Date:06/11/2025

### Load packages --------------------------------------------------------------
library(stringr)
library(XML)
library(dplyr)
library(shiny)
library(bslib)
library(htmlwidgets)
library(DT)
library(janitor)
library(ggplot2)
library(ggrepel)
library(ggiraph)
library(scales)
library(fontawesome)
library(tidyr)
library(shinymanager)
library(shinycssloaders)
library(glue)
library(magrittr)
library(forcats)
library(purrr)
library(arrow)

### Set directories ------------------------------------------------------------
bex_data_dir <- "../../../../../../(06) Testing/Bex_testing/" # temporary location
mgmt_report_dir <- "../../../../../../(04) Project Reports/Monthly Reports/"
mgmt_lookup_dir <- "../../../../../../(12) Data/lookups/"
mgmt_data_dir <- "../../../../../../(12) Data/management_report/"
#text_dir <- "./"

### Utility functions ----------------------------------------------------------
latest_date <- Sys.Date() %>% 
  lubridate::floor_date("month") %m-% months(2)

start_date <- latest_date %>% 
  lubridate::floor_date("month") %m-% months(12)

hosp_colours <- c("Aberdeen Royal Infirmary" = "#3F3685",
                  "Glasgow Royal Infirmary" = "#CDA1C9",
                  "Golden Jubilee University National Hospital" = "#0078D4",
                  "Ninewells Hospital" = "#C1DD93",
                  "Queen Elizabeth University Hospital" = "#1E7F84",
                  "Raigmore Hospital" = "#9F9BC2",
                  "Royal Infirmary of Edinburgh at Little France" = "#9B4393",
                  "St John's Hospital" = "#80BCEA",
                  "University Hospital Crosshouse" = "#83BB26",
                  "University Hospital Hairmyres" = "#8FBFC2",
                  "Victoria Hospital" = "#6B5C85",
                  "Western General Hospital" = "#E39C8C",
                  "Other Hospital Listed" = "#948DA3")

### Read in data ---------------------------------------------------------------

#### Utilisation data ----
util_procsmth <- read_parquet(paste0(mgmt_data_dir, "util_procsmth.parquet"))
util_procsday <- read_parquet(paste0(mgmt_data_dir, "util_procsday.parquet"))
util_procspec <- read_parquet(paste0(mgmt_data_dir, "util_procspec.parquet"))
  
#### Equity data ----
equity_agesex <- read_parquet(paste0(bex_data_dir, "mgmt_data/equity_agesex.parquet"))
equity_simd <- read_parquet(paste0(bex_data_dir, "mgmt_data/equity_simd.parquet"))
equity_urbrural <- read_parquet(paste0(bex_data_dir, "mgmt_data/equity_urbrural.parquet"))
equity_pemc <- read_parquet(paste0(bex_data_dir, "mgmt_data/equity_pemc.parquet"))

#### Outcome data ----

#### Text ----
#source(paste0(text_dir, "mgmt_report_text.R"))
# Or just write text in-page (aim is to keep it miniimal)

### Source scripts -------------------------------------------------------------
walk(list.files("./figures", full.names = TRUE), source)
walk(list.files("./pages", full.names = TRUE), source)
