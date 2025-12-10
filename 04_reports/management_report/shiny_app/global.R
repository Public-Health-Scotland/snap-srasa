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
# funnel plot code - will eventually replace with package
source("/conf/quality/SHA_team/SNAP/Analysts/Dylan/functions/funnel_lines.R")

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

### Read in data ---------------------------------------------------------------

#### Utilisation data ----
util_procsmth <- read_parquet(paste0(mgmt_data_dir, "util_procsmth.parquet"))
util_procsday <- read_parquet(paste0(mgmt_data_dir, "util_procsday.parquet"))

#### Equity data ----
equity_agesex <- read_parquet(paste0(mgmt_data_dir, "equity_agesex.parquet"))
equity_agemean <- read_parquet(paste0(mgmt_data_dir, "equity_agemean.parquet"))
equity_simd <- read_parquet(paste0(mgmt_data_dir, "equity_simd.parquet"))
# equity_urbrural <- read_parquet(paste0(mgmt_data_dir, "equity_urbrural.parquet"))
# equity_pemc <- read_parquet(paste0(mgmt_data_dir, "equity_pemc.parquet"))

#### Specialty data ----
spec_procsmth <- read_parquet(paste0(mgmt_data_dir, "spec_procsmth.parquet"))
#equity_specsday <- read_parquet(paste0(mgmt_data_dir, "equity_specsday.parquet"))

#### Data Quality data ----
dq_compprocs <- read_parquet(paste0(mgmt_data_dir, "dq_compprocs.parquet"))

#### Outcome data ----

#### Text ----
#source(paste0(text_dir, "mgmt_report_text.R"))
# Or just write text in-page (aim is to keep it miniimal)

### Source scripts -------------------------------------------------------------
walk(list.files("./figures", full.names = TRUE), source)
walk(list.files("./pages", full.names = TRUE), source)
