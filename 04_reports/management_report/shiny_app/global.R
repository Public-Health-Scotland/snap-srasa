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
library(glue)
library(magrittr)
library(forcats)
library(purrr)
library(arrow)

### Set directories ------------------------------------------------------------
data_dir <- "../../../../../../(06) Testing/Bex_testing/" # temporary location
report_dir <- "../../../../../../(04) Project Reports/Monthly Reports/Management Report/"
lookup_dir <- "../../../../../../(12) Data/Lookups/"
#text_dir <- "./"

### Utility functions ----------------------------------------------------------
latest_year <- 2024

### Read in data ---------------------------------------------------------------

#### Utilisation data ----

#### Equity data ----
equity_agesex <- read_parquet(paste0(data_dir, "mgmt_data/equity_agesex.parquet"))
equity_simd <- read_parquet(paste0(data_dir, "mgmt_data/equity_simd.parquet"))
equity_urbrural <- read_parquet(paste0(data_dir, "mgmt_data/equity_urbrural.parquet"))
equity_pemc <- read_parquet(paste0(data_dir, "mgmt_data/equity_pemc.parquet"))

#### Outcome data ----

#### Text ----
#source(paste0(text_dir, "mgmt_report_text.R"))
# Or just write text in-page (aim is to keep it miniimal)

### Source scripts -------------------------------------------------------------
walk(list.files("./figures", full.names = TRUE), source)
walk(list.files("./pages", full.names = TRUE), source)
