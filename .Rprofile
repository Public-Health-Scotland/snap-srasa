######################################.
### SNAP SRASA project - R Profile ###
######################################.

#setwd("/conf/quality/srasa/(11) Scripts/Dylan/snap-srasa") #this isn't great as its to your branchy area not dynamic

# Source Renv
source("renv/activate.R")

# Load packages
library(docstring)
library(shiny)
library(ggiraph)
library(stringr)
library(dplyr)
library(lubridate)
library(odbc) # required for connection to data
library(tidyr)
library(janitor)
library(DT)
library(arrow)
library(XML)
library(bslib)
library(htmlwidgets)
library(ggplot2)
library(ggrepel)
library(scales)
library(fontawesome)
library(shinymanager)
library(shinycssloaders)
library(glue)
library(magrittr)
library(forcats)
library(purrr)
library(readr)
library(ggalluvial)
library(cli)
library(conflicted)
library(fuzzyjoin)
library(openxlsx)

library(phsverse)
library(phslookups)

# Directories
lookup_dir <- "../../../(12) Data/lookups/"
data_dir <- "../../../(12) Data/"

# Set constants
phase1_list <- read_csv(paste0(lookup_dir, "phase1_procedure_codes.csv")) %>% 
  dplyr::pull(code)

phase2_list <- read_csv(paste0(lookup_dir, "phase2_procedure_codes.csv")) %>% 
  dplyr::pull(code)

source("./02_setup/get_approach_lists.R")

hosp_order <- c("Aberdeen Royal Infirmary",
                "Dumfries & Galloway Royal Infirmary",
                "Forth Valley Royal Hospital",
                "Glasgow Royal Infirmary",
                "Golden Jubilee University National Hospital",
                "Ninewells Hospital",
                "Queen Elizabeth University Hospital",
                "Raigmore Hospital",
                "Royal Alexandra Hospital",
                "Royal Infirmary of Edinburgh at Little France",
                "St John's Hospital",
                "University Hospital Crosshouse",
                "University Hospital Hairmyres",
                "Victoria Hospital",
                "Western General Hospital",
                "Other Hospital Listed")

hb_order <- c("Ayrshire & Arran",
              "Borders",
              "Dumfries & Galloway",
              "Fife",
              "Forth Valley",
              "Grampian",
              "Greater Glasgow & Clyde",
              "Highland",
              "Lanarkshire",
              "Lothian",
              "Tayside",
              "Orkney",
              "Shetland",
              "Western Isles",
              "All")

age_group_order = c("0-4","5-9","10-14","15-19","20-24","25-29",
                    "30-34","35-39","40-44","45-49","50-54",
                    "55-59","60-64","65-69","70-74","75-79",
                    "80-84","85-89","90+")

#colours:
  col1 <- "#12436D"
  col1_lt <- "#94AABD"
  col2 <- "#28A197"
  col2_lt <- "#B4DEDB"
  col3 <- "#801650"
  col3_lt <- "#CCA2B9"
  col4 <- "#F46A25"
  col4_lt <- "#FBC3A8"
  col5 <- "#3D3D3D"
  col5_lt <- "#A8A8A8"
  col6 <- "#3E8ECC"
  col6_lt <- "#A8CCE8"
  col7 <- "#3F085C"
  col7_lt <- "#A285D1"
  col8 <- "#A285D1"


# Conflict preferences
conflict_prefer('filter','dplyr')
conflict_prefer('mutate','dplyr')
conflict_prefer('summarise', 'dplyr')
conflict_prefer('rename','dplyr')
conflict_prefer('count', 'dplyr')
conflict_prefer('arrange','dplyr')

conflict_prefer('select','dplyr')
conflict_prefer('case_when','dplyr')
conflict_prefer('order_by','dplyr')
conflict_prefer('lag','dplyr')
conflict_prefer('lead','dplyr')
conflict_prefer('first','dplyr')
conflict_prefer('last', 'dplyr')

conflict_prefer('yday', 'lubridate')

# Function
list.files(c("./02_setup",
             "./05_utilities"),
           pattern = "*.[rR]",
           full.names = TRUE) %>% 
  walk(source)

# Project screen
cat("\014\033[0;35m
Welcome to the SNAP SRASA project! \033[0m\n",
    "\033[0;32m
              ____
             [____]
            ](◦)(◦)[
           ___\\--/___
          |__| >< |__|
           | |____| |
           |_| __ |_|
           |_|[::]|_|
  vWv        |_||_|        vWv
  (_)        |_||_|        (_)
   |        _|_||_|_        |
__\\|/______|___||___|______\\|/__
               
\033[0m\n")



