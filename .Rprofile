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
                "Glasgow Royal Infirmary",
                "Golden Jubilee University National Hospital",
                "Ninewells Hospital",
                "Queen Elizabeth University Hospital",
                "Raigmore Hospital",
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

# Function
list.files("./02_setup/", full.names = TRUE) %>% 
  walk(source)

# Project screen
cat("

 Welcome to the SNAP SRASA project!
     _______                   ________ 
    |ooooooo|      ____       | __  __ |
    |[]+++[]|     [____]      ||SN||AP||
    |+ ___ +|    ](.)(.)[     ||__||__||
    |:|PHS|:|   ___\\--/___    |[][][][]|    
    |:|___|:|  |__|    |__|   |++++++++|
    |[]===[]|   | |____| |    | ______ |
    |||||||||   |_| __ |_|    ||R0B0T5||
    ||||||||| _ |_|[::]|_| __ ||______||    
    |_______|     |_||_|      |________|   
                  |_||_|                   
                 _|_||_|_                  
                |___||___|   
               
")


