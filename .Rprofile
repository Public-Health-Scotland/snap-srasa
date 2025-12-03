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

# Set constants
candidate_codes <- read_csv("../../../(12) Data/lookups/ras_procedure_codes.csv") %>%  #move all this to r profile
  rename(op_specialty = specialty)
candidate_list <- dplyr::pull(candidate_codes, code)

approach_codes <- read_csv("../../../(12) Data/lookups/approach_codes.csv") 
approach_list <- dplyr::pull(approach_codes, approach_code)

robotics_list <- approach_codes$approach_code[!is.na(approach_codes$robotic)]
minimal_list <- approach_codes$approach_code[!is.na(approach_codes$minimal)]
robotic_conv_list <- approach_codes$approach_code[!is.na(approach_codes$robotic_conv)]
minimal_conv_list <- approach_codes$approach_code[!is.na(approach_codes$minimal_conv)]

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


# Directories
lookup_dir <- "../../../(12) Data/lookups/"
data_dir <- "../../../(12) Data/"

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


