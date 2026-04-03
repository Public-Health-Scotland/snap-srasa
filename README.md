# SRASA Read Me
Scottish National Audit Programme (SNAP) - Scottish Robotic Assisted Surgery Audit (SRASA)

## SRASA Purpose
The Scottish Robotic Assisted Surgery Audit (SRASA) will provide a national framework for monitoring the safety, quality, and outcomes of robotic assisted surgical procedures across NHS Scotland. 
Its purpose is to capture consistent, high-quality data on case mix, indications, intraoperative metrics, postoperative outcomes, and patient reported experience measures.

## How to install and run the project
On the snap-srasa GitHub code page, click on the down arrow on the green ‘Code’ button, then copy the SSH URL for the project. Then in Posit Workbench  go to File > New Project > Version control > Git then paste the URL into the box labelled ‘Repository URL’ (for more info on this see [RStudio Git and GitHub guidance](https://public-health-scotland.github.io/git-guide/rstudio-setup.html)). Next, indicate where you would like to create the project by clicking ‘Browse’ and then navigate to the ‘(11) Scripts’ folder in ‘srasa’ (the full file path can be shared upon request), and then create a folder and name this after yourself. Click on your named folder and hit ‘Choose’. This should clone the project to your chosen directory. 
Please note that the code in the snap-srasa project will only work for those with access to quality>srasa on PHS’s stats drive. 

## How to use the project
Make sure to open the project file and pull the latest version from ‘main’ via Git/GitHub each time you want to work on SRASA.  A Git command cheatsheet can be found [here](https://education.github.com/git-cheat-sheet-education.pdf). 
All required functions and packages are loaded on start-up via the .Rprofile file (this will require continual maintenance as the project develops).  

## Coding Approach
The snap-srasa project adheres to tidyverse coding style as laid out [here](https://style.tidyverse.org/). Functions and variable names should be written in snake_case. Functions should be written using docstring to write documentation and make each function searchable within the project (a quick guide [here](https://josephcrispell.github.io/2021/07/26/creating-R-docstring.html))

## Script Layout
### Control Scripts
Specific tasks have been written as functions whenever possible to make the code neat and easier to maintain. Key stages of data extraction and analysis are managed by ‘control’ scripts which organise the running order of project functions. The key control scripts are:

+ control_smr_extract.R - this pulls and prepares SRASA data from SMR01, with various data cleaning and wrangling steps. This needs to be run each month, after SMR01 data rollover is complete (status can be checked [here](http://nssproddb02-int.csa.scot.nhs.uk:7777/pls/dbquery/dbadmin.flatfiledates)). 
+ control_intuitive_extract.R - this loads manually-downloaded data files from Intuitive's customer portal, and binds them into a single dataset. The files should be downloaded and this script run monthly, along with control_smr_extract.R.

### Function Folders
The functions for the project are saved in various folders depending on their purpose:

+	01_control – control scripts to run key processes 
+	02_setup – functions to extract/load and save data
+	03_clean_modify - functions which add new variables or perform cleaning/editing 
+	04_reports – contains sub-folders for running regular reports
+	05_utilities - helper functions
+	06_data_requests - scripts written for BAU or repeatable data queries from stakeholders

## Authors
+ Bex Madden
+ Dylan Lewis
