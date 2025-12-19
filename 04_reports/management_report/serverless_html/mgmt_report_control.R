library(bslib)
library(dplyr)
library(ggplot2)
library(ggiraph)
library(arrow)
library(stringr)
library(lubridate)
library(phsstyles)
library(snappack)


# folders -----------------------------------------------------------------

mgmt_data_dir <- paste0(data_dir, "management_report/")
output_dir <- "./04_reports/management_report/serverless_html/"

# data --------------------------------------------------------------------

util_procsmth <- read_parquet(paste0(mgmt_data_dir, "util_procsmth.parquet"))
util_procsday <- read_parquet(paste0(mgmt_data_dir, "util_procsday.parquet"))

equity_agesex <- read_parquet(paste0(mgmt_data_dir, "equity_agesex.parquet"))
equity_agemean <- read_parquet(paste0(mgmt_data_dir, "equity_agemean.parquet"))
equity_simd <- read_parquet(paste0(mgmt_data_dir, "equity_simd.parquet"))
equity_resprop <- read_parquet(paste0(mgmt_data_dir, "equity_resprop.parquet"))

spec_procsmth <- read_parquet(paste0(mgmt_data_dir, "spec_procsmth.parquet"))


# functions ---------------------------------------------------------------

source(paste0(output_dir, "mgmt_report_html_funcs.R"))
source(paste0(output_dir, "mgmt_report_plots.R"))

# report ------------------------------------------------------------------

test_report <- produce_report("Greater Glasgow & Clyde")
#htmltools::save_html(test_report, "test_report.html")
