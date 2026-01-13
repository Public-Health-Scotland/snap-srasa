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
script_dir <- "./04_reports/management_report/serverless_html/"
output_dir <- "/conf/quality/srasa/(04) Project Reports/Monthly Reports/Management Report/"



# functions ---------------------------------------------------------------

source(paste0(script_dir, "mgmt_report_html_funcs.R"))
source(paste0(script_dir, "mgmt_report_plots.R"))

# controls ----------------------------------------------------------------

date_to <- as.Date("2025-10-01")
date_from <- date_to %m-% months(12)
health_boards <- list(
  'Ayrshire & Arran',
  'Fife',
  'Grampian',
  'Greater Glasgow & Clyde',
  'Highland',
  'Lanarkshire',
  'Lothian',
  'Tayside'
)

# data --------------------------------------------------------------------

util_procsmth <- read_parquet(paste0(mgmt_data_dir, "util_procsmth.parquet")) |>
  filter(op_mth < date_to, op_mth >= date_from)
util_procsday <- read_parquet(paste0(mgmt_data_dir, "util_procsday.parquet")) |>
  filter(op_mth < date_to, op_mth >= date_from)

equity_agesex <- read_parquet(paste0(mgmt_data_dir, "equity_agesex.parquet"))
equity_agemean <- read_parquet(paste0(mgmt_data_dir, "equity_agemean.parquet"))
equity_simd <- read_parquet(paste0(mgmt_data_dir, "equity_simd.parquet"))
equity_resprop <- read_parquet(paste0(mgmt_data_dir, "equity_resprop.parquet"))

spec_procsmth <- read_parquet(paste0(mgmt_data_dir, "spec_procsmth.parquet")) |>
  filter(op_mth_year < date_to, op_mth_year >= date_from)


# reports -----------------------------------------------------------------

#test_report <- produce_report("Greater Glasgow & Clyde", "2024-10-01", "2025-10-01")
#save_self_contained_html(test_report, paste0(output_dir, "test_new_func.html"))

batch_reports(
  health_boards,
  date_from,
  date_to,
  paste0(output_dir, format(date_to, "%B %y"))
)
