################################################################.
### SNAP SRASA - Monthly MGMT data derivation control script ###
#########################################################.

# Bex Madden 
# 26/03/2026

### Run data derivation scripts ------------------------------------------------
# need to be run in order as some rely on outputs of others

source("./04_reports/management_report/data_derivation/mgmt_utilisation_data.R")
source("./04_reports/management_report/data_derivation/mgmt_specialty_data.R")
source("./04_reports/management_report/data_derivation/mgmt_procedure_data.R")
source("./04_reports/management_report/data_derivation/mgmt_dataquality_data.R")