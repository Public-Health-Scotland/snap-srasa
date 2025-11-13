##########################################.
#### SRASA Management Report - Server ####
##########################################.

#Author: Bex Madden
#Date:06/11/2025


server <- function(input, output, session) {
  
  # Introduction page
  
  # Utilisation page

  #Equity page
  equity_agesex_server("equity_agesex")
  equity_simd_server("equity_simd")
  equity_urbrural_server("equity_urbrural")
  equity_pemc_server("equity_pemc")
  
  
  
  #Outcome page
  
}