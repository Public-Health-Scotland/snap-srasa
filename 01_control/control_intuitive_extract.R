##################################################################.
### SNAP SRASA - Monthly Intuitive data extract control script ###
##################################################################.

# Bex Madden & Dylan Lewis
# 01/12/2025

### get file download parameters in console ------------------------------------
get_int_dwnld_params()

### source scripts -------------------------------------------------------------
list.files("./02_setup/", full.names = TRUE) %>% 
  walk(source)
list.files("./03_clean_modify/", full.names = TRUE) %>% 
  walk(source)

### compile and save intuitive data --------------------------------------------
compile_intuitive_data() %>% 
  append_intuitive_data()
  