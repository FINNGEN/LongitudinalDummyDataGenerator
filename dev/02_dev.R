###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
usethis::use_tibble()

## Add one line by package you want to add as dependency
usethis::use_package( "dplyr" )
usethis::use_package( "stringr")
usethis::use_package( "readr")
usethis::use_package( "lubridate")
usethis::use_package( "ParallelLogger")
usethis::use_package( "scales")



# data
usethis::use_data_raw(name="summary_data")

# fuctions
usethis::use_r("generate_dummy_longitudinal_data")
