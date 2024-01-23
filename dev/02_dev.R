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
usethis::use_package( "slider")
usethis::use_package( "rlang")



# data
usethis::use_data_raw(name="summary_data")

# fuctions
usethis::use_r("generate_dummy_service_sector_data")
usethis::use_r("generate_dummy_covariates_minimal_baseline_data")
usethis::use_r("scanReportToTibble")

usethis::use_r("generate_all_datasets_to_files")




# vignete
usethis::use_vignette("create_dummy_data")
