## code to prepare `summary_data` dataset goes here

library(tidyverse)

summary_data_path <- file.path(getwd(), "data-raw/summary_data")

summary_data_folders <- list.dirs(summary_data_path, full.names = F)[-1]

summary_data_versions_list <- list()

for(folder in summary_data_folders){

  tables_list <- list()

  col_types = cols(
    start_year = col_integer(),
    start_age = col_integer(),
    observation_years = col_integer(),
    n_patients = col_integer(),
    .default = col_double()
  )
  count_periods <- read_tsv(file.path(summary_data_path, folder, "count_periods.tsv"), col_types = col_types)
  if(folder=="DF6v2"){
    # TEMP: forgot stats for DEATH, copy them from CANC at the moment
    count_periods <-count_periods %>%  mutate(DEATH_logmean  = CANC_logmean, DEATH_logsd = CANC_logsd  )
  }
  tables_list[["count_periods"]]<-count_periods

  col_types = cols(
    SOURCE = col_character(),
    VOCAB = col_character(),
    EVENT_YEAR = col_integer(),
    n_events = col_integer(),
    n_patients = col_integer()
  )
  tables_list[["event_year_count"]] <- read_tsv(file.path(summary_data_path, folder, "event_year_count.tsv"), col_types = col_types)

  col_types = cols(
    SOURCE = col_character(),
    CODE1 = col_character(),
    CODE2 = col_character(),
    CODE3 = col_character(),
    VOCAB = col_character(),
    n_events = col_integer(),
    n_patients = col_integer()
  )
  tables_list[["codes_count"]] <- read_tsv(file.path(summary_data_path, folder, "codes_count.tsv"), col_types = col_types)

  col_types = cols(
    SOURCE = col_character(),
    CODE4 = col_character(),
    n_events = col_integer(),
    n_patients = col_integer()
  )
  tables_list[["code4_count"]]<- read_tsv(file.path(summary_data_path, folder, "code4_count.tsv"), col_types = col_types)

  col_types = cols(
    SOURCE = col_character(),
    LEVEL = col_character(),
    n_events = col_integer(),
    n_patients = col_integer()
  )
  tables_list[["level_count"]] <- read_tsv(file.path(summary_data_path, folder, "level_count.tsv"), col_types = col_types)


  summary_data_versions_list[[folder]]<-tables_list


}

usethis::use_data(summary_data_versions_list, overwrite = TRUE)
