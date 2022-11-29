


generate_all_dummy_data_to_files<-function(
    output_folder,
    service_sector_data_version="R10v2",
    person_level_data_version="R10v1",
    n_patients_minimum = 100,
    per_patients_baseline = 0.99,
    per_patients_covariates = 0.95,
    per_patients_service_sector = 0.99,
    seed = 13,
    nTreaths=(parallel::detectCores() -2)
){





  n_patients_baseline <- round(n_patients_minimum*per_patients_baseline)
  n_patients_covaraites <- round(n_patients_minimum*per_patients_covariates)
  n_patients_service_sector <- round(n_patients_minimum*per_patients_service_sector)


  # prepare parallel loger
  logger <- ParallelLogger::createLogger(
    name = "DUMMY DATA GENERATOR",
    threshold = "INFO",
    appenders = list(
      ParallelLogger::createFileAppender(
        layout = ParallelLogger::layoutTimestamp,
        fileName = file.path(output_folder, "generate_dummy.log.txt")
      )
    )
  )
  ParallelLogger::registerLogger(logger)


  #
  # generate service sector data
  #
  ParallelLogger::logInfo("Generate service_sector_data")

  service_sector_data <- generate_dummy_service_sector_data(
    service_sector_data_version=service_sector_data_version,
    n_patients = n_patients_service_sector,
    n_cuts = nTreaths,
    seed = seed,
    nTreaths = nTreaths
  )


  #
  # generate person level data
  #
  ParallelLogger::logInfo("Generate person level data")

  res <- generate_dummy_covariates_minimal_baseline_data(
    person_level_data_version = person_level_data_version,
    n_patients_minimum = n_patients_minimum,
    seed = seed,
    service_sector_data = service_sector_data
  )

  minimum_data <-  res$minimum_data
  covariates_data <-  res$covariates_data
  baseline_data <-  res$baseline_data

  # re adjust number of subjects
  covariates_data <- covariates_data |> dplyr::sample_n(n_patients_covaraites)
  baseline_data <- baseline_data |> dplyr::sample_n(n_patients_baseline)




  ## SAVE
  ParallelLogger::logInfo("Save data in ", output_folder)

  file_name <- stringr::str_c("dummy_service_sector_", service_sector_data_version, ".txt" )
  service_sector_data |> readr::write_tsv(file.path(output_folder, file_name))

  file_name <- stringr::str_c("dummy_minimum_", person_level_data_version, ".txt" )
  minimum_data |> readr::write_tsv(file.path(output_folder, file_name))

  file_name <- stringr::str_c("dummy_covariates_", person_level_data_version, ".txt" )
  covariates_data |> readr::write_tsv(file.path(output_folder, file_name))

  file_name <- stringr::str_c("dummy_baseline_", person_level_data_version, ".txt" )
  baseline_data |> readr::write_tsv(file.path(output_folder, file_name))


  ParallelLogger::logInfo("Saved data")
  ParallelLogger::unregisterLogger(logger)


}
