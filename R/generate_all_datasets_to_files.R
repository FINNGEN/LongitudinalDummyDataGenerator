
#' Generate All Dummy Data to Files
#'
#' Generates dummy data for service sector and person level data in the
#' "summary_data_versions_list" internal variable, and saves them to files.
#'
#' @param output_folder Path to the folder where the generated data will be saved.
#' @param service_sector_data_version Version of the service sector data (default: "R10v2").
#' @param person_level_data_version Version of the person level data (default: "R12v1").
#' @param n_patients_minimum Number of patients to generate (default: 100).
#' @param per_patients_service_sector Proportion of patients relative to n_patients_minimum to generate service sector data from  (default: 0.99).
#' @param seed Seed value for random number generation (default: 13).
#' @param nTreaths Number of threads for parallel processing (default: parallel::detectCores() - 2).
#' @importFrom ParallelLogger createLogger logInfo unregisterLogger createFileAppender layoutTimestamp
#' @importFrom stringr str_c
#' @importFrom readr write_tsv
#' @export
generate_all_dummy_data_to_files<-function(
    output_folder,
    service_sector_data_version="R112v1",
    person_level_data_version="R12v1",
    birth_mother_level_data_version="R12v1",
    vision_level_data_version="R12v1",
    n_patients_minimum = 100,
    per_patients_service_sector = 0.99,
    seed = 13,
    nTreaths=(parallel::detectCores() -2)
){


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

  minimum_extended <- generate_dummy_minimum_extended_data(
    person_level_data_version = person_level_data_version,
    n_patients_minimum = n_patients_minimum,
    seed = seed,
    service_sector_data = service_sector_data
  )

  #
  # generate birth mother level data
  #
  ParallelLogger::logInfo("Generate birth mother level data")

  birth_mother_extended <- generate_dummy_mothers_birth_register_data(
    birth_mother_level_data_version = birth_mother_level_data_version,
    n_patients_minimum = n_patients_minimum,
    seed = seed,
    service_sector_data = service_sector_data
  )


  #
  # generate vision level data
  #
  ParallelLogger::logInfo("Generate vision level data")

  vision_extended <- generate_dummy_vision_register_data(
    vision_level_data_version = vision_level_data_version,
    n_patients_minimum = n_patients_minimum,
    seed = seed
  )


  ## SAVE
  ParallelLogger::logInfo("Save data in ", output_folder)

  file_name <- stringr::str_c("dummy_service_sector_", service_sector_data_version, ".txt" )
  service_sector_data |> readr::write_tsv(file.path(output_folder, file_name))

  file_name <- stringr::str_c("dummy_minimum_extended_", person_level_data_version, ".txt" )
  minimum_extended |> readr::write_tsv(file.path(output_folder, file_name))

  file_name <- stringr::str_c("dummy_birth_mother_extended_", birth_mother_level_data_version, ".txt" )
  birth_mother_extended |> readr::write_tsv(file.path(output_folder, file_name))

  file_name <- stringr::str_c("dummy_vision_extended_", vision_level_data_version, ".txt" )
  vision_extended |> readr::write_tsv(file.path(output_folder, file_name))

  ParallelLogger::logInfo("Saved data")
  ParallelLogger::unregisterLogger(logger)


}
