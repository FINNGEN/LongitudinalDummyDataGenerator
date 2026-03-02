
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
#' @param n_cuts number of cuts to split the generation, Default: nTreaths (default: nTreaths).
#' @importFrom ParallelLogger createLogger logInfo unregisterLogger createFileAppender layoutTimestamp
#' @importFrom stringr str_c
#' @importFrom readr write_tsv
#' @export
generate_all_dummy_data_to_files<-function(
    output_folder,
    service_sector_data_version="R112v1",
    person_level_data_version="R12v1",
    kidney_level_data_version = "R12v1",
    birth_mother_level_data_version="R12v1",
    kanta_medication_delivery_level_data_version="R12v1",
    vision_level_data_version="R12v1",
    kanta_prescription_level_data_version="R12v1",
    other_drugs_level_data_version="R12v1",
    hla_imputed_level_data_version="R12v1",
    vaccination_level_data_version="R12v1",
    n_patients_minimum = 100,
    per_patients_service_sector = 0.99,
    seed = 13,
    nTreaths=(parallel::detectCores() -2),
    n_cuts = nTreaths
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
    n_cuts = n_cuts,
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
  # generate kidney level data
  #
  ParallelLogger::logInfo("Generate kidney level data")

  kidney_data <- generate_dummy_kidney_register_data(
    kidney_level_data_version = kidney_level_data_version,
    n_patients_minimum = n_patients_minimum,
    seed = seed
  )

  #
  # generate birth mother level data
  #
  ParallelLogger::logInfo("Generate birth mother level data")

  birth_mother_data <- generate_dummy_mothers_birth_register_data(
    birth_mother_level_data_version = birth_mother_level_data_version,
    n_patients_minimum = n_patients_minimum,
    seed = seed,
    service_sector_data = service_sector_data
  )

  #
  # generate kanta medication delivery level data
  #
  ParallelLogger::logInfo("Generate kanta medication delivery level data")

  kanta_medication_delivery_data <- generate_dummy_kanta_medication_delivery_data(
    kanta_medication_delivery_level_data_version = kanta_medication_delivery_level_data_version,
    n_patients_minimum = n_patients_minimum,
    seed = seed,
    service_sector_data = service_sector_data
  )


  #
  # generate vision level data
  #
  ParallelLogger::logInfo("Generate vision level data")

  vision_data <- generate_dummy_vision_register_data(
    vision_level_data_version = vision_level_data_version,
    n_patients_minimum = n_patients_minimum,
    seed = seed
  )

  #
  # generate kanta prescription level data
  #
  ParallelLogger::logInfo("Generate kanta prescription level data")

  kanta_prescription <- generate_dummy_kanta_prescription_data(
    kanta_prescription_level_data_version = kanta_prescription_level_data_version,
    n_patients_minimum = n_patients_minimum,
    seed = seed,
    kanta_medication_delivery = kanta_medication_delivery
  )

  #
  # generate other drugs data
  #
  ParallelLogger::logInfo("Generate other drugs level data")

  other_drugs <- generate_dummy_other_drugs_data(
    other_drugs_level_data_version = other_drugs_level_data_version,
    n_patients_minimum = n_patients_minimum,
    seed = seed
  )

  #
  # generate HLA imputed level data
  #
  ParallelLogger::logInfo("Generate HLA imputed level data")

  hla_imputed <- generate_dummy_hla_imputed_data(
    hla_imputed_level_data_version = hla_imputed_level_data_version,
    n_patients_minimum = n_patients_minimum,
    seed = seed
  )

  #
  # generate vaccination level data
  #
  ParallelLogger::logInfo("Generate vaccination level data")

  vaccination <- generate_dummy_vaccination_data(
    vaccination_level_data_version = vaccination_level_data_version,
    n_patients_minimum = n_patients_minimum,
    seed = seed,
    service_sector_data = service_sector_data,
    kanta_medication_delivery = kanta_medication_delivery
  )

  ## SAVE
  ParallelLogger::logInfo("Save data in ", output_folder)

  file_name <- stringr::str_c("dummy_service_sector_", service_sector_data_version, ".txt" )
  service_sector_data |> readr::write_tsv(file.path(output_folder, file_name))

  file_name <- stringr::str_c("dummy_minimum_extended_", person_level_data_version, ".txt" )
  minimum_extended |> readr::write_tsv(file.path(output_folder, file_name))

  file_name <- stringr::str_c("dummy_kidney_", kidney_level_data_version, ".txt" )
  kidney_data |> readr::write_tsv(file.path(output_folder, file_name))

  file_name <- stringr::str_c("dummy_birth_mother_", birth_mother_level_data_version, ".txt" )
  birth_mother_data |> readr::write_tsv(file.path(output_folder, file_name))

  file_name <- stringr::str_c("dummy_kanta_medication_delivery_", kanta_medication_delivery_level_data_version, ".txt" )
  kanta_medication_delivery_data |> readr::write_tsv(file.path(output_folder, file_name))

  file_name <- stringr::str_c("dummy_vision_", vision_level_data_version, ".txt" )
  vision_data |> readr::write_tsv(file.path(output_folder, file_name))

  file_name <- stringr::str_c("dummy_other_drugs_", other_drugs_level_data_version, ".txt" )
  other_drugs |> readr::write_tsv(file.path(output_folder, file_name))

  file_name <- stringr::str_c("dummy_kanta_prescription_", kanta_prescription_level_data_version, ".txt" )
  kanta_prescription |> readr::write_tsv(file.path(output_folder, file_name))

  file_name <- stringr::str_c("dummy_hla_imputed_", hla_imputed_level_data_version, ".txt" )
  hla_imputed |> readr::write_tsv(file.path(output_folder, file_name))

  file_name <- stringr::str_c("dummy_vaccination_", vaccination_level_data_version, ".txt" )
  vaccination |> readr::write_tsv(file.path(output_folder, file_name))

  ParallelLogger::logInfo("Saved data")
  ParallelLogger::unregisterLogger(logger)


}
