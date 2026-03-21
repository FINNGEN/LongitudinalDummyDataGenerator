library(tidyverse)

summary_data_path <- file.path(getwd(), "data-raw/summary_data")

summary_data_folders <- list.dirs(summary_data_path, full.names = F, recursive = F)

summary_data_versions_list <- list()

for(folder in summary_data_folders){

  tables_list <- list()


  #
  # SERVICE SECOTR DATA
  #
  if(dir.exists(file.path(summary_data_path, folder, "service_sector"))){

    ## observation_periods_summary_summary.tsv

    col_types = cols(
      first_visit_year = col_integer(),
      first_visit_age = col_integer(),
      years_to_last_visit = col_integer(),
      n_patients = col_integer(),
      INPAT_logmean = col_double(),
      INPAT_logsd = col_double(),
      PURCH_logmean = col_double(),
      PURCH_logsd = col_double(),
      OUTPAT_logmean = col_double(),
      OUTPAT_logsd = col_double(),
      OPER_OUT_logmean = col_double(),
      OPER_OUT_logsd = col_double(),
      PRIM_OUT_logmean = col_double(),
      PRIM_OUT_logsd = col_double(),
      OPER_IN_logmean = col_double(),
      OPER_IN_logsd = col_double(),
      REIMB_logmean = col_double(),
      REIMB_logsd = col_double(),
      CANC_logmean = col_double(),
      CANC_logsd = col_double(),
      n_person_death = col_double()
    )
    tables_list[["observation_periods"]] <- read_tsv(
      file.path(summary_data_path, folder, "service_sector", "observation_periods_summary_summary.tsv"),
      col_types = col_types)


    ## count_visits_per_patient.tsv

    col_types = cols(
      visit_year_bin = col_character(),
      SOURCE = col_character(),
      CODE5 = col_character(),
      CODE6 = col_character(),
      CODE7 = col_double(),
      CODE8 = col_character(),
      CODE9 = col_character(),
      n_visits = col_integer(),
      n_events_per_visit_logmean = col_double(),
      n_events_per_visit_logsd = col_double(),
      n_patients = col_integer()
    )

    tables_list[["count_visits_per_patient"]] <- read_tsv(
      file.path(summary_data_path, folder, "service_sector", "count_visits_per_patient.tsv"),
      col_types = col_types)


    ## SOURCE_VOCAB_EVENT_YEAR_summary.tsv

    col_types =cols(
      SOURCE = col_character(),
      vocabulary = col_character(),
      event_year = col_integer(),
      n_events = col_integer(),
      n_patients = col_integer()
    )

    tables_list[["SOURCE_VOCAB_EVENT_YEAR"]] <- read_tsv(
      file.path(summary_data_path, folder, "service_sector", "SOURCE_VOCAB_EVENT_YEAR_summary.tsv"),
      col_types = col_types)


    ## CODE1_CODE2_CODE3_vocabulary_summary.tsv

    col_types = cols(
      SOURCE = col_character(),
      CODE1 = col_character(),
      CODE2 = col_character(),
      CODE3 = col_character(),
      vocabulary = col_character(),
      n_events = col_integer(),
      n_patients = col_integer()
    )

    tables_list[["CODE1_CODE2_CODE3_vocabulary"]]<- read_tsv(
      file.path(summary_data_path, folder, "service_sector", "CODE1_CODE2_CODE3_vocabulary_summary.tsv"),
      col_types = col_types)


    ## CODE4_summary.tsv

    col_types = cols(
      SOURCE = col_character(),
      CODE4 = col_character(),
      n_events = col_integer(),
      n_patients = col_integer()
    )

    tables_list[["CODE4"]] <- read_tsv(
      file.path(summary_data_path, folder, "service_sector", "CODE4_summary.tsv"),
      col_types = col_types)

    summary_data_versions_list[[folder]]<- list(service_sector=tables_list)
  }


  #
  # BIRTH MOTHER DATA
  #
  if(dir.exists(file.path(summary_data_path, folder, "birth"))){
    ## birth_mother scan report

    col_types = cols(
      .default = col_character()
    )

    ScanReport_birth_mother <- read_csv(
      file.path(summary_data_path, folder, "birth", "ScanReport_birth_mother.csv"),
      col_types = col_types)

    ScanReport_birth_mother <- ScanReport_birth_mother |> mutate_at(vars(starts_with("Frequency...")), as.integer)

    summary_data_versions_list[[folder]][["birth_mother"]]<- list(ScanReport_birth_mother=ScanReport_birth_mother)

  }

  #
  # VISION DATA
  #
  if(dir.exists(file.path(summary_data_path, folder, "vision"))){
    ## vision scan report

    col_types = cols(
      .default = col_character()
    )

    ScanReport_vision <- read_csv(
      file.path(summary_data_path, folder, "vision", "ScanReport_vision.csv"),
      col_types = col_types)

    ScanReport_vision <- ScanReport_vision |> mutate_at(vars(starts_with("Frequency...")), as.integer)

    summary_data_versions_list[[folder]][["vision"]] = list(ScanReport_vision=ScanReport_vision)

  }

  #
  # KIDNEY DATA
  #
  if(dir.exists(file.path(summary_data_path, folder, "kidney"))){
    ## vision scan report

    col_types = cols(
      .default = col_character()
    )

    ScanReport_kidney <- read_csv(
      file.path(summary_data_path, folder, "kidney", "ScanReport_kidney.csv"),
      col_types = col_types)

    ScanReport_kidney <- ScanReport_kidney |> mutate_at(vars(starts_with("Frequency...")), as.integer)

    summary_data_versions_list[[folder]][["kidney"]] = list(ScanReport_kidney=ScanReport_kidney)

  }

  #
  # KANTA MEDICATION DELIVERY
  #
  if(dir.exists(file.path(summary_data_path, folder, "kanta_medication_delivery"))){
    ## minimum scanreport

    col_types = cols(
      .default = col_character()
    )

    ScanReport_kanta_medication_delivery <- read_csv(
      file.path(summary_data_path, folder, "kanta_medication_delivery", "ScanReport_kanta_medication_delivery.csv"),
      col_types = col_types)

    ScanReport_kanta_medication_delivery <- ScanReport_kanta_medication_delivery |> mutate_at(vars(starts_with("Frequency...")), as.integer)

    summary_data_versions_list[[folder]][["kanta_medication_delivery"]] = list(ScanReport_kanta_medication_delivery=ScanReport_kanta_medication_delivery)
  }

  #
  # MINIMUM DATA
  #
  if(dir.exists(file.path(summary_data_path, folder, "minimum"))){
    ## minimum scanreport

    col_types = cols(
      .default = col_character()
    )

    ScanReport_minimum <- read_csv(
      file.path(summary_data_path, folder, "minimum", "ScanReport_minimum.csv"),
      col_types = col_types)

    ScanReport_minimum <- ScanReport_minimum |> mutate_at(vars(starts_with("Frequency...")), as.integer)

    summary_data_versions_list[[folder]][["minimum"]] = list(ScanReport_minimum=ScanReport_minimum)
  }

  #
  # KANTA PRESCRIPTION
  #
  if(dir.exists(file.path(summary_data_path, folder, "kanta_prescription"))){
    ## Kanta prescription scanreport

    col_types = cols(
      .default = col_character()
    )

    ScanReport_kanta_prescription <- read_csv(
      file.path(summary_data_path, folder, "kanta_prescription", "ScanReport_kanta_prescription.csv"),
      col_types = col_types)

    ScanReport_kanta_prescription <- ScanReport_kanta_prescription |> mutate_at(vars(starts_with("Frequency...")), as.integer)

    summary_data_versions_list[[folder]][["kanta_prescription"]] = list(ScanReport_kanta_prescription=ScanReport_kanta_prescription)
  }

  #
  # OTHER DRUGS
  #
  if(dir.exists(file.path(summary_data_path, folder, "other_drugs"))){
    ## other drugs scanreport

    col_types = cols(
      .default = col_character()
    )

    ScanReport_other_drugs <- read_csv(
      file.path(summary_data_path, folder, "other_drugs", "ScanReport_other_drugs.csv"),
      col_types = col_types)

    ScanReport_other_drugs <- ScanReport_other_drugs |> mutate_at(vars(starts_with("Frequency...")), as.integer)

    summary_data_versions_list[[folder]][["other_drugs"]] = list(ScanReport_other_drugs=ScanReport_other_drugs)
  }

  #
  # HLA Imputed
  #
  if(dir.exists(file.path(summary_data_path, folder, "hla_imputed"))){
    ## hla imputed scanreport

    col_types = cols(
      .default = col_character()
    )

    ScanReport_hla_imputed <- read_csv(
      file.path(summary_data_path, folder, "hla_imputed", "Allele_HLA_counts.csv"),
      col_types = col_types)

    summary_data_versions_list[[folder]][["hla_imputed"]] = list(ScanReport_hla_imputed=ScanReport_hla_imputed)
  }

  #
  # Vaccination
  #
  if(dir.exists(file.path(summary_data_path, folder, "vaccination"))){
    ## vaccination scanreport

    col_types = cols(
      .default = col_character()
    )

    ScanReport_vaccination <- read_csv(
      file.path(summary_data_path, folder, "vaccination", "ScanReport_vaccination.csv"),
      col_types = col_types)

    ScanReport_vaccination <- ScanReport_vaccination |> mutate_at(vars(starts_with("Frequency...")), as.integer)

    summary_data_versions_list[[folder]][["vaccination"]] = list(ScanReport_vaccination=ScanReport_vaccination)
  }

  #
  # spirometry
  #
  if(dir.exists(file.path(summary_data_path, folder, "spirometry"))){
    ## spirometry scanreport

    col_types = cols(
      .default = col_character()
    )

    ScanReport_spirometry <- read_csv(
      file.path(summary_data_path, folder, "spirometry", "ScanReport_spirometry.csv"),
      col_types = col_types)

    ScanReport_spirometry <- ScanReport_spirometry |> mutate_at(vars(starts_with("Frequency...")), as.integer)

    summary_data_versions_list[[folder]][["spirometry"]] = list(ScanReport_spirometry=ScanReport_spirometry)
  }

  #
  # covariates
  #
  if(dir.exists(file.path(summary_data_path, folder, "covariates"))){
    ## covariates scanreport

    col_types = cols(
      .default = col_character()
    )

    ScanReport_covariates <- read_csv(
      file.path(summary_data_path, folder, "covariates", "ScanReport_covariates.csv"),
      col_types = col_types)

    ScanReport_covariates <- ScanReport_covariates |> mutate_at(vars(starts_with("Frequency...")), as.integer)

    summary_data_versions_list[[folder]][["covariates"]] = list(ScanReport_covariates=ScanReport_covariates)
  }


}

usethis::use_data(summary_data_versions_list, overwrite = TRUE)

