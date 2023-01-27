

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param person_level_data_version PARAM_DESCRIPTION, Default: 'R10v1'
#' @param n_patients_minimum PARAM_DESCRIPTION, Default: 100
#' @param seed PARAM_DESCRIPTION, Default: 13
#' @param service_sector_data PARAM_DESCRIPTION, Default: NULL
#' @return list of created tables
#' @export
#' @importFrom dplyr mutate arrange desc distinct transmute left_join if_else select rename
#' @importFrom lubridate as_date dyears year
generate_dummy_covariates_minimal_baseline_data <- function(
  person_level_data_version="R10v1",
  n_patients_minimum = 100,
  seed=13,
  service_sector_data = NULL
) {

  #set seed
  set.seed(seed)
  # load data
  data("summary_data_versions_list", package = "LongitudinalDummyDataGenerator")

  summary_tables <- summary_data_versions_list[[person_level_data_version]]


  # covariates has almost all the info, hence we use it to produces the minimum and baseline tables
  covariates_data <- scanReportToTibble(summary_tables$covariates$ScanReport_covaraites, n_patients_minimum)

  # correct types
  covariates_data <- covariates_data |>
    dplyr::mutate(
      HEIGHT = as.integer(HEIGHT),
      WEIGHT = as.integer(WEIGHT),
      regionofbirth = as.integer(regionofbirth),
      movedabroad = as.integer(movedabroad),
      NUMBER_OF_OFFSPRING = as.integer(NUMBER_OF_OFFSPRING)
    )

  # if a service sector data is given, use it to sync some of the columns with covarate data
  if(!is.null(service_sector_data)){

    # summary from service_sector_data
    ss_summary <- service_sector_data |>
      dplyr::arrange(dplyr::desc(APPROX_EVENT_DAY)) |>
      dplyr::distinct(FINNGENID, .keep_all = T) |>
      dplyr::transmute(
        FINNGENID = FINNGENID,
        bday = lubridate::as_date(APPROX_EVENT_DAY - lubridate::dyears(EVENT_AGE)),
        is_death = SOURCE=="DEATH",
        age_last = EVENT_AGE
      )

    # correct column based on service sector data
    covariates_data <- covariates_data |>
      dplyr::left_join(ss_summary, by="FINNGENID") |>
      dplyr::mutate(
        BL_YEAR = dplyr::if_else(is.na(bday), BL_YEAR, as.character( lubridate::year(bday+lubridate::dyears(as.numeric(BL_AGE))))),
        DEATH = if_else(is_death==TRUE, 1, 0),
        AGE_AT_DEATH_OR_END_OF_FOLLOWUP = dplyr::if_else(is.na(bday), AGE_AT_DEATH_OR_END_OF_FOLLOWUP, as.character(age_last)),
        AGE_AT_DEATH_OR_END_OF_FOLLOWUP2 = dplyr::if_else(is.na(bday), AGE_AT_DEATH_OR_END_OF_FOLLOWUP2, as.character(age_last))
      )
  }

  # minimun
  minimum_data <- covariates_data |>
    dplyr::select(
      FINNGENID,
      BL_YEAR, BL_AGE,  SEX,
      HEIGHT,  HEIGHT_AGE,
      WEIGHT,  WEIGHT_AGE,
      SMOKE2,  SMOKE3,  SMOKE5,  SMOKE_AGE,
      regionofbirth,     regionofbirthname,
      movedabroad,     NUMBER_OF_OFFSPRING  )


  # baseline data
  baseline_data <- covariates_data |>
    dplyr::select(
      FINNGENID,
      BL_AGE, BL_YEAR,
      FU_END_AGE = AGE_AT_DEATH_OR_END_OF_FOLLOWUP,
      SEX
    )

  # covarites data
  covariates_data <- covariates_data |>
    dplyr::rename(FID=FINNGENID)

  # return
  res <- list(
    minimum_data = minimum_data,
    covariates_data = covariates_data,
    baseline_data = baseline_data
  )

  return(res)

}




