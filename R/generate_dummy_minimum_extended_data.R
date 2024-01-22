

#' @title generate_dummy_minimum_extended_data
#' @description FUNCTION_DESCRIPTION
#' @param person_level_data_version PARAM_DESCRIPTION, Default: 'R12v1'
#' @param n_patients_minimum PARAM_DESCRIPTION, Default: 100
#' @param seed PARAM_DESCRIPTION, Default: 13
#' @param service_sector_data PARAM_DESCRIPTION, Default: NULL
#' @return list of created tables
#' @export
#' @importFrom dplyr mutate arrange desc distinct transmute left_join if_else select rename
#' @importFrom lubridate as_date dyears year
generate_dummy_minimum_extended_data <- function(
  person_level_data_version="R12v1",
  n_patients_minimum = 100,
  seed=13,
  service_sector_data = NULL
) {

  #set seed
  set.seed(seed)
  # load data
  data("summary_data_versions_list", package = "LongitudinalDummyDataGenerator")

  summary_tables <- summary_data_versions_list[[person_level_data_version]]


  # covariates has almost all the info, hence we use it to produces the minimum extended
  #covariates_data <- scanReportToTibble(summary_tables$covariates$ScanReport_covaraites, n_patients_minimum)

  # DF12 - Directly use the minimum extended Scan report rather than covariates
  minimum_data <- scanReportToTibble(summary_tables$minimum$ScanReport_minimum, n_patients_minimum)

  # correct types
  #covariates_data <- covariates_data |>
  #  dplyr::mutate(
  #    HEIGHT = as.integer(HEIGHT),
  #    WEIGHT = as.integer(WEIGHT),
  #    regionofbirth = as.integer(regionofbirth),
  #    movedabroad = as.integer(movedabroad),
  #    NUMBER_OF_OFFSPRING = as.integer(NUMBER_OF_OFFSPRING)
  #  )

  # Prepare the minimum data columns
  minimum_data <- minimum_data |>
    dplyr::mutate(
      BL_YEAR = as.integer(BL_YEAR),
      HEIGHT = as.integer(HEIGHT),
      WEIGHT = as.integer(WEIGHT),
      CURRENT_SMOKER = as.integer(CURRENT_SMOKER),
      EVER_SMOKER = as.integer(EVER_SMOKER),
      regionofbirth = as.integer(regionofbirth),
      movedabroad = as.integer(movedabroad),
      NUMBER_OF_OFFSPRING = as.integer(NUMBER_OF_OFFSPRING),
      DEATH = as.integer(DEATH)
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
    # covariates_data <- covariates_data |>
    #   dplyr::left_join(ss_summary, by="FINNGENID") |>
    #   dplyr::mutate(
    #     BL_YEAR = dplyr::if_else(is.na(bday), BL_YEAR, as.character( lubridate::year(bday+lubridate::dyears(as.numeric(BL_AGE))))),
    #     DEATH = if_else(is_death==TRUE, 1, 0),
    #     AGE_AT_DEATH_OR_END_OF_FOLLOWUP = dplyr::if_else(is.na(bday), AGE_AT_DEATH_OR_END_OF_FOLLOWUP, as.character(age_last)),
    #     AGE_AT_DEATH_OR_END_OF_FOLLOWUP2 = dplyr::if_else(is.na(bday), AGE_AT_DEATH_OR_END_OF_FOLLOWUP2, as.character(age_last)),
    #     #
    #     DEATH_AGE = dplyr::if_else(is_death, AGE_AT_DEATH_OR_END_OF_FOLLOWUP, as.character(NA) ),
    #     DEATH_YEAR = lubridate::year(bday+lubridate::dyears(as.numeric(DEATH_AGE)))
    #   )

    # Generated dummy minimum extended data will be based on service sector data
    # APPROX_BIRTH_DATE - Get `bday` from service sector data when `bday` is not missing
    # BL_YEAR - Calculated using `bday` by adding BL_AGE when `bday` is not missing
    # DEATH_FU_AGE -  Copy `age_last` from service sector data when `bday` is not missing
    # AGE_AT_DEATH_OR_END_OF_FOLLOWUP - Copy `age_last` from service sector data when `bday` is not missing
    #                                   Copy DEATH_FU_AGE when `bday` is missing as it should be same as DEATH_FU_AGE
    # DEATH_APPROX_EVENT_DAY - Calculated using `bday` by adding `age_last` when `bday` is not missing
    #                          Add DEATH_FU_AGE when APPROX_BIRTH_DATE by adding DEATH_FU_AGE when `bday` is missing
    minimum_data <- minimum_data |>
      dplyr::left_join(ss_summary, by = "FINNGENID") |>
      dplyr::mutate(
        APPROX_BIRTH_DATE = dplyr::if_else(is.na(bday), APPROX_BIRTH_DATE,  as.character(bday) ),
        BL_YEAR = dplyr::if_else(is.na(bday), lubridate::year(lubridate::as_date(APPROX_BIRTH_DATE)+lubridate::dyears(as.numeric(BL_AGE))),  lubridate::year(bday+lubridate::dyears(as.numeric(BL_AGE))) ),
        DEATH = if_else(is_death==TRUE, 1, 0),
        DEATH_FU_AGE = dplyr::if_else(is.na(bday), DEATH_FU_AGE, as.character(age_last)),
        AGE_AT_DEATH_OR_END_OF_FOLLOWUP = dplyr::if_else(is.na(bday), DEATH_FU_AGE, as.character(age_last)),
        #
        DEATH_APPROX_EVENT_DAY = dplyr::if_else(is.na(bday), lubridate::as_date(lubridate::as_date(APPROX_BIRTH_DATE)+lubridate::dyears(as.numeric(DEATH_FU_AGE))) ,lubridate::as_date(bday+lubridate::dyears(as.numeric(age_last))) )
      )
  }

  # minimun
  minimum_extended_data <- minimum_data |>
    dplyr::select(
      FINNGENID,
      APPROX_BIRTH_DATE,
      BL_YEAR, BL_AGE,  SEX,
      HEIGHT,  HEIGHT_AGE,
      WEIGHT,  WEIGHT_AGE,
      BMI,
      #SMOKE2,  SMOKE3,  SMOKE5,  SMOKE_AGE,
      SMOKE2,  SMOKE3,  SMOKE5,  CURRENT_SMOKER, EVER_SMOKER, SMOKE_AGE,
      regionofbirth,     regionofbirthname,
      movedabroad,     NUMBER_OF_OFFSPRING,
      #
      #COHORT = cohort,
      COHORT,
      #FU_END_AGE = AGE_AT_DEATH_OR_END_OF_FOLLOWUP,
      #DEATH, DEATH_AGE, DEATH_YEAR,
      DEATH, DEATH_APPROX_EVENT_DAY, DEATH_FU_AGE, AGE_AT_DEATH_OR_END_OF_FOLLOWUP
      #APPROX_BIRTH_DAY = bday
      )


 return(minimum_extended_data)

}




