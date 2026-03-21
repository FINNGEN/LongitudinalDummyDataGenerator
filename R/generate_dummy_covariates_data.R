
#' @title generate_dummy_covariates_date
#' @description FUNCTION_DESCRIPTION
#' @param covariates_level_data_version PARAM_DESCRIPTION, Default: 'R12v1'
#' @param n_patients_minimum PARAM_DESCRIPTION, Default: 100
#' @param description PARAM_DESCRIPTION, Default: covariates dummy data generator
#' @param seed PARAM_DESCRIPTION, Default: 13
#' @param minimum_extended PARAM_DESCRIPTION, Default: NULL
#' @return list of created tables
#' @export
#' @importFrom dplyr mutate arrange desc distinct transmute left_join if_else select coalesce
#' @importFrom lubridate as_date dyears make_date days year
generate_dummy_covariates_register_data <- function(
    covariates_level_data_version="R12v1",
    n_patients_minimum = 3000,
    seed=13,
    minimum_extended = NULL
) {

  #set seed
  set.seed(seed)
  # load data
  data("summary_data_versions_list", package = "LongitudinalDummyDataGenerator")

  # process covariates data
  covariates_tables <- summary_data_versions_list[[covariates_level_data_version]]
  covariates_data <- scanReportToTibble(covariates_tables$covariates$ScanReport_covariates, n_patients_minimum)

  # Replace BL_YEAR, BL_AGE, SEX, DEATH, DEATH_FU_AGE and AGE_AT_DEATH_OR_END_OF_FOLLOWUP columns from minimum_extended
  if(!is.null(minimum_extended)){

    minimum_extended <- minimum_extended |>
      dplyr::mutate(BL_YEAR = as.character(BL_YEAR),
                    BL_AGE = as.character(BL_AGE),
                    SEX = as.character(SEX),
                    DEATH = as.character(DEATH),
                    DEATH_FU_AGE = as.character(DEATH_FU_AGE),
                    AGE_AT_DEATH_OR_END_OF_FOLLOWUP = as.character(AGE_AT_DEATH_OR_END_OF_FOLLOWUP)
                   )

    # Replace the columns from minimum_extended
    covariates_data <- covariates_data |>
      dplyr::mutate(BL_YEAR = dplyr::coalesce(minimum_extended$BL_YEAR[match(FINNGENID, minimum_extended$FINNGENID)], BL_YEAR),
                    BL_AGE = dplyr::coalesce(minimum_extended$BL_AGE[match(FINNGENID, minimum_extended$FINNGENID)], BL_AGE),
                    SEX = dplyr::coalesce(minimum_extended$SEX[match(FINNGENID, minimum_extended$FINNGENID)], SEX),
                    DEATH = dplyr::coalesce(minimum_extended$DEATH[match(FINNGENID, minimum_extended$FINNGENID)], DEATH),
                    DEATH_FU_AGE = dplyr::coalesce(minimum_extended$DEATH_FU_AGE[match(FINNGENID, minimum_extended$FINNGENID)], DEATH_FU_AGE),
                    AGE_AT_DEATH_OR_END_OF_FOLLOWUP = dplyr::coalesce(minimum_extended$AGE_AT_DEATH_OR_END_OF_FOLLOWUP[match(FINNGENID, minimum_extended$FINNGENID)], AGE_AT_DEATH_OR_END_OF_FOLLOWUP)
                   )

  }

  return(covariates_data)

}
