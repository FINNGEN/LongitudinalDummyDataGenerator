
#' @title generate_dummy_vision_data
#' @description FUNCTION_DESCRIPTION
#' @param vision_level_data_version PARAM_DESCRIPTION, Default: 'R10v3'
#' @param n_patients_minimum PARAM_DESCRIPTION, Default: 100
#' @param description PARAM_DESCRIPTION, Default: Vision dummy data generator
#' @param seed PARAM_DESCRIPTION, Default: 13
#' @param minimum_extended PARAM_DESCRIPTION, Default: NULL
#' @return list of created tables
#' @export
#' @importFrom dplyr mutate arrange desc distinct transmute left_join if_else select rename
#' @importFrom lubridate as_date dyears make_date days year
generate_dummy_vision_register_data <- function(
    vision_level_data_version="R12v4",
    n_patients_minimum = 100,
    seed=13,
    minimum_extended = NULL
) {

  #set seed
  set.seed(seed)
  # load data
  data("summary_data_versions_list", package = "LongitudinalDummyDataGenerator")

  # Person level information to match for SEX variable in FUTURE. IT IS NEEDED NOW
  #summary_tables <- summary_data_versions_list[[person_level_data_version]]
  # covariates has almost all the info, hence we use it to produces the minimum extended
  #covariates_data <- scanReportToTibble(summary_tables$covariates$ScanReport_covaraites, n_patients_minimum)

  # process vision data
  vision_tables <- summary_data_versions_list[[vision_level_data_version]]
  vision_data <- scanReportToTibble(vision_tables$vision$ScanReport_vision, n_patients_minimum)

  # EVENT_AGE has only 0 with frequency > 50 within original data which is why by default we will get only 0.0
  # To fix this we will assign random EVENT_AGE double numbers > 0.0 and < DEATH_FU_AGE from minimum_phenotype
  if(!is.null(minimum_extended)){

    # Get DEATH_FU_AGE from minimum_extended data
    vision_data <- vision_data |>
      dplyr::left_join(minimum_extended |> dplyr::select(FINNGENID, DEATH_FU_AGE), by = "FINNGENID") |>
      dplyr::rowwise() |>
      dplyr::mutate(EVENT_AGE = sprintf("%.2f", runif(1, min = 0.01, max = as.double(DEATH_FU_AGE)))) |>
      dplyr::select(-DEATH_FU_AGE)

  }

  return(vision_data)

}
