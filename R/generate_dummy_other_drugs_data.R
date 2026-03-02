
#' @title generate_dummy_other_drugs_data
#' @description FUNCTION_DESCRIPTION
#' @param other_drugs_level_data_version PARAM_DESCRIPTION, Default: 'R12v1'
#' @param n_patients_minimum PARAM_DESCRIPTION, Default: 100
#' @param description PARAM_DESCRIPTION, Default: Vision dummy data generator
#' @param seed PARAM_DESCRIPTION, Default: 13
#' @return list of created tables
#' @export
#' @importFrom dplyr mutate arrange desc distinct transmute left_join if_else select rename stringi
#' @importFrom lubridate as_date dyears make_date days year
generate_dummy_other_drugs_data <- function(
    other_drugs_level_data_version="R12v1",
    n_patients_minimum = 100,
    seed=13
) {

  #set seed
  set.seed(seed)
  # load data
  data("summary_data_versions_list", package = "LongitudinalDummyDataGenerator")

  # process other drugs data
  other_drugs_tables <- summary_data_versions_list[[other_drugs_level_data_version]]
  other_drugs_data <- scanReportToTibble(other_drugs_tables$other_drugs$ScanReport_other_drugs, n_patients_minimum)

  # scanReportTibble creates a single entry for each person but we need to more events.
  # The OMOP_CONCEPT_ID and OMOP_CONCEPT_NAME columns should be NULL

  # OMOP_CONCEPT_ID and OMOP_CONCEPT_NAME columns should be NULL
  other_drugs_data <- other_drugs_data |>
    dplyr::mutate(OMOP_CONCEPT_ID = NA_integer_,
                  OMOP_CONCEPT_NAME = NA_character_)

  return(other_drugs_data)

}
